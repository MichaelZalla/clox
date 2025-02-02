#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "chunk.h"
#include "common.h"
#include "compiler.h"
#include "object.h"
#include "scanner.h"
#include "value.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef struct
{
	Token currentToken;
	Token previousToken;
	bool hadError;
	bool panicMode;
} Parser;

typedef enum
{
	PREC_NONE,
	PREC_ASSIGNMENT, // =
	PREC_OR,				 // or
	PREC_AND,				 // and
	PREC_EQUALITY,	 // == !=
	PREC_COMPARISON, // < <= > >=
	PREC_TERM,			 // + -
	PREC_FACTOR,		 // * /
	PREC_UNARY,			 // - !
	PREC_CALL,			 // . ()
	PREC_PRIMARY
} Precedence;

typedef void (*ParseFn)(bool canAssign);

// Represents a single row in the parsing table.
typedef struct
{
	ParseFn prefix;
	ParseFn infix;
	Precedence precedence;
} ParseRule;

typedef struct
{
	Token name; // Stores a copy of the Token struct for the identifier.
	int depth;
} Local;

typedef enum
{
	TYPE_FUNCTION,
	TYPE_SCRIPT
} FunctionType;

typedef struct
{
	// References the Compiler associated with this compiler's enclosing function.
	struct Compiler *enclosing;

	ObjFunction *function;
	FunctionType type;

	Local locals[UINT8_COUNT];
	int localCount;
	int scopeDepth;
} Compiler;

Parser parser;

Compiler *current = NULL;

static Chunk *currentChunk()
{
	return &current->function->chunk;
}

static void errorAt(Token *token, const char *message)
{
	if (parser.panicMode)
	{
		return;
	}

	parser.panicMode = true;

	fprintf(stderr, "[line %d] Error", token->line);

	if (token->type == TOKEN_EOF)
	{
		fprintf(stderr, " at end");
	}
	else if (token->type == TOKEN_ERROR)
	{
		// Print nothing (as an ERROR token has no lexeme associated).
	}
	else
	{
		// Print the token's lexeme.
		fprintf(stderr, " at '%.*s'", token->length, token->start);
	}

	fprintf(stderr, ": %s\n", message);

	parser.hadError = true;
}

static void error(const char *message)
{
	errorAt(&parser.previousToken, message);
}

static void errorAtCurrent(const char *message)
{
	errorAt(&parser.currentToken, message);
}

static void advance()
{
	// Cache the (previously) current token.
	parser.previousToken = parser.currentToken;

	// Consumes the next token, and loops through any resulting errors.
	for (;;)
	{
		parser.currentToken = scanToken();

		if (parser.currentToken.type != TOKEN_ERROR)
		{
			break;
		}

		// Informs the user that the scanner encountered a scanning error.
		errorAtCurrent(parser.currentToken.start);
	}
}

static void consume(TokenType type, const char *message)
{
	if (parser.currentToken.type == type)
	{
		advance();

		return;
	}

	errorAtCurrent(message);
}

static bool check(TokenType type)
{
	return parser.currentToken.type == type;
}

static bool match(TokenType type)
{
	if (!check(type))
	{
		return false;
	}

	advance();

	return true;
}

static void emitByte(uint8_t byte)
{
	// Writes the given byte (which may be an opcode,or operand).
	// Passing line information lets us associate a runtime error with a line.
	writeChunk(currentChunk(), byte, parser.previousToken.line);
}

static void emitBytes(uint8_t byte1, uint8_t byte2)
{
	// Convenience function to write an opcode, followed by its operand.
	emitByte(byte1);
	emitByte(byte2);
}

static void emitLoop(int loopStart)
{
	// Emit a backwards jump.
	emitByte(OP_LOOP);

	// We grow our backwards jump offset by 2 to account for the 16-bit operand.
	int currentOffsetFromLoopStart = currentChunk()->count - loopStart + 2;

	// Range check.
	if (currentOffsetFromLoopStart > UINT16_MAX)
	{
		error("Loop body is too large!");
	}

	// Writes the negative offset as a 16-bit `OP_LOOP` operand.

	int highOrderBits = (currentOffsetFromLoopStart >> 8) & 0xff;
	int lowOrderBits = (currentOffsetFromLoopStart) & 0xff;

	emitByte(highOrderBits);
	emitByte(lowOrderBits);
}

static int emitJump(uint8_t instruction)
{
	// By making `instruction` an argument to `emitJump()`, we can emit different
	// jump instructions that follow the same instruction-data format.
	emitByte(instruction);

	// Emits a (temporary) placeholder address; this will be back-patched to hold
	// the address that represents the first instruction following an "if" block.

	// We use 16 bits to encode the address, allowing users to skip a maximum of
	// 65,535 bytes' worth of instructions per conditional.
	emitByte(0xff);
	emitByte(0xff);

	// Returns the address (in our bytecode array) of the jump instruction above.
	return currentChunk()->count - 2;
}

static void emitReturn()
{
	emitByte(OP_NIL);
	emitByte(OP_RETURN);
}

static uint8_t makeConstant(Value value)
{
	Chunk *chunk = currentChunk();

	int index = addConstant(chunk, value);

	if (index > UINT8_MAX)
	{
		error("Too many constants in one chunk!");

		return 0;
	}

	return (uint8_t)index;
}

static void emitConstant(Value value)
{
	uint8_t index = makeConstant(value);

	emitBytes(OP_CONSTANT, index);
}

static void patchJump(int conditionalAddress)
{
	// Computes the size of the "then" block statement, in bytes.
	int currentAddress = currentChunk()->count;

	// Subtracts 2 bytes to account for the 16-bit jump offset (to be patched).
	int bytesToSkip = currentAddress - conditionalAddress - 2;

	// Range check.
	if (bytesToSkip > UINT16_MAX)
	{
		error("Too many instructions to jump over.");
	}

	// Patches the 16-bit jump offset (higher 8 bits and lower 8 bits).
	int highOrderBits = (bytesToSkip >> 8) & 0xff;
	int lowOrderBits = (bytesToSkip) & 0xff;

	currentChunk()->code[conditionalAddress] = highOrderBits;
	currentChunk()->code[conditionalAddress + 1] = lowOrderBits;
}

static void initCompiler(Compiler *compiler, FunctionType type)
{
	compiler->function = NULL;
	compiler->type = type;

	compiler->localCount = 0;
	compiler->scopeDepth = 0;

	// More GC paranoia.
	compiler->function = newFunction();

	// Grows the compiler stack, updating `current` (i.e., head).
	compiler->enclosing = (struct Compiler *)current;
	current = (Compiler *)compiler;

	if (type != TYPE_SCRIPT)
	{
		// Function objects are runtime values, so they can outlive the source code;
		// token lexemes point into the source code, so their references are bound
		// to the lifetime of the source code (bytes). We're careful to copy the
		// function name's original name, to the heap (as an ObjString).

		current->function->name = copyString(
				parser.previousToken.start,
				parser.previousToken.length);
	}

	// Reserves this compiler's first slot in `locals[]` for its own internal use.
	Local *local = &current->locals[current->localCount];

	current->localCount += 1;

	local->depth = 0;
	local->name.start = ""; // Empty name prevents users from referencing it.
	local->name.length = 0;
}

static ObjFunction *endCompiler()
{
	emitReturn();

	ObjFunction *function = current->function;

#ifdef DEBUG_PRINT_CODE
	if (!parser.hadError)
	{
		bool isScript = function->name == NULL;

		disassembleChunk(currentChunk(), isScript ? "<script>" : function->name->chars);
	}
#endif

	current = (Compiler *)current->enclosing;

	return function;
}

static void beginScope()
{
	current->scopeDepth += 1;
}

static void endScope()
{
	current->scopeDepth -= 1;

	// Rolls back the "locals" stack, "freeing" any locals in this scope.
	while (current->localCount > 0 && current->locals[current->localCount - 1].depth > current->scopeDepth)
	{
		// Relinquishes this local's slot on the VM's runtime stack.
		emitByte(OP_POP);

		// Relinquishes this local's position in the compiler's "locals" stack.
		current->localCount -= 1;
	}
}

// Forward declarations.
static void expression();
static void statement();
static void declaration();
static ParseRule *getRule(TokenType type);
static void parsePrecedence(Precedence precedence);
static void function(FunctionType type);

static uint8_t identifierConstant(Token *name)
{
	// Produces an ObjString from the identifier token's lexeme.
	ObjString *identifierString = copyString(name->start, name->length);

	Value value = OBJ_VAL((Obj *)identifierString);

	// Inserts the Value (string) into the chunk's constants table.
	return makeConstant(value);
}

static bool identifiersEqual(Token *a, Token *b)
{
	if (a->length != b->length)
	{
		return false;
	}

	return memcmp(a->start, b->start, a->length) == 0;
}

static int resolveLocal(Compiler *compiler, Token *name)
{
	// Walks the list of locals that are currently in scope; if one has the same
	// name as the identifier token, the identifier must refer to that variable.

	for (int i = compiler->localCount - 1; i >= 0; i--)
	{
		Local *local = &compiler->locals[i];

		if (identifiersEqual(&local->name, name))
		{
			// Checks whether or not the local is fully defined yet, or just declared.
			if (local->depth == -1)
			{
				error("Can't read local variable inside its own initializer.");
			}

			return i;
		}
	}

	return -1;
}

static void addLocal(Token name)
{
	// Bounds check.
	if (current->localCount == UINT8_COUNT)
	{
		error("Too many local variables in scope.");

		return;
	}

	// Writes the local (name) to the current compiler's stack of locals.
	Local *local = &current->locals[current->localCount];

	// Increments the "locals" stack pointer.
	current->localCount += 1;

	local->name = name;
	local->depth = -1; // Sentinel.
}

static void declareVariable()
{
	// In Lox, a variable is "declared" when it is added to its scope.

	if (current->scopeDepth == 0)
	{
		// Local variables can only be declared inside of a block scope. A top-level
		// variable declaration would be a global; registering of globals is handled
		// by `identifierConstant()`.

		// Global variables are also late-bound in Lox, so the compiler doesn't
		// track which declarations for them it has seen.

		return;
	}

	Token *name = &parser.previousToken;

	// Start at the end of `locals[]` and work backwards, looking for an
	// existing variables with the same name.
	for (int i = current->localCount - 1; i >= 0; i--)
	{
		Local *local = &current->locals[i];

		if (local->depth != -1 && local->depth < current->scopeDepth)
		{
			// We've traversed into a different scope, or we've reached the beginning
			// of the locals array.

			break;
		}

		// Here, we know that `local` exists in the same scope as `name`.
		if (identifiersEqual(name, &local->name))
		{
			error("Already a variable with this name in scope.");
		}
	}

	// Adds this local to the compiler's list of variables in the current scope.
	addLocal(*name);
}

static uint8_t parseVariable(const char *errorMessage)
{
	consume(TOKEN_IDENTIFIER, errorMessage);

	declareVariable();

	if (current->scopeDepth > 0)
	{
		// At runtime, local variables aren't resolved by name—so there's no need
		// to insert a name into the constants table; we return a dummy index of 0.

		return 0;
	}

	// Returns the new constant's index in the current chunk's constants table.
	return identifierConstant(&parser.previousToken);
}

static void markInitialized()
{
	if (current->scopeDepth == 0)
	{
		// If we're initializing a function name at the top level of a script, then
		// we aren't declaring a local—and, therefore, we have no "depth" to update.

		return;
	}

	Local *lastDeclaredLocal = &current->locals[current->localCount - 1];

	lastDeclaredLocal->depth = current->scopeDepth;
}

static void defineVariable(uint8_t globalVarConstantIndex)
{
	// In Lox, a variable is "defined" when it becomes available for use.

	if (current->scopeDepth > 0)
	{
		// There is no code emitted to "create" a local variable at runtime;
		// `defineVariable()` is called by `variableDeclaration()`, which will have
		// already placed an initializer value (i.e., temporary, or nil) on top of
		// the stack. This local variable effectively represents that stack address.

		// At this point, the variable's initializer expression will be compiled;
		// we can now mark this variable as "initialized".
		markInitialized();

		return;
	}

	emitBytes(OP_DEFINE_GLOBAL, globalVarConstantIndex);
}

static uint8_t argumentList()
{
	uint8_t argCount = 0;

	// Compiles (and counts) any arguments passed between the pair of parentheses.
	if (!check(TOKEN_RIGHT_PAREN))
	{
		do
		{
			expression();

			// Checks limit.
			if (argCount == 255)
			{
				error("Encountered more than 255 arguments.");
			}

			argCount += 1;
		} while (match(TOKEN_COMMA));
	}

	consume(TOKEN_RIGHT_PAREN, "Expected ')' after arguments.");

	return argCount;
}

static void and_(bool canAssign)
{
	// The logical `and` operator strings together multiple expressions, and
	// operates like a control-flow expression (if, for, etc); the left-hand
	// operand may short-circuit evaluation of the right-hand operand, if the
	// left-hand operand is found to be falsey.

	// Therefore we may jump over the right-hand expression, conditionally.

	// Note: If we do jump, we leave the falsey expression on the stack, because
	// logical operators (like "and") form compound expressions that reduce to
	// simple boolean expressions—and expressions should produce a stack result.

	int jumpOverRightOperand = emitJump(OP_JUMP_IF_FALSE);

	// Pops the left-hand operand off the stack, as the result of this "and" will
	// be the result of the right-hand operand expression.
	emitByte(OP_POP);

	// Parse the right-hand operand expression.
	parsePrecedence(PREC_AND);

	// Patch the "over right" jump offset (above) in case we can jump.
	patchJump(jumpOverRightOperand);
}

static void binary(bool canAssign)
{
	// Assumes that we've already consumed the tokens for the entire left-hand
	// operand (expression), as well as the token for the (infix) binary operator,
	// and that we have that stored in `previousToken`.
	TokenType operatorType = parser.previousToken.type;

	ParseRule *rule = getRule(operatorType);

	// Compiles the right operand (expression).

	// Each binary operator's right-hand operand precedence is one level higher
	// than its own; this is because the binary operators are left-associative:
	//
	// Here, there are no higher-precedence expressions to consume on the right...
	//
	// 1 + 2 + 3 + 4
	// ((1 + 2) + 3) + 4
	//
	// ...but here, there are...
	//
	// 1 + 2 * 3 - 4
	// (1 + (2 * 3)) - 4

	parsePrecedence((Precedence)(rule->precedence + 1));

	// Emits the bytecode instruction that performs the given binary operation.
	switch (operatorType)
	{
	case TOKEN_BANG_EQUAL:
		emitBytes(OP_EQUAL, OP_NOT);
		break;
	case TOKEN_EQUAL_EQUAL:
		emitByte(OP_EQUAL);
		break;
	case TOKEN_GREATER:
		emitByte(OP_GREATER);
		break;
	case TOKEN_GREATER_EQUAL:
		// Evaluates the negative of the inverse operation (i.e,. less).
		emitBytes(OP_LESS, OP_NOT);
		break;
	case TOKEN_LESS:
		emitByte(OP_LESS);
		break;
	case TOKEN_LESS_EQUAL:
		// Evaluates the negative of the inverse operation (i.e,. greater).
		emitBytes(OP_GREATER, OP_NOT);
		break;
	case TOKEN_PLUS:
		emitByte(OP_ADD);
		break;
	case TOKEN_MINUS:
		emitByte(OP_SUBTRACT);
		break;
	case TOKEN_STAR:
		emitByte(OP_MULTIPLY);
		break;
	case TOKEN_SLASH:
		emitByte(OP_DIVIDE);
		break;
	default:
		// Unreachable.
		return;
	}
}

static void call(bool canAssign)
{
	// Compiles any arguments being passed in this call.
	uint8_t argCount = argumentList();

	emitBytes(OP_CALL, argCount);
}

static void literal(bool canAssign)
{
	switch (parser.previousToken.type)
	{
	case TOKEN_TRUE:
		emitByte(OP_TRUE);
		break;

	case TOKEN_FALSE:
		emitByte(OP_FALSE);
		break;

	case TOKEN_NIL:
		emitByte(OP_NIL);
		break;

	default:
		// Unreachable.
		return;
	}
}

static void grouping(bool canAssign)
{
	// Compiles the expression between the pair of parentheses.
	expression();

	// Consumes the closing parentheses.
	consume(TOKEN_RIGHT_PAREN, "Expect ')' after grouping expression.");
}

static void number(bool canAssign)
{
	// Assumes that we've already consumed the token for this number literal,
	// and have stored it in `previousToken`; we convert the lexeme to a double
	// using `strtod()`.
	double value = strtod(parser.previousToken.start, NULL);

	emitConstant(NUMBER_VAL(value));
}

static void or_(bool canAssign)
{
	// The logical "or" operator strings together multiple expressions, and
	// operates like a control-flow expression  (if, for, etc). As an
	// optimization, we can skip evaluation of the right-hand operand if we find
	// the left-hand operand to be truthy.

	// When the left-hand side is truthy, we jump over the right-hand operand;
	// if it's falsey, we "jump over the jump", evaluating the right-hand side.

	int jumpOverJump = emitJump(OP_JUMP_IF_FALSE);

	int jumpOverRightOperand = emitJump(OP_JUMP);

	patchJump(jumpOverJump);

	// Pops the left-hand operand off the stack, as the result of this "or" will
	// be the result of the right-hand operand expression.
	emitByte(OP_POP);

	// Parse the right-hand operand expression.
	parsePrecedence(PREC_OR);

	// Patch the "over right" jump offset (above) in case the left-hand is truthy.
	patchJump(jumpOverRightOperand);
}

static void string(bool canAssign)
{
	// Captures the string's characters directly from the lexeme—omitting the
	// opening and closing quotes.

	const char *start = parser.previousToken.start + 1;

	int length = parser.previousToken.length - 2;

	Obj *object = (Obj *)copyString(start, length);

	Value value = OBJ_VAL(object);

	emitConstant(value);
}

static void namedVariable(Token name, bool canAssign)
{
	uint8_t getOp, setOp;

	// Tries to find a local variable with the given name.
	int arg = resolveLocal(current, &name);

	if (arg != -1)
	{
		// A local variable with this name was found.

		getOp = OP_GET_LOCAL;
		setOp = OP_SET_LOCAL;
	}
	else
	{
		// Assume `name` to be a global identifier.

		// In this case, `arg` represents an index into the current chunk's constant
		// table, representing this global variable's identifier (name).
		arg = identifierConstant(&name);

		getOp = OP_GET_GLOBAL;
		setOp = OP_SET_GLOBAL;
	}

	if (canAssign && match(TOKEN_EQUAL))
	{
		expression();

		emitBytes(setOp, arg);
	}
	else
	{
		emitBytes(getOp, arg);
	}
}

static void variable(bool canAssign)
{
	namedVariable(parser.previousToken, canAssign);
}

static void unary(bool canAssign)
{
	// Assumes that we've already consumed the token for the unary operator,
	// and have it stored in `previousToken`.
	TokenType operatorType = parser.previousToken.type;

	// Compile the operand.

	// Using `PREC_UNARY` precedence allows Lox users to nest unary expressions,
	// (e.g., `!!someDoubleNegative`).
	parsePrecedence(PREC_UNARY);

	// Emit the operator instruction.
	switch (operatorType)
	{
	case TOKEN_BANG:
		emitByte(OP_NOT);
		break;

	case TOKEN_MINUS:
		emitByte(OP_NEGATE);
		break;

	default:
		// Unreachable.
		return;
	}
}

// Pratt parser table.
ParseRule rules[] = {
		[TOKEN_LEFT_PAREN] = {grouping, call, PREC_CALL},
		[TOKEN_RIGHT_PAREN] = {NULL, NULL, PREC_NONE},
		[TOKEN_LEFT_BRACE] = {NULL, NULL, PREC_NONE},
		[TOKEN_RIGHT_BRACE] = {NULL, NULL, PREC_NONE},
		[TOKEN_COMMA] = {NULL, NULL, PREC_NONE},
		[TOKEN_DOT] = {NULL, NULL, PREC_NONE},

		[TOKEN_MINUS] = {unary, binary, PREC_TERM},
		[TOKEN_PLUS] = {NULL, binary, PREC_TERM},

		[TOKEN_SEMICOLON] = {NULL, NULL, PREC_NONE},

		[TOKEN_SLASH] = {NULL, binary, PREC_FACTOR},
		[TOKEN_STAR] = {NULL, binary, PREC_FACTOR},

		[TOKEN_BANG] = {unary, NULL, PREC_NONE},
		[TOKEN_BANG_EQUAL] = {NULL, binary, PREC_EQUALITY},
		[TOKEN_EQUAL] = {NULL, NULL, PREC_NONE},
		[TOKEN_EQUAL_EQUAL] = {NULL, binary, PREC_EQUALITY},
		[TOKEN_GREATER] = {NULL, binary, PREC_COMPARISON},
		[TOKEN_GREATER_EQUAL] = {NULL, binary, PREC_COMPARISON},
		[TOKEN_LESS] = {NULL, binary, PREC_COMPARISON},
		[TOKEN_LESS_EQUAL] = {NULL, binary, PREC_COMPARISON},

		[TOKEN_IDENTIFIER] = {variable, NULL, PREC_NONE},
		[TOKEN_STRING] = {string, NULL, PREC_NONE},
		[TOKEN_NUMBER] = {number, NULL, PREC_NONE},

		[TOKEN_AND] = {NULL, and_, PREC_AND},
		[TOKEN_CLASS] = {NULL, NULL, PREC_NONE},
		[TOKEN_ELSE] = {NULL, NULL, PREC_NONE},
		[TOKEN_FALSE] = {literal, NULL, PREC_NONE},
		[TOKEN_FOR] = {NULL, NULL, PREC_NONE},
		[TOKEN_FUN] = {NULL, NULL, PREC_NONE},
		[TOKEN_IF] = {NULL, NULL, PREC_NONE},
		[TOKEN_NIL] = {literal, NULL, PREC_NONE},
		[TOKEN_OR] = {NULL, or_, PREC_OR},
		[TOKEN_PRINT] = {NULL, NULL, PREC_NONE},
		[TOKEN_RETURN] = {NULL, NULL, PREC_NONE},
		[TOKEN_SUPER] = {NULL, NULL, PREC_NONE},
		[TOKEN_THIS] = {NULL, NULL, PREC_NONE},
		[TOKEN_TRUE] = {literal, NULL, PREC_NONE},
		[TOKEN_VAR] = {NULL, NULL, PREC_NONE},
		[TOKEN_WHILE] = {NULL, NULL, PREC_NONE},
		[TOKEN_ERROR] = {NULL, NULL, PREC_NONE},
		[TOKEN_EOF] = {NULL, NULL, PREC_NONE},
};

static void parsePrecedence(Precedence precedence)
{
	advance();

	// Look up a prefix parser for the current token (the first token is always
	// going to belong to a prefix expression, by definition of our grammar).

	ParseRule *previousRule = getRule(parser.previousToken.type);

	ParseFn prefixRuleFn = previousRule->prefix;

	if (prefixRuleFn == NULL)
	{
		error("Expect expression.");

		return;
	}

	bool canAssign = precedence <= PREC_ASSIGNMENT;

	// Compiles the rest of the prefix expression, consuming any other tokens it
	// needs, and then returns back.
	prefixRuleFn(canAssign);

	while (precedence <= getRule(parser.currentToken.type)->precedence)
	{
		// Look for an infix parser for the next token.

		advance();

		ParseRule *previousRule = getRule(parser.previousToken.type);

		ParseFn infixRuleFn = previousRule->infix;

		// Consume the infix operator and hand off control to the infix parsing
		// function that we found.
		infixRuleFn(canAssign);
	}

	// A non-assignable expression was on the left side of a TOKEN_EQUAL.
	if (canAssign && match(TOKEN_EQUAL))
	{
		error("Invalid assignment target.");
	}
}

static ParseRule *getRule(TokenType type)
{
	return &rules[type];
}

static void expression()
{
	// Note: Each bytecode instruction that is part of an expression has a "stack
	// effect" that measures how the instruction grows or shrinks the stack. The
	// net stack effect of all instructions in a given expression is always 1.
	//
	//       |+1  +1  -1  +1  -1| = 1
	// e.g., ((1   2   +)  3   /)
	//
	// Put in other words, any expression produces 1 net-new value on the stack.

	parsePrecedence(PREC_ASSIGNMENT);
}

static void block()
{
	// A left brace has already been consumed (in `statement()` or `function()`).

	while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF))
	{
		declaration();
	}

	consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void functionDeclaration()
{
	// Functions are first-class values in Lox, and a function declaration simply
	// creates and stores one in a newly declared variable.

	// Parses the function's name, just like any other variable declaration.
	uint8_t globalFunctionName = parseVariable("Expect function name.");

	// Immediately marks the function's identifier as initialized, as this will
	// allow a user to refer to it from inside the function's own body (i.e.,
	// permits a recursive function definition).
	markInitialized();

	function(TYPE_FUNCTION);

	defineVariable(globalFunctionName);
}

static void function(FunctionType type)
{
	// Creates a separate Compiler, on the C stack, for the function currently
	// being compiled; as we compile the function body, all new bytecode will be
	// emitted into the Chunk owned by the ObjFunction owned by this Compiler.
	Compiler compiler;

	// Sets `compiler` as our `current` compiler.
	initCompiler(&compiler, type);
	beginScope();

	// Parses the function's parameters, enclosed in parentheses.

	consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");

	if (!check(TOKEN_RIGHT_PAREN))
	{
		do
		{
			// Counts the new parameter in arity.
			current->function->arity += 1;

			// Range check.
			if (current->function->arity > 255)
			{
				errorAtCurrent("Function has more than 255 parameters.");
			}

			// A function parameter is simply a local variable declared in the outer-
			// most lexical scope of the function body; for function declarations,
			// there are no parameter initializers (or default values) in Lox—so
			// these will be initialized later, as part of function calls.

			uint8_t parameter = parseVariable("Expect parameter name.");

			defineVariable(parameter);
		} while (match(TOKEN_COMMA));
	}

	consume(TOKEN_RIGHT_PAREN, "Expect ')' after function parameters.");

	// Parses the function body (block).
	consume(TOKEN_LEFT_BRACE, "Expect '{' before function body.");
	block();

	// Yields the new ObjFunction from `compile()`.
	ObjFunction *function = endCompiler();

	// Stores the compiled ObjFunction in the _surrounding_ function's constants
	// table (i.e., `compiler.enclosing->function->chunk->constants`).
	emitBytes(OP_CONSTANT, makeConstant(OBJ_VAL((Obj *)function)));
}

static void variableDeclaration()
{
	// Declares a new global or local variable, compiling its initializer
	// expression if provided; when no initializer is provided, the variable is
	// initialized to `VAL_NIL`.

	// If the declaration occurs in the top-level scope (i.e., depth zero), it's
	// treated as a global variable, and its identifier is registered in the
	// current chunk's constants table. If the declaration is nested inside of
	// a scope, then it's treated as a local variable; local variables are not
	// registered in any runtime data structure (like `constants[]`), but, rather
	// the compiler performs some bookkeeping in the context of the current scope.

	uint8_t globalIdentifierConstantIndexOrZero = parseVariable("Expect variable name.");

	if (match(TOKEN_EQUAL))
	{
		// Variable declaration with an initializer; leaves the evaluated result
		// on the stack.
		expression();
	}
	else
	{
		// Variable declaration without an initializer; leaves `nil` on the stack.
		emitByte(OP_NIL);
	}

	// Consumes the semicolon terminating the declaration.
	consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

	defineVariable(globalIdentifierConstantIndexOrZero);
}

static void expressionStatement()
{
	// Compiles the expression.
	expression();

	// Consumes the semicolon terminating the expression statement.
	consume(TOKEN_SEMICOLON, "Expect ';' after expression.");

	// Discards the result.
	emitByte(OP_POP);
}

static void forStatement()
{
	// Creates a new lexical scope, as our "for" statement may have an initializer
	// that declares new variables; those should be scoped to the "for" statement.
	beginScope();

	// Compiles the "for" statement's optional clauses.
	consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");

	// Compiles the initializer clause.
	if (match(TOKEN_SEMICOLON))
	{
		// No initializer.
	}
	else if (match(TOKEN_VAR))
	{
		// Variable declaration as initializer.
		variableDeclaration();
	}
	else
	{
		// Expression as initializer.
		// Note: We call `expressionStatement()` rather than `expression()`, here,
		// as non-declaration initializers only serve to produce side-effects—so we
		// don't want to keep the expression's result on the stack. The call to
		// `expressionStatement()` accomplishes this, while also taking a semicolon.
		expressionStatement();
	}

	// Record where this loop statement's condition clause begins.
	int loopTopStart = currentChunk()->count;

	// Compiles the condition clause.
	int jumpToExitLoop = -1; // Sentinel.

	if (!match(TOKEN_SEMICOLON))
	{
		// Compiles the condition expression, producing a value on the stack.
		expression();

		// Consumes the semicolon following the condition expression.
		consume(TOKEN_SEMICOLON, "Expect ';' after loop condition.");

		// Jumps out of the loop if the condition is false.
		jumpToExitLoop = emitJump(OP_JUMP_IF_FALSE);

		// Pops the condition expression off the stack if we didn't jump/exit.
		emitByte(OP_POP);
	}

	// Compiles the increment clause.
	if (!match(TOKEN_RIGHT_PAREN))
	{
		// Emit an unconditional jump to the loop's main body; this is necessary
		// because our single-pass compiler must compile the increment clause before
		// compiling the body—yet we expect to execute the body (once) before we
		// ever execute the increment clause.
		int jumpToLoopBody = emitJump(OP_JUMP);

		// Record where this increment clause begins in the bytecode.
		int incrementClauseStart = currentChunk()->count;

		// Compiles the increment clause (expression); because this expression is
		// only used to produce side-effects (such as incrementing a counter), we
		// don't want to keep its result on the stack.
		expression();
		emitByte(OP_POP);

		// Takes the closing parentheses.
		consume(TOKEN_RIGHT_PAREN, "Expect ')' after 'for' clauses.");

		// After running the increment clause, we loop back to the condition clause.
		emitLoop(loopTopStart);

		// Since an increment clause was provided, we treat it as the "top" of our
		// loop—meaning that, on any subsequent iteration of the loop, we begin by
		// executive the increment clause, which itself jumps back to the condition.
		loopTopStart = incrementClauseStart;

		// We're now at the loop's main body.
		patchJump(jumpToLoopBody);
	}

	// Compiles the "for" statement's body.
	statement();

	// Jumps backwards to either (a) the increment clause (if there is one), (b)
	// the condition clause (if there is one), or (c) the main loop body (if no
	// condition or increment causes were provided, i.e., an infinite for loop).
	emitLoop(loopTopStart);

	if (jumpToExitLoop != -1)
	{
		// If a looping condition was provided, then patch the "exit" jump.
		patchJump(jumpToExitLoop);

		// Pops the condition expression off the stack if we did jump/exit.
		emitByte(OP_POP);

		// If no jump was emitted, then there is no jump to patch, and no condition
		// value to pop off the stack.
	}

	// Exits the scope that we created at the beginning of the statement.
	endScope();
}

static void ifStatement()
{
	// Compiles a jump condition.
	consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
	expression();
	consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

	// Emits a jump instruction, recording where we are in the bytecode.
	int jumpIfFalseAddress = emitJump(OP_JUMP_IF_FALSE);

	// If we take the "then" branch, pop the condition expression off the stack.
	emitByte(OP_POP);

	// Compiles a "then" block.
	statement();

	int jumpElseAddress = emitJump(OP_JUMP);

	// Back-patches the jump offset for `OP_JUMP_IF_FALSE` (above).
	patchJump(jumpIfFalseAddress);

	// If we take the "else" branch, pop the condition expression off the stack.
	emitByte(OP_POP);

	// Compiles an "else" block.
	if (match(TOKEN_ELSE))
	{
		statement();
	}

	// Patches the unconditional jump's offset, above, so that control flow skips
	// the "else" block if the "then" block was taken.
	patchJump(jumpElseAddress);
}

static void printStatement()
{
	// Compiles the expression that follows the `print` token.
	expression();

	// Consumes the semicolon terminating the print statement.
	consume(TOKEN_SEMICOLON, "Expect ';' after value.");

	emitByte(OP_PRINT);
}

static void returnStatement()
{
	if (current->type == TYPE_SCRIPT)
	{
		error("Can't return from top-level code.");
	}

	if (match(TOKEN_SEMICOLON))
	{
		// If no explicit return value was given, implicitly return `nil`.
		emitReturn();

		return;
	}

	// Compiles the expression that follows the `return` token.
	expression();

	// Consumes the semicolon terminating the return statement.
	consume(TOKEN_SEMICOLON, "Expect ';' after return value.");

	emitByte(OP_RETURN);
}

static void whileStatement()
{
	// Record where this "while" statement begins in the bytecode.
	int loopStart = currentChunk()->count;

	// Compiles a jump anti-condition.
	consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
	expression();
	consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

	int jumpAfterWhileBody = emitJump(OP_JUMP_IF_FALSE);

	// Drops the condition expression from the stack (didn't jump).
	emitByte(OP_POP);

	// Compiles the while statement's body.
	statement();

	// Loops back to re-evaluate the "while" condition.
	emitLoop(loopStart);

	// End of statement body.
	patchJump(jumpAfterWhileBody);

	// Drops the condition expression from the stack (did jump).
	emitByte(OP_POP);
}

static void synchronize()
{
	parser.panicMode = false;

	while (parser.currentToken.type != TOKEN_EOF)
	{
		if (parser.previousToken.type == TOKEN_SEMICOLON)
		{
			// The preceding token indicates a statement boundary; return.
			return;
		}

		switch (parser.currentToken.type)
		{
		case TOKEN_CLASS:
			/* falls through */
		case TOKEN_FUN:
			/* falls through */
		case TOKEN_VAR:
			/* falls through */
		case TOKEN_FOR:
			/* falls through */
		case TOKEN_IF:
			/* falls through */
		case TOKEN_WHILE:
			/* falls through */
		case TOKEN_PRINT:
			/* falls through */
		case TOKEN_RETURN:
			// The current token indicates the start of a new statement; return.
			return;
		default:; // Do nothing.
		}

		// Skips tokens indiscriminately until we reach something that looks like
		// a statement boundary.
		advance();
	}
}

static void declaration()
{
	if (match(TOKEN_FUN))
	{
		functionDeclaration();
	}
	else if (match(TOKEN_VAR))
	{
		variableDeclaration();
	}
	else
	{
		statement();
	}

	// Lox uses statements as the boundaries for synchronizing error reporting.
	if (parser.panicMode)
	{
		synchronize();
	}
}

static void statement()
{
	// Note: The sum stack effects of all bytecode instructions in a statement
	// will ways be zero; therefore, a statement produces no values—ultimately
	// leaving the original stack unchanged.

	// This property is especially necessary to support looping control flow—such
	// as a `for` statement—as these may loop for many more iterations than the
	// stack has capacity to grow (or shrink).

	if (match(TOKEN_PRINT))
	{
		// A print statement.
		printStatement();
	}
	else if (match(TOKEN_FOR))
	{
		// A "for" statement.
		forStatement();
	}
	else if (match(TOKEN_IF))
	{
		// An "if" statement.
		ifStatement();
	}
	else if (match(TOKEN_RETURN))
	{
		// A "return" statement.
		returnStatement();
	}
	else if (match(TOKEN_WHILE))
	{
		// A "while" statement.
		whileStatement();
	}
	else if (match(TOKEN_LEFT_BRACE))
	{
		// A block of one or more statements.
		beginScope();
		block();
		endScope();
	}
	else
	{
		// An expression that drops its stack result.
		expressionStatement();
	}
}

ObjFunction *compile(const char *source)
{
	initScanner(source);

	Compiler compiler;

	initCompiler(&compiler, TYPE_SCRIPT);

	parser.hadError = false;
	parser.panicMode = false;

	advance();

	while (!match(TOKEN_EOF))
	{
		declaration();
	}

	ObjFunction *function = endCompiler();

	return parser.hadError ? NULL : function;
}
