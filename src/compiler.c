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

typedef struct
{
	Local locals[UINT8_COUNT];
	int localCount;
	int scopeDepth;
} Compiler;

Parser parser;

Compiler *current = NULL;

Chunk *compilingChunk;

Chunk *currentChunk()
{
	return compilingChunk;
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

static void emitReturn()
{
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

static void initCompiler(Compiler *compiler)
{
	compiler->localCount = 0;
	compiler->scopeDepth = 0;
	current = compiler;
}

static void endCompiler()
{
	emitReturn();

#ifdef DEBUG_PRINT_CODE
	if (!parser.hadError)
	{
		disassembleChunk(currentChunk(), "Code");
	}
#endif
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
		[TOKEN_LEFT_PAREN] = {grouping, NULL, PREC_NONE},
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

		[TOKEN_AND] = {NULL, NULL, PREC_NONE},
		[TOKEN_CLASS] = {NULL, NULL, PREC_NONE},
		[TOKEN_ELSE] = {NULL, NULL, PREC_NONE},
		[TOKEN_FALSE] = {literal, NULL, PREC_NONE},
		[TOKEN_FOR] = {NULL, NULL, PREC_NONE},
		[TOKEN_FUN] = {NULL, NULL, PREC_NONE},
		[TOKEN_IF] = {NULL, NULL, PREC_NONE},
		[TOKEN_NIL] = {literal, NULL, PREC_NONE},
		[TOKEN_OR] = {NULL, NULL, PREC_NONE},
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
	while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF))
	{
		declaration();
	}

	consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
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

static void printStatement()
{
	// Compiles the expression that follows the `print` token.
	expression();

	// Consumes the semicolon terminating the print statement.
	consume(TOKEN_SEMICOLON, "Expect ';' after value.");

	emitByte(OP_PRINT);
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
	if (match(TOKEN_VAR))
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
		printStatement();
	}
	else if (match(TOKEN_LEFT_BRACE))
	{
		beginScope();
		block();
		endScope();
	}
	else
	{
		expressionStatement();
	}
}

bool compile(const char *source, Chunk *chunk)
{
	initScanner(source);

	Compiler compiler;

	initCompiler(&compiler);

	compilingChunk = chunk;

	parser.hadError = false;
	parser.panicMode = false;

	advance();

	while (!match(TOKEN_EOF))
	{
		declaration();
	}

	endCompiler();

	return !parser.hadError;
}
