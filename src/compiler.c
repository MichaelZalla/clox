#include <stdio.h>
#include <stdlib.h>

#include "chunk.h"
#include "common.h"
#include "compiler.h"
#include "scanner.h"

typedef struct
{
	Token currentToken;
	Token previousToken;
	bool hadError;
	bool panicMode;
} Parser;

Parser parser;

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

static void endCompiler()
{
	emitReturn();
}

// Forward declarations.
static void expression();
static void parsePrecedence(Precedence precedence);

static void binary()
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

static void grouping()
{
	// Compiles the expression between the pair of parentheses.
	expression();

	// Consumes the closing parentheses.
	consume(TOKEN_RIGHT_PAREN, "Expect ')' after grouping expression.");
}

static void number()
{
	// Assumes that we've already consumed the token for this number literal,
	// and have stored it in `previousToken`; we convert the lexeme to a double
	// using `strtod()`.
	double value = strtod(parser.previousToken.start, NULL);

	emitConstant(value);
}

static void unary()
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
	case TOKEN_MINUS:
		emitByte(OP_NEGATE);

		break;
	default:
		// Unreachable.

		return;
	}
}

static void parsePrecedence(Precedence precedence)
{
}

static void expression()
{
	parsePrecedence(PREC_ASSIGNMENT);
}

bool compile(const char *source, Chunk *chunk)
{
	initScanner(source);

	compilingChunk = chunk;

	parser.hadError = false;
	parser.panicMode = false;

	advance();

	expression();

	consume(TOKEN_EOF, "Expect end of expression.");

	endCompiler();

	return !parser.hadError;
}
