#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

typedef enum
{
	OP_CONSTANT,			// Load a constant (by index) for use.
	OP_NIL,						// Literal.
	OP_TRUE,					// Literal.
	OP_FALSE,					// Literal.
	OP_POP,						// Stack manipulation.
	OP_DEFINE_GLOBAL, // Global declaration.
	OP_EQUAL,					// Comparison.
	OP_GREATER,				// Comparison.
	OP_LESS,					// Comparison.
	OP_ADD,						// Binary add.
	OP_SUBTRACT,			// Binary subtract.
	OP_MULTIPLY,			// Binary multiply.
	OP_DIVIDE,				// Binary divide.
	OP_NEGATE,				// Unary negate.
	OP_NOT,						// Unary not.
	OP_PRINT,					// Prints to stdout.
	OP_RETURN,				// Return from the current function.
} OpCode;

typedef struct
{
	int count;
	int capacity;
	uint8_t *code; // Cache-friendly, dense, dynamic byte array.
	int *lines;
	ValueArray constants;
} Chunk;

void initChunk(Chunk *chunk);													 // Initializes a new Chunk.
void freeChunk(Chunk *chunk);													 // Deallocates a Chunk.
void writeChunk(Chunk *chunk, uint8_t byte, int line); // Appends a (code) byte to `code`.

int addConstant(Chunk *chunk, Value value); // Appends a Value to `constants`.

#endif // !clox_chunk_h
