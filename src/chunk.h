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
	OP_GET_LOCAL,			// Local read.
	OP_SET_LOCAL,			// Local write.
	OP_GET_GLOBAL,		// Global read.
	OP_DEFINE_GLOBAL, // Global declaration.
	OP_SET_GLOBAL,		// Global assignment.
	OP_GET_UPVALUE,		// Read from a higher (non-global) lexical scope.
	OP_SET_UPVALUE,		// Write to a higher (non-global) lexical scope.
	OP_GET_PROPERTY,	// Produces a value from an instance's field.
	OP_SET_PROPERTY,	// Assigns a value to an instance's field.
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
	OP_JUMP,					// Unconditional jump forward.
	OP_JUMP_IF_FALSE, // Jump if false.
	OP_LOOP,					// Unconditional jump backward.
	OP_CALL,					// Calls into a new frame.
	OP_INVOKE,				// Fastpath for method invocations on an instance.
	OP_CLOSURE,				// Similar to OP_CONSTANT but with special runtime handling.
	OP_CLOSE_UPVALUE, // Moves a stack-allocated Value to the heap.
	OP_RETURN,				// Return from the current function.
	OP_CLASS,					// Begins a new class body.
	OP_INHERIT,				// Pre-populates a class `methods` table using a superclass.
	OP_METHOD,				// Begins a method body as part of a class declaration.
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
