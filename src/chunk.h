#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

typedef enum
{
	OP_CONSTANT, // Load a constant (by index) for use.
	OP_RETURN,	 // Return from the current function.
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
