#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"

typedef enum
{
	OP_RETURN, // Return from the current function.
} OpCode;

typedef struct
{
	int count;
	int capacity;
	uint8_t *code; // Cache-friendly, dense, dynamic byte array.
} Chunk;

void initChunk(Chunk *chunk);								 // Initializes a new Chunk.
void freeChunk(Chunk *chunk);								 // Deallocates a Chunk.
void writeChunk(Chunk *chunk, uint8_t byte); // Appends a byte to the end of the Chunk.

#endif // !clox_chunk_h
