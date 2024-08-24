#include <stdlib.h>

#include "chunk.h"
#include "memory.h"
#include "value.h"

void initChunk(Chunk *chunk)
{
	chunk->count = 0;
	chunk->capacity = 0;
	chunk->code = NULL;
	chunk->lines = NULL;

	initValueArray(&chunk->constants);
}

void freeChunk(Chunk *chunk)
{
	FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
	FREE_ARRAY(uint8_t, chunk->lines, chunk->capacity);

	freeValueArray(&chunk->constants);

	initChunk(chunk);
}

void writeChunk(Chunk *chunk, uint8_t opcodeOrOperand, int line)
{
	if (chunk->capacity < chunk->count + 1)
	{
		int oldCapacity = chunk->capacity;

		chunk->capacity = NEXT_CAPACITY(oldCapacity);

		chunk->code = GROW_ARRAY(uint8_t, chunk->code, oldCapacity, chunk->capacity);
		chunk->lines = GROW_ARRAY(int, chunk->lines, oldCapacity, chunk->capacity);
	}

	chunk->code[chunk->count] = opcodeOrOperand;
	chunk->lines[chunk->count] = line;
	chunk->count++;
}

int addConstant(Chunk *chunk, Value value)
{
	writeValueArray(&chunk->constants, value);

	// Returns the new constant's index in the `ValueArray`.
	return chunk->constants.count - 1;
}
