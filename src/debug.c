#include <stdio.h>

#include "debug.h"

void disassembleChunk(Chunk *chunk, const char *name)
{
	printf("== %s ==\n", name);

	for (int offset = 0; offset < chunk->count;)
	{
		offset = disassembleInstruction(chunk, offset);
	}
}

int disassembleInstruction(Chunk *chunk, int offset)
{
	printf("%04d ", offset); // Prints byte offset of the given instruction.

	uint8_t instruction = chunk->code[offset]; // Read a single byte (opcode).

	switch (instruction)
	{
	case OP_RETURN:
		return simpleInstruction("OP_RETURN", offset);
	default:
		printf("Unknown opcode %d\n", instruction); // Compiler bug.
		return offset + 1;
	}
}

int simpleInstruction(const char *name, int offset)
{
	printf("%s\n", name);

	return offset + 1; // A single-byte (simple) instruction.
}