#include <stdio.h>

#include "value.h"
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
	printf("%04d ", offset); // Prints byte offset (index) of the instruction.

	uint8_t instruction = chunk->code[offset]; // Read a single byte (opcode).

	switch (instruction)
	{
	case OP_CONSTANT:
		return constantInstruction("OP_CONSTANT", chunk, offset);
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

int constantInstruction(const char *name, Chunk *chunk, int offset)
{
	uint8_t constantIndex = chunk->code[offset + 1];

	printf("%-16s %4d '", name, constantIndex);
	printValue(chunk->constants.values[constantIndex]);
	printf("'\n");

	return offset + 2; // A two-byte instruction (e.g., `[CONSTANT] [index]`).
}
