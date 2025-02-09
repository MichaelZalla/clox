#include <stdio.h>

#include "value.h"
#include "object.h"
#include "debug.h"

void disassembleChunk(Chunk *chunk, const char *name)
{
	printf("== %s ==\n", name);

	for (int offset = 0; offset < chunk->count;)
	{
		offset = disassembleInstruction(chunk, offset);
	}
}

// Forward declarations.
static int byteInstruction(const char *name, Chunk *chunk, int offset);
static int jumpInstruction(const char *name, int didJump, Chunk *chunk, int offset);

int disassembleInstruction(Chunk *chunk, int offset)
{
	printf("%04d ", offset); // Prints byte offset (index) of the instruction.

	if (offset > 0 && chunk->lines[offset] == chunk->lines[offset - 1])
	{
		printf("   | ");
	}
	else
	{
		printf("%4d ", chunk->lines[offset]);
	}

	uint8_t instruction = chunk->code[offset]; // Read a single byte (opcode).

	switch (instruction)
	{
	case OP_CONSTANT:
		return constantInstruction("OP_CONSTANT", chunk, offset);
	case OP_NIL:
		return simpleInstruction("OP_NIL", offset);
	case OP_TRUE:
		return simpleInstruction("OP_TRUE", offset);
	case OP_FALSE:
		return simpleInstruction("OP_FALSE", offset);
	case OP_POP:
		return simpleInstruction("OP_POP", offset);
	case OP_GET_LOCAL:
		// Local names are erased by the compiler, so we output the local's index.
		return byteInstruction("OP_GET_LOCAL", chunk, offset);
	case OP_SET_LOCAL:
		// Local names are erased by the compiler, so we output the local's index.
		return byteInstruction("OP_SET_LOCAL", chunk, offset);
	case OP_GET_GLOBAL:
		return constantInstruction("OP_GET_GLOBAL", chunk, offset);
	case OP_DEFINE_GLOBAL:
		return constantInstruction("OP_DEFINE_GLOBAL", chunk, offset);
	case OP_SET_GLOBAL:
		return constantInstruction("OP_SET_GLOBAL", chunk, offset);
	case OP_GET_UPVALUE:
		return byteInstruction("OP_GET_UPVALUE", chunk, offset);
	case OP_SET_UPVALUE:
		return byteInstruction("OP_SET_UPVALUE", chunk, offset);
	case OP_EQUAL:
		return simpleInstruction("OP_EQUAL", offset);
	case OP_GREATER:
		return simpleInstruction("OP_GREATER", offset);
	case OP_LESS:
		return simpleInstruction("OP_LESS", offset);
	case OP_ADD:
		return simpleInstruction("OP_ADD", offset);
	case OP_SUBTRACT:
		return simpleInstruction("OP_SUBTRACT", offset);
	case OP_MULTIPLY:
		return simpleInstruction("OP_MULTIPLY", offset);
	case OP_DIVIDE:
		return simpleInstruction("OP_DIVIDE", offset);
	case OP_NOT:
		return simpleInstruction("OP_NOT", offset);
	case OP_NEGATE:
		return simpleInstruction("OP_NEGATE", offset);
	case OP_PRINT:
		return simpleInstruction("OP_PRINT", offset);
	case OP_JUMP:
		return jumpInstruction("OP_JUMP", 1, chunk, offset);
	case OP_JUMP_IF_FALSE:
		return jumpInstruction("OP_JUMP_IF_FALSE", 1, chunk, offset);
	case OP_LOOP:
		return jumpInstruction("OP_LOOP", -1, chunk, offset);
	case OP_CALL:
		return byteInstruction("OP_CALL", chunk, offset);
	case OP_CLOSURE:
	{
		offset += 1;

		uint8_t constantIndex = chunk->code[offset++];

		printf("%-16s %4d ", "OP_CLOSURE", constantIndex);
		printValue(chunk->constants.values[constantIndex]);
		printf("\n");

		Value *value = &chunk->constants.values[constantIndex];

		ObjFunction *function = AS_FUNCTION(*value);

		for (int j = 0; j < function->upvalueCount; j++)
		{
			int isLocal = chunk->code[offset++];
			int index = chunk->code[offset++];

			printf("%04d      |                     %s %d\n", offset - 2, isLocal ? "local" : "upvalue", index);
		}

		return offset;
	}
	case OP_CLOSE_UPVALUE:
		return simpleInstruction("OP_CLOSE_UPVALUE", offset);
	case OP_RETURN:
		return simpleInstruction("OP_RETURN", offset);
	default:
		printf("Unknown opcode %d\n", instruction); // Compiler bug.

		return offset + 1; // A single-byte (simple) instruction (`[OP_*]`).
	}
}

int simpleInstruction(const char *name, int offset)
{
	printf("%s\n", name);

	return offset + 1; // A single-byte (simple) instruction (`[OP_*]`).
}

static int byteInstruction(const char *name, Chunk *chunk, int offset)
{
	uint8_t slot = chunk->code[offset + 1]; // Reads the `locals[]` stack index.

	// Prints the instruction (e.g., `OP_GET_LOCAL`) and the `locals[]` index.
	printf("%-16s %4d\n", name, slot);

	return offset + 2; // A two-byte instruction (`[OP_*] [index]`).
}

static int jumpInstruction(const char *name, int didJump, Chunk *chunk, int offset)
{
	// Reads the jump offset.

	uint8_t highOffsetBits = chunk->code[offset + 1];
	uint8_t lowOffsetBits = chunk->code[offset + 2];

	uint16_t jumpOffset = ((uint16_t)highOffsetBits << 8) | (uint16_t)lowOffsetBits;

	int destination = offset + 3 + didJump * jumpOffset;

	printf("%-16s %4d -> %d\n", name, offset, destination);

	return offset + 3; // Next instruction.
}

int constantInstruction(const char *name, Chunk *chunk, int offset)
{
	uint8_t constantIndex = chunk->code[offset + 1];

	// Prints the instruction (e.g., `OP_CONSTANT`) and the constant index.
	printf("%-16s %4d '", name, constantIndex);

	// Prints the constant's value from the current chunk's constants array.
	printValue(chunk->constants.values[constantIndex]);
	printf("'\n");

	return offset + 2; // A two-byte instruction (`[OP_*] [constantIndex]`).
}
