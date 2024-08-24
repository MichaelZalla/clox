#include "common.h"
#include "chunk.h"
#include "debug.h"

int main(int argc, const char *argv[])
{
	Chunk chunk;

	initChunk(&chunk);

	int constantIndex = addConstant(&chunk, 1.2);

	writeChunk(&chunk, OP_CONSTANT, 123);	// CONSTANT
	writeChunk(&chunk, constantIndex, 123); // 1.2
	writeChunk(&chunk, OP_RETURN, 123);		// RETURN

	disassembleChunk(&chunk, "test chunk");
	freeChunk(&chunk);

	return 0;
}
