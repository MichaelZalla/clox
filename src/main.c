#include "common.h"
#include "chunk.h"
#include "debug.h"

int main(int argc, const char *argv[])
{
	Chunk chunk;

	initChunk(&chunk);

	int constantIndex = addConstant(&chunk, 1.2);

	writeChunk(&chunk, OP_CONSTANT);   // CONSTANT
	writeChunk(&chunk, constantIndex); // 1.2

	writeChunk(&chunk, OP_RETURN); // RETURN

	disassembleChunk(&chunk, "test chunk");
	freeChunk(&chunk);

	return 0;
}
