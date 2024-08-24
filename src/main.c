#include "common.h"
#include "chunk.h"
#include "vm.h"
#include "debug.h"

int main(int argc, const char *argv[])
{
	initVM();

	Chunk chunk;

	initChunk(&chunk);

	int constantIndex = addConstant(&chunk, 1.2);

	writeChunk(&chunk, OP_CONSTANT, 123);	// CONSTANT
	writeChunk(&chunk, constantIndex, 123); // 1.2
	writeChunk(&chunk, OP_RETURN, 123);		// RETURN

	disassembleChunk(&chunk, "test chunk");
	interpret(&chunk);
	freeVM();
	freeChunk(&chunk);

	return 0;
}
