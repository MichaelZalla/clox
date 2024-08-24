#include "common.h"
#include "chunk.h"
#include "vm.h"
#include "debug.h"

int main(int argc, const char *argv[])
{
	initVM();

	Chunk chunk;

	initChunk(&chunk);

	// Expression: - (1.2 + 3.4) / 5.6

	int constantIndex = addConstant(&chunk, 1.2);

	writeChunk(&chunk, OP_CONSTANT, 123);	// CONSTANT
	writeChunk(&chunk, constantIndex, 123); // 1.2

	constantIndex = addConstant(&chunk, 3.4);

	writeChunk(&chunk, OP_CONSTANT, 123);	// CONSTANT
	writeChunk(&chunk, constantIndex, 123); // 3.4

	writeChunk(&chunk, OP_ADD, 123); // ADD

	constantIndex = addConstant(&chunk, 5.6);

	writeChunk(&chunk, OP_CONSTANT, 123);	// CONSTANT
	writeChunk(&chunk, constantIndex, 123); // 5.6

	writeChunk(&chunk, OP_DIVIDE, 123); // DIVIDE

	writeChunk(&chunk, OP_NEGATE, 123); // NEGATE

	writeChunk(&chunk, OP_RETURN, 123); // RETURN

	disassembleChunk(&chunk, "test chunk");

	interpret(&chunk);

	freeVM();
	freeChunk(&chunk);

	return 0;
}
