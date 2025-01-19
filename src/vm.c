#include <stdarg.h>
#include <stdio.h>

#include "common.h"
#include "debug.h"
#include "value.h"
#include "vm.h"
#include "compiler.h"

VM vm;

// Forward declarations.
static InterpretResult run();
static Value peek(int distance);

void resetStack()
{
	vm.stackTop = vm.stack;
}

static void runtimeError(const char *format, ...)
{
	va_list args;

	va_start(args, format);
	vfprintf(stderr, format, args);
	va_end(args);

	fputs("\n", stderr);

	// Note that the interpreter advances past an instruction before executing it,
	// meaning that `vm.ip` will, at this point, point to the instruction after
	// the instruction that triggered this runtime error.
	size_t instruction = vm.ip - vm.chunk->code - 1;

	int line = vm.chunk->lines[instruction];

	fprintf(stderr, "[line %d] in script\n", line);

	resetStack();
}

void initVM()
{
	resetStack();
}

void freeVM()
{
}

InterpretResult interpret(const char *source)
{
	Chunk chunk;

	initChunk(&chunk);

	if (!compile(source, &chunk))
	{
		freeChunk(&chunk);

		return INTERPRET_COMPILER_ERROR;
	}

	vm.chunk = &chunk;

	vm.ip = vm.chunk->code;

	InterpretResult result = run();

	freeChunk(&chunk);

	return result;
}

static InterpretResult run()
{
#define READ_BYTE() (*vm.ip++)
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])
#define BINARY_OP(valueType, op)                    \
	do                                                \
	{                                                 \
		if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) \
		{                                               \
			runtimeError("Operands must be numbers!");    \
                                                    \
			return INTERPRET_RUNTIME_ERROR;               \
		}                                               \
                                                    \
		double b = AS_NUMBER(pop());                    \
		double a = AS_NUMBER(pop());                    \
                                                    \
		push(valueType(a op b));                        \
	} while (false);

	for (;;)
	{
#ifdef DEBUG_TRACE_EXECUTION
		printf("          ");
		for (Value *slot = vm.stack; slot < vm.stackTop; slot++)
		{
			printf("[ ");
			printValue(*slot);
			printf(" ]");
		}
		printf("\n");

		disassembleInstruction(vm.chunk, (int)(vm.ip - vm.chunk->code));
#endif
		uint8_t instruction;

		switch (instruction = READ_BYTE())
		{
		case OP_CONSTANT:
		{
			Value constant = READ_CONSTANT();
			push(constant);
			break;
		}
		case OP_ADD:
			BINARY_OP(NUMBER_VAL, +);
			break;
		case OP_SUBTRACT:
			BINARY_OP(NUMBER_VAL, -);
			break;
		case OP_MULTIPLY:
			BINARY_OP(NUMBER_VAL, *);
			break;
		case OP_DIVIDE:
			BINARY_OP(NUMBER_VAL, /);
			break;
		case OP_NEGATE:
		{
			// Type check.
			if (!IS_NUMBER(peek(0)))
			{
				runtimeError("Operand must be a number.");

				return INTERPRET_RUNTIME_ERROR;
			}

			// Unwrap the operand, negate it, wrap it back into a Value, and push.

			Value popped = pop();

			double asDoubleNegated = -AS_NUMBER(popped);

			push(NUMBER_VAL(asDoubleNegated));

			break;
		}
		case OP_RETURN:
		{
			printValue(pop());
			printf("\n");
			return INTERPRET_OK;
		}
		}
	}

#undef BINARY_OP
#undef READ_CONSTANT
#undef READ_BYTE
}

void push(Value value)
{
	*vm.stackTop = value;
	vm.stackTop++;
}

Value pop()
{
	vm.stackTop--;
	return *vm.stackTop;
}

static Value peek(int distance)
{
	return vm.stackTop[-1 - distance];
}