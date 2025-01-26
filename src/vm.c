#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#include "common.h"
#include "debug.h"
#include "object.h"
#include "memory.h"
#include "value.h"
#include "table.h"
#include "vm.h"
#include "compiler.h"

VM vm;

// Forward declarations.
static InterpretResult run();
static Value peek(int distance);
static bool isFalsey(Value value);
static void concatenate();

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

	vm.objects = NULL;

	initTable(&vm.strings);
	initTable(&vm.globals);
}

void freeVM()
{
	freeTable(&vm.strings);
	freeTable(&vm.globals);

	freeObjects();
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
#define READ_SHORT() \
	(vm.ip += 2, (uint16_t)((vm.ip[-2] << 8) | vm.ip[-1]))
#define READ_STRING() AS_STRING(READ_CONSTANT())
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
		// Prints the current Value stack.
		printf("          ");
		for (Value *slot = vm.stack; slot < vm.stackTop; slot++)
		{
			printf("[ ");
			printValue(*slot);
			printf(" ]");
		}
		printf("\n");

		// Prints the instruction.
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

		case OP_NIL:
			push(NIL_VAL);
			break;

		case OP_TRUE:
			push(BOOL_VAL(true));
			break;

		case OP_FALSE:
			push(BOOL_VAL(false));
			break;

		case OP_POP:
			pop();
			break;

		case OP_GET_LOCAL:
		{
			// Takes a single byte operand (representing a `locals[]` stack index).
			uint8_t slot = READ_BYTE();

			// Uses it to look up the local's current value (somewhere on the stack).
			Value currentValue = vm.stack[slot];

			// Copies the local's current value to the top of the stack, for use.
			push(currentValue);

			break;
		}

		case OP_SET_LOCAL:
		{
			// Takes a single byte operand (representing a `locals[]` stack index).
			uint8_t slot = READ_BYTE();

			// Writes a new value to that `locals[]` stack slot, using the top value
			// on the stack.
			vm.stack[slot] = peek(0);

			// Note: We don't pop this value from the top of the stack, because an
			// assignment is an expression in Lox—and all expressions should produce
			// a value; that is to say, they should leave the result of the expression
			// on the stack.

			break;
		}

		case OP_GET_GLOBAL:
		{
			// Reads the string (value) stored in the current chunk's constant table,
			// at the index given by the byte that follows OP_GET_GLOBAL.
			ObjString *globalVariableName = READ_STRING();

			Value value;

			if (!tableGet(&vm.globals, globalVariableName, &value))
			{
				runtimeError("Undefined global variable '%s'.", globalVariableName->chars);

				return INTERPRET_RUNTIME_ERROR;
			}

			push(value);

			break;
		}

		case OP_DEFINE_GLOBAL:
		{
			// Reads the string (value) stored in the current chunk's constant table,
			// at the index given by the byte that follows OP_DEFINE_GLOBAL.
			ObjString *globalVariableName = READ_STRING();

			Value initializer = peek(0);

			// Inserts the key (name) and value (initializer) into `vm.globals`.

			// Note: By definition, Lox allows the user to redeclare previously
			// declared globals.
			tableSet(&vm.globals, globalVariableName, initializer);

			// Note: We peek the initializer (value) above and pop() it later, here;
			// this prevents the Value's memory from leaking if GC is triggered in the
			// middle of adding it to the hash table (which may very well occur, as a
			// table insert might require a table resize).
			pop();

			break;
		}

		case OP_SET_GLOBAL:
		{
			// Reads the string (value) stored in the current chunk's constant table,
			// at the index given by the byte that follows OP_SET_GLOBAL.
			ObjString *globalVariableName = READ_STRING();

			bool isNewKey = tableSet(&vm.globals, globalVariableName, peek(0));

			// Checks whether the variable (identifier) is already defined.
			if (isNewKey)
			{
				// If the identifier wasn't previously defined in the table, this should
				// be considered an invalid assignment expression, i.e., "Undefined
				// variable" scenario.

				// Deletes the associated "zombie" entry from the table.
				tableDelete(&vm.globals, globalVariableName);

				runtimeError("Undefined global variable '%s'.", globalVariableName->chars);

				return INTERPRET_RUNTIME_ERROR;
			}

			// We avoid popping the assignment's value off the stack because an
			// assignment is an expression, and all expressions preserve the stack.

			break;
		}

		case OP_EQUAL:
		{
			Value b = pop();
			Value a = pop();

			Value isEqual = BOOL_VAL(valuesEqual(a, b));

			push(isEqual);

			break;
		}

		case OP_GREATER:
			BINARY_OP(BOOL_VAL, >);
			break;

		case OP_LESS:
			BINARY_OP(BOOL_VAL, <);
			break;

		case OP_ADD:
			if (IS_STRING(peek(0)) && IS_STRING(peek(1)))
			{
				concatenate();
			}
			else if (IS_NUMBER(peek(0)) && IS_NUMBER(peek(1)))
			{
				double b = AS_NUMBER(pop());
				double a = AS_NUMBER(pop());

				double sum = a + b;

				push(NUMBER_VAL(sum));
			}
			else
			{
				runtimeError("Operands must be two numbers, or two strings.");

				return INTERPRET_RUNTIME_ERROR;
			}
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

		case OP_NOT:
			push(BOOL_VAL(isFalsey(pop())));
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

		case OP_PRINT:
			// Prints the evaluated expression, located at the top of the stack.
			printValue(pop());
			printf("\n");
			break;

		case OP_JUMP:
		{
			uint16_t jumpOffset = READ_SHORT();

			vm.ip += jumpOffset;

			break;
		}

		case OP_JUMP_IF_FALSE:
		{
			uint16_t jumpOffset = READ_SHORT();

			if (isFalsey(peek(0)))
			{
				vm.ip += jumpOffset;
			}

			break;
		}

		case OP_LOOP:
		{
			uint16_t jumpOffset = READ_SHORT();

			vm.ip -= jumpOffset;

			break;
		}

		case OP_RETURN:
		{
			// Exit the interpreter.
			return INTERPRET_OK;
		}
		}
	}

#undef READ_BYTE
#undef READ_CONSTANT
#undef READ_SHORT
#undef READ_STRING
#undef BINARY_OP
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

static bool isFalsey(Value value)
{
	return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

static void concatenate()
{
	ObjString *b = AS_STRING(pop());
	ObjString *a = AS_STRING(pop());

	int length = a->length + b->length;

	// Allocates memory of the bytes, plus a null terminator.
	char *chars = ALLOCATE(char, length + 1);

	memcpy(chars, a->chars, a->length);
	memcpy(chars + a->length, b->chars, b->length);
	chars[length] = '\0';

	ObjString *result = takeString(chars, length);

	push(OBJ_VAL((Obj *)result));
}