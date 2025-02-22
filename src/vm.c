#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "common.h"
#include "debug.h"
#include "object.h"
#include "memory.h"
#include "value.h"
#include "table.h"
#include "vm.h"
#include "compiler.h"

VM vm;

static Value clockNative(int argCount, Value *args)
{
	return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

// Forward declarations.
static InterpretResult run();
static Value peek(int distance);
static bool call(ObjClosure *closure, int argCount);
static bool callValue(Value callee, int argCount);
static ObjUpvalue *captureUpvalue(Value *local);
static void closeUpvalue(Value *lastLocation);
static bool isFalsey(Value value);
static void concatenate();

void resetStack()
{
	// Clears the VM's value stack.
	vm.stackTop = vm.stack;

	// Clears the VM's frame stack.
	vm.frameCount = 0;

	// An empty stack means no upvalues referencing stack slots.
	vm.openUpvalues = NULL;
}

static void runtimeError(const char *format, ...)
{
	va_list args;

	va_start(args, format);
	vfprintf(stderr, format, args);
	va_end(args);

	fputs("\n", stderr);

	// Walks the call stack from top (most recent) to bottom (least recent),
	// printing the corresponding function name as well as the line (in source)
	// associated with the bytecode pointed to by the frame's instruction pointer.

	for (int i = vm.frameCount - 1; i >= 0; i--)
	{
		CallFrame *frame = &vm.frames[i];

		ObjFunction *function = frame->closure->function;

		// Computes the index of the bytecode instruction pointed to by IP.

		// Note that the interpreter advances past an instruction before executing
		// it, meaning that `ip` will, at this point, point to the instruction
		// _after_ the instruction that triggered this runtime error.
		size_t instructionIndex = frame->ip - function->chunk.code - 1;

		// Prints line number.
		fprintf(stderr, "[line %d] in ", function->chunk.lines[instructionIndex]);

		// Prints function identifier.
		if (function->name == NULL)
		{
			fprintf(stderr, "script\n");
		}
		else
		{
			fprintf(stderr, "%s()\n", function->name->chars);
		}
	}

	resetStack();
}

static void defineNative(const char *name, NativeFn function)
{
	// Registers a new string identifier for the native function; we temporarily
	// keep the resulting Value on the stack, to avoid accidental GC.
	push(OBJ_VAL(copyString(name, (int)strlen(name))));

	// Creates a `Value` enclosing the new `ObjNative`; we temporarily keep the
	// resulting Value on the stack, to avoid accidental GC.
	push(OBJ_VAL(newNative(function)));

	tableSet(&vm.globals, AS_STRING(vm.stack[0]), vm.stack[1]);

	// Frees temporaries.
	pop();
	pop();
}

void initVM()
{
	resetStack();

	vm.bytesAllocated = 0;

	vm.nextGC = 1024 * 1024; // Avoids a few GC runs on very small starting heaps.

	vm.objects = NULL;

	vm.grayCount = 0;
	vm.grayCapacity = 0;
	vm.grayStack = NULL;

	initTable(&vm.strings);
	initTable(&vm.globals);

	defineNative("clock", clockNative);
}

void freeVM()
{
	freeTable(&vm.strings);
	freeTable(&vm.globals);

	freeObjects();
}

InterpretResult interpret(const char *source)
{
	ObjFunction *function = compile(source);

	if (function == NULL)
	{
		return INTERPRET_COMPILER_ERROR;
	}

	// The first slot in our Value stack always holds the top-level "function".

	// Makes sure that our GC is aware of this heap-allocated function, even
	// before it is referenced by an ObjClosure (that replaces it on the stack).
	push(OBJ_VAL(function));

	// Replaces the ObjFunction Value occupying the start of the Value stack.
	ObjClosure *closure = newClosure(function);

	// Yeah, it's weird.
	pop();

	// The closure finally sits at the start of the VM's Value stack.
	push(OBJ_VAL(closure));

	// Prepares a call frame to invoke the top-level "main()" closure.
	call(closure, 0);

	return run();
}

static InterpretResult run()
{
	CallFrame *frame = &vm.frames[vm.frameCount - 1];

#define READ_BYTE() (*(frame->ip++))

#define READ_SHORT() \
	(frame->ip += 2,   \
	 (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]))

#define READ_CONSTANT() (frame->closure->function->chunk.constants.values[READ_BYTE()])

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
		Chunk *chunk = &frame->closure->function->chunk;

		disassembleInstruction(chunk, (int)(frame->ip - chunk->code));
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
			// Takes a byte operand, representing a (frame-relative) stack slot.
			uint8_t relativeIndex = READ_BYTE();

			// Uses it to look up the local's current value (somewhere on the stack).
			Value currentValue = frame->slots[relativeIndex];

			// Copies the local's current value to the top of the stack, for use.
			push(currentValue);

			break;
		}

		case OP_SET_LOCAL:
		{
			// Takes a byte operand, representing a (frame-relative) stack slot.
			uint8_t relativeIndex = READ_BYTE();

			// Writes a new value to that `locals[]` stack slot, using the top value
			// on the stack.
			frame->slots[relativeIndex] = peek(0);

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

		case OP_GET_UPVALUE:
		{
			uint8_t upvalueSlot = READ_BYTE();

			// Looks up the corresponding Upvalue in the current function (closure),
			// dereferencing the pointer to the Value that it points at; the resulting
			// Value is pushed to the stack.
			push(*frame->closure->upvalues[upvalueSlot]->location);

			break;
		}

		case OP_SET_UPVALUE:
		{
			uint8_t upvalueSlot = READ_BYTE();

			// Deferences the upvalue's Value pointer, copying the stack's top Value
			// to the defererenced memory (note that the address of `location` here
			// remains unchanged).
			*frame->closure->upvalues[upvalueSlot]->location = peek(0);

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

			frame->ip += jumpOffset;

			break;
		}

		case OP_JUMP_IF_FALSE:
		{
			uint16_t jumpOffset = READ_SHORT();

			if (isFalsey(peek(0)))
			{
				frame->ip += jumpOffset;
			}

			break;
		}

		case OP_LOOP:
		{
			uint16_t jumpOffset = READ_SHORT();

			frame->ip -= jumpOffset;

			break;
		}

		case OP_CALL:
		{
			int argCount = READ_BYTE();

			if (!callValue(peek(argCount), argCount))
			{
				// Aborts the interpreter.
				return INTERPRET_RUNTIME_ERROR;
			}

			// Advances the cached `frame` pointer to the newly pushed call frame.
			frame = &vm.frames[vm.frameCount - 1];

			break;
		}

		case OP_CLOSURE:
		{
			// Reads a constant index from the stack, and uses it to retrieve the
			// ObjFunction (Value) from the current call frame's function's constant
			// table.
			ObjFunction *function = AS_FUNCTION(READ_CONSTANT());

			// Produces a new closure that uses this constant function definition.
			ObjClosure *closure = newClosure(function);

			// Leaves the closure on the stack.
			push(OBJ_VAL(closure));

			// Initializes the closure's array of `ObjUpvalue` pointers.
			for (int i = 0; i < closure->function->upvalueCount; i++)
			{
				uint8_t isLocal = READ_BYTE();
				uint8_t index = READ_BYTE();

				if (isLocal)
				{
					// This upvalue captures a local declared in the enclosing function.

					// We need to grab a pointer to the captured local's slot in the
					// surrounding function's Value-stack window.

					Value *localValue = frame->slots + index;

					closure->upvalues[i] = captureUpvalue(localValue);
				}
				else
				{
					// This upvalue captures a local declared in a higher function. The
					// _current closure_ that stores the referenced upvalue is contained
					// in the _current call frame_.
					closure->upvalues[i] = frame->closure->upvalues[index];
				}
			}

			break;
		}

		case OP_CLOSE_UPVALUE:
		{
			// Copies this local's Value to its unique, heap-allocated upvalue object.
			closeUpvalue(vm.stackTop - 1);

			// Pops this local off the top of the stack, letting it go out of scope.
			pop();

			break;
		}

		case OP_RETURN:
		{
			// Holds on to the current call's return value.
			Value result = pop();

			// Closes any open upvalues that point to locals declared in this
			// function's top-level lexical scope (including call arguments).
			closeUpvalue(frame->slots);

			// Frees the current call frame.
			vm.frameCount -= 1;

			if (vm.frameCount == 0)
			{
				// Clears the VM's stack, removing the top-level function Value.
				pop();

				// Exits the interpreter.
				return INTERPRET_OK;
			}

			// Pops the current call frame's storage slice off the VM's stack.
			vm.stackTop = frame->slots;

			// Returns the return value back onto the stack.
			push(result);

			// Updates the `run()` function's cached frame pointer.
			frame = &vm.frames[vm.frameCount - 1];

			break;
		}
		case OP_CLASS:
		{
			// Reads the string (value) stored in the current chunk's constant table,
			// at the index given by the byte that follows OP_CLASS.
			ObjString *className = READ_STRING();

			ObjClass *class = newClass(className);

			// Produces a new Value wrapping an ObjClass on the Value stack.
			push(OBJ_VAL(class));

			break;
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

static bool call(ObjClosure *closure, int argCount)
{
	// Validates argument count.
	if (argCount != closure->function->arity)
	{
		runtimeError("Expected %d arguments but got %d.", closure->function->arity, argCount);

		return false;
	}

	// Checks for frame stack overflow.
	if (vm.frameCount == FRAMES_MAX)
	{
		runtimeError("Stack overflow.");

		return false;
	}

	// Prepares a call frame to invoke the callee.
	CallFrame *frame = &vm.frames[vm.frameCount++];

	// Function pointer.
	frame->closure = closure;

	// Instruction pointer begins at the start of the function's compiled body.
	frame->ip = closure->function->chunk.code;

	// Positions the frame's `locals[]` stack to align with the callee Value.
	frame->slots = vm.stackTop - argCount - 1;

	return true;
}

static bool callValue(Value callee, int argCount)
{
	if (IS_OBJ(callee))
	{
		switch (OBJ_TYPE(callee))
		{
		case OBJ_CLOSURE:
			return call(AS_CLOSURE(callee), argCount);
		case OBJ_NATIVE:
		{
			NativeFn native = AS_NATIVE(callee);

			// Invokes the native function wrapper, retrieving a result.
			Value result = native(argCount, vm.stackTop - argCount);

			// Unwinds the VM's stack, dropping the call arguments; we add 1 to
			// account for the ObjClosure value being dropped.
			vm.stackTop -= argCount + 1;

			// Pushes the result back onto the stack.
			push(result);

			return true;
		}
		default:
			break; // A non-callable object type.
		}
	}

	runtimeError("Can only call functions and classes.");

	return false;
}

static ObjUpvalue *captureUpvalue(Value *local)
{
	// Walks the list of open upvalues that point to locals on the stack.

	ObjUpvalue *previousUpvalue = NULL;
	ObjUpvalue *currentUpvalue = vm.openUpvalues;

	while (currentUpvalue != NULL && currentUpvalue->location > local)
	{
		previousUpvalue = currentUpvalue;
		currentUpvalue = (ObjUpvalue *)currentUpvalue->next;
	}

	if (currentUpvalue != NULL && currentUpvalue->location == local)
	{
		// We've found an existing open upvalue referencing the same stack local.

		return currentUpvalue;
	}

	// We've run out of open upvalues to consider for re-use, or our current up-
	// value points to a stack slot that is _below_ the one we're interested in.

	// Creates a new ObjUpvalue that "captures" the given Value stack slot.

	ObjUpvalue *createdUpvalue = newUpvalue(local);

	// We need to insert the newly created open upvalue _before_ the object
	// pointed at by `currentUpvalue` (which may possibly be null).

	createdUpvalue->next = (struct ObjUpvalue *)currentUpvalue;

	if (previousUpvalue == NULL)
	{
		vm.openUpvalues = createdUpvalue;
	}
	else
	{
		previousUpvalue->next = (struct ObjUpvalue *)createdUpvalue;
	}

	return createdUpvalue;
}

static void closeUpvalue(Value *lastLocation)
{
	// Closes all upvalues on the VM's Value stack that reference a local whose
	// stack slot is greater or equal to `lastLocation`.

	while (vm.openUpvalues != NULL && vm.openUpvalues->location >= lastLocation)
	{
		// Points to the most recently allocated open upvalue (highest on stack).
		ObjUpvalue *upvalue = vm.openUpvalues;

		// Writes the Value stored at `upvalue->location` into `upvalue.closed`.
		upvalue->closed = *upvalue->location;

		// Updates `location` to point to the upvalue struct's own `closed` field.
		upvalue->location = &upvalue->closed;

		// Removes this no-longer-open upvalue from the `openUpvalues` list.
		vm.openUpvalues = (ObjUpvalue *)vm.openUpvalues->next;
	}
}

static bool isFalsey(Value value)
{
	return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

static void concatenate()
{
	// Avoids popping either string Value from the Value stack, as we want both
	// strings to be reachable0 by the GC, in the event that either `ALLOCATE()`
	// or `takeString()` lead to GC being triggered.

	ObjString *b = AS_STRING(peek(0));
	ObjString *a = AS_STRING(peek(1));

	int length = a->length + b->length;

	// Allocates memory of the bytes, plus a null terminator.
	char *chars = ALLOCATE(char, length + 1);

	memcpy(chars, a->chars, a->length);
	memcpy(chars + a->length, b->chars, b->length);
	chars[length] = '\0';

	ObjString *result = takeString(chars, length);

	pop();
	pop();

	push(OBJ_VAL(result));
}