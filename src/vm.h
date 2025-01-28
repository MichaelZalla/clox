#ifndef clox_vm_h
#define clox_vm_h

#include "object.h"
#include "table.h"
#include "value.h"

#define FRAMES_MAX 64

// Scales our value stack proportionally to FRAMES_MAX.
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

typedef struct
{
	ObjFunction *function; // Used to look up constants, etc, in this fn's chunk.
	uint8_t *ip;					 // IP / counter for a particular function invocation.
	Value *slots;					 // A slice of the running program's Value stack.
} CallFrame;

typedef struct
{
	CallFrame frames[FRAMES_MAX];
	int frameCount;
	Value stack[STACK_MAX];
	Value *stackTop;
	Table strings;
	Table globals;
	Obj *objects;
} VM;

typedef enum
{
	INTERPRET_OK,
	INTERPRET_COMPILER_ERROR,
	INTERPRET_RUNTIME_ERROR,
} InterpretResult;

extern VM vm;

void initVM();
void freeVM();
InterpretResult interpret(const char *source);
void push(Value value);
Value pop();

#endif // !clox_vm_h
