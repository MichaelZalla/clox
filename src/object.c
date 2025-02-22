#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "value.h"
#include "table.h"
#include "vm.h"

#define ALLOCATE_OBJ(type, objectType) \
	(type *)allocateObject(sizeof(type), objectType)

static Obj *allocateObject(size_t size, ObjType type)
{
	// Allocates a new `Obj` on the heap.

	// Note: By specifying `size`, we can allocate `Obj`s of various sizes, i.e.,
	// allocating an `Obj` large enough to "hold" an `ObjString` of some length.
	Obj *object = (Obj *)reallocate(NULL, 0, size);

	// Initializes object fields.
	object->type = type;
	object->isMarked = false;
	object->next = vm.objects;

	// Updates the head pointer for the VM's `objects` list.
	vm.objects = object;

#ifdef DEBUG_LOG_GC
	printf("%p allocate %zu for ", (void *)object, size);
	switch (type)
	{
	case OBJ_CLASS:
	{
		printf("class\n");
		break;
	}
	case OBJ_CLOSURE:
	{
		printf("closure\n");
		break;
	}
	case OBJ_FUNCTION:
	{
		printf("function\n");
		break;
	}
	case OBJ_NATIVE:
	{
		printf("native\n");
		break;
	}
	case OBJ_STRING:
	{
		printf("string\n");
		break;
	}
	case OBJ_UPVALUE:
	{
		printf("upvalue\n");
		break;
	}
	}
#endif

	return object;
}

ObjClass *newClass(ObjString *name)
{
	// Allocates a new `ObjClass` on the heap.

	ObjClass *class = ALLOCATE_OBJ(ObjClass, OBJ_CLASS);

	class->name = name;

	return class;
}

static ObjString *allocateString(char *chars, int length, uint32_t hash)
{
	// Allocates a new `ObjString` on the heap.
	ObjString *string = ALLOCATE_OBJ(ObjString, OBJ_STRING);

	// Initializes fields.
	string->length = length;
	string->chars = chars;
	string->hash = hash;

	// Ensures that the string is reachable by the GC before we try writing it to
	// the hash table—which may trigger a table resize, i.e., a reallocation.
	push(OBJ_VAL(string));

	// Make sure this string is represented in our set of interned strings.
	// [string key : nil value]
	tableSet(&vm.strings, string, NIL_VAL);

	pop();

	return string;
}

uint32_t hashString(const char *key, int length)
{
	// FNC-1a hash function.

	uint32_t hash = 2166136261u;

	for (int i = 0; i < length; i++)
	{
		hash ^= (uint8_t)key[i];
		hash *= 16777619;
	}

	return hash;
}

ObjClosure *newClosure(ObjFunction *function)
{
	// Allocates a dynamic array of (dynamically allocated) upvalue _pointers_.
	ObjUpvalue **upvalues = ALLOCATE(ObjUpvalue *, function->upvalueCount);

	// Zeroes pointers.
	for (int i = 0; i < function->upvalueCount; i++)
	{
		upvalues[i] = NULL;
	}

	// Note: Allocating and zeroing the memory for upvalues above, before
	// this memory belongs to the ObjClosure itself, is ceremony for the GC.

	ObjClosure *closure = ALLOCATE_OBJ(ObjClosure, OBJ_CLOSURE);

	closure->function = function;

	closure->upvalues = upvalues;
	closure->upvalueCount = function->upvalueCount;

	return closure;
}

ObjFunction *newFunction()
{
	ObjFunction *function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION);

	function->arity = 0;
	function->upvalueCount = 0;
	function->name = NULL;

	initChunk(&function->chunk);

	return function;
}

ObjNative *newNative(NativeFn function)
{
	ObjNative *native = ALLOCATE_OBJ(ObjNative, OBJ_NATIVE);

	native->function = function;

	return native;
}

ObjString *takeString(char *chars, int length)
{
	uint32_t hash = hashString(chars, length);

	// Re-uses an existing interned string, if possible.
	ObjString *internedString = tableFindString(&vm.strings, chars, length, hash);

	if (internedString != NULL)
	{
		// Since we've been given ownership of the original string, and it can be
		// represented by an existing (interned) string, we free the original.
		FREE_ARRAY(char, chars, length + 1);

		return internedString;
	}

	return allocateString(chars, length, hash);
}

ObjString *copyString(const char *chars, int length)
{
	// Computes a hash code.
	uint32_t hash = hashString(chars, length);

	// Re-uses an existing interned string, if possible.
	ObjString *internedString = tableFindString(&vm.strings, chars, length, hash);

	if (internedString != NULL)
	{
		return internedString;
	}

	// Make a heap allocation to store the string's bytes, plus a null terminator.
	char *heapChars = ALLOCATE(char, length + 1);

	// Copy bytes.
	memcpy(heapChars, chars, length);

	// Null terminator.
	heapChars[length] = '\0';

	// Create an ObjString* using the allocation.
	return allocateString(heapChars, length, hash);
}

ObjUpvalue *newUpvalue(Value *slot)
{
	ObjUpvalue *upvalue = ALLOCATE_OBJ(ObjUpvalue, OBJ_UPVALUE);

	upvalue->location = slot;
	upvalue->closed = NIL_VAL;
	upvalue->next = NULL;

	return upvalue;
}

static void printFunction(ObjFunction *function)
{
	// Internal "main()" function; may be printed if DEBUG_TRACE_EXECUTION is
	// enabled and prints the entire Value stack.
	if (function->name == NULL)
	{
		printf("<script>");
		return;
	}

	// User-defined function.
	printf("<fn %s>", function->name->chars);
}

void printObject(Value value)
{
	switch (OBJ_TYPE(value))
	{
	case OBJ_CLASS:
	{
		ObjClass *class = AS_CLASS(value);

		printf("%s", class->name->chars);

		break;
	}
	case OBJ_CLOSURE:
	{
		ObjClosure *closure = AS_CLOSURE(value);

		printFunction(closure->function);
		break;
	}
	case OBJ_FUNCTION:
		printFunction(AS_FUNCTION(value));
		break;
	case OBJ_NATIVE:
		printf("<native fn>");
		break;
	case OBJ_STRING:
		printf("%s", AS_CSTRING(value));
		break;
	case OBJ_UPVALUE:
		// Note: Upvalue objects aren't first-class objects in Lox, and so a Lox
		// user can't actually "print" one—but this will keep the C compiler happy.
		printf("upvalue");
		break;
	}
}