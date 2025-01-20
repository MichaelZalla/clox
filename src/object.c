#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "value.h"
#include "vm.h"

#define ALLOCATE_OBJ(type, objectType) \
	(type *)allocateObject(sizeof(type), objectType)

static Obj *allocateObject(size_t size, ObjType type)
{
	// Allocates a new `Obj` on the heap.

	// Note: By specifying `size`, we can allocate `Obj`s of various sizes, i.e.,
	// allocating an `Obj` large enough to "hold" an `ObjString` of some length.
	Obj *object = (Obj *)reallocate(NULL, 0, size);

	// Initializes fields.
	object->type = type;

	object->next = vm.objects;

	vm.objects = object;

	return object;
}

static ObjString *allocateString(char *chars, int length)
{
	// Allocates a new `ObjString` on the heap.
	ObjString *string = ALLOCATE_OBJ(ObjString, OBJ_STRING);

	// Initializes fields.
	string->length = length;
	string->chars = chars;

	return string;
}

ObjString *takeString(char *chars, int length)
{
	return allocateString(chars, length);
}

ObjString *copyString(const char *chars, int length)
{
	// Make a heap allocation to store the string's bytes, plus a null terminator.
	char *heapChars = ALLOCATE(char, length + 1);

	// Copy bytes.
	memcpy(heapChars, chars, length);

	// Null terminator.
	heapChars[length] = '\0';

	// Create an ObjString* using the allocation.
	return allocateString(heapChars, length);
}

void printObject(Value value)
{
	switch (OBJ_TYPE(value))
	{
	case OBJ_STRING:
		printf("%s", AS_CSTRING(value));
		break;
	}
}