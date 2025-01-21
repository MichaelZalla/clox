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

	// Initializes fields.
	object->type = type;

	object->next = vm.objects;

	vm.objects = object;

	return object;
}

static ObjString *allocateString(char *chars, int length, uint32_t hash)
{
	// Allocates a new `ObjString` on the heap.
	ObjString *string = ALLOCATE_OBJ(ObjString, OBJ_STRING);

	// Initializes fields.
	string->length = length;
	string->chars = chars;
	string->hash = hash;

	// Make sure this string is represented in our set of interned strings.
	// [string key : nil value]
	tableSet(&vm.strings, string, NIL_VAL);

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

void printObject(Value value)
{
	switch (OBJ_TYPE(value))
	{
	case OBJ_STRING:
		printf("%s", AS_CSTRING(value));
		break;
	}
}