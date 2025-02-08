#include <stdlib.h>

#include "object.h"
#include "memory.h"
#include "vm.h"

void *reallocate(void *pointer, size_t oldSize, size_t newSize)
{
	if (newSize == 0)
	{
		free(pointer);

		return NULL;
	}

	// If `ptr` is null, this is identical to calling `malloc(newSize)`.
	void *result = realloc(pointer, newSize);

	if (result == NULL)
		exit(1);

	return result;
}

static void freeObject(Obj *object)
{
	switch (object->type)
	{
	case OBJ_CLOSURE:
	{
		ObjClosure *closure = (ObjClosure *)object;

		// Frees this closure's dynamic array of upvalue pointers; note that. here,
		// we don't free the _upvalues_ themselves.

		FREE_ARRAY(ObjUpvalue *, closure->upvalues, closure->upvalueCount);

		// Note that we don't free the closure's ObjFunction, which may be shared.

		FREE(ObjClosure, object);

		break;
	}
	case OBJ_FUNCTION:
	{
		ObjFunction *function = (ObjFunction *)object;

		freeChunk(&function->chunk);

		// Note: We allow GC to free the function's (ObjString) name.

		FREE(ObjFunction, object);

		break;
	}
	case OBJ_NATIVE:
	{
		FREE(ObjNative, object);

		break;
	}
	case OBJ_STRING:
	{
		ObjString *string = (ObjString *)object;

		FREE_ARRAY(char, string->chars, string->length + 1);

		FREE(ObjString, object);

		break;
	}
	case OBJ_UPVALUE:
	{
		// Note that we don't free the upvalue's referenced Value variable, which
		// may be shared.

		FREE(ObjUpvalue, object);

		break;
	}
	}
}

void freeObjects()
{
	Obj *object = vm.objects;

	while (object != NULL)
	{
		Obj *next = object->next;

		freeObject(object);

		object = next;
	}
}