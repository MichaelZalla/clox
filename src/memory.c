#include <stdlib.h>

#include "object.h"
#include "memory.h"
#include "vm.h"

#ifdef DEBUG_LOG_GC
#include <stdio.h>
#include "debug.h"
#endif

void *reallocate(void *pointer, size_t oldSize, size_t newSize)
{
	if (newSize > oldSize)
	{
#ifdef DEBUG_STRESS_GC
		collectGarbage();
#endif
	}

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

void collectGarbage()
{
#ifdef DEBUG_LOG_GC
	printf("-- gc begin\n");
#endif

#ifdef DEBUG_LOG_GC
	printf("-- gc end\n");
#endif
}

static void freeObject(Obj *object)
{
#ifdef DEBUG_LOG_GC
	printf("%p free type ", (void *)object);
	switch (object->type)
	{
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