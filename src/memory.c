#include <stdlib.h>

#include "object.h"
#include "memory.h"
#include "vm.h"

#ifdef DEBUG_LOG_GC
#include <stdio.h>
#include "debug.h"
#endif

#define GC_HEAP_GROW_FACTOR 2

// Forward declarations.
static void markRoots();
static void traceReferences();
static void sweep();

void *reallocate(void *pointer, size_t oldSize, size_t newSize)
{
	vm.bytesAllocated += newSize - oldSize;

	if (newSize > oldSize)
	{
#ifdef DEBUG_STRESS_GC
		collectGarbage();
#endif
	}

	if (vm.bytesAllocated > vm.nextGC)
	{
		collectGarbage();
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

void markObject(Obj *object)
{
	if (object == NULL)
	{
		return;
	}

	if (object->isMarked)
	{
		return;
	}

#ifdef DEBUG_LOG_GC
	printf("%p mark ", (void *)object);
	printValue(OBJ_VAL(object));
	printf("\n");
#endif

	object->isMarked = true;

	if (vm.grayCapacity < vm.grayCount + 1)
	{
		vm.grayCapacity = NEXT_CAPACITY(vm.grayCapacity);

		vm.grayStack = (Obj **)realloc(vm.grayStack, sizeof(Obj *) * vm.grayCapacity);

		// Terminates if we failed to resize through `realloc()`.
		if (vm.grayStack == NULL)
		{
			exit(1);
		}
	}

	vm.grayStack[vm.grayCount++] = object;
}

void markValue(Value value)
{
	if (IS_OBJ(value))
	{
		markObject(AS_OBJ(value));
	}
}

static void markArray(ValueArray *array)
{
	for (int i = 0; i < array->count; i++)
	{
		markValue(array->values[i]);
	}
}

static void blackenObject(Obj *object)
{
#ifdef DEBUG_LOG_GC
	printf("%p blacken ", (void *)object);
	printValue(OBJ_VAL(object));
	printf("\n");
#endif

	// By definition, a "black" object is any object whose `isMarked` flag is set
	// and which isn't still sitting in the VM's `grayStack`.

	switch (object->type)
	{
	case OBJ_UPVALUE:
	{
		// A closed upvalue holds a reference to its closed-over Value on the heap.
		markValue(((ObjUpvalue *)object)->closed);

		break;
	}
	case OBJ_FUNCTION:
	{
		ObjFunction *function = (ObjFunction *)object;

		// A function holds a reference to a heap-allocated ObjString (`name`).
		markObject((Obj *)function->name);

		// A function's Chunk holds a heap-allocated `constants` table.
		markArray(&function->chunk.constants);

		break;
	}
	case OBJ_INSTANCE:
	{
		ObjInstance *instance = (ObjInstance *)object;

		markObject((Obj *)instance->class);

		markTable(&instance->fields);

		break;
	}
	case OBJ_CLASS:
	{
		ObjClass *class = (ObjClass *)object;

		// A class holds a reference to a heap-allocated ObjString (`name`).
		markObject((Obj *)class->name);

		// Traces through the keys (strings) and method values.
		markTable(&class->methods);

		break;
	}
	case OBJ_CLOSURE:
	{
		ObjClosure *closure = (ObjClosure *)object;

		// A closure holds a reference to its associated ObjFunction.
		markObject((Obj *)closure->function);

		// A closure holds a linked list of ObjUpvalue pointers.
		for (int i = 0; i < closure->upvalueCount; i++)
		{
			markObject((Obj *)closure->upvalues[i]);
		}

		break;
	}
	case OBJ_NATIVE:
		/* falls through */
	case OBJ_STRING:
		// No outgoing references.
		break;
	}
}

void collectGarbage()
{
#ifdef DEBUG_LOG_GC
	printf("-- gc begin\n");
	size_t before = vm.bytesAllocated;
#endif

	// Marks all immediately reachable heap-allocated Objects as "gray".
	markRoots();

	// Processes each "gray"-marked Object—marking it as "black"—while also
	// marking as "gray" any new Objects that are reachable through it.
	traceReferences();

	// Prunes the `strings` table for entries that reference unreachable strings.
	tableRemoveWhite(&vm.strings);

	// Traverses the VM's Object-list, looking for any "white" objects to reclaim.
	sweep();

	// Sets the VM's new heap threshold that will trigger the next GC cycle.
	vm.nextGC = vm.bytesAllocated * GC_HEAP_GROW_FACTOR;

#ifdef DEBUG_LOG_GC
	printf("-- gc end\n");
	printf(
			"   collected %zu bytes (from %zu to %zu) next at %zu\n",
			before - vm.bytesAllocated,
			before,
			vm.bytesAllocated,
			vm.nextGC);
#endif
}

static void freeObject(Obj *object)
{
#ifdef DEBUG_LOG_GC
	printf("%p free type ", (void *)object);
	switch (object->type)
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
	case OBJ_INSTANCE:
	{
		printf("instance\n");
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
	case OBJ_CLASS:
	{
		ObjClass *class = (ObjClass *)object;

		freeTable(&class->methods);

		// Frees the ObjClass, without freeing its name string.
		FREE(ObjClass, object);

		break;
	}
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
	case OBJ_INSTANCE:
	{
		ObjInstance *instance = (ObjInstance *)object;

		// Frees the table's Value entries, but not heap-allocated objects that
		// those Value entries may have pointed to; the GC handles their lifetimes.
		freeTable(&instance->fields);

		FREE(ObjInstance, object);

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

	free(vm.grayStack);
}

static void markRoots()
{
	// Marks all (Object-type) Values on the value stack.
	for (Value *valueSlot = vm.stack; valueSlot < vm.stackTop; valueSlot++)
	{
		markValue(*valueSlot);
	}

	// Marks each call frame's associated closure object.
	for (int i = 0; i < vm.frameCount; i++)
	{
		markObject((Obj *)vm.frames[i].closure);
	}

	// Marks all open upvalues that the VM can directly reach.
	for (ObjUpvalue *upvalue = vm.openUpvalues; upvalue != NULL; upvalue = (ObjUpvalue *)upvalue->next)
	{
		markObject((Obj *)upvalue);
	}

	// Marks all global Values and their string keys.
	markTable(&vm.globals);

	// Marks all objects in memory that were allocated by any compilers.
	markCompilerRoots();
}

static void traceReferences()
{
	// Traverses the VM's gray-marked Object list and blackens each item; with
	// each item that we visit, we may encounter references to white objects that
	// must be marked gray, and added to this list.

	// The call to `traceReferences()` terminates when no unvisited items remain
	// in `vm.grayStack`.

	while (vm.grayCount > 0)
	{
		Obj *object = vm.grayStack[--vm.grayCount];

		blackenObject(object);
	}

	// At this point, every object on the heap is either "black" or "white";
	// whatever is still "white" is unreachable memory that we can reclaim.
}

static void sweep()
{
	Obj *previousObject = NULL;
	Obj *currentObject = vm.objects;

	// Traverses the VM's Object list, removing and deallocating any Object marked
	// as "white", i.e., that does not have its `isMarked` flag set.

	while (currentObject != NULL)
	{
		// Checks if the object was marked this GC cycle, via `traceReferences()`.
		if (currentObject->isMarked)
		{
			// Resets the reachable object's `isMarked` flag for the next GC trace.
			currentObject->isMarked = false;

			// Continues forward.
			previousObject = currentObject;

			currentObject = currentObject->next;
		}
		else
		{
			// Prunes the object from the Objects list, and free its memory.
			Obj *unreachableObject = currentObject;

			currentObject = currentObject->next;

			if (previousObject == NULL)
			{
				vm.objects = currentObject;
			}
			else
			{
				previousObject->next = currentObject;
			}

			freeObject(unreachableObject);
		}
	}
}