#ifndef clox_memory_h
#define clox_memory_h

#include "common.h"
#include "compiler.h"
#include "object.h"

#define ALLOCATE(type, count) \
	(type *)reallocate(NULL, 0, sizeof(type) * (count))

#define FREE(type, pointer) reallocate(pointer, sizeof(type), 0);

#define NEXT_CAPACITY(capacity) \
	((capacity) < 8 ? 8 : (capacity) * 2)

#define GROW_ARRAY(type, pointer, oldCount, newCount) \
	(type *)reallocate(                                 \
			pointer,                                        \
			sizeof(type) * (oldCount),                      \
			sizeof(type) * (newCount))

#define FREE_ARRAY(type, pointer, oldCount) \
	(type *)reallocate(                       \
			pointer,                              \
			sizeof(type) * (oldCount),            \
			0)

void *reallocate(void *pointer, size_t oldSize, size_t newSize);
void markObject(Obj *object);
void markValue(Value value);
void collectGarbage();
void freeObjects();

#endif // !clox_memory_h
