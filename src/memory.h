#ifndef clox_memory_h
#define clox_memory_h

// NEXT_CAPACITY(prevCapacity)
// GROW_ARRAY(uint8_t, chunk->code, prevCapacity, chunk->capacity)

#include "common.h"

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

#endif // !clox_memory_h
