#include <stdlib.h>

#include "memory.h"

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
