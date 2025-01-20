#include <stdio.h>
#include <string.h>

#include "object.h"
#include "memory.h"
#include "value.h"

bool valuesEqual(Value a, Value b)
{
	// Note: We avoid using a basic memcmp() because it runs the risk of comparing
	// padding bytes that are not guaranteed by C to hold any particular values.

	if (a.type != b.type)
	{
		return false;
	}

	switch (a.type)
	{
	case VAL_BOOL:
		return AS_BOOL(a) == AS_BOOL(b);
	case VAL_NIL:
		return true;
	case VAL_NUMBER:
		return AS_NUMBER(a) == AS_NUMBER(b);
	case VAL_OBJ:
	{
		ObjString *aString = AS_STRING(a);
		ObjString *bString = AS_STRING(b);

		if (aString->length == bString->length)
		{
			return memcmp(aString->chars, bString->chars, aString->length) == 0;
		}

		return false;
	}
	default:
		// Unreachable.
		return false;
	}
}

void initValueArray(ValueArray *array)
{
	array->count = 0;
	array->capacity = 0;
	array->values = NULL;
}

void writeValueArray(ValueArray *array, Value value)
{
	if (array->capacity < array->count + 1)
	{
		int oldCapacity = array->capacity;

		array->capacity = NEXT_CAPACITY(array->capacity);

		array->values = GROW_ARRAY(Value, array->values, oldCapacity, array->capacity);
	}

	array->values[array->count] = value;
	array->count++;
}

void freeValueArray(ValueArray *array)
{
	FREE_ARRAY(Value, array->values, array->capacity);

	initValueArray(array);
}

void printValue(Value value)
{
	switch (value.type)
	{
	case VAL_BOOL:
		printf(AS_BOOL(value) ? "true" : "false");
		break;
	case VAL_NIL:
		printf("nil");
		break;
	case VAL_NUMBER:
		printf("%g", AS_NUMBER(value));
		break;
	case VAL_OBJ:
		printObject(value);
		break;
	}
}
