#ifndef clox_value_h
#define clox_value_h

#include <string.h>

#include "common.h"

typedef struct Obj Obj;
typedef struct ObjString ObjString;

#ifdef NAN_BOXING

#define SIGN_BIT ((uint64_t)0x8000000000000000)

// High bits 50 to 62 (excludes bit 63, i.e., the sign bit).
//
//   - Bit 50: Intel "QNaN Floating-Point Indefinite".
//   - Bit 51: quiet NaN bit.
//   - Bits 52-60: NaN bits.
//
#define QUIET_NAN_BITS ((uint64_t)0x7ffc000000000000)

#define TAG_NIL 1		// "01"
#define TAG_FALSE 2 // "10"
#define TAG_TRUE 3	// "11"

// A 64-bit boxed Value type.
typedef uint64_t Value; // A bitwise-operation-friendly type.

// Every Value that is not a valid number uses a special "quiet NaN"
// representation; we assume that this representation (QUIET_NAN_BITS) is
// distinct from any *meaningful* NaN that may arise from a user running a valid
// Lox program—even if the program produces a bad result, e.g., divide by zero.

#define FALSE_VAL ((Value)(uint64_t)(QUIET_NAN_BITS | TAG_FALSE))
#define TRUE_VAL ((Value)(uint64_t)(QUIET_NAN_BITS | TAG_TRUE))

#define IS_BOOL(value) (((value) | 1) == TRUE_VAL) // Only 2 valid bit patterns.

#define IS_NIL(value) ((value) == NIL_VAL)

// If the value's 13 "quiet NaN bits" hold a pattern other than "all 1's", we
// assume it represents one of Lox's special sentinel values (such as `nil`).
#define IS_NUMBER(value) (((value) & QUIET_NAN_BITS) != QUIET_NAN_BITS)

// Tests whether all quiet NaN bits are set, and the sign bit is set; the sign
// bit serves as our "tag" bit for Object types.
#define IS_OBJ(value) \
	(((value) & (QUIET_NAN_BITS | SIGN_BIT)) == (QUIET_NAN_BITS | SIGN_BIT))

// Assumes that we'll only call `AS_BOOL()` on a `true` or a `false` Value.
#define AS_BOOL(value) ((value) == TRUE_VAL)

#define AS_NUMBER(value) valueToDouble(value)

// Masks out the sign bit and quiet NaN bits, leaving the pointer address.
#define AS_OBJ(value) \
	((Obj *)(uintptr_t)((value) & ~(SIGN_BIT | QUIET_NAN_BITS)))

#define BOOL_VAL(value) ((value) ? TRUE_VAL : FALSE_VAL)

#define NIL_VAL ((Value)(uint64_t)(QUIET_NAN_BITS | TAG_NIL))

#define NUMBER_VAL(value) doubleToValue(value)

// Takes the pointer and sets all of the quiet NaN bits and the sign bit;
// although a 64-bit pointer is stored as 64 bits, in practice, only the lowest
// 48 bits carry information—the higher bits are typically always zero.
#define OBJ_VAL(value) \
	(Value)(SIGN_BIT | QUIET_NAN_BITS | (uint64_t)(uintptr_t)(value))

static inline double
valueToDouble(Value value)
{
	double num;

	memcpy(&num, &value, sizeof(Value));

	return num;
}

static inline Value doubleToValue(double num)
{
	// The following is the supported C and C++ idiom for type-punning, meaning
	// that most compilers recognize the pattern and will optimize away the
	// memcpy() call entirely.

	Value value;

	memcpy(&value, &num, sizeof(double));

	return value;
}
#else
// A 128-bit tagged union Value type.
typedef enum
{
	VAL_BOOL,
	VAL_NIL,
	VAL_NUMBER,
	VAL_OBJ,
} ValueType;

typedef struct
{
	ValueType type; // 4 bytes
	// <PADDING>  	// 4 bytes
	union
	{
		bool boolean;	 // 4 bytes
		double number; // 8 bytes
		Obj *obj;			 // 8 bytes
	} as;						 // 8 bytes
} Value;					 // 16-byte aligned

#define IS_BOOL(value) ((value).type == VAL_BOOL)
#define IS_NIL(value) ((value).type == VAL_NIL)
#define IS_NUMBER(value) ((value).type == VAL_NUMBER)
#define IS_OBJ(value) ((value).type == VAL_OBJ)

#define AS_BOOL(value) ((value).as.boolean)
#define AS_NUMBER(value) ((value).as.number)
#define AS_OBJ(value) ((value).as.obj)

#define BOOL_VAL(value) ((Value){VAL_BOOL, {.boolean = value}})
#define NIL_VAL ((Value){VAL_NIL, {.number = 0}})
#define NUMBER_VAL(value) ((Value){VAL_NUMBER, {.number = value}})
#define OBJ_VAL(value) ((Value){VAL_OBJ, {.obj = (Obj *)value}})
#endif

typedef struct
{
	int count;
	int capacity;
	Value *values;
} ValueArray;

bool valuesEqual(Value a, Value b);

void initValueArray(ValueArray *array);
void writeValueArray(ValueArray *array, Value value);
void freeValueArray(ValueArray *array);
void printValue(Value value);

#endif // !clox_value_h
