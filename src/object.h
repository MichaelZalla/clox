#ifndef clox_object_h
#define clox_object_h

#include "common.h"
#include "value.h"

#define OBJ_TYPE(value) (AS_OBJ(value)->type)

// Note: `value` is assumed to be of type `Value`, not `Obj`.
#define IS_STRING(value) isObjType(value, OBJ_STRING)

// Value as ObjString*.
#define AS_STRING(value) ((ObjString *)AS_OBJ(value))

// Value as char*.
#define AS_CSTRING(value) (((ObjString *)AS_OBJ(value))->chars)

typedef enum
{
	OBJ_STRING,
} ObjType;

struct Obj
{
	ObjType type;
};

struct ObjString
{
	Obj obj;
	int length;
	char *chars;
}

static inline bool
isObjType(Value value, ObjType type)
{
	// Note: Were this function body just a macro, `value` would be evaluated
	// twice—once for each occurrence in this function body.
	return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif