#ifndef clox_object_h
#define clox_object_h

#include "chunk.h"
#include "common.h"
#include "value.h"

#define OBJ_TYPE(value) (AS_OBJ(value)->type)

// Note: `value` is assumed to be of type `Value`, not `Obj`.
#define IS_FUNCTION(value) isObjType(value, OBJ_FUNCTION)

// Note: `value` is assumed to be of type `Value`, not `Obj`.
#define IS_STRING(value) isObjType(value, OBJ_STRING)

// Value as ObjFunction*.
#define AS_FUNCTION(value) ((ObjFunction *)AS_OBJ(value))

// Value as ObjString*.
#define AS_STRING(value) ((ObjString *)AS_OBJ(value))

// Value as char*.
#define AS_CSTRING(value) (((ObjString *)AS_OBJ(value))->chars)

typedef enum
{
  OBJ_FUNCTION,
  OBJ_STRING,
} ObjType;

struct Obj
{
  ObjType type;
  struct Obj *next;
};

typedef struct
{
  Obj obj;         // Functions are first-class objects in Lox, so we need to be
                   // able to pass them around on the stack.
  int arity;       // The number of parameters to the function.
  Chunk chunk;     // Function body's bytecode.
  ObjString *name; // For human-readable runtime errors.
} ObjFunction;

struct ObjString
{
  Obj obj;
  int length;
  char *chars;
  uint32_t hash;
};

ObjFunction *newFunction();
ObjString *takeString(char *chars, int length);
ObjString *copyString(const char *start, int length);
void printObject(Value value);

static inline bool isObjType(Value value, ObjType type)
{
  // Note: Were this function body just a macro, `value` would be evaluated
  // twice—once for each occurrence in this function body.
  return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif