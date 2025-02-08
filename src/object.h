#ifndef clox_object_h
#define clox_object_h

#include "chunk.h"
#include "common.h"
#include "value.h"

#define OBJ_TYPE(value) (AS_OBJ(value)->type)

#define IS_CLOSURE(value) isObjType(value, OBJ_CLOSURE)
#define IS_FUNCTION(value) isObjType(value, OBJ_FUNCTION)
#define IS_NATIVE(value) isObjType(value, OBJ_NATIVE)
#define IS_STRING(value) isObjType(value, OBJ_STRING)

#define AS_CLOSURE(value) ((ObjClosure *)AS_OBJ(value))
#define AS_FUNCTION(value) ((ObjFunction *)AS_OBJ(value))
#define AS_NATIVE(value) (((ObjNative *)AS_OBJ(value))->function)
#define AS_STRING(value) ((ObjString *)AS_OBJ(value))
#define AS_CSTRING(value) (((ObjString *)AS_OBJ(value))->chars)

typedef enum
{
  OBJ_CLOSURE,
  OBJ_FUNCTION,
  OBJ_NATIVE,
  OBJ_STRING,
} ObjType;

struct Obj
{
  ObjType type;
  struct Obj *next;
};

typedef struct
{
  Obj obj;          // Functions are first-class objects in Lox, so we need to be
                    // able to pass them around on the stack.
  int arity;        // The number of parameters to the function.
  Chunk chunk;      // Function body's bytecode.
  ObjString *name;  // For human-readable runtime errors.
  int upvalueCount; // Stores the function body's (static) upvalue count.
                    // We store it in `Function` so as to read it at runtime.
} ObjFunction;

typedef Value (*NativeFn)(int argCount, Value *args);

typedef struct
{
  Obj obj;
  NativeFn function;
} ObjNative;

struct ObjString
{
  Obj obj;
  int length;
  char *chars;
  uint32_t hash;
};

typedef struct
{
  Obj obj;
  ObjFunction *function;
} ObjClosure;

ObjClosure *newClosure(ObjFunction *function);
ObjFunction *newFunction();
ObjNative *newNative(NativeFn function);
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