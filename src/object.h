#ifndef clox_object_h
#define clox_object_h

#include "chunk.h"
#include "common.h"
#include "table.h"
#include "value.h"

#define OBJ_TYPE(value) (AS_OBJ(value)->type)

#define IS_BOUND_METHOD(value) isObjType(value, OBJ_BOUND_METHOD)
#define IS_CLASS(value) isObjType(value, OBJ_CLASS)
#define IS_CLOSURE(value) isObjType(value, OBJ_CLOSURE)
#define IS_FUNCTION(value) isObjType(value, OBJ_FUNCTION)
#define IS_INSTANCE(value) isObjType(value, OBJ_INSTANCE)
#define IS_NATIVE(value) isObjType(value, OBJ_NATIVE)
#define IS_STRING(value) isObjType(value, OBJ_STRING)

#define AS_BOUND_METHOD(value) ((ObjBoundMethod *)AS_OBJ(value))
#define AS_CLASS(value) ((ObjClass *)AS_OBJ(value))
#define AS_CLOSURE(value) ((ObjClosure *)AS_OBJ(value))
#define AS_FUNCTION(value) ((ObjFunction *)AS_OBJ(value))
#define AS_INSTANCE(value) ((ObjInstance *)AS_OBJ(value))
#define AS_NATIVE(value) (((ObjNative *)AS_OBJ(value))->function)
#define AS_STRING(value) ((ObjString *)AS_OBJ(value))
#define AS_CSTRING(value) (((ObjString *)AS_OBJ(value))->chars)

typedef enum
{
  OBJ_BOUND_METHOD,
  OBJ_CLASS,
  OBJ_INSTANCE,
  OBJ_CLOSURE,
  OBJ_FUNCTION,
  OBJ_NATIVE,
  OBJ_STRING,
  OBJ_UPVALUE,
} ObjType;

struct Obj
{
  ObjType type;
  bool isMarked;
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
  // Points to the closed-over variable; when we assign to the variable (in the
  // closure) that the closure captures, we are writing to the original `Value`,
  // and not a copy.
  Value *location;

  // Indicates that this upvalue's variable is relocated to the heap.
  Value closed;

  // Intrusive linked list; each open upvalue points to the next open upvalue
  // that references a local variable further down the stack. When compiling one
  // or more closures inside the body of some function, we can quickly find any
  // upvalue that has already been allocated for some given local variable. The
  // VM holds a pointer to the head of this list at all times.
  struct ObjUpvalue *next;
} ObjUpvalue;

typedef struct
{
  Obj obj;
  ObjFunction *function;
  ObjUpvalue **upvalues;
  int upvalueCount; // Redundant here, but necessary for proper GC.
} ObjClosure;

typedef struct
{
  Obj obj;
  ObjString *name;
  Table methods;
} ObjClass;

typedef struct
{
  Obj obj;
  ObjClass *class;
  Table fields;
} ObjInstance;

typedef struct
{
  Obj obj;
  Value receiver;
  ObjClosure *method;
} ObjBoundMethod;

ObjBoundMethod *newBoundMethod(Value receiver, ObjClosure *method);
ObjClass *newClass(ObjString *name);
ObjClosure *newClosure(ObjFunction *function);
ObjInstance *newInstance(ObjClass *class);
ObjFunction *newFunction();
ObjNative *newNative(NativeFn function);
ObjString *takeString(char *chars, int length);
ObjString *copyString(const char *start, int length);
ObjUpvalue *newUpvalue(Value *slot);
void printObject(Value value);

static inline bool isObjType(Value value, ObjType type)
{
  // Note: Were this function body just a macro, `value` would be evaluated
  // twice—once for each occurrence in this function body.
  return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif