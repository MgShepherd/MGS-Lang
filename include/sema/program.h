#ifndef _SEMA_PROGRAM_H_
#define _SEMA_PROGRAM_H_

#include "parsing/type.h"
#include <stdlib.h>

typedef struct {
  const void **elements;
  size_t count;
  size_t capacity;
} Scopes;

typedef struct {
  const Program *program;
  const Functions functions;
  const TypeDefinitions type_defs;
  Scopes scopes;
  bool requires_array_type;
} SemaState;

unsigned char analyse_program(Program *program);

unsigned char analyse_block(SemaState *state, Block *block, const DataType *func_type);

void free_sema_state(SemaState *state);

#endif // _SEMA_PROGRAM_H_
