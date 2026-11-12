#ifndef _SEMA_PROGRAM_H_
#define _SEMA_PROGRAM_H_

#include <stdlib.h>

#include "parsing/data_type.h"
#include "parsing/program.h"

typedef struct {
  const void **elements;
  size_t count;
  size_t capacity;
} Scopes;

typedef struct {
  const char *name;
  const void *scope;
  // Identifiers only store a pointer to an already existing d_type so not freed when destroyed
  const DataType *d_type;
} Identifier;

typedef struct {
  Identifier *elements;
  size_t count;
  size_t capacity;
} Identifiers;

typedef struct {
  Identifiers identifiers;
  const Functions functions;
  const TypeDefinitions type_defs;
  Scopes scopes;
} SemaState;

unsigned char analyse_program(const Program *program);

unsigned char analyse_block(SemaState *state, Block *block, const DataType *func_type);

void free_sema_state(SemaState *state);

#endif // _SEMA_PROGRAM_H_
