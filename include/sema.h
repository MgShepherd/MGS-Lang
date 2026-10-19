#ifndef _SEMA_H_
#define _SEMA_H_

#include "parsing/data_type.h"
#include "parsing/program.h"
#include "parsing/type.h"

typedef struct {
  const void **elements;
  size_t count;
  size_t capacity;
} Scopes;

typedef struct {
  const char *name;
  // TODO: Do we need i_type anymore now that is variable stored within datatype?
  IdentifierType i_type;
  const void *scope;
  // Identifiers only store a pointer to an already existing d_type so not freed when destroyed
  const DataType *d_type;
} Identifier;

typedef struct {
  Identifier *elements;
  size_t count;
  size_t capacity;
} Identifiers;

/*
 * analyse_program takes in a parsed AST program and will analyse to ensure this is a valid program
 * Will return 0 for a valid program, 1 on unexpected error, and 2 for an invalid program
 */
unsigned char analyse_program(const Program *program);

#endif // _SEMA_H_
