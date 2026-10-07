#ifndef _SEMA_H_
#define _SEMA_H_

#include "parsing/program.h"
#include "parsing/type.h"

typedef struct {
  const char *name;
  DataType d_type;
  IdentifierType i_type;
  const void *scope;
} Identifier;

typedef struct {
  Identifier *elements;
  size_t count;
  size_t capacity;
} Identifiers;

typedef struct {
  const void **elements;
  size_t count;
  size_t capacity;
} Scopes;

/*
 * analyse_program takes in a parsed AST program and will analyse to ensure this is a valid program
 * Will return 0 for a valid program, 1 on unexpected error, and 2 for an invalid program
 */
unsigned char analyse_program(const Program *program);

#endif // _SEMA_H_
