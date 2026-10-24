#ifndef _PARSING_USER_TYPE_H_
#define _PARSING_USER_TYPE_H_

#include "parsing/expression.h"

typedef struct {
  TerminalIdentifier term_ident;
  DataType d_type;
} Field;

typedef struct {
  Field *elements;
  size_t count;
  size_t capacity;
} Fields;

typedef struct {
  const char *name;
  Fields fields;
} StructType;

typedef struct {
  StructType s_type;
} UserTypeUnion;

typedef struct {
  UserTypeKind u_kind;
  UserTypeUnion u_union;
} UserType;

unsigned char parse_user_type(UserType *user_type, const Tokens *tokens, size_t *idx);

unsigned char parse_fields(Fields *fields, const Tokens *tokens, size_t *idx);

#endif // _PARSING_USER_TYPE_H_
