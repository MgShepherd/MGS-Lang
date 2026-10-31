#ifndef _PARSING_TYPE_DEFINITION_H_
#define _PARSING_TYPE_DEFINITION_H_

#include "parsing/expression.h"

typedef struct TypeDefinition TypeDefinition;

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
} StructDefinition;

typedef struct {
  StructDefinition s_def;
} TypeDefinitionUnion;

struct TypeDefinition {
  TypeDefinitionKind td_kind;
  TypeDefinitionUnion td_union;
};

unsigned char parse_type_definition(TypeDefinition *type_def, const Tokens *tokens, size_t *idx);

unsigned char parse_fields(Fields *fields, const Tokens *tokens, size_t *idx);

#endif // _PARSING_TYPE_DEFINITION_H_
