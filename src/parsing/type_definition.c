#include "parsing/type_definition.h"
#include "dynamic_array.h"
#include "parsing/data_type.h"
#include "parsing/utils.h"
#include <assert.h>
#include <stdio.h>

#define STRUCT_FIELD_LEN_ESTIMATE 10

unsigned char parse_struct_definition(StructDefinition *struct_def, ParseState *state);

unsigned char parse_type_definition(TypeDefinition *type_def, ParseState *state) {
  const Token *ident_tok = expect_next(T_IDENTIFIER, state);
  if (ident_tok == NULL) {
    fprintf(stderr, "Expected identifier token for type definition\n");
    return 1;
  }
  assert(ident_tok->item != NULL);

  if (expect_next(T_COLON, state) == NULL) {
    fprintf(stderr, "Expected ':' after identifier for type definition\n");
    return 1;
  }

  const TokenType kind = peek_index(state->tokens, state->idx);
  if (kind == T_STRUCT) {
    type_def->td_kind = TD_STRUCT;

    StructDefinition s_def;
    s_def.name = ident_tok->item;
    if (parse_struct_definition(&s_def, state) != 0) {
      return 1;
    }
    type_def->td_union.s_def = s_def;
  } else {
    fprintf(stderr, "Unexpected token type %s for type definition\n", t_type_to_string(kind));
    return 1;
  }

  return 0;
}

unsigned char parse_fields(Fields *fields, ParseState *state) {
  while (true) {
    Field field;
    const Token *ident = expect_next(T_IDENTIFIER, state);
    if (ident == NULL) {
      fprintf(stderr, "Expected identifier for field value\n");
      return 1;
    }
    assert(ident->item != NULL);
    field.term_ident.name = ident->item;

    if (expect_next(T_COLON, state) == NULL) {
      fprintf(stderr, "Expected colon after field value\n");
      return 1;
    }

    if (parse_data_type(&field.d_type, state) != 0) {
      fprintf(stderr, "Failed to parse datatype for field\n");
      return 1;
    }

    dyn_array_insert(fields, field);

    if (peek_index(state->tokens, state->idx) != T_COMMA) {
      return 0;
    }
    state->idx += 1;
  }
}

unsigned char parse_struct_definition(StructDefinition *struct_def, ParseState *state) {
  if (expect_next(T_STRUCT, state) == NULL) {
    fprintf(stderr, "Expected 'struct' keyword for struct type\n");
    return 1;
  }

  if (expect_next(T_LEFT_CURLY, state) == NULL) {
    fprintf(stderr, "Expected '{' for struct type\n");
    return 1;
  }

  dyn_array_init(&struct_def->fields, sizeof(Field), STRUCT_FIELD_LEN_ESTIMATE);
  if (parse_fields(&struct_def->fields, state) != 0) {
    fprintf(stderr, "Faield to parse struct fields\n");
    return 1;
  }

  if (expect_next(T_RIGHT_CURLY, state) == NULL) {
    fprintf(stderr, "Expected '}' for struct type\n");
    return 1;
  }

  if (expect_next(T_SEMI, state) == NULL) {
    fprintf(stderr, "Expected ';' for end of struct type\n");
    return 1;
  }

  return 0;
}
