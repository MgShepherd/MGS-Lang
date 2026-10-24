#include "parsing/user_type.h"
#include "dynamic_array.h"
#include "parsing/utils.h"
#include <assert.h>
#include <stdio.h>

#define STRUCT_FIELD_LEN_ESTIMATE 10

unsigned char parse_struct_definition(StructType *struct_type, const Tokens *tokens, size_t *idx);

unsigned char parse_user_type(UserType *user_type, const Tokens *tokens, size_t *idx) {
  const Token *ident_tok = expect_next(T_IDENTIFIER, tokens, idx);
  if (ident_tok == NULL) {
    fprintf(stderr, "Expected identifier token for user defined type\n");
    return 1;
  }
  assert(ident_tok->item != NULL);

  if (expect_next(T_COLON, tokens, idx) == NULL) {
    fprintf(stderr, "Expected ':' after identifier for user defined type\n");
    return 1;
  }

  const TokenType kind = peek_index(tokens, *idx);
  if (kind == T_STRUCT) {
    user_type->u_kind = U_STRUCT;

    StructType s_type;
    s_type.name = ident_tok->item;
    if (parse_struct_definition(&s_type, tokens, idx) != 0) {
      return 1;
    }
    user_type->u_union.s_type = s_type;
  } else {
    fprintf(stderr, "Unexpected token type %s for user defined type\n", t_type_to_string(kind));
    return 1;
  }

  return 0;
}

unsigned char parse_fields(Fields *fields, const Tokens *tokens, size_t *idx) {
  while (true) {
    Field field;
    const Token *ident = expect_next(T_IDENTIFIER, tokens, idx);
    if (ident == NULL) {
      fprintf(stderr, "Expected identifier for field value\n");
      return 1;
    }
    assert(ident->item != NULL);
    field.term_ident.name = ident->item;

    if (expect_next(T_COLON, tokens, idx) == NULL) {
      fprintf(stderr, "Expected colon after field value\n");
      return 1;
    }

    if (parse_data_type(&field.d_type, tokens, idx) != 0) {
      fprintf(stderr, "Failed to parse datatype for field\n");
      return 1;
    }

    dyn_array_insert(fields, field);

    if (peek_index(tokens, *idx) != T_COMMA) {
      return 0;
    }
    *idx += 1;
  }
}

unsigned char parse_struct_definition(StructType *struct_type, const Tokens *tokens, size_t *idx) {
  if (expect_next(T_STRUCT, tokens, idx) == NULL) {
    fprintf(stderr, "Expected 'struct' keyword for struct type\n");
    return 1;
  }

  if (expect_next(T_LEFT_CURLY, tokens, idx) == NULL) {
    fprintf(stderr, "Expected '{' for struct type\n");
    return 1;
  }

  dyn_array_init(&struct_type->fields, sizeof(Field), STRUCT_FIELD_LEN_ESTIMATE);
  if (parse_fields(&struct_type->fields, tokens, idx) != 0) {
    fprintf(stderr, "Faield to parse struct fields\n");
    return 1;
  }

  if (expect_next(T_RIGHT_CURLY, tokens, idx) == NULL) {
    fprintf(stderr, "Expected '}' for struct type\n");
    return 1;
  }

  if (expect_next(T_SEMI, tokens, idx) == NULL) {
    fprintf(stderr, "Expected ';' for end of struct type\n");
    return 1;
  }

  return 0;
}
