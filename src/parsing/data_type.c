#include "parsing/data_type.h"
#include "lexer.h"
#include "parsing/type.h"
#include "parsing/utils.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>

unsigned char parse_primitive(PrimitiveType *primitive_type, const Tokens *tokens, size_t *idx);
unsigned char parse_struct(StructType *struct_type, const Tokens *tokens, size_t *idx);

unsigned char parse_data_type(DataType *data_type, const Tokens *tokens, size_t *idx) {
  data_type->d_kind = D_NONE;
  if (*idx >= tokens->count) {
    fprintf(stderr, "Failed to parse data type as reached end of input\n");
    return 1;
  }

  data_type->variable = expect_next(T_VAR, tokens, idx) != NULL;

  const TokenType next = peek_index(tokens, *idx);
  if (tok_to_primitive_type(next)) {
    data_type->d_kind = D_PRIMITIVE;
    if (parse_primitive(&data_type->d_union.p_type, tokens, idx) != 0) {
      return 1;
    }
  } else if (next == T_IDENTIFIER) {
    data_type->d_kind = D_STRUCT;
    if (parse_struct(&data_type->d_union.s_type, tokens, idx) != 0) {
      return 1;
    }
  } else {
    fprintf(stderr, "Token type %s cannot be converted to data type\n", t_type_to_string(next));
    return 1;
  }

  return 0;
}

const char *d_type_to_string(const DataType *data_type) {
  assert(data_type->d_kind != D_NONE);

  switch (data_type->d_kind) {
  case D_PRIMITIVE:
    return p_type_to_string(data_type->d_union.p_type);
  default:
    assert(false);
  }
}

bool do_data_types_match(const DataType *d_type_1, const DataType *d_type_2) {
  if (d_type_1->d_kind != d_type_2->d_kind) {
    return false;
  }

  switch (d_type_1->d_kind) {
  case D_PRIMITIVE:
    return d_type_1->d_union.p_type == d_type_2->d_union.p_type;
  case D_STRUCT:
    const StructType *s1_type = &d_type_1->d_union.s_type;
    const StructType *s2_type = &d_type_2->d_union.s_type;
    return strcmp(s1_type->name, s2_type->name) == 0;
  default:
    assert(false);
  }
}

unsigned char parse_primitive(PrimitiveType *p_type, const Tokens *tokens, size_t *idx) {
  const TokenType next_type = tokens->elements[(*idx)++].t_type;
  *p_type = tok_to_primitive_type(next_type);
  if (p_type == P_NONE) {
    fprintf(stderr, "Failed to covert token type %s into primitive data type\n", t_type_to_string(next_type));
    return 1;
  }

  return 0;
}

unsigned char parse_struct(StructType *struct_type, const Tokens *tokens, size_t *idx) {
  const Token *ident_tok = expect_next(T_IDENTIFIER, tokens, idx);
  if (ident_tok == NULL) {
    fprintf(stderr, "Expected struct identifier\n");
    return 1;
  }
  assert(ident_tok->item != NULL);
  struct_type->name = ident_tok->item;

  return 0;
}
