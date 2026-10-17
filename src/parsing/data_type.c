#include "parsing/data_type.h"
#include "parsing/type.h"

#include <assert.h>
#include <stdio.h>

unsigned char parse_data_type(DataType *data_type, const Tokens *tokens, size_t *idx) {
  data_type->d_kind = D_NONE;
  if (*idx >= tokens->count) {
    fprintf(stderr, "Failed to parse data type as reached end of input\n");
    return 1;
  }

  const TokenType next_type = tokens->elements[(*idx)++].t_type;
  const PrimitiveType p_type = tok_to_primitive_type(next_type);
  if (p_type == P_NONE) {
    fprintf(stderr, "Failed to covert token type %s into datatype\n", t_type_to_string(next_type));
    return 1;
  }

  data_type->d_kind = D_PRIMITIVE;
  data_type->d_union.p_type = p_type;

  return 0;
}

const char *d_type_to_string(const DataType *data_type) {
  assert(data_type->d_kind != D_NONE);
  assert(data_type->d_kind == D_PRIMITIVE);

  return p_type_to_string(data_type->d_union.p_type);
}

bool do_data_types_match(const DataType *d_type_1, const DataType *d_type_2) {
  if (d_type_1->d_kind != d_type_2->d_kind) {
    return false;
  }

  switch (d_type_1->d_kind) {
  case D_PRIMITIVE:
    return d_type_1->d_union.p_type == d_type_2->d_union.p_type;
  default:
    assert(false);
  }
}
