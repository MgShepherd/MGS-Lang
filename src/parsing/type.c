#include "parsing/type.h"

#include <assert.h>
#include <string.h>

#define X(N)                                                                                                           \
  case N:                                                                                                              \
    return #N;

const char *s_type_to_string(StatementType s) {
  switch (s) {
    STATEMENT_TYPES
  default:
    return "unknown";
  }
}

const char *e_type_to_string(ExpressionType e) {
  switch (e) {
    EXPRESSION_TYPES
  default:
    return "unknown";
  }
}

const char *d_type_to_string(DataType d) {
  switch (d) {
    DATA_TYPES
  default:
    return "unknown";
  }
}

const char *o_type_to_string(OperatorType o) {
  switch (o) {
    OPERATOR_TYPES
  default:
    return "unknown";
  }
}

const char *l_type_to_string(LiteralType l) {
  switch (l) {
    LITERAL_TYPES
  default:
    return "unknown";
  }
}
#undef X

DataType tok_to_data_type(TokenType t_type) {
  switch (t_type) {
  case T_I32:
    return D_I32;
  case T_BOOL:
    return D_BOOL;
  default:
    return D_NONE;
  }
}

OperatorType tok_to_op_type(TokenType t_type) {
  switch (t_type) {
  case T_PLUS:
    return O_PLUS;
  case T_MINUS:
    return O_MINUS;
  case T_LT:
    return O_LT;
  case T_LTE:
    return O_LTE;
  case T_GT:
    return O_GT;
  case T_GTE:
    return O_GTE;
  default:
    return O_NONE;
  }
}
