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

const char *p_type_to_string(PrimitiveType p) {
  switch (p) {
    PRIMITIVE_TYPES
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

const char *term_type_to_string(TerminalType t) {
  switch (t) {
    TERMINAL_TYPES
  default:
    return "unknown";
  }
}

const char *i_type_to_string(IdentifierType i) {
  switch (i) {
    IDENTIFIER_TYPE
  default:
    return "unknown";
  }
}
#undef X

PrimitiveType tok_to_primitive_type(TokenType t_type) {
  switch (t_type) {
  case T_I32:
    return P_I32;
  case T_I64:
    return P_I64;
  case T_I8:
    return P_I8;
  case T_I16:
    return P_I16;
  case T_BOOL:
    return P_BOOL;
  case T_VOID:
    return P_VOID;
  default:
    return P_NONE;
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
