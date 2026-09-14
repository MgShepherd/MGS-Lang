#ifndef _PARSING_TYPE_H_
#define _PARSING_TYPE_H_

#include "lexer.h"

#define STATEMENT_TYPES                                                                                                \
  X(S_NONE)                                                                                                            \
  X(S_DECLARATION)                                                                                                     \
  X(S_ASSIGNMENT)                                                                                                      \
  X(S_IF)                                                                                                              \
  X(S_RETURN)

#define EXPRESSION_TYPES                                                                                               \
  X(E_NONE)                                                                                                            \
  X(E_COMPOUND)                                                                                                        \
  X(E_TERMINAL)

#define DATA_TYPES                                                                                                     \
  X(D_NONE)                                                                                                            \
  X(D_BOOL)                                                                                                            \
  X(D_I32)

#define OPERATOR_TYPES                                                                                                 \
  X(O_NONE)                                                                                                            \
  X(O_LT)                                                                                                              \
  X(O_LTE)                                                                                                             \
  X(O_GT)                                                                                                              \
  X(O_GTE)                                                                                                             \
  X(O_PLUS)                                                                                                            \
  X(O_MINUS)

#define LITERAL_TYPES                                                                                                  \
  X(L_NONE)                                                                                                            \
  X(L_NUM)                                                                                                             \
  X(L_BOOL)

#define X(N) N,
typedef enum { STATEMENT_TYPES } StatementType;
typedef enum { EXPRESSION_TYPES } ExpressionType;
typedef enum { DATA_TYPES } DataType;
typedef enum { OPERATOR_TYPES } OperatorType;
typedef enum { LITERAL_TYPES } LiteralType;
#undef X

const char *s_type_to_string(StatementType s);
const char *e_type_to_string(ExpressionType e);
const char *d_type_to_string(DataType d);
const char *o_type_to_string(OperatorType o);
const char *l_type_to_string(LiteralType l);

/*
 * Converts a provided token type into the matching datatype
 * Will return D_NONE if invalid datatype
 */
DataType tok_to_data_type(TokenType t_type);

/*
 * Converts a provided token type into the matching operator type
 * Will return O_NONE if invalid datatype
 */
OperatorType tok_to_op_type(TokenType t_type);

#endif // _PARSING_TYPE_H_
