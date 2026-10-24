#ifndef _PARSING_TYPE_H_
#define _PARSING_TYPE_H_

#include "lexer.h"

#define STATEMENT_TYPES                                                                                                \
  X(S_NONE)                                                                                                            \
  X(S_DECLARATION)                                                                                                     \
  X(S_ASSIGNMENT)                                                                                                      \
  X(S_IF)                                                                                                              \
  X(S_VOID)                                                                                                            \
  X(S_RETURN)

#define EXPRESSION_TYPES                                                                                               \
  X(E_NONE)                                                                                                            \
  X(E_COMPOUND)                                                                                                        \
  X(E_TERMINAL)

#define PRIMITIVE_TYPES                                                                                                \
  X(P_NONE)                                                                                                            \
  X(P_BOOL)                                                                                                            \
  X(P_VOID)                                                                                                            \
  X(P_I8)                                                                                                              \
  X(P_I16)                                                                                                             \
  X(P_I32)                                                                                                             \
  X(P_I64)

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

#define TERMINAL_TYPES                                                                                                 \
  X(TERM_NONE)                                                                                                         \
  X(TERM_LITERAL)                                                                                                      \
  X(TERM_IDENTIFIER)                                                                                                   \
  X(TERM_ARRAY_LITERAL)                                                                                                \
  X(TERM_ARRAY_ACCESS)                                                                                                 \
  X(TERM_FUNC_CALL)

#define IDENTIFIER_TYPES                                                                                               \
  X(I_NONE)                                                                                                            \
  X(I_VARIABLE)                                                                                                        \
  X(I_CONST)

#define DATATYPE_KINDS                                                                                                 \
  X(D_NONE)                                                                                                            \
  X(D_ARRAY)                                                                                                           \
  X(D_PRIMITIVE)

#define ASSIGNMENT_TYPES                                                                                               \
  X(A_NONE)                                                                                                            \
  X(A_IDENTIFIER)                                                                                                      \
  X(A_ARRAY_ELEMENT)

#define X(N) N,
typedef enum { STATEMENT_TYPES } StatementType;
typedef enum { EXPRESSION_TYPES } ExpressionType;
typedef enum { PRIMITIVE_TYPES } PrimitiveType;
typedef enum { OPERATOR_TYPES } OperatorType;
typedef enum { LITERAL_TYPES } LiteralType;
typedef enum { TERMINAL_TYPES } TerminalType;
typedef enum { DATATYPE_KINDS } DataTypeKind;
typedef enum { ASSIGNMENT_TYPES } AssignmentType;
#undef X

const char *s_type_to_string(StatementType s);
const char *e_type_to_string(ExpressionType e);
const char *p_type_to_string(PrimitiveType p);
const char *o_type_to_string(OperatorType o);
const char *l_type_to_string(LiteralType l);
const char *term_type_to_string(TerminalType t);

/*
 * Converts a provided token type into the matching primitive type
 * Will return P_NONE if invalid primitive
 */
PrimitiveType tok_to_primitive_type(TokenType t_type);

/*
 * Converts a provided token type into the matching operator type
 * Will return O_NONE if invalid datatype
 */
OperatorType tok_to_op_type(TokenType t_type);

#endif // _PARSING_TYPE_H_
