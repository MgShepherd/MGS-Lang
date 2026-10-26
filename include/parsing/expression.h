#ifndef _PARSING_EXPRESSION_H_
#define _PARSING_EXPRESSION_H_

#include "lexer.h"
#include "parsing/data_type.h"
#include "parsing/type.h"
#include <stdint.h>

typedef struct Expression Expression;

typedef union {
  int8_t i8;
  int16_t i16;
  int32_t i32;
  int64_t i64;
  bool b;
} LiteralUnion;

typedef struct {
  LiteralUnion l_union;
  // d_type will be populated as part of semantic analysis
  const DataType *d_type;
} Literal;

typedef struct {
  Expression *elements;
  size_t count;
  size_t capacity;
} Expressions;

typedef struct {
  // TODO: Update this to use the char* rather than the actual token (same as TerminalIdentifier)
  const Token *name;
  Expressions parameters;
} FunctionCall;

typedef struct {
  const char *name;
  // Will be populated by sema
  const void *scope;
} TerminalIdentifier;

typedef struct {
  TerminalIdentifier term_ident;
  const Token *index_tok;
  // idx will be populated by sema
  uint64_t idx;
} ArrayAccess;

typedef struct {
  const Token *tok;
  // Will be populated by sema
  Literal literal;
} TerminalLiteral;

typedef struct {
  Expressions items;
  // d_type will be populated as part of semantic analysis
  const DataType *d_type;
} ArrayLiteral;

typedef struct {
  // d_type will be populated as part of semantic analysis
  const DataType *d_type;
} StructLiteral;

typedef struct {
  TerminalLiteral lit;
  TerminalIdentifier ident;
  FunctionCall func_call;
  ArrayLiteral arr_literal;
  ArrayAccess arr_access;
  StructLiteral struct_literal;
} TerminalUnion;

typedef struct {
  TerminalUnion t_union;
  TerminalType t_type;
} Terminal;

typedef struct {
  Terminal item;
  const Token *sign;
} TerminalExpr;

typedef struct {
  TerminalExpr lhs;
  OperatorType op;
  Expression *rhs;
} CompoundExpr;

typedef union {
  TerminalExpr term;
  CompoundExpr comp;
} ExpressionUnion;

struct Expression {
  ExpressionUnion e_union;
  ExpressionType e_type;
};

unsigned char parse_terminal_identifier(TerminalIdentifier *term_ident, const Tokens *tokens, size_t *idx);
unsigned char parse_array_access(ArrayAccess *arr_access, const Tokens *tokens, size_t *idx);

/*
 * parse_expression will attempt to convert the next tokens into an expression and output to provided expression
 * Will update the idx pointer to point at the next token when valid
 * Returns 0 on success, 1 on failure
 */
unsigned char parse_expression(Expression *expression, const Tokens *tokens, size_t *idx);

/*
 * Frees a single expression
 */
void expression_free(Expression *expression);

#endif // _PARSING_EXPRESSION_H_
