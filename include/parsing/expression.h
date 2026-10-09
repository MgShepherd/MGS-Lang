#ifndef _PARSING_EXPRESSION_H_
#define _PARSING_EXPRESSION_H_

#include "lexer.h"
#include "parsing/type.h"

typedef struct Expression Expression;

typedef union {
  long long num;
  bool b;
} LiteralUnion;

typedef struct {
  LiteralType l_type;
  LiteralUnion l_union;
} Literal;

typedef struct {
  Expression *elements;
  size_t count;
  size_t capacity;
} CallParameters;

typedef struct {
  const Token *name;
  CallParameters parameters;
} FunctionCall;

typedef struct {
  const char *name;
  // Will be populated by sema
  const void *scope;
} TerminalIdentifier;

typedef struct {
  const Token *tok;
  // Will be populated by sema
  Literal literal;
} TerminalLiteral;

typedef struct {
  TerminalLiteral lit;
  TerminalIdentifier ident;
  FunctionCall func_call;
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
