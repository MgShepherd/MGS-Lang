#include "parsing/expression.h"
#include "lexer.h"
#include "parsing/type.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>

unsigned char parse_terminal_expr(TerminalExpr *term, const Tokens *tokens, size_t *idx);

unsigned char parse_expression(Expression *expression, const Tokens *tokens, size_t *idx) {
  if (*idx >= tokens->count) {
    fprintf(stderr, "Attempted to get value token but reached end of input\n");
    return 1;
  }

  TerminalExpr term;
  if (parse_terminal_expr(&term, tokens, idx) != 0) {
    return 1;
  }

  const OperatorType op = *idx >= tokens->count ? O_NONE : tok_to_op_type(tokens->elements[*idx].t_type);
  if (op == O_NONE) {
    expression->e_type = E_TERMINAL;
    expression->e_union.term = term;
    return 0;
  }
  *idx += 1;

  Expression rhs;
  if (parse_expression(&rhs, tokens, idx) != 0) {
    return 1;
  }

  Expression *rhs_ptr = malloc(sizeof(Expression));
  if (rhs_ptr == NULL) {
    fprintf(stderr, "Failed to allocate memory for Right hand side of expression\n");
    return 1;
  }
  *rhs_ptr = rhs;

  CompoundExpr comp = {
      .lhs = term,
      .op = op,
      .rhs = rhs_ptr,
  };

  expression->e_type = E_COMPOUND;
  expression->e_union.comp = comp;
  return 0;
}

void expression_free(Expression *expression) {
  switch (expression->e_type) {
  case E_COMPOUND:
    expression_free(expression->e_union.comp.rhs);
    free(expression->e_union.comp.rhs);
  default:
    break;
  }
}

unsigned char parse_terminal_expr(TerminalExpr *term, const Tokens *tokens, size_t *idx) {
  static const TokenType TERMINAL_TOKEN_TYPES[] = {T_NUMERIC_LIT, T_IDENTIFIER, T_TRUE, T_FALSE};
  static const size_t TERMINAL_TOKEN_LEN = sizeof(TERMINAL_TOKEN_TYPES) / sizeof(TokenType);

  const Token *next = &tokens->elements[(*idx)++];

  if (next->t_type == T_PLUS || next->t_type == T_MINUS) {
    term->sign = next;
    next = &tokens->elements[(*idx)++];
  } else {
    term->sign = NULL;
  }

  for (size_t i = 0; i < TERMINAL_TOKEN_LEN; i++) {
    if (next->t_type == TERMINAL_TOKEN_TYPES[i]) {
      term->tok = next;
      return 0;
    }
  }

  fprintf(stderr, "Invalid token type for expression: %s\n", t_type_to_string(next->t_type));
  return 1;
}
