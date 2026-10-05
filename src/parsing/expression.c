#include "parsing/expression.h"
#include "dynamic_array.h"
#include "lexer.h"
#include "parsing/type.h"
#include "parsing/utils.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>

#define PARAMETER_LIST_LEN_ESTIMATE 5

unsigned char parse_terminal_expr(TerminalExpr *term, const Tokens *tokens, size_t *idx);
unsigned char parse_func_call_parameters(CallParameters *params, const Tokens *tokens, size_t *idx);

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
    break;
  case E_TERMINAL:
    if (expression->e_union.term.item.t_type == TERM_FUNC_CALL) {
      for (size_t i = 0; i < expression->e_union.term.item.t_union.func_call.parameters.count; i++) {
        expression_free(&expression->e_union.term.item.t_union.func_call.parameters.elements[i]);
      }
      dyn_array_free(&expression->e_union.term.item.t_union.func_call.parameters);
    }
    break;
  default:
    break;
  }
}

unsigned char parse_terminal_expr(TerminalExpr *term, const Tokens *tokens, size_t *idx) {
  static const TokenType LITERAL_TOKEN_TYPES[] = {T_NUMERIC_LIT, T_TRUE, T_FALSE};
  static const size_t LITERAL_TOKEN_LEN = sizeof(LITERAL_TOKEN_TYPES) / sizeof(TokenType);

  const Token *next = &tokens->elements[(*idx)++];

  if (next->t_type == T_PLUS || next->t_type == T_MINUS) {
    term->sign = next;
    next = &tokens->elements[(*idx)++];
  } else {
    term->sign = NULL;
  }

  if (next->t_type == T_IDENTIFIER) {
    const TokenType after_type = peek_index(tokens, *idx);
    if (after_type != T_LEFT_PAREN) {
      term->item.t_union.tok = next;
      term->item.t_type = TERM_TOK;
      return 0;
    }
    *idx += 1;

    FunctionCall func_call;
    func_call.name = next;

    dyn_array_init(&func_call.parameters, sizeof(Expression), PARAMETER_LIST_LEN_ESTIMATE);
    if (parse_func_call_parameters(&func_call.parameters, tokens, idx) != 0) {
      fprintf(stderr, "Failed to process parameter list for function\n");
      return 1;
    }

    if (expect_next(T_RIGHT_PAREN, tokens, idx) == NULL) {
      fprintf(stderr, "Expected ')' for end of parameter list\n");
      return 1;
    }

    term->item.t_union.func_call = func_call;
    term->item.t_type = TERM_FUNC_CALL;
    return 0;
  }

  for (size_t i = 0; i < LITERAL_TOKEN_LEN; i++) {
    if (next->t_type == LITERAL_TOKEN_TYPES[i]) {
      term->item.t_union.tok = next;
      term->item.t_type = TERM_TOK;
      return 0;
    }
  }

  fprintf(stderr, "Invalid token type for expression: %s\n", t_type_to_string(next->t_type));
  return 1;
}

unsigned char parse_func_call_parameters(CallParameters *params, const Tokens *tokens, size_t *idx) {
  if (peek_index(tokens, *idx) == T_RIGHT_PAREN) {
    return 0;
  }

  while (true) {
    Expression expression;
    if (parse_expression(&expression, tokens, idx) != 0) {
      fprintf(stderr, "Failed to parse expression for function parameter\n");
      return 1;
    }

    dyn_array_insert(params, expression);

    if (peek_index(tokens, *idx) != T_COMMA) {
      return 0;
    }
    *idx += 1;
  }

  return 0;
}
