#include "sema/expression.h"
#include "sema/terminal.h"
#include "sema/utils.h"
#include <assert.h>
#include <stdio.h>

unsigned char analyse_term_expression(SemaState *state, TerminalExpr *term, const DataType *expr_type);
unsigned char analyse_comp_expression(SemaState *state, CompoundExpr *comp, const DataType *expr_type);
unsigned char analyse_operator_type(OperatorType op, const DataType *expr_type);

static const DataType I32_TYPE = {.d_kind = D_PRIMITIVE, .d_union = {.p_type = P_I32}};

unsigned char analyse_expression(SemaState *state, Expression *expr, const DataType *expr_type) {
  assert(expr->e_type != E_NONE);
  switch (expr->e_type) {
  case E_TERMINAL:
    return analyse_term_expression(state, &expr->e_union.term, expr_type);
  case E_COMPOUND:
    return analyse_comp_expression(state, &expr->e_union.comp, expr_type);
  default:
    assert(false);
  }
}

unsigned char analyse_term_expression(SemaState *state, TerminalExpr *term, const DataType *expr_type) {
  if (term->sign != NULL && !is_integer_type(expr_type)) {
    fprintf(stderr, "Invalid use of sign: %s, must only be used with integer values\n",
            t_type_to_string(term->sign->t_type));
    return 1;
  }

  switch (term->item.t_type) {
  case TERM_IDENTIFIER:
    return analyse_identifier(state, &term->item.t_union.ident, expr_type);
  case TERM_LITERAL:
    return analyse_literal(&term->item.t_union.lit, term->sign, expr_type);
  case TERM_FUNC_CALL:
    return analyse_func_call(state, &term->item.t_union.func_call, expr_type);
  case TERM_ARRAY_LITERAL:
    return analyse_array_literal(state, &term->item.t_union.arr_literal, expr_type);
  case TERM_ARRAY_ACCESS:
    return analyse_array_access(state, &term->item.t_union.arr_access, expr_type);
  case TERM_STRUCT_LITERAL:
    return analyse_struct_literal(state, &term->item.t_union.struct_literal, expr_type);
    return 0;
  default:
    assert(false);
  }
}

unsigned char analyse_comp_expression(SemaState *state, CompoundExpr *comp, const DataType *expr_type) {
  // TODO: Check whether this needs updating now that we have multiple integer types
  const DataType *term_type = expr_type;
  if (expr_type->d_kind == D_PRIMITIVE && expr_type->d_union.p_type == P_BOOL) {
    term_type = &I32_TYPE;
  }

  if (expr_type->d_kind == D_ARRAY) {
    fprintf(stderr, "Using arrays as part of compound expressions is not supported\n");
    return 1;
  }

  unsigned char result = analyse_term_expression(state, &comp->lhs, term_type);
  if (result != 0) {
    return result;
  }

  result = analyse_operator_type(comp->op, expr_type);
  if (result != 0) {
    return result;
  }

  assert(comp->rhs->e_type != E_NONE);
  switch (comp->rhs->e_type) {
  case E_COMPOUND:
    return analyse_comp_expression(state, &comp->rhs->e_union.comp, expr_type);
  case E_TERMINAL:
    return analyse_term_expression(state, &comp->rhs->e_union.term, term_type);
  default:
    assert(false);
  }
}

unsigned char analyse_operator_type(OperatorType op, const DataType *expr_type) {
  static const OperatorType BOOL_OPS[] = {O_GT, O_LT, O_GTE, O_LTE};
  static const size_t BOOL_OPS_LEN = sizeof(BOOL_OPS) / sizeof(OperatorType);

  static const OperatorType INT_OPS[] = {O_MINUS, O_PLUS};
  static const size_t INT_OPS_LEN = sizeof(INT_OPS) / sizeof(OperatorType);

  assert(expr_type->d_kind == D_PRIMITIVE);

  if (expr_type->d_union.p_type == P_BOOL) {
    for (size_t i = 0; i < BOOL_OPS_LEN; i++) {
      if (BOOL_OPS[i] == op) {
        return 0;
      }
    }
  } else if (is_integer_type(expr_type)) {
    for (size_t i = 0; i < INT_OPS_LEN; i++) {
      if (INT_OPS[i] == op) {
        return 0;
      }
    }
  }

  fprintf(stderr, "Invalid operator type %s for expression type %s\n", o_type_to_string(op),
          d_type_to_string(expr_type));
  return 1;
}
