#include "sema/data_type.h"
#include "sema/utils.h"
#include <assert.h>

int64_t get_array_length(SemaState *state, const Expression *expr);

unsigned char analyse_data_type(SemaState *state, DataType *d_type, const Expression *expr) {
  if (d_type->d_kind == D_ARRAY) {
    d_type->d_union.a_type.length = get_array_length(state, expr);

    if (!state->requires_array_type) {
      state->requires_array_type = true;
    }
  }
  return 0;
}

int64_t get_array_length(SemaState *state, const Expression *expr) {
  // TODO: For now we don't get the array length for compound expressions, should improve this in the future
  if (expr == NULL || expr->e_type == E_COMPOUND) {
    return -1;
  }

  const Terminal *term = &expr->e_union.term.item;
  // Set the array length based on the expression if we know the length at compile time, otherwise set to -1
  switch (term->t_type) {
  case TERM_ARRAY_LITERAL:
    const ArrayLiteral *array_lit = &term->t_union.array_literal;
    return array_lit->items.count;
  case TERM_IDENTIFIER:
    const Identifier *ident = get_identifier(state, term->t_union.ident.name);
    assert(ident->d_type->d_kind == D_ARRAY);
    return ident->d_type->d_union.a_type.length;
    break;
  case TERM_FUNC_CALL:
    const Function *func = get_function(&state->functions, term->t_union.func_call.name->item);
    assert(func->return_type.d_kind == D_ARRAY);
    return func->return_type.d_union.a_type.length;
  default:
    return -1;
  }
}
