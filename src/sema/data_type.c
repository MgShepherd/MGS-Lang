#include "sema/data_type.h"
#include <stdio.h>

unsigned char analyse_data_type(DataType *d_type, const Expression *expr) {
  if (d_type->d_kind == D_ARRAY) {
    if (expr->e_type != E_TERMINAL) {
      fprintf(stderr, "Can perform compound operations on arrays\n");
      return 1;
    }

    Terminal term = expr->e_union.term.item;
    if (term.t_type != TERM_ARRAY_LITERAL) {
      fprintf(stderr, "Expected array literal, got type %s\n", term_type_to_string(term.t_type));
      return 1;
    }

    d_type->d_union.a_type.length = (uint64_t)term.t_union.arr_literal.items.count;
  }
  return 0;
}
