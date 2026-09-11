#include "sema.h"
#include "dynamic_array.h"
#include "lexer.h"
#include "parsing/type.h"

#include <limits.h>
#include <stdio.h>
#include <string.h>

#define INVALID_PROGRAM_CODE 2
#define NUM_VARIABLES_ESTIMATE 10
#define INT_BASE 10

unsigned char analyse_func(Identifiers *identifiers, const Function *func);

unsigned char analyse_statement(Identifiers *identifiers, Statement *statement, DataType func_type);
unsigned char analyse_dec_statement(Identifiers *identifiers, DeclarationStatement *dec);
unsigned char analyse_assign_statement(Identifiers *identifiers, AssignmentStatement *assign);
unsigned char analyse_ret_statement(Identifiers *identifiers, ReturnStatement *ret, DataType func_type);

unsigned char analyse_expression(Identifiers *identifiers, Expression *expr, DataType expr_type);
unsigned char analyse_term_expression(Identifiers *identifiers, TerminalExpr *term, DataType expr_type);
unsigned char analyse_comp_expression(Identifiers *identifiers, CompoundExpr *comp, DataType expr_type);
unsigned char analyse_operator_type(OperatorType op, DataType expr_type);

const Identifier *get_identifier(const Identifiers *identifiers, const char *name);

unsigned char analyse_program(Identifiers *identifiers, const Program *program) {
  dyn_array_init(identifiers, sizeof(Identifier), NUM_VARIABLES_ESTIMATE);
  assert(identifiers->elements != NULL);

  unsigned char result = 0;
  for (size_t i = 0; i < program->functions.count; i++) {
    result = analyse_func(identifiers, &program->functions.elements[i]);
    if (result != 0) {
      return result;
    }
  }

  return 0;
}

unsigned char analyse_func(Identifiers *identifiers, const Function *func) {
  unsigned char result = 0;
  for (size_t i = 0; i < func->statements.count; i++) {
    result = analyse_statement(identifiers, &func->statements.elements[i], func->return_type);
    if (result != 0) {
      return result;
    }
  }
  return 0;
}

unsigned char analyse_statement(Identifiers *identifiers, Statement *statement, DataType func_type) {
  switch (statement->s_type) {
  case S_DECLARATION:
    return analyse_dec_statement(identifiers, &statement->s_union.dec);
  case S_ASSIGNMENT:
    return analyse_assign_statement(identifiers, &statement->s_union.assign);
  case S_RETURN:
    return analyse_ret_statement(identifiers, &statement->s_union.ret, func_type);
  default:
    fprintf(stderr, "Unexpected statement type, should not be possible\n");
    assert(false);
  }
  return 0;
}

unsigned char analyse_dec_statement(Identifiers *identifiers, DeclarationStatement *dec) {
  Identifier new_ident = {
      .d_type = dec->d_type,
      .variable = dec->variable,
      .name = dec->lhs,
  };

  dyn_array_insert(identifiers, new_ident);

  return analyse_expression(identifiers, &dec->expr, dec->d_type);
}

unsigned char analyse_assign_statement(Identifiers *identifiers, AssignmentStatement *assign) {
  const Identifier *ident = get_identifier(identifiers, assign->lhs);
  if (ident == NULL) {
    fprintf(stderr, "Undefined variable: %s\n", assign->lhs);
    return INVALID_PROGRAM_CODE;
  }

  if (!ident->variable) {
    fprintf(stderr, "Attempted to modify constant: %s\n", assign->lhs);
    return INVALID_PROGRAM_CODE;
  }

  assign->d_type = ident->d_type;
  return analyse_expression(identifiers, &assign->expr, assign->d_type);
}
unsigned char analyse_ret_statement(Identifiers *identifiers, ReturnStatement *ret, DataType func_type) {
  ret->d_type = func_type;
  return analyse_expression(identifiers, &ret->expr, ret->d_type);
}

unsigned char analyse_expression(Identifiers *identifiers, Expression *expr, DataType expr_type) {
  assert(expr->e_type != E_NONE);
  switch (expr->e_type) {
  case E_TERMINAL:
    return analyse_term_expression(identifiers, &expr->e_union.term, expr_type);
  case E_COMPOUND:
    return analyse_comp_expression(identifiers, &expr->e_union.comp, expr_type);
  default:
    assert(false);
  }
}

unsigned char analyse_term_expression(Identifiers *identifiers, TerminalExpr *term, DataType expr_type) {
  term->literal.l_type = L_NONE;

  if (term->sign != NULL && expr_type != D_I32) {
    fprintf(stderr, "Invalid use of sign: %s, must only be used with integer values\n",
            t_type_to_string(term->sign->t_type));
    return INVALID_PROGRAM_CODE;
  }

  switch (term->tok->t_type) {
  case T_IDENTIFIER:
    const Identifier *ident = get_identifier(identifiers, term->tok->item);
    if (ident == NULL) {
      fprintf(stderr, "Undefined variable: %s\n", term->tok->item);
      return INVALID_PROGRAM_CODE;
    }

    if (ident->d_type != expr_type) {
      fprintf(stderr, "Variable %s does not have expected type %s\n", ident->name, d_type_to_string(expr_type));
      return INVALID_PROGRAM_CODE;
    }
    break;
  case T_NUMERIC_LIT:
    if (expr_type != D_I32) {
      fprintf(stderr, "Numeric literal %s used in non-numerical expression type: %s\n", term->tok->item,
              d_type_to_string(expr_type));
      return INVALID_PROGRAM_CODE;
    }

    const long long int_val = strtoll(term->tok->item, NULL, INT_BASE);
    if (int_val == LLONG_MIN || int_val == LLONG_MAX || (int_val == 0 && strcmp(term->tok->item, "0") != 0)) {
      fprintf(stderr, "Failed to convert value into numeric literal: %s\n", term->tok->item);
      return INVALID_PROGRAM_CODE;
    }

    term->literal.l_type = L_NUM;
    term->literal.l_union.num = int_val;
    break;
  case T_TRUE:
  case T_FALSE:
    term->literal.l_type = L_BOOL;
    term->literal.l_union.b = term->tok->t_type == T_TRUE;
    break;
  default:
    fprintf(stderr, "Unexpected token type for terminal expression: %s\n", t_type_to_string(term->tok->t_type));
    return INVALID_PROGRAM_CODE;
  }

  return 0;
}

// TODO: With all these recursive functions, need to define some recursion limits in order to stop stack overflows
unsigned char analyse_comp_expression(Identifiers *identifiers, CompoundExpr *comp, DataType expr_type) {
  // TODO: Will need to refactor how this works when we support multiple integer types, how will we determine the type
  DataType term_type = expr_type;
  if (expr_type == D_BOOL) {
    term_type = D_I32;
  }

  unsigned char result = analyse_term_expression(identifiers, &comp->lhs, term_type);
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
    return analyse_comp_expression(identifiers, &comp->rhs->e_union.comp, expr_type);
  case E_TERMINAL:
    return analyse_term_expression(identifiers, &comp->rhs->e_union.term, term_type);
  default:
    assert(false);
  }
}

unsigned char analyse_operator_type(OperatorType op, DataType expr_type) {
  static const OperatorType BOOL_OPS[] = {O_GT, O_LT, O_GTE, O_LTE};
  static const size_t BOOL_OPS_LEN = sizeof(BOOL_OPS) / sizeof(OperatorType);

  static const OperatorType INT_OPS[] = {O_MINUS, O_PLUS};
  static const size_t INT_OPS_LEN = sizeof(INT_OPS) / sizeof(OperatorType);

  switch (expr_type) {
  case D_BOOL:
    for (size_t i = 0; i < BOOL_OPS_LEN; i++) {
      if (BOOL_OPS[i] == op) {
        return 0;
      }
    }
    break;
  case D_I32:
    for (size_t i = 0; i < INT_OPS_LEN; i++) {
      if (INT_OPS[i] == op) {
        return 0;
      }
    }
    break;
  default:
    assert(false);
  }

  fprintf(stderr, "Invalid operator type %s for expression type %s\n", o_type_to_string(op),
          d_type_to_string(expr_type));
  return INVALID_PROGRAM_CODE;
}

const Identifier *get_identifier(const Identifiers *identifiers, const char *name) {
  for (size_t i = 0; i < identifiers->count; i++) {
    if (strcmp(identifiers->elements[i].name, name) == 0) {
      return &identifiers->elements[i];
    }
  }
  return NULL;
}
