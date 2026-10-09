#include "sema.h"
#include "dynamic_array.h"
#include "lexer.h"
#include "parsing/type.h"

#include <limits.h>
#include <stdio.h>
#include <string.h>

#define INVALID_PROGRAM_CODE 2
#define NUM_VARIABLES_ESTIMATE 10
#define NUM_SCOPES_ESTIMATE 5
#define INT_BASE 10

typedef struct {
  Identifiers identifiers;
  const Functions functions;
  Scopes scopes;
} SemaState;

unsigned char analyse_func(SemaState *state, Function *func);
unsigned char analyse_parameters(SemaState *state, Parameters *parameters);

unsigned char analyse_statements(SemaState *state, const Statements *statements, DataType func_type);
unsigned char analyse_statement(SemaState *state, Statement *statement, DataType func_type);
unsigned char analyse_dec_statement(SemaState *state, DeclarationStatement *dec);
unsigned char analyse_assign_statement(SemaState *state, AssignmentStatement *assign);
unsigned char analyse_ret_statement(SemaState *state, ReturnStatement *ret, DataType func_type);
unsigned char analyse_if_block(SemaState *state, IfBlock *if_block, DataType func_type);
unsigned char analyse_if_branch(SemaState *state, IfBranch *if_branch, DataType func_type);
unsigned char analyse_void_statement(SemaState *state, VoidStatement *void_s);

unsigned char analyse_expression(SemaState *state, Expression *expr, DataType expr_type);
unsigned char analyse_term_expression(SemaState *state, TerminalExpr *term, DataType expr_type);
unsigned char analyse_comp_expression(SemaState *state, CompoundExpr *comp, DataType expr_type);
unsigned char analyse_operator_type(OperatorType op, DataType expr_type);

unsigned char analyse_terminal_token(SemaState *state, Literal *literal, const Token *tok, DataType expr_type);
unsigned char analyse_terminal_func_call(SemaState *state, const FunctionCall *func_call, DataType expr_type);
unsigned char analyse_terminal_identifier(SemaState *state, TerminalIdentifier *term_ident, DataType expr_type);
unsigned char analyse_terminal_literal(TerminalLiteral *term_lit, DataType expr_type);

unsigned char insert_identifier(SemaState *state, const char *name, DataType d_type, IdentifierType i_type);
const Identifier *get_identifier(const SemaState *state, const char *name);
const Function *get_function(const Functions *functions, const char *name);
bool is_identifier_in_scope(const Scopes *scopes, const Identifier *identifier);

void free_sema_state(SemaState *state);

unsigned char analyse_program(const Program *program) {
  Identifiers identifiers;
  dyn_array_init(&identifiers, sizeof(Identifier), NUM_VARIABLES_ESTIMATE);
  assert(identifiers.elements != NULL);
  Scopes scopes;
  dyn_array_init(&scopes, sizeof(void *), NUM_SCOPES_ESTIMATE);
  assert(scopes.elements != NULL);

  SemaState state = {
      .identifiers = identifiers,
      .functions = program->functions,
      .scopes = scopes,
  };

  unsigned char result = 0;
  for (size_t i = 0; i < program->functions.count; i++) {
    dyn_array_insert(&state.scopes, &program->functions.elements[i]);
    result = analyse_func(&state, &program->functions.elements[i]);
    if (result != 0) {
      break;
    }
    dyn_array_pop(&state.scopes);
  }

  free_sema_state(&state);
  return result;
}

unsigned char analyse_func(SemaState *state, Function *func) {
  if (analyse_parameters(state, &func->parameters) != 0) {
    return 1;
  }

  if (analyse_statements(state, &func->statements, func->return_type) != 0) {
    return 1;
  }

  return 0;
}

unsigned char analyse_parameters(SemaState *state, Parameters *parameters) {
  for (size_t i = 0; i < parameters->count; i++) {
    Parameter *param = &parameters->elements[i];
    if (insert_identifier(state, param->term_ident.name, param->d_type, I_CONST) != 0) {
      return 1;
    }

    param->term_ident.scope = state->identifiers.elements[state->identifiers.count - 1].scope;
  }
  return 0;
}

unsigned char analyse_statements(SemaState *state, const Statements *statements, DataType func_type) {
  unsigned char result = 0;
  for (size_t i = 0; i < statements->count; i++) {
    result = analyse_statement(state, &statements->elements[i], func_type);
    if (result != 0) {
      return result;
    }
  }
  return 0;
}

unsigned char analyse_statement(SemaState *state, Statement *statement, DataType func_type) {
  switch (statement->s_type) {
  case S_DECLARATION:
    return analyse_dec_statement(state, &statement->s_union.dec);
  case S_ASSIGNMENT:
    return analyse_assign_statement(state, &statement->s_union.assign);
  case S_RETURN:
    return analyse_ret_statement(state, &statement->s_union.ret, func_type);
  case S_IF:
    return analyse_if_block(state, &statement->s_union.if_block, func_type);
  case S_VOID:
    return analyse_void_statement(state, &statement->s_union.void_s);
  default:
    fprintf(stderr, "Unexpected statement type, should not be possible\n");
    assert(false);
  }
  return 0;
}

unsigned char analyse_dec_statement(SemaState *state, DeclarationStatement *dec) {
  if (dec->d_type == D_VOID) {
    fprintf(stderr, "Cannot use void as datatype for variable\n");
    return INVALID_PROGRAM_CODE;
  }

  unsigned char result = analyse_expression(state, &dec->expr, dec->d_type);
  if (result != 0) {
    return result;
  }

  IdentifierType i_type = dec->variable ? I_VARIABLE : I_CONST;
  result = insert_identifier(state, dec->term_ident.name, dec->d_type, i_type);
  if (result != 0) {
    return result;
  }

  dec->term_ident.scope = state->scopes.elements[state->scopes.count - 1];

  return 0;
}

unsigned char analyse_assign_statement(SemaState *state, AssignmentStatement *assign) {
  const Identifier *ident = get_identifier(state, assign->term_ident.name);
  if (ident == NULL) {
    fprintf(stderr, "Undefined variable: %s\n", assign->term_ident.name);
    return INVALID_PROGRAM_CODE;
  }

  if (ident->i_type != I_VARIABLE) {
    fprintf(stderr, "Attempted to modify constant: %s\n", assign->term_ident.name);
    return INVALID_PROGRAM_CODE;
  }

  assign->d_type = ident->d_type;
  assign->term_ident.scope = ident->scope;
  return analyse_expression(state, &assign->expr, assign->d_type);
}
unsigned char analyse_ret_statement(SemaState *state, ReturnStatement *ret, DataType func_type) {
  ret->d_type = func_type;
  return analyse_expression(state, &ret->expr, ret->d_type);
}

unsigned char analyse_if_block(SemaState *state, IfBlock *if_block, DataType func_type) {
  dyn_array_insert(&state->scopes, &if_block->if_branch);
  unsigned char result = analyse_if_branch(state, &if_block->if_branch, func_type);
  if (result != 0) {
    return result;
  }
  dyn_array_pop(&state->scopes);

  for (size_t i = 0; i < if_block->else_if_branches.count; i++) {
    dyn_array_insert(&state->scopes, &if_block->else_if_branches.elements[i]);
    result = analyse_if_branch(state, &if_block->else_if_branches.elements[i], func_type);
    if (result != 0) {
      return result;
    }
    dyn_array_pop(&state->scopes);
  }

  if (if_block->else_body.elements != NULL) {
    dyn_array_insert(&state->scopes, &if_block->else_body);
    result = analyse_statements(state, &if_block->else_body, func_type);
    if (result != 0) {
      return result;
    }
    dyn_array_pop(&state->scopes);
  }

  return 0;
}

unsigned char analyse_if_branch(SemaState *state, IfBranch *if_branch, DataType func_type) {
  unsigned char result = analyse_expression(state, &if_branch->expr, D_BOOL);
  if (result != 0) {
    return result;
  }

  result = analyse_statements(state, &if_branch->body, func_type);
  if (result != 0) {
    return result;
  }

  return 0;
}

unsigned char analyse_void_statement(SemaState *state, VoidStatement *void_s) {
  return analyse_expression(state, &void_s->expr, D_VOID);
}

unsigned char analyse_expression(SemaState *state, Expression *expr, DataType expr_type) {
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

unsigned char analyse_term_expression(SemaState *state, TerminalExpr *term, DataType expr_type) {
  if (term->sign != NULL && expr_type != D_I32) {
    fprintf(stderr, "Invalid use of sign: %s, must only be used with integer values\n",
            t_type_to_string(term->sign->t_type));
    return INVALID_PROGRAM_CODE;
  }

  switch (term->item.t_type) {
  case TERM_IDENTIFIER:
    return analyse_terminal_identifier(state, &term->item.t_union.ident, expr_type);
  case TERM_LITERAL:
    return analyse_terminal_literal(&term->item.t_union.lit, expr_type);
  case TERM_FUNC_CALL:
    return analyse_terminal_func_call(state, &term->item.t_union.func_call, expr_type);
  default:
    assert(false);
  }
}

unsigned char analyse_terminal_func_call(SemaState *state, const FunctionCall *func_call, DataType expr_type) {
  const Function *func = get_function(&state->functions, func_call->name->item);
  if (func == NULL) {
    fprintf(stderr, "Undefined function: %s\n", func_call->name->item);
    return INVALID_PROGRAM_CODE;
  }

  if (func_call->parameters.count != func->parameters.count) {
    fprintf(stderr, "Mismatched number of parameters in function call, expected %zu, got %zu\n", func->parameters.count,
            func_call->parameters.count);
    return INVALID_PROGRAM_CODE;
  }

  for (size_t i = 0; i < func_call->parameters.count; i++) {
    if (analyse_expression(state, &func_call->parameters.elements[i], func->parameters.elements[i].d_type) != 0) {
      fprintf(stderr, "Failed to analyse parameter expression\n");
      return INVALID_PROGRAM_CODE;
    }
  }

  if (func->return_type != expr_type) {
    fprintf(stderr, "Function %s does not return expected type %s\n", func->name, d_type_to_string(expr_type));
    return INVALID_PROGRAM_CODE;
  }
  return 0;
}

unsigned char analyse_terminal_identifier(SemaState *state, TerminalIdentifier *term_ident, DataType expr_type) {
  const Identifier *ident = get_identifier(state, term_ident->name);
  if (ident == NULL) {
    fprintf(stderr, "Undefined variable: %s\n", term_ident->name);
    return INVALID_PROGRAM_CODE;
  }

  if (ident->d_type != expr_type) {
    fprintf(stderr, "Variable %s does not have expected type %s\n", ident->name, d_type_to_string(expr_type));
    return INVALID_PROGRAM_CODE;
  }

  term_ident->scope = ident->scope;

  return 0;
}

unsigned char analyse_terminal_literal(TerminalLiteral *term_lit, DataType expr_type) {
  switch (term_lit->tok->t_type) {
  case T_NUMERIC_LIT:
    if (expr_type != D_I32) {
      fprintf(stderr, "Numeric literal %s used in non-numerical expression type: %s\n", term_lit->tok->item,
              d_type_to_string(expr_type));
      return INVALID_PROGRAM_CODE;
    }

    const long long int_val = strtoll(term_lit->tok->item, NULL, INT_BASE);
    if (int_val == LLONG_MIN || int_val == LLONG_MAX || (int_val == 0 && strcmp(term_lit->tok->item, "0") != 0)) {
      fprintf(stderr, "Failed to convert value into numeric literal: %s\n", term_lit->tok->item);
      return INVALID_PROGRAM_CODE;
    }

    term_lit->literal.l_type = L_NUM;
    term_lit->literal.l_union.num = int_val;
    return 0;
  case T_TRUE:
  case T_FALSE:
    if (expr_type != D_BOOL) {
      fprintf(stderr, "Boolean literal %s used in non-boolean expression type: %s\n", term_lit->tok->item,
              d_type_to_string(expr_type));
      return INVALID_PROGRAM_CODE;
    }

    term_lit->literal.l_type = L_BOOL;
    term_lit->literal.l_union.b = term_lit->tok->t_type == T_TRUE;
    return 0;
  default:
    fprintf(stderr, "Unexpected token type for terminal expression: %s\n", t_type_to_string(term_lit->tok->t_type));
    return INVALID_PROGRAM_CODE;
  }

  return 0;
}

unsigned char analyse_comp_expression(SemaState *state, CompoundExpr *comp, DataType expr_type) {
  // TODO: Will need to refactor how this works when we support multiple integer types, how will we determine the type
  DataType term_type = expr_type;
  if (expr_type == D_BOOL) {
    term_type = D_I32;
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

unsigned char insert_identifier(SemaState *state, const char *name, DataType d_type, IdentifierType i_type) {
  const Identifier *existing = get_identifier(state, name);
  if (existing != NULL) {
    fprintf(stderr, "Variable %s shadows existing variable in scope\n", existing->name);
    return INVALID_PROGRAM_CODE;
  }

  Identifier new_ident = {
      .d_type = d_type,
      .i_type = i_type,
      .name = name,
      .scope = state->scopes.elements[state->scopes.count - 1],
  };

  dyn_array_insert(&state->identifiers, new_ident);
  return 0;
}

const Identifier *get_identifier(const SemaState *state, const char *name) {
  for (size_t i = 0; i < state->identifiers.count; i++) {
    if (strcmp(state->identifiers.elements[i].name, name) != 0) {
      continue;
    }

    if (!is_identifier_in_scope(&state->scopes, &state->identifiers.elements[i])) {
      continue;
    }

    return &state->identifiers.elements[i];
  }
  return NULL;
}

const Function *get_function(const Functions *functions, const char *name) {
  for (size_t i = 0; i < functions->count; i++) {
    if (strcmp(functions->elements[i].name, name) == 0) {
      return &functions->elements[i];
    }
  }
  return NULL;
}

bool is_identifier_in_scope(const Scopes *scopes, const Identifier *identifier) {
  for (size_t i = 0; i < scopes->count; i++) {
    if (scopes->elements[i] == identifier->scope) {
      return true;
    }
  }
  return false;
}

void free_sema_state(SemaState *state) {
  dyn_array_free(&state->identifiers);
  dyn_array_free(&state->scopes);
}
