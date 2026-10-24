#include "sema.h"
#include "dynamic_array.h"
#include "lexer.h"
#include "parsing/data_type.h"
#include "parsing/expression.h"
#include "parsing/type.h"

#include <assert.h>
#include <limits.h>
#include <stdint.h>
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
unsigned char analyse_parameters(SemaState *state, Fields *parameters);

unsigned char analyse_user_type(SemaState *state, UserType *user_type);

unsigned char analyse_statements(SemaState *state, const Statements *statements, const DataType *func_type);
unsigned char analyse_statement(SemaState *state, Statement *statement, const DataType *func_type);
unsigned char analyse_dec_statement(SemaState *state, DeclarationStatement *dec);
unsigned char analyse_assign_statement(SemaState *state, AssignmentStatement *assign);
unsigned char analyse_ret_statement(SemaState *state, ReturnStatement *ret, const DataType *func_type);
unsigned char analyse_if_block(SemaState *state, IfBlock *if_block, const DataType *func_type);
unsigned char analyse_if_branch(SemaState *state, IfBranch *if_branch, const DataType *func_type);
unsigned char analyse_void_statement(SemaState *state, VoidStatement *void_s);

unsigned char analyse_expression(SemaState *state, Expression *expr, const DataType *expr_type);
unsigned char analyse_term_expression(SemaState *state, TerminalExpr *term, const DataType *expr_type);
unsigned char analyse_comp_expression(SemaState *state, CompoundExpr *comp, const DataType *expr_type);
unsigned char analyse_operator_type(OperatorType op, const DataType *expr_type);
unsigned char analyse_data_type(DataType *d_type, const Expression *expr);

unsigned char analyse_terminal_token(SemaState *state, Literal *literal, const Token *tok, const DataType *expr_type);
unsigned char analyse_terminal_func_call(SemaState *state, const FunctionCall *func_call, const DataType *expr_type);
unsigned char analyse_terminal_identifier(SemaState *state, TerminalIdentifier *term_ident, const DataType *expr_type);
unsigned char analyse_terminal_literal(TerminalLiteral *term_lit, const Token *sign, const DataType *expr_type);
unsigned char analyse_terminal_array_literal(SemaState *state, ArrayLiteral *arr_lit, const DataType *expr_type);
unsigned char analyse_terminal_array_access(SemaState *state, ArrayAccess *arr_access, const DataType *expr_type);

unsigned char analyse_numeric_literal(TerminalLiteral *term_lit, bool is_negative, const DataType *expr_type);

unsigned char analyse_block(SemaState *state, Block *block, const DataType *func_type);

unsigned char insert_identifier(SemaState *state, const char *name, const DataType *d_type);
const Identifier *get_identifier(const SemaState *state, const char *name);
const Function *get_function(const Functions *functions, const char *name);
bool is_identifier_in_scope(const Scopes *scopes, const Identifier *identifier);
bool is_integer_type(const DataType *d_type);
bool is_number_in_range(long long input, long long min, long long max);
unsigned char tok_to_integer(long long *output_num, const Token *tok, bool is_negative);
bool is_void_type(const DataType *d_type);

void free_sema_state(SemaState *state);

static const DataType BOOL_TYPE = {.d_kind = D_PRIMITIVE, .d_union = {.p_type = P_BOOL}};
static const DataType VOID_TYPE = {.d_kind = D_PRIMITIVE, .d_union = {.p_type = P_VOID}};
static const DataType I32_TYPE = {.d_kind = D_PRIMITIVE, .d_union = {.p_type = P_I32}};

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
  bool has_valid_main = false;
  for (size_t i = 0; i < program->functions.count; i++) {
    dyn_array_insert(&state.scopes, &program->functions.elements[i]);
    result = analyse_func(&state, &program->functions.elements[i]);
    if (result != 0) {
      goto sema_cleanup;
    }
    dyn_array_pop(&state.scopes);

    if (strcmp(program->functions.elements[i].name, "main") == 0) {
      if (!is_integer_type(&program->functions.elements[i].return_type)) {
        fprintf(stderr, "Return type from main function must be an integer type (i8, i16, i32, i64)\n");
        result = 1;
        goto sema_cleanup;
      }
      has_valid_main = true;
    }
  }

  if (!has_valid_main) {
    fprintf(stderr, "All programs must have a main function defined\n");
    result = 1;
  }

sema_cleanup:
  free_sema_state(&state);
  return result;
}

unsigned char analyse_func(SemaState *state, Function *func) {
  if (analyse_parameters(state, &func->parameters) != 0) {
    return 1;
  }

  return analyse_block(state, &func->block, &func->return_type);
}

unsigned char analyse_parameters(SemaState *state, Fields *parameters) {
  for (size_t i = 0; i < parameters->count; i++) {
    Field *param = &parameters->elements[i];
    if (insert_identifier(state, param->term_ident.name, &param->d_type) != 0) {
      return 1;
    }

    param->term_ident.scope = state->identifiers.elements[state->identifiers.count - 1].scope;
  }
  return 0;
}

unsigned char analyse_statements(SemaState *state, const Statements *statements, const DataType *func_type) {
  unsigned char result = 0;
  for (size_t i = 0; i < statements->count; i++) {
    result = analyse_statement(state, &statements->elements[i], func_type);
    if (result != 0) {
      return result;
    }
  }
  return 0;
}

unsigned char analyse_statement(SemaState *state, Statement *statement, const DataType *func_type) {
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
  assert(dec->d_type.d_kind != D_NONE);
  if (is_void_type(&dec->d_type)) {
    fprintf(stderr, "Cannot use void as datatype for variable\n");
    return INVALID_PROGRAM_CODE;
  }

  unsigned char result = analyse_expression(state, &dec->expr, &dec->d_type);
  if (result != 0) {
    return result;
  }

  result = analyse_data_type(&dec->d_type, &dec->expr);
  if (result != 0) {
    return result;
  }

  result = insert_identifier(state, dec->term_ident.name, &dec->d_type);
  if (result != 0) {
    return result;
  }

  dec->term_ident.scope = state->scopes.elements[state->scopes.count - 1];

  return 0;
}

unsigned char analyse_assign_statement(SemaState *state, AssignmentStatement *assign) {
  const Identifier *ident = NULL;
  switch (assign->a_output.a_type) {
  case A_IDENTIFIER:
    TerminalIdentifier *term_ident = &assign->a_output.a_union.term_ident;
    ident = get_identifier(state, term_ident->name);

    if (ident == NULL) {
      fprintf(stderr, "Undefined variable: %s\n", term_ident->name);
      return INVALID_PROGRAM_CODE;
    }

    assign->d_type = ident->d_type;
    term_ident->scope = ident->scope;
    break;
  case A_ARRAY_ELEMENT:
    ArrayAccess *arr_element = &assign->a_output.a_union.arr_element;
    if (analyse_terminal_array_access(state, arr_element, NULL) != 0) {
      fprintf(stderr, "Failed to analyse array access for assignment statement\n");
      return INVALID_PROGRAM_CODE;
    }

    // TODO: We are re-fetching the identifier here even though already fetched for analyse_terminal_array_access
    ident = get_identifier(state, arr_element->term_ident.name);
    assert(ident != NULL && ident->d_type->d_kind == D_ARRAY);

    assign->d_type = ident->d_type->d_union.a_type.element_type;
    break;
  default:
    assert(false);
  }

  if (!assign->d_type->variable) {
    fprintf(stderr, "Attempted to modify constant: %s\n", ident->name);
    return INVALID_PROGRAM_CODE;
  }

  return analyse_expression(state, &assign->expr, assign->d_type);
}
unsigned char analyse_ret_statement(SemaState *state, ReturnStatement *ret, const DataType *func_type) {
  ret->d_type = func_type;
  return analyse_expression(state, &ret->expr, ret->d_type);
}

unsigned char analyse_if_block(SemaState *state, IfBlock *if_block, const DataType *func_type) {
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

  if (if_block->else_branch.block.body.count > 0) {
    dyn_array_insert(&state->scopes, &if_block->else_branch.block.body);
    result = analyse_if_branch(state, &if_block->else_branch, func_type);
    if (result != 0) {
      return result;
    }
    dyn_array_pop(&state->scopes);
  }

  return 0;
}

unsigned char analyse_if_branch(SemaState *state, IfBranch *if_branch, const DataType *func_type) {
  unsigned char result;
  if (if_branch->expr.e_type != E_NONE) {
    result = analyse_expression(state, &if_branch->expr, &BOOL_TYPE);
    if (result != 0) {
      return result;
    }
  }

  return analyse_block(state, &if_branch->block, func_type);
}

unsigned char analyse_void_statement(SemaState *state, VoidStatement *void_s) {
  return analyse_expression(state, &void_s->expr, &VOID_TYPE);
}

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
    return INVALID_PROGRAM_CODE;
  }

  switch (term->item.t_type) {
  case TERM_IDENTIFIER:
    return analyse_terminal_identifier(state, &term->item.t_union.ident, expr_type);
  case TERM_LITERAL:
    return analyse_terminal_literal(&term->item.t_union.lit, term->sign, expr_type);
  case TERM_FUNC_CALL:
    return analyse_terminal_func_call(state, &term->item.t_union.func_call, expr_type);
  case TERM_ARRAY_LITERAL:
    return analyse_terminal_array_literal(state, &term->item.t_union.arr_literal, expr_type);
  case TERM_ARRAY_ACCESS:
    return analyse_terminal_array_access(state, &term->item.t_union.arr_access, expr_type);
  default:
    assert(false);
  }
}

unsigned char analyse_terminal_func_call(SemaState *state, const FunctionCall *func_call, const DataType *expr_type) {
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
    if (analyse_expression(state, &func_call->parameters.elements[i], &func->parameters.elements[i].d_type) != 0) {
      fprintf(stderr, "Failed to analyse parameter expression\n");
      return INVALID_PROGRAM_CODE;
    }
  }

  if (!do_data_types_match(&func->return_type, expr_type)) {
    fprintf(stderr, "Function %s does not return expected type %s\n", func->name, d_type_to_string(expr_type));
    return INVALID_PROGRAM_CODE;
  }
  return 0;
}

unsigned char analyse_terminal_array_access(SemaState *state, ArrayAccess *arr_access, const DataType *expr_type) {
  const Identifier *ident = get_identifier(state, arr_access->term_ident.name);
  if (ident == NULL) {
    fprintf(stderr, "Undefined variable: %s\n", arr_access->term_ident.name);
    return INVALID_PROGRAM_CODE;
  }

  if (ident->d_type->d_kind != D_ARRAY) {
    fprintf(stderr, "Expected variable: %s to be an array type for array access\n", arr_access->term_ident.name);
    return INVALID_PROGRAM_CODE;
  }

  const ArrayType *array_type = &ident->d_type->d_union.a_type;
  if (expr_type != NULL && !do_data_types_match(array_type->element_type, expr_type)) {
    fprintf(stderr, "Element type of array %s does not match expected expression type: %s\n",
            arr_access->term_ident.name, d_type_to_string(expr_type));
    return INVALID_PROGRAM_CODE;
  }
  long long arr_index;
  if (tok_to_integer(&arr_index, arr_access->index_tok, false) != 0) {
    fprintf(stderr, "Expected integer value for array index\n");
    return INVALID_PROGRAM_CODE;
  }

  arr_access->term_ident.scope = ident->scope;
  arr_access->idx = (uint64_t)arr_index;

  if (arr_access->idx >= array_type->length) {
    fprintf(stderr, "Array index %lld out of bounds for array %s\n", arr_access->idx, arr_access->term_ident.name);
    return INVALID_PROGRAM_CODE;
  }

  return 0;
}

unsigned char analyse_terminal_identifier(SemaState *state, TerminalIdentifier *term_ident, const DataType *expr_type) {
  const Identifier *ident = get_identifier(state, term_ident->name);
  if (ident == NULL) {
    fprintf(stderr, "Undefined variable: %s\n", term_ident->name);
    return INVALID_PROGRAM_CODE;
  }

  if (!do_data_types_match(expr_type, ident->d_type)) {
    fprintf(stderr, "Variable %s does not have expected type %s\n", ident->name, d_type_to_string(expr_type));
    return INVALID_PROGRAM_CODE;
  }

  term_ident->scope = ident->scope;

  return 0;
}

unsigned char analyse_terminal_literal(TerminalLiteral *term_lit, const Token *sign, const DataType *expr_type) {
  switch (term_lit->tok->t_type) {
  case T_NUMERIC_LIT:
    if (!is_integer_type(expr_type)) {
      fprintf(stderr, "Numeric literal %s used in non-numerical expression type: %s\n", term_lit->tok->item,
              d_type_to_string(expr_type));
      return INVALID_PROGRAM_CODE;
    }
    const bool is_negative = sign != NULL && sign->t_type == T_MINUS;
    return analyse_numeric_literal(term_lit, is_negative, expr_type);
  case T_TRUE:
  case T_FALSE:
    if (expr_type->d_kind != D_PRIMITIVE || expr_type->d_union.p_type != P_BOOL) {
      fprintf(stderr, "Boolean literal %s used in non-boolean expression type: %s\n", term_lit->tok->item,
              d_type_to_string(expr_type));
      return INVALID_PROGRAM_CODE;
    }

    term_lit->literal.d_type = expr_type;
    term_lit->literal.l_union.b = term_lit->tok->t_type == T_TRUE;
    return 0;
  default:
    fprintf(stderr, "Unexpected token type for terminal expression: %s\n", t_type_to_string(term_lit->tok->t_type));
    return INVALID_PROGRAM_CODE;
  }

  return 0;
}

unsigned char analyse_terminal_array_literal(SemaState *state, ArrayLiteral *arr_lit, const DataType *expr_type) {
  assert(expr_type->d_kind == D_ARRAY);
  arr_lit->d_type = expr_type;

  // TODO: Check array length in type matches amount of elements provided - more error case handling needed in tests
  for (size_t i = 0; i < arr_lit->items.count; i++) {
    if (analyse_expression(state, &arr_lit->items.elements[i], arr_lit->d_type->d_union.a_type.element_type) != 0) {
      fprintf(stderr, "Failed to analyse array item\n");
      return INVALID_PROGRAM_CODE;
    }
  }

  return 0;
}

unsigned char analyse_numeric_literal(TerminalLiteral *term_lit, bool is_negative, const DataType *expr_type) {
  assert(term_lit->tok->t_type == T_NUMERIC_LIT && term_lit->tok->item != NULL);
  term_lit->literal.d_type = expr_type;

  long long int_val;
  if (tok_to_integer(&int_val, term_lit->tok, is_negative) != 0) {
    return INVALID_PROGRAM_CODE;
  }

  assert(term_lit->literal.d_type->d_kind == D_PRIMITIVE);

  switch (term_lit->literal.d_type->d_union.p_type) {
  case P_I8:
    if (!is_number_in_range(int_val, INT8_MIN, INT8_MAX)) {
      fprintf(stderr, "Value %lld does not fit in i8 integer\n", int_val);
      return INVALID_PROGRAM_CODE;
    }
    term_lit->literal.l_union.i8 = (int8_t)int_val;
    break;
  case P_I16:
    if (!is_number_in_range(int_val, INT16_MIN, INT16_MAX)) {
      fprintf(stderr, "Value %lld does not fit in i16 integer\n", int_val);
      return INVALID_PROGRAM_CODE;
    }
    term_lit->literal.l_union.i16 = (int16_t)int_val;
    break;
  case P_I32:
    if (!is_number_in_range(int_val, INT32_MIN, INT32_MAX)) {
      fprintf(stderr, "Value %lld does not fit in i32 integer\n", int_val);
      return INVALID_PROGRAM_CODE;
    }
    term_lit->literal.l_union.i32 = (int32_t)int_val;
    break;
  case P_I64:
    term_lit->literal.l_union.i64 = (int64_t)int_val;
    break;
  default:
    assert(false);
  }

  return 0;
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
  return INVALID_PROGRAM_CODE;
}

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

unsigned char insert_identifier(SemaState *state, const char *name, const DataType *d_type) {
  const Identifier *existing = get_identifier(state, name);
  if (existing != NULL) {
    fprintf(stderr, "Variable %s shadows existing variable in scope\n", existing->name);
    return INVALID_PROGRAM_CODE;
  }

  Identifier new_ident = {
      .d_type = d_type,
      .name = name,
      .scope = state->scopes.elements[state->scopes.count - 1],
  };

  dyn_array_insert(&state->identifiers, new_ident);
  return 0;
}

unsigned char analyse_block(SemaState *state, Block *block, const DataType *func_type) {
  unsigned char result = analyse_statements(state, &block->body, func_type);
  if (result != 0) {
    return result;
  }

  // TODO: This is inefficient to have to search through the statements again, can we improve this
  block->has_return = false;
  for (size_t i = 0; i < block->body.count; i++) {
    if (block->body.elements[i].s_type != S_RETURN) {
      continue;
    }

    block->has_return = true;
    if (i < block->body.count - 1) {
      fprintf(stderr, "Dead code after return statement\n");
      return 1;
    }
  }

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

bool is_integer_type(const DataType *d_type) {
  assert(d_type->d_kind != D_NONE);

  if (d_type->d_kind != D_PRIMITIVE) {
    return false;
  }

  switch (d_type->d_union.p_type) {
  case P_I32:
  case P_I64:
  case P_I8:
  case P_I16:
    return true;
  default:
    return false;
  }
}

bool is_number_in_range(long long input, long long min, long long max) { return input >= min && input <= max; }

unsigned char tok_to_integer(long long *output_num, const Token *tok, bool is_negative) {
  const size_t input_len = strlen(tok->item);
  char num_buf[input_len + 2];
  char sign = is_negative ? '-' : '+';
  if (snprintf(num_buf, input_len + 2, "%c%s", sign, tok->item) < 0) {
    fprintf(stderr, "Failed to write numerical value to string buffer\n");
    return INVALID_PROGRAM_CODE;
  }

  // TODO: Check what happens for i64 when provided value is LLONG_MIN or LLONG_MAX which is valid - shouldn't error
  const long long int_val = strtoll(num_buf, NULL, INT_BASE);
  if (int_val == LLONG_MIN || int_val == LLONG_MAX || (int_val == 0 && strcmp(tok->item, "0") != 0)) {
    fprintf(stderr, "Value %s does not fit in integer\n", num_buf);
    return INVALID_PROGRAM_CODE;
  }

  *output_num = int_val;
  return 0;
}

bool is_void_type(const DataType *d_type) {
  if (d_type->d_kind == D_PRIMITIVE) {
    return d_type->d_union.p_type == P_VOID;
  } else if (d_type->d_kind == D_ARRAY) {
    return is_void_type(d_type->d_union.a_type.element_type);
  }

  assert(false);
}
