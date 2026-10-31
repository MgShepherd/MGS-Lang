#include "sema/statement.h"

#include "dynamic_array.h"
#include "sema/data_type.h"
#include "sema/expression.h"
#include "sema/terminal.h"
#include "sema/utils.h"

#include <assert.h>
#include <stdio.h>

unsigned char analyse_statement(SemaState *state, Statement *statement, const DataType *func_type);
unsigned char analyse_dec_statement(SemaState *state, DeclarationStatement *dec);
unsigned char analyse_assign_statement(SemaState *state, AssignmentStatement *assign);
unsigned char analyse_ret_statement(SemaState *state, ReturnStatement *ret, const DataType *func_type);
unsigned char analyse_if_block(SemaState *state, IfBlock *if_block, const DataType *func_type);
unsigned char analyse_if_branch(SemaState *state, IfBranch *if_branch, const DataType *func_type);
unsigned char analyse_void_statement(SemaState *state, VoidStatement *void_s);

static const DataType BOOL_TYPE = {.d_kind = D_PRIMITIVE, .d_union = {.p_type = P_BOOL}};
static const DataType VOID_TYPE = {.d_kind = D_PRIMITIVE, .d_union = {.p_type = P_VOID}};

unsigned char analyse_statements(SemaState *state, const Statements *statements, const DataType *func_type) {
  for (size_t i = 0; i < statements->count; i++) {
    if (analyse_statement(state, &statements->elements[i], func_type) != 0) {
      return 1;
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
    return 1;
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
      return 1;
    }

    assign->d_type = ident->d_type;
    term_ident->scope = ident->scope;

    if (!assign->d_type->variable) {
      fprintf(stderr, "Attempted to modify constant: %s\n", ident->name);
      return 1;
    }
    break;
  case A_ARRAY_ELEMENT:
    ArrayAccess *arr_element = &assign->a_output.a_union.arr_element;
    if (analyse_array_access(state, arr_element, NULL) != 0) {
      fprintf(stderr, "Failed to analyse array access for assignment statement\n");
      return 1;
    }

    // TODO: We are re-fetching the identifier here even though already fetched for analyse_terminal_array_access
    ident = get_identifier(state, arr_element->term_ident.name);
    assert(ident != NULL && ident->d_type->d_kind == D_ARRAY);

    assign->d_type = ident->d_type->d_union.a_type.element_type;

    if (!assign->d_type->variable) {
      fprintf(stderr, "Attempted to modify constant: %s\n", ident->name);
      return 1;
    }
    break;
  case A_STRUCT_FIELD:
    StructAccess *struct_field = &assign->a_output.a_union.struct_field;
    if (analyse_struct_access(state, struct_field, NULL) != 0) {
      fprintf(stderr, "Failed to analyse struct field for assignment statement\n");
      return 1;
    }

    assign->d_type = struct_field->d_type;
    if (!struct_field->variable) {
      fprintf(stderr, "Attempted to modify constant: %s\n", struct_field->term_ident.name);
      return 1;
    }
    break;
  default:
    assert(false);
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
