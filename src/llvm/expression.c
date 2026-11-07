#include "llvm/expression.h"
#include "parsing/expression.h"
#include "parsing/type.h"
#include "llvm-c/Core.h"
#include "llvm-c/Types.h"
#include "llvm/utils.h"
#include <assert.h>
#include <stdio.h>

void build_terminal_expr(const LLVMValueRef out_ptr, const IRState *state, const TerminalExpr *term,
                         const LLVMTypeRef d_type);
void build_compound_expr(const LLVMValueRef out_ptr, const IRState *state, const CompoundExpr *comp,
                         const LLVMTypeRef d_type);
void build_func_call(const LLVMValueRef out_ptr, const IRState *state, const FunctionCall *func_call,
                     const LLVMTypeRef d_type);

void build_identifier(const LLVMValueRef out_ptr, const IRState *state, const TerminalIdentifier *term_ident,
                      const LLVMTypeRef d_type);
void build_literal(const LLVMValueRef out_ptr, const IRState *state, const Literal *literal, const LLVMTypeRef d_type);
void build_struct_literal(const LLVMValueRef out_ptr, const IRState *state, const StructLiteral *struct_lit);
void build_struct_access(const LLVMValueRef out_ptr, const IRState *state, const StructAccess *struct_access);

void build_expression(const LLVMValueRef out_ptr, const IRState *state, const Expression *expr,
                      const LLVMTypeRef d_type) {
  switch (expr->e_type) {
  case E_TERMINAL:
    build_terminal_expr(out_ptr, state, &expr->e_union.term, d_type);
    break;
  case E_COMPOUND:
    build_compound_expr(out_ptr, state, &expr->e_union.comp, d_type);
    break;
  default:
    assert(false);
  }
}

void build_terminal_expr(const LLVMValueRef out_ptr, const IRState *state, const TerminalExpr *term,
                         const LLVMTypeRef d_type) {
  switch (term->item.t_type) {
  case TERM_IDENTIFIER:
    build_identifier(out_ptr, state, &term->item.t_union.ident, d_type);
    break;
  case TERM_LITERAL:
    build_literal(out_ptr, state, &term->item.t_union.lit.literal, d_type);
    break;
  case TERM_FUNC_CALL:
    build_func_call(out_ptr, state, &term->item.t_union.func_call, d_type);
    break;
  case TERM_STRUCT_LITERAL:
    build_struct_literal(out_ptr, state, &term->item.t_union.struct_literal);
    break;
  case TERM_STRUCT_ACCESS:
    build_struct_access(out_ptr, state, &term->item.t_union.struct_access);
    break;
  default:
    assert(false);
  }

  if (term->sign != NULL && term->sign->t_type == T_MINUS && out_ptr != NULL) {
    char *term_name = get_terminal_name(&term->item);
    LLVMBuildNeg(state->builder, out_ptr, term_name);
    free(term_name);
  }
}

void build_compound_expr(const LLVMValueRef out_ptr, const IRState *state, const CompoundExpr *comp,
                         const LLVMTypeRef d_type) {
  assert(comp != NULL && comp->op != O_NONE);

  LLVMTypeRef term_type = d_type;
  // TODO: We need to revisit boolean chained expressions as they may not work anymore
  if (term_type == LLVMInt1TypeInContext(state->context)) {
    term_type = LLVMInt32TypeInContext(state->context);
  }

  char *lhs_name = get_terminal_name(&comp->lhs.item);
  LLVMValueRef lhs_ptr = LLVMBuildAlloca(state->builder, term_type, lhs_name);
  build_terminal_expr(lhs_ptr, state, &comp->lhs, term_type);
  assert(lhs_ptr != NULL);

  LLVMValueRef rhs_ptr;
  if (comp->rhs->e_type == E_TERMINAL) {
    rhs_ptr = LLVMBuildAlloca(state->builder, term_type, "RHS_PTR");
    build_terminal_expr(rhs_ptr, state, &comp->rhs->e_union.term, term_type);
  } else {
    rhs_ptr = LLVMBuildAlloca(state->builder, term_type, "RHS_PTR");
    build_compound_expr(rhs_ptr, state, &comp->rhs->e_union.comp, term_type);
  }
  assert(rhs_ptr != NULL);

  LLVMValueRef lhs = LLVMBuildLoad2(state->builder, term_type, lhs_ptr, "LHS");
  LLVMValueRef rhs = LLVMBuildLoad2(state->builder, term_type, rhs_ptr, "RHS");

  LLVMValueRef result;
  switch (comp->op) {
  case O_PLUS:
    result = LLVMBuildAdd(state->builder, lhs, rhs, "ADD");
    break;
  case O_MINUS:
    result = LLVMBuildSub(state->builder, lhs, rhs, "SUB");
    break;
  case O_LT:
    result = LLVMBuildICmp(state->builder, LLVMIntSLT, lhs, rhs, "LT");
    break;
  case O_GT:
    result = LLVMBuildICmp(state->builder, LLVMIntSGT, lhs, rhs, "GT");
    break;
  case O_LTE:
    result = LLVMBuildICmp(state->builder, LLVMIntSLE, lhs, rhs, "LTE");
    break;
  case O_GTE:
    result = LLVMBuildICmp(state->builder, LLVMIntSGE, lhs, rhs, "GTE");
    break;
  default:
    assert(false);
  }

  if (out_ptr != NULL) {
    LLVMBuildStore(state->builder, result, out_ptr);
  }
}

void build_literal(const LLVMValueRef out_ptr, const IRState *state, const Literal *literal, const LLVMTypeRef p_type) {
  assert(literal->d_type->d_kind != D_NONE);

  assert(literal->d_type->d_kind == D_PRIMITIVE);

  LLVMValueRef lit;
  switch (literal->d_type->d_union.p_type) {
  case P_I8:
    lit = LLVMConstInt(p_type, literal->l_union.i8, false);
    break;
  case P_I16:
    lit = LLVMConstInt(p_type, literal->l_union.i16, false);
    break;
  case P_I32:
    lit = LLVMConstInt(p_type, literal->l_union.i32, false);
    break;
  case P_I64:
    lit = LLVMConstInt(p_type, literal->l_union.i64, false);
    break;
  case P_BOOL:
    lit = LLVMConstInt(p_type, literal->l_union.b, false);
    break;
  default:
    assert(false);
  }
  assert(lit != NULL);

  if (out_ptr != NULL) {
    LLVMBuildStore(state->builder, lit, out_ptr);
  }
}

void build_identifier(const LLVMValueRef out_ptr, const IRState *state, const TerminalIdentifier *term_ident,
                      const LLVMTypeRef d_type) {
  const ValueRef *value_ref = load_identifier(&state->values, term_ident);
  assert(value_ref != NULL);

  const LLVMValueRef loaded = LLVMBuildLoad2(state->builder, d_type, value_ref->ptr, term_ident->name);

  if (out_ptr != NULL) {
    LLVMBuildStore(state->builder, loaded, out_ptr);
  }
}

void build_func_call(const LLVMValueRef out_ptr, const IRState *state, const FunctionCall *func_call,
                     const LLVMTypeRef d_type) {
  const LlvmFunc *func = load_function(&state->funcs, func_call->name->item);
  assert(func != NULL);

  LLVMValueRef params[func->num_params];
  for (size_t i = 0; i < func->num_params; i++) {
    // TODO: Check whether this is correct, I don't think we want to be error dumping in this scenario
    if (func->param_types[i] != LLVMInt32TypeInContext(state->context)) {
      LLVMDumpType(func->param_types[i]);
    }

    const LLVMValueRef param_ptr = LLVMBuildAlloca(state->builder, func->param_types[i], "param_ptr");
    build_expression(param_ptr, state, &func_call->parameters.elements[i], func->param_types[i]);
    params[i] = LLVMBuildLoad2(state->builder, func->param_types[i], param_ptr, "param");
  }

  const char *name = func_call->name->item;
  LLVMValueRef func_result;
  if (d_type != LLVMVoidTypeInContext(state->context)) {
    func_result = LLVMBuildCall2(state->builder, func->func_type, func->ptr, params, func->num_params, name);
  } else {
    func_result = LLVMBuildCall2(state->builder, func->func_type, func->ptr, params, func->num_params, "");
  }

  if (out_ptr != NULL) {
    LLVMBuildStore(state->builder, func_result, out_ptr);
  }
}

void build_struct_literal(const LLVMValueRef out_ptr, const IRState *state, const StructLiteral *struct_lit) {
  assert(struct_lit->d_type->d_kind == D_STRUCT);

  const LLVMTypeRef struct_type = get_type(state, struct_lit->d_type);
  const TypeDefinitionRef *type_def = load_type_definition(&state->type_refs, struct_lit->d_type->d_union.s_type.name);
  assert(struct_lit->field_assignments.count == type_def->num_fields);

  for (size_t i = 0; i < struct_lit->field_assignments.count; i++) {
    FieldAssignment *current = &struct_lit->field_assignments.elements[i];

    LLVMValueRef struct_field = LLVMBuildStructGEP2(state->builder, struct_type, out_ptr, i, current->name);
    build_expression(struct_field, state, &current->expression, type_def->field_types[i]);
  }
}

void build_struct_access(const LLVMValueRef out_ptr, const IRState *state, const StructAccess *struct_access) {
  assert(struct_access->type_def != NULL && struct_access->type_def->td_kind == TD_STRUCT);
  const StructDefinition *struct_def = &struct_access->type_def->td_union.s_def;
  assert(struct_def != NULL);

  const TypeDefinitionRef *type_def = load_type_definition(&state->type_refs, struct_def->name);
  const LLVMTypeRef field_type = get_type(state, struct_access->d_type);
  assert(type_def != NULL && field_type != NULL);

  const LLVMValueRef element_ptr =
      get_element_ptr(state, &struct_access->term_ident, struct_access->field_idxs, struct_access->num_field_nestings);
  const LLVMValueRef element_val = LLVMBuildLoad2(state->builder, field_type, element_ptr, "load_struct_field");

  if (out_ptr != NULL) {
    LLVMBuildStore(state->builder, element_val, out_ptr);
  }
}
