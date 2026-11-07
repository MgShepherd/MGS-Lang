#include "llvm/expression.h"
#include "parsing/expression.h"
#include "parsing/type.h"
#include "llvm-c/Core.h"
#include "llvm-c/Types.h"
#include "llvm/utils.h"
#include <assert.h>
#include <stdio.h>

LLVMValueRef build_terminal_expr(const IRState *state, const TerminalExpr *term, const LLVMTypeRef d_type, bool store);
LLVMValueRef build_compound_expr(const IRState *state, const CompoundExpr *comp, const LLVMTypeRef d_type, bool store);
LLVMValueRef build_func_call(const IRState *state, const FunctionCall *func_call, const LLVMTypeRef d_type, bool store);

LLVMValueRef build_identifier(const IRState *state, const TerminalIdentifier *term_ident, const LLVMTypeRef d_type,
                              bool store);
LLVMValueRef build_literal(const IRState *state, const Literal *literal, const LLVMTypeRef d_type, bool store);
LLVMValueRef build_struct_literal(const IRState *state, const StructLiteral *struct_lit, bool store);
LLVMValueRef build_struct_access(const IRState *state, const StructAccess *struct_access, bool store);

LLVMValueRef build_expression(const IRState *state, const Expression *expr, const LLVMTypeRef d_type, bool store) {
  switch (expr->e_type) {
  case E_TERMINAL:
    return build_terminal_expr(state, &expr->e_union.term, d_type, store);
  case E_COMPOUND:
    return build_compound_expr(state, &expr->e_union.comp, d_type, store);
  default:
    assert(false);
  }
}

LLVMValueRef build_terminal_expr(const IRState *state, const TerminalExpr *term, const LLVMTypeRef d_type, bool store) {
  LLVMValueRef result = NULL;
  switch (term->item.t_type) {
  case TERM_IDENTIFIER:
    result = build_identifier(state, &term->item.t_union.ident, d_type, store);
    break;
  case TERM_LITERAL:
    result = build_literal(state, &term->item.t_union.lit.literal, d_type, store);
    break;
  case TERM_FUNC_CALL:
    result = build_func_call(state, &term->item.t_union.func_call, d_type, store);
    break;
  case TERM_STRUCT_LITERAL:
    result = build_struct_literal(state, &term->item.t_union.struct_literal, store);
    break;
  case TERM_STRUCT_ACCESS:
    result = build_struct_access(state, &term->item.t_union.struct_access, store);
    break;
  default:
    assert(false);
  }

  if (term->sign != NULL && term->sign->t_type == T_MINUS) {
    char *term_name = get_terminal_name(&term->item);
    if (store) {
      LLVMValueRef loaded = LLVMBuildLoad2(state->builder, d_type, result, "load_negate");
      LLVMValueRef negated = LLVMBuildNeg(state->builder, loaded, term_name);
      LLVMBuildStore(state->builder, negated, result);
    } else {
      result = LLVMBuildNeg(state->builder, result, term_name);
    }
    free(term_name);
  }

  return result;
}

LLVMValueRef build_compound_expr(const IRState *state, const CompoundExpr *comp, const LLVMTypeRef d_type, bool store) {
  assert(comp != NULL && comp->op != O_NONE);

  LLVMTypeRef term_type = d_type;
  // TODO: We need to revisit boolean chained expressions as they may not work anymore
  if (term_type == LLVMInt1TypeInContext(state->context)) {
    term_type = LLVMInt32TypeInContext(state->context);
  }

  LLVMValueRef lhs = build_terminal_expr(state, &comp->lhs, term_type, false);

  LLVMValueRef rhs;
  if (comp->rhs->e_type == E_TERMINAL) {
    rhs = build_terminal_expr(state, &comp->rhs->e_union.term, term_type, false);
  } else {
    rhs = build_compound_expr(state, &comp->rhs->e_union.comp, term_type, false);
  }

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

  if (!store) {
    return result;
  }

  LLVMValueRef ptr = LLVMBuildAlloca(state->builder, d_type, "alloc_comp");
  LLVMBuildStore(state->builder, result, ptr);
  return ptr;
}

LLVMValueRef build_literal(const IRState *state, const Literal *literal, const LLVMTypeRef p_type, bool store) {
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

  if (!store) {
    return lit;
  }

  LLVMValueRef ptr = LLVMBuildAlloca(state->builder, p_type, "lit_alloc");
  LLVMBuildStore(state->builder, lit, ptr);
  return ptr;
}

LLVMValueRef build_identifier(const IRState *state, const TerminalIdentifier *term_ident, const LLVMTypeRef d_type,
                              bool store) {
  const ValueRef *value_ref = load_identifier(&state->values, term_ident);
  assert(value_ref != NULL);

  const LLVMValueRef loaded = LLVMBuildLoad2(state->builder, d_type, value_ref->ptr, term_ident->name);

  if (!store) {
    return loaded;
  }

  LLVMValueRef ptr = LLVMBuildAlloca(state->builder, d_type, "alloc_ident");
  LLVMBuildStore(state->builder, loaded, ptr);
  return ptr;
}

LLVMValueRef build_func_call(const IRState *state, const FunctionCall *func_call, const LLVMTypeRef d_type,
                             bool store) {
  const LlvmFunc *func = load_function(&state->funcs, func_call->name->item);
  assert(func != NULL);

  LLVMValueRef params[func->num_params];
  for (size_t i = 0; i < func->num_params; i++) {
    params[i] = build_expression(state, &func_call->parameters.elements[i], func->param_types[i], false);
  }

  const char *name = func_call->name->item;
  LLVMValueRef func_result;
  if (d_type != LLVMVoidTypeInContext(state->context)) {
    func_result = LLVMBuildCall2(state->builder, func->func_type, func->ptr, params, func->num_params, name);
  } else {
    func_result = LLVMBuildCall2(state->builder, func->func_type, func->ptr, params, func->num_params, "");
  }

  if (!store) {
    return func_result;
  }

  LLVMValueRef ptr = LLVMBuildAlloca(state->builder, d_type, "alloc_func");
  LLVMBuildStore(state->builder, func_result, ptr);
  return ptr;
}

LLVMValueRef build_struct_literal(const IRState *state, const StructLiteral *struct_lit, bool store) {
  assert(struct_lit->d_type->d_kind == D_STRUCT);

  const LLVMTypeRef struct_type = get_type(state, struct_lit->d_type);
  const TypeDefinitionRef *type_def = load_type_definition(&state->type_refs, struct_lit->d_type->d_union.s_type.name);
  assert(struct_lit->field_assignments.count == type_def->num_fields);

  LLVMValueRef ptr = LLVMBuildAlloca(state->builder, struct_type, "alloc_struct");

  for (size_t i = 0; i < struct_lit->field_assignments.count; i++) {
    FieldAssignment *current = &struct_lit->field_assignments.elements[i];

    LLVMValueRef field_ptr = LLVMBuildStructGEP2(state->builder, struct_type, ptr, i, current->name);
    LLVMValueRef field_result = build_expression(state, &current->expression, type_def->field_types[i], false);
    LLVMBuildStore(state->builder, field_result, field_ptr);
  }

  if (store) {
    return ptr;
  }

  return LLVMBuildLoad2(state->builder, struct_type, ptr, "load_struct");
}

LLVMValueRef build_struct_access(const IRState *state, const StructAccess *struct_access, bool store) {
  assert(struct_access->type_def != NULL && struct_access->type_def->td_kind == TD_STRUCT);
  const StructDefinition *struct_def = &struct_access->type_def->td_union.s_def;
  assert(struct_def != NULL);

  const TypeDefinitionRef *type_def = load_type_definition(&state->type_refs, struct_def->name);
  const LLVMTypeRef field_type = get_type(state, struct_access->d_type);
  assert(type_def != NULL && field_type != NULL);

  const LLVMValueRef element_ptr =
      get_element_ptr(state, &struct_access->term_ident, struct_access->field_idxs, struct_access->num_field_nestings);
  const LLVMValueRef element_val = LLVMBuildLoad2(state->builder, field_type, element_ptr, "load_struct_field");

  if (!store) {
    return element_val;
  }

  LLVMValueRef ptr = LLVMBuildAlloca(state->builder, field_type, "access_ptr");
  LLVMBuildStore(state->builder, element_val, ptr);
  return ptr;
}
