#include "llvm/utils.h"
#include "parsing/data_type.h"
#include "parsing/expression.h"
#include "parsing/type.h"
#include "sema/utils.h"
#include "llvm-c/Core.h"
#include "llvm-c/Types.h"
#include "llvm/type_definition.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>

LLVMTypeRef get_primitive_type(const IRState *state, PrimitiveType p_type);

// TODO: Should these array length and element pointer functions really live in utils - probably best in their own file
void build_array_length_check(IRState *state, const TerminalIdentifier *term_ident, const TypeDefinitionRef *type_def,
                              LLVMValueRef index);

const ValueRef *load_identifier(const ValueRefs *values, const TerminalIdentifier *term_ident) {
  for (size_t i = 0; i < values->count; i++) {
    if (strcmp(values->elements[i].name, term_ident->name) != 0) {
      continue;
    }

    if (term_ident->scope != values->elements[i].scope) {
      continue;
    }

    return &values->elements[i];
  }

  return NULL;
}

const LlvmFunc *load_function(const FuncRefs *funcs, const char *identifier) {
  for (size_t i = 0; i < funcs->count; i++) {
    if (strcmp(funcs->elements[i].name, identifier) == 0) {
      return &funcs->elements[i];
    }
  }

  return NULL;
}

const TypeDefinitionRef *load_type_definition(const TypeDefinitionRefs *type_refs, const char *identifier) {
  for (size_t i = 0; i < type_refs->count; i++) {
    if (strcmp(type_refs->elements[i].name, identifier) == 0) {
      return &type_refs->elements[i];
    }
  }

  return NULL;
}

LLVMTypeRef get_type(IRState *state, const DataType *d_type) {
  assert(state != NULL && d_type->d_kind != D_NONE);

  if (d_type->d_kind == D_PRIMITIVE) {
    return get_primitive_type(state, d_type->d_union.p_type);
  } else if (d_type->d_kind == D_STRUCT) {
    const TypeDefinitionRef *type_def = load_type_definition(&state->type_refs, d_type->d_union.s_type.name);
    return type_def->ref;
  } else if (d_type->d_kind == D_ARRAY) {
    const TypeDefinitionRef *type_def = load_type_definition(&state->type_refs, ARRAY_TYPE_DEFINITION_NAME);
    return type_def->ref;
  }

  assert(false);
}

LLVMValueRef get_element_ptr(IRState *state, const TerminalIdentifier *term_ident, const size_t *indices,
                             size_t num_indices) {
  const ValueRef *outer_val = load_identifier(&state->values, term_ident);
  assert(outer_val != NULL);
  const LLVMTypeRef outer_type = get_type(state, outer_val->d_type);

  LLVMValueRef llvm_indices[num_indices + 1];

  // First index ensures that we are stepping into the struct itself
  llvm_indices[0] = LLVMConstInt(LLVMInt32TypeInContext(state->context), 0, false);

  for (size_t i = 0; i < num_indices; i++) {
    llvm_indices[i + 1] = LLVMConstInt(LLVMInt32TypeInContext(state->context), indices[i], false);
  }

  // TODO: Improve how we create LLVM names in order to make IR easier to read/understand
  return LLVMBuildInBoundsGEP2(state->builder, outer_type, outer_val->ptr, llvm_indices, num_indices + 1,
                               "access_element");
}

LLVMValueRef get_array_element_ptr(IRState *state, const TerminalIdentifier *term_ident, const size_t *indices,
                                   size_t num_indices) {
  const TypeDefinitionRef *type_def = load_type_definition(&state->type_refs, ARRAY_TYPE_DEFINITION_NAME);
  assert(type_def != NULL);

  const size_t array_ptr_idx = 1;
  const LLVMValueRef array_data_ptr = get_element_ptr(state, term_ident, &array_ptr_idx, 1);
  const LLVMValueRef array_data =
      LLVMBuildLoad2(state->builder, type_def->field_types[array_ptr_idx], array_data_ptr, "get_array_data");

  // TODO: Update this to support multi-dimensional arrays
  const LLVMTypeRef index_type = LLVMInt32TypeInContext(state->context);
  LLVMValueRef array_indices[] = {LLVMConstInt(index_type, indices[0], false)};

  const ValueRef *outer_val = load_identifier(&state->values, term_ident);
  const ArrayType *array_type = &outer_val->d_type->d_union.a_type;
  // When length is -1, means the length is not known at compile time and we should add a runtime check on the length
  if (array_type->length == -1) {
    build_array_length_check(state, term_ident, type_def, array_indices[0]);
  }

  return LLVMBuildGEP2(state->builder, index_type, array_data, array_indices, num_indices, "access_array");
}

LLVMTypeRef get_primitive_type(const IRState *state, PrimitiveType p_type) {
  switch (p_type) {
  case P_I8:
    return LLVMInt8TypeInContext(state->context);
  case P_I16:
    return LLVMInt16TypeInContext(state->context);
  case P_I32:
    return LLVMInt32TypeInContext(state->context);
  case P_I64:
    return LLVMInt64TypeInContext(state->context);
  case P_BOOL:
    return LLVMInt1TypeInContext(state->context);
  case P_VOID:
    return LLVMVoidTypeInContext(state->context);
  default:
    assert(false);
  }
}

// TODO: Update this to support multi-dimensional arrays
void build_array_length_check(IRState *state, const TerminalIdentifier *term_ident, const TypeDefinitionRef *type_def,
                              LLVMValueRef index) {
  size_t array_length_idx = 0;
  LLVMValueRef array_length_ptr = get_element_ptr(state, term_ident, &array_length_idx, 1);
  LLVMValueRef array_length =
      LLVMBuildLoad2(state->builder, type_def->field_types[array_length_idx], array_length_ptr, "get_array_len");

  LLVMBasicBlockRef continue_block =
      LLVMAppendBasicBlockInContext(state->context, state->current_func, "continue_block");
  LLVMBasicBlockRef exit_block = LLVMAppendBasicBlockInContext(state->context, state->current_func, "exit_block");

  LLVMValueRef length_check = LLVMBuildICmp(state->builder, LLVMIntSLT, index, array_length, "array_bounds_check");
  LLVMBuildCondBr(state->builder, length_check, continue_block, exit_block);

  LLVMPositionBuilderAtEnd(state->builder, exit_block);

  const LlvmFunc *exit_func = load_function(&state->funcs, "exit");
  assert(exit_func != NULL);
  LLVMValueRef exit_code = LLVMConstInt(LLVMInt32TypeInContext(state->context), 1, false);
  LLVMBuildCall2(state->builder, exit_func->func_type, exit_func->ptr, &exit_code, 1, "");
  // Unreachable is needed as the exit function never returns, but LLVM expects the block to have a terminator
  LLVMBuildUnreachable(state->builder);

  LLVMPositionBuilderAtEnd(state->builder, continue_block);
}
