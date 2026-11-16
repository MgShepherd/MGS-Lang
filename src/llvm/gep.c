#include "llvm/gep.h"
#include "parsing/type.h"

#include "llvm/utils.h"
#include <assert.h>

void build_array_length_check(IRState *state, const IdentifierKey *ident_key, const TypeDefinitionRef *type_def,
                              LLVMValueRef index);

LLVMValueRef get_element_ptr(IRState *state, const IdentifierKey *ident_key, const size_t *indices,
                             size_t num_indices) {
  const Identifier *ident = load_identifier(&state->program->identifiers, ident_key);
  assert(ident_key != NULL && ident->ptr != NULL);
  const LLVMTypeRef outer_type = get_type(state, &ident->d_type);

  LLVMValueRef llvm_indices[num_indices + 1];

  // First index ensures that we are stepping into the struct itself
  llvm_indices[0] = LLVMConstInt(LLVMInt32TypeInContext(state->context), 0, false);

  for (size_t i = 0; i < num_indices; i++) {
    llvm_indices[i + 1] = LLVMConstInt(LLVMInt32TypeInContext(state->context), indices[i], false);
  }

  // TODO: Improve how we create LLVM names in order to make IR easier to read/understand
  return LLVMBuildInBoundsGEP2(state->builder, outer_type, ident->ptr, llvm_indices, num_indices + 1, "access_element");
}

LLVMValueRef get_array_element_ptr(IRState *state, const IdentifierKey *ident_key, const size_t *indices,
                                   size_t num_indices) {
  const TypeDefinitionRef *type_def = load_type_definition(&state->type_refs, ARRAY_TYPE_DEFINITION_NAME);
  assert(type_def != NULL);

  const size_t array_ptr_idx = 1;
  const LLVMValueRef array_data_ptr = get_element_ptr(state, ident_key, &array_ptr_idx, 1);
  const LLVMValueRef array_data =
      LLVMBuildLoad2(state->builder, type_def->field_types[array_ptr_idx], array_data_ptr, "get_array_data");

  // TODO: Update this to support multi-dimensional arrays
  const LLVMTypeRef index_type = LLVMInt32TypeInContext(state->context);
  LLVMValueRef array_indices[] = {LLVMConstInt(index_type, indices[0], false)};

  const Identifier *ident = load_identifier(&state->program->identifiers, ident_key);
  const ArrayType *array_type = &ident->d_type.d_union.a_type;
  // When length is -1, means the length is not known at compile time and we should add a runtime check on the length
  if (array_type->length == -1) {
    build_array_length_check(state, ident_key, type_def, array_indices[0]);
  }

  return LLVMBuildGEP2(state->builder, index_type, array_data, array_indices, num_indices, "access_array");
}

// TODO: Update this to support multi-dimensional arrays
void build_array_length_check(IRState *state, const IdentifierKey *ident_key, const TypeDefinitionRef *type_def,
                              LLVMValueRef index) {
  size_t array_length_idx = 0;
  LLVMValueRef array_length_ptr = get_element_ptr(state, ident_key, &array_length_idx, 1);
  LLVMValueRef array_length =
      LLVMBuildLoad2(state->builder, type_def->field_types[array_length_idx], array_length_ptr, "get_array_len");

  LLVMBasicBlockRef continue_block =
      LLVMAppendBasicBlockInContext(state->context, state->current_func, "continue_block");
  LLVMBasicBlockRef exit_block = LLVMAppendBasicBlockInContext(state->context, state->current_func, "exit_block");

  LLVMValueRef length_check = LLVMBuildICmp(state->builder, LLVMIntSLT, index, array_length, "array_bounds_check");
  LLVMBuildCondBr(state->builder, length_check, continue_block, exit_block);

  LLVMPositionBuilderAtEnd(state->builder, exit_block);

  const Function *exit_func = load_function(&state->program->functions, "exit");
  assert(exit_func != NULL);
  LLVMValueRef exit_code = LLVMConstInt(LLVMInt32TypeInContext(state->context), 1, false);
  LLVMBuildCall2(state->builder, exit_func->func_type, exit_func->ptr, &exit_code, 1, "");
  // Unreachable is needed as the exit function never returns, but LLVM expects the block to have a terminator
  LLVMBuildUnreachable(state->builder);

  LLVMPositionBuilderAtEnd(state->builder, continue_block);
}
