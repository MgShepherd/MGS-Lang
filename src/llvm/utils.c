#include "llvm/utils.h"

#include <assert.h>
#include <string.h>

LLVMTypeRef get_primitive_type(const IRState *state, PrimitiveType p_type);

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

LLVMTypeRef get_type(const IRState *state, const DataType *d_type) {
  assert(state != NULL && d_type->d_kind != D_NONE);

  if (d_type->d_kind == D_PRIMITIVE) {
    return get_primitive_type(state, d_type->d_union.p_type);
  } else if (d_type->d_kind == D_STRUCT) {
    const TypeDefinitionRef *type_def = load_type_definition(&state->type_refs, d_type->d_union.s_type.name);
    return type_def->ref;
  }

  assert(false);
}

LLVMValueRef get_element_ptr(const IRState *state, const TerminalIdentifier *term_ident, const size_t *indices,
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
