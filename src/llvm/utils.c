#include "llvm/utils.h"
#include "parsing/data_type.h"
#include "llvm/type_definition.h"

#include <assert.h>
#include <stdio.h>
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

LLVMTypeRef get_type(IRState *state, const DataType *d_type) {
  assert(state != NULL && d_type->d_kind != D_NONE);

  if (d_type->d_kind == D_PRIMITIVE) {
    return get_primitive_type(state, d_type->d_union.p_type);
  } else if (d_type->d_kind == D_STRUCT) {
    const TypeDefinitionRef *type_def = load_type_definition(&state->type_refs, d_type->d_union.s_type.name);
    return type_def->ref;
  } else if (d_type->d_kind == D_ARRAY) {
    // For array types, we need to check if there is already a type defintion for the array interal represention - if
    // not we need to create one - this should only need to happen for the very first array of any type defined in the
    // program
    const TypeDefinitionRef *type_def = load_type_definition(&state->type_refs, ARRAY_TYPE_DEFINITION_NAME);
    if (type_def != NULL) {
      return type_def->ref;
    }

    const TypeDefinitionRef *array_def = build_array_type_definition(state);
    return array_def->ref;
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
  llvm_indices[0] = LLVMConstInt(LLVMInt64TypeInContext(state->context), 0, false);

  for (size_t i = 0; i < num_indices; i++) {
    llvm_indices[i + 1] = LLVMConstInt(LLVMInt64TypeInContext(state->context), indices[i], false);
  }

  // TODO: Improve how we create LLVM names in order to make IR easier to read/understand
  return LLVMBuildInBoundsGEP2(state->builder, outer_type, outer_val->ptr, llvm_indices, num_indices + 1,
                               "access_element");
}

char *get_array_type_def_name(const ArrayType *array_type) {
  // Array type def prefix starts with ! in order to ensure this never collides with user defined types
  static const char *ARRAY_TYPE_DEF_PREFIX = "!ARRAY_";

  const char *element_type_name = d_type_to_string(array_type->element_type);
  // Add 1 to length to account for null terminator
  const size_t name_len = strlen(ARRAY_TYPE_DEF_PREFIX) + strlen(element_type_name) + 1;
  char *name = malloc(sizeof(char) * name_len);
  assert(name != NULL);

  size_t result = snprintf(name, name_len, "%s%s", ARRAY_TYPE_DEF_PREFIX, element_type_name);
  assert(result > 0);

  return name;
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
