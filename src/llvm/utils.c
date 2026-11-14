#include "llvm/utils.h"
#include "parsing/data_type.h"
#include "parsing/expression.h"
#include "parsing/type.h"
#include "llvm-c/Core.h"
#include "llvm-c/Types.h"
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
    const TypeDefinitionRef *type_def = load_type_definition(&state->type_refs, ARRAY_TYPE_DEFINITION_NAME);
    return type_def->ref;
  }

  assert(false);
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
