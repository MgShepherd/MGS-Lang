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

const Identifier *load_identifier(const Identifiers *identifiers, const IdentifierKey *ident_key) {
  for (size_t i = 0; i < identifiers->count; i++) {
    if (strcmp(identifiers->elements[i].name, ident_key->name) != 0) {
      continue;
    }

    if (ident_key->scope != identifiers->elements[i].scope) {
      continue;
    }

    return &identifiers->elements[i];
  }

  return NULL;
}

const Function *load_function(const IRState *state, const char *identifier) {
  for (size_t i = 0; i < state->program->functions.count; i++) {
    if (strcmp(state->program->functions.elements[i].name, identifier) == 0) {
      return &state->program->functions.elements[i];
    }
  }

  for (size_t i = 0; i < state->external_funcs.count; i++) {
    if (strcmp(state->external_funcs.elements[i].name, identifier) == 0) {
      return &state->external_funcs.elements[i];
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
