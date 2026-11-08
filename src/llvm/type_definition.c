#include "llvm/type_definition.h"
#include "dynamic_array.h"
#include "llvm-c/Core.h"
#include "llvm/utils.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>

#define DEFAULT_ADDRESS_SPACE 0

void build_type_definition(IRState *state, const TypeDefinition *type_definition) {
  TypeDefinitionRef type_ref;

  assert(type_definition->td_kind == TD_STRUCT);
  const StructDefinition *struct_definition = &type_definition->td_union.s_def;

  LLVMTypeRef *field_types = malloc(sizeof(LLVMTypeRef) * struct_definition->fields.count);
  assert(field_types != NULL);

  for (size_t i = 0; i < struct_definition->fields.count; i++) {
    field_types[i] = get_type(state, &struct_definition->fields.elements[i].d_type);
  }

  type_ref.ref = LLVMStructCreateNamed(state->context, struct_definition->name);
  LLVMStructSetBody(type_ref.ref, field_types, struct_definition->fields.count, false);

  type_ref.name = struct_definition->name;
  type_ref.field_types = field_types;
  type_ref.num_fields = struct_definition->fields.count;

  dyn_array_insert(&state->type_refs, type_ref);
}

const TypeDefinitionRef *build_array_type_definition(IRState *state) {
  TypeDefinitionRef type_ref;

  LLVMTypeRef *field_types = malloc(sizeof(LLVMTypeRef) * NUM_ARRAY_TYPE_DEF_FIELDS);
  assert(field_types != NULL);

  field_types[0] = LLVMPointerTypeInContext(state->context, DEFAULT_ADDRESS_SPACE);
  // We are using an int 64 to represent the length of the array
  field_types[1] = LLVMInt64TypeInContext(state->context);

  type_ref.ref = LLVMStructCreateNamed(state->context, ARRAY_TYPE_DEFINITION_NAME);
  LLVMStructSetBody(type_ref.ref, field_types, NUM_ARRAY_TYPE_DEF_FIELDS, false);

  type_ref.field_types = field_types;
  type_ref.name = ARRAY_TYPE_DEFINITION_NAME;
  type_ref.num_fields = NUM_ARRAY_TYPE_DEF_FIELDS;

  dyn_array_insert(&state->type_refs, type_ref);
  return &state->type_refs.elements[state->type_refs.count - 1];
}
