#include "sema/type_definition.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>

unsigned char analyse_type_definition(const TypeDefinition *type_def) {
  assert(type_def->td_kind == TD_STRUCT);
  const StructDefinition *struct_def = &type_def->td_union.s_def;

  const char *provided_field_names[struct_def->fields.count];
  size_t num_provided_fields = 0;

  for (size_t i = 0; i < struct_def->fields.count; i++) {
    const Field *current_field = &struct_def->fields.elements[i];

    for (size_t i = 0; i < num_provided_fields; i++) {
      if (strcmp(provided_field_names[i], current_field->term_ident.name) == 0) {
        fprintf(stderr, "Field %s provided multiple times in struct definition\n", current_field->term_ident.name);
        return 1;
      }
    }
    provided_field_names[num_provided_fields++] = current_field->term_ident.name;
  }

  return 0;
}
