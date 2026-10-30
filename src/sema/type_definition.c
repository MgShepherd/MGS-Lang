#include "sema/type_definition.h"
#include "sema/utils.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int field_compare(const void *field1, const void *field2);

unsigned char analyse_type_definition(TypeDefinition *type_def) {
  assert(type_def->td_kind == TD_STRUCT);
  StructDefinition *struct_def = &type_def->td_union.s_def;

  const char *provided_field_names[struct_def->fields.count];
  size_t num_provided_fields = 0;

  qsort(struct_def->fields.elements, struct_def->fields.count, sizeof(Field), field_compare);

  for (size_t i = 0; i < struct_def->fields.count; i++) {
    const Field *current_field = &struct_def->fields.elements[i];

    if (is_void_type(&current_field->d_type)) {
      fprintf(stderr, "Cannot use void as type for field in type definition\n");
      return 1;
    }

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

int field_compare(const void *item1, const void *item2) {
  const Field *field1 = (Field *)item1;
  const Field *field2 = (Field *)item2;
  return strcmp(field1->term_ident.name, field2->term_ident.name);
}
