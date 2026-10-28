#include "sema/utils.h"

#include "dynamic_array.h"
#include <limits.h>
#include <stdio.h>
#include <string.h>

#define INT_BASE 10

unsigned char insert_identifier(SemaState *state, const char *name, const DataType *d_type) {
  const Identifier *existing = get_identifier(state, name);
  if (existing != NULL) {
    fprintf(stderr, "Variable %s shadows existing variable in scope\n", existing->name);
    return 1;
  }

  Identifier new_ident = {
      .d_type = d_type,
      .name = name,
      .scope = state->scopes.elements[state->scopes.count - 1],
  };

  dyn_array_insert(&state->identifiers, new_ident);
  return 0;
}

const Identifier *get_identifier(const SemaState *state, const char *name) {
  for (size_t i = 0; i < state->identifiers.count; i++) {
    if (strcmp(state->identifiers.elements[i].name, name) != 0) {
      continue;
    }

    if (!is_identifier_in_scope(&state->scopes, &state->identifiers.elements[i])) {
      continue;
    }

    return &state->identifiers.elements[i];
  }
  return NULL;
}

const Function *get_function(const Functions *functions, const char *name) {
  for (size_t i = 0; i < functions->count; i++) {
    if (strcmp(functions->elements[i].name, name) == 0) {
      return &functions->elements[i];
    }
  }
  return NULL;
}

const TypeDefinition *get_type_def(const TypeDefinitions *type_defs, const char *name) {
  for (size_t i = 0; i < type_defs->count; i++) {
    assert(type_defs->elements[i].td_kind == TD_STRUCT);
    const StructDefinition *struct_def = &type_defs->elements[i].td_union.s_def;
    if (strcmp(struct_def->name, name) == 0) {
      return &type_defs->elements[i];
    }
  }
  return NULL;
}

bool is_identifier_in_scope(const Scopes *scopes, const Identifier *identifier) {
  for (size_t i = 0; i < scopes->count; i++) {
    if (scopes->elements[i] == identifier->scope) {
      return true;
    }
  }
  return false;
}

bool is_integer_type(const DataType *d_type) {
  assert(d_type->d_kind != D_NONE);

  if (d_type->d_kind != D_PRIMITIVE) {
    return false;
  }

  switch (d_type->d_union.p_type) {
  case P_I32:
  case P_I64:
  case P_I8:
  case P_I16:
    return true;
  default:
    return false;
  }
}

bool is_number_in_range(long long input, long long min, long long max) { return input >= min && input <= max; }

unsigned char tok_to_integer(long long *output_num, const Token *tok, bool is_negative) {
  const size_t input_len = strlen(tok->item);
  char num_buf[input_len + 2];
  char sign = is_negative ? '-' : '+';
  if (snprintf(num_buf, input_len + 2, "%c%s", sign, tok->item) < 0) {
    fprintf(stderr, "Failed to write numerical value to string buffer\n");
    return 1;
  }

  // TODO: Check what happens for i64 when provided value is LLONG_MIN or LLONG_MAX which is valid - shouldn't error
  const long long int_val = strtoll(num_buf, NULL, INT_BASE);
  if (int_val == LLONG_MIN || int_val == LLONG_MAX || (int_val == 0 && strcmp(tok->item, "0") != 0)) {
    fprintf(stderr, "Value %s does not fit in integer\n", num_buf);
    return 1;
  }

  *output_num = int_val;
  return 0;
}

bool is_void_type(const DataType *d_type) {
  if (d_type->d_kind == D_PRIMITIVE) {
    return d_type->d_union.p_type == P_VOID;
  } else if (d_type->d_kind == D_ARRAY) {
    return is_void_type(d_type->d_union.a_type.element_type);
  } else if (d_type->d_kind == D_STRUCT) {
    return false;
  }

  assert(false);
}
