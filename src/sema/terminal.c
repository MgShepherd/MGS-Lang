#include "sema/terminal.h"

#include "parsing/data_type.h"
#include "parsing/type_definition.h"
#include "sema/expression.h"
#include "sema/utils.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>

int field_assignment_compare(const void *item1, const void *item2);

unsigned char analyse_func_call(SemaState *state, const FunctionCall *func_call, const DataType *expr_type) {
  const Function *func = get_function(&state->functions, func_call->name->item);
  if (func == NULL) {
    fprintf(stderr, "Undefined function: %s\n", func_call->name->item);
    return 1;
  }

  if (func_call->parameters.count != func->parameters.count) {
    fprintf(stderr, "Mismatched number of parameters in function call, expected %zu, got %zu\n", func->parameters.count,
            func_call->parameters.count);
    return 1;
  }

  for (size_t i = 0; i < func_call->parameters.count; i++) {
    if (analyse_expression(state, &func_call->parameters.elements[i], &func->parameters.elements[i].d_type) != 0) {
      fprintf(stderr, "Failed to analyse parameter expression\n");
      return 1;
    }
  }

  if (!do_data_types_match(&func->return_type, expr_type)) {
    fprintf(stderr, "Function %s does not return expected type %s\n", func->name, d_type_to_string(expr_type));
    return 1;
  }
  return 0;
}

unsigned char analyse_identifier(SemaState *state, TerminalIdentifier *term_ident, const DataType *expr_type) {
  const Identifier *ident = get_identifier(state, term_ident->name);
  if (ident == NULL) {
    fprintf(stderr, "Undefined variable: %s\n", term_ident->name);
    return 1;
  }

  if (!do_data_types_match(expr_type, ident->d_type)) {
    fprintf(stderr, "Variable %s does not have expected type %s\n", ident->name, d_type_to_string(expr_type));
    return 1;
  }

  term_ident->scope = ident->scope;

  return 0;
}

unsigned char analyse_literal(TerminalLiteral *term_lit, const Token *sign, const DataType *expr_type) {
  switch (term_lit->tok->t_type) {
  case T_NUMERIC_LIT:
    if (!is_integer_type(expr_type)) {
      fprintf(stderr, "Numeric literal %s used in non-numerical expression type: %s\n", term_lit->tok->item,
              d_type_to_string(expr_type));
      return 1;
    }
    const bool is_negative = sign != NULL && sign->t_type == T_MINUS;
    return analyse_numeric_literal(term_lit, is_negative, expr_type);
  case T_TRUE:
  case T_FALSE:
    if (expr_type->d_kind != D_PRIMITIVE || expr_type->d_union.p_type != P_BOOL) {
      fprintf(stderr, "Boolean literal %s used in non-boolean expression type: %s\n", term_lit->tok->item,
              d_type_to_string(expr_type));
      return 1;
    }

    term_lit->literal.d_type = expr_type;
    term_lit->literal.l_union.b = term_lit->tok->t_type == T_TRUE;
    return 0;
  default:
    fprintf(stderr, "Unexpected token type for terminal expression: %s\n", t_type_to_string(term_lit->tok->t_type));
    return 1;
  }

  return 0;
}

unsigned char analyse_array_literal(SemaState *state, ArrayLiteral *arr_lit, const DataType *expr_type) {
  assert(expr_type->d_kind == D_ARRAY);
  arr_lit->d_type = expr_type;

  // TODO: Check array length in type matches amount of elements provided - more error case handling needed in tests
  for (size_t i = 0; i < arr_lit->items.count; i++) {
    if (analyse_expression(state, &arr_lit->items.elements[i], arr_lit->d_type->d_union.a_type.element_type) != 0) {
      fprintf(stderr, "Failed to analyse array item\n");
      return 1;
    }
  }

  return 0;
}

unsigned char analyse_struct_literal(SemaState *state, StructLiteral *struct_lit, const DataType *expr_type) {
  assert(expr_type->d_kind == D_STRUCT);
  struct_lit->d_type = expr_type;

  const TypeDefinition *object_type = get_type_def(&state->type_defs, expr_type->d_union.s_type.name);
  if (object_type == NULL) {
    fprintf(stderr, "Unable to find type definition for provided data type: %s\n", d_type_to_string(expr_type));
    return 1;
  }

  const StructDefinition *struct_def = &object_type->td_union.s_def;

  if (struct_def->fields.count != struct_lit->field_assignments.count) {
    fprintf(stderr, "Expected %zu fields in struct literal, got %zu\n", struct_def->fields.count,
            struct_lit->field_assignments.count);
    return 1;
  }

  const char *provided_fields[struct_def->fields.count];
  size_t num_provided_fields = 0;

  qsort(struct_lit->field_assignments.elements, struct_lit->field_assignments.count, sizeof(FieldAssignment),
        field_assignment_compare);

  for (size_t i = 0; i < struct_lit->field_assignments.count; i++) {
    const Field *field_def = NULL;
    FieldAssignment *current_field = &struct_lit->field_assignments.elements[i];

    for (size_t i = 0; i < struct_def->fields.count; i++) {
      if (strcmp(struct_def->fields.elements[i].term_ident.name, current_field->name) == 0) {
        field_def = &struct_def->fields.elements[i];
      }
    }

    if (field_def == NULL) {
      fprintf(stderr, "Field %s is not present in struct %s\n", current_field->name, struct_def->name);
      return 1;
    }

    for (size_t i = 0; i < num_provided_fields; i++) {
      if (strcmp(provided_fields[i], current_field->name) == 0) {
        fprintf(stderr, "Field %s provided multiple times in struct literal\n", current_field->name);
        return 1;
      }
    }
    provided_fields[num_provided_fields++] = current_field->name;

    if (analyse_expression(state, &current_field->expression, &field_def->d_type) != 0) {
      fprintf(stderr, "Failed to analyse expression for field: %s\n", current_field->name);
      return 1;
    }
  }

  return 0;
}

unsigned char analyse_struct_access(SemaState *state, StructAccess *struct_access, const DataType *expr_type) {
  const Identifier *ident = get_identifier(state, struct_access->term_ident.name);
  if (ident == NULL) {
    fprintf(stderr, "Undefined variable: %s\n", struct_access->field_name);
    return 1;
  }
  assert(ident->d_type != NULL && ident->d_type->d_kind == D_STRUCT);

  const TypeDefinition *type_def = get_type_def(&state->type_defs, ident->d_type->d_union.s_type.name);
  assert(type_def != NULL && type_def->td_kind == TD_STRUCT);
  const StructDefinition *struct_def = &type_def->td_union.s_def;

  const Field *field = NULL;
  for (size_t i = 0; i < struct_def->fields.count; i++) {
    if (strcmp(struct_def->fields.elements[i].term_ident.name, struct_access->field_name) == 0) {
      field = &struct_def->fields.elements[i];
      struct_access->field_idx = i;
      break;
    }
  }

  if (field == NULL) {
    fprintf(stderr, "Undefined field %s for struct %s\n", struct_access->field_name, struct_def->name);
    return 1;
  }

  if (expr_type != NULL && !do_data_types_match(&field->d_type, expr_type)) {
    fprintf(stderr, "Field %s does not have expected type %s\n", struct_access->field_name,
            d_type_to_string(expr_type));
    return 1;
  }

  struct_access->d_type = &field->d_type;
  struct_access->term_ident.scope = ident->scope;
  struct_access->type_def = type_def;
  struct_access->variable = ident->d_type->variable;
  return 0;
}

unsigned char analyse_array_access(SemaState *state, ArrayAccess *arr_access, const DataType *expr_type) {
  const Identifier *ident = get_identifier(state, arr_access->term_ident.name);
  if (ident == NULL) {
    fprintf(stderr, "Undefined variable: %s\n", arr_access->term_ident.name);
    return 1;
  }

  if (ident->d_type->d_kind != D_ARRAY) {
    fprintf(stderr, "Expected variable: %s to be an array type for array access\n", arr_access->term_ident.name);
    return 1;
  }

  const ArrayType *array_type = &ident->d_type->d_union.a_type;
  if (expr_type != NULL && !do_data_types_match(array_type->element_type, expr_type)) {
    fprintf(stderr, "Element type of array %s does not match expected expression type: %s\n",
            arr_access->term_ident.name, d_type_to_string(expr_type));
    return 1;
  }
  long long arr_index;
  if (tok_to_integer(&arr_index, arr_access->index_tok, false) != 0) {
    fprintf(stderr, "Expected integer value for array index\n");
    return 1;
  }

  arr_access->term_ident.scope = ident->scope;
  arr_access->idx = (uint64_t)arr_index;

  if (arr_access->idx >= array_type->length) {
    fprintf(stderr, "Array index %lld out of bounds for array %s\n", arr_access->idx, arr_access->term_ident.name);
    return 1;
  }

  return 0;
}

unsigned char analyse_numeric_literal(TerminalLiteral *term_lit, bool is_negative, const DataType *expr_type) {
  assert(term_lit->tok->t_type == T_NUMERIC_LIT && term_lit->tok->item != NULL);
  term_lit->literal.d_type = expr_type;

  long long int_val;
  if (tok_to_integer(&int_val, term_lit->tok, is_negative) != 0) {
    return 1;
  }

  assert(term_lit->literal.d_type->d_kind == D_PRIMITIVE);

  switch (term_lit->literal.d_type->d_union.p_type) {
  case P_I8:
    if (!is_number_in_range(int_val, INT8_MIN, INT8_MAX)) {
      fprintf(stderr, "Value %lld does not fit in i8 integer\n", int_val);
      return 1;
    }
    term_lit->literal.l_union.i8 = (int8_t)int_val;
    break;
  case P_I16:
    if (!is_number_in_range(int_val, INT16_MIN, INT16_MAX)) {
      fprintf(stderr, "Value %lld does not fit in i16 integer\n", int_val);
      return 1;
    }
    term_lit->literal.l_union.i16 = (int16_t)int_val;
    break;
  case P_I32:
    if (!is_number_in_range(int_val, INT32_MIN, INT32_MAX)) {
      fprintf(stderr, "Value %lld does not fit in i32 integer\n", int_val);
      return 1;
    }
    term_lit->literal.l_union.i32 = (int32_t)int_val;
    break;
  case P_I64:
    term_lit->literal.l_union.i64 = (int64_t)int_val;
    break;
  default:
    assert(false);
  }

  return 0;
}

int field_assignment_compare(const void *item1, const void *item2) {
  const FieldAssignment *field_assign1 = (FieldAssignment *)item1;
  const FieldAssignment *field_assign2 = (FieldAssignment *)item2;
  return strcmp(field_assign1->name, field_assign2->name);
}
