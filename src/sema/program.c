#include "sema/program.h"

#include "dynamic_array.h"
#include "parsing/type.h"
#include "sema/data_type.h"
#include "sema/statement.h"
#include "sema/type_definition.h"
#include "sema/utils.h"
#include <stdio.h>
#include <string.h>

#define NUM_SCOPES_ESTIMATE 5

unsigned char analyse_func(SemaState *state, Function *func);
unsigned char analyse_parameters(SemaState *state, Fields *parameters);

unsigned char analyse_program(Program *program) {
  Scopes scopes;
  dyn_array_init(&scopes, sizeof(void *), NUM_SCOPES_ESTIMATE);
  assert(scopes.elements != NULL);

  SemaState state = {
      .program = program,
      .functions = program->functions,
      .type_defs = program->type_defs,
      .scopes = scopes,
  };

  for (size_t i = 0; i < program->type_defs.count; i++) {
    if (analyse_type_definition(&state, &program->type_defs.elements[i]) != 0) {
      free_sema_state(&state);
      return 1;
    }
  }

  bool has_valid_main = false;
  for (size_t i = 0; i < program->functions.count; i++) {
    dyn_array_insert(&state.scopes, &program->functions.elements[i]);
    if (analyse_func(&state, &program->functions.elements[i]) != 0) {
      free_sema_state(&state);
      return 1;
    }
    dyn_array_pop(&state.scopes);

    if (strcmp(program->functions.elements[i].name, "main") == 0) {
      if (!is_integer_type(&program->functions.elements[i].return_type)) {
        fprintf(stderr, "Return type from main function must be an integer type (i8, i16, i32, i64)\n");
        free_sema_state(&state);
        return 1;
      }
      has_valid_main = true;
    }
  }

  if (!has_valid_main) {
    fprintf(stderr, "All programs must have a main function defined\n");
    free_sema_state(&state);
    return 1;
  }

  program->requires_array_type = state.requires_array_type;

  free_sema_state(&state);
  return 0;
}

unsigned char analyse_block(SemaState *state, Block *block, const DataType *func_type) {
  unsigned char result = analyse_statements(state, block, func_type);
  if (result != 0) {
    return result;
  }

  return 0;
}

void free_sema_state(SemaState *state) { dyn_array_free(&state->scopes); }

unsigned char analyse_func(SemaState *state, Function *func) {
  if (analyse_parameters(state, &func->parameters) != 0) {
    return 1;
  }

  if (analyse_block(state, &func->block, &func->return_type) != 0) {
    return 1;
  }

  return analyse_data_type(state, &func->return_type, NULL);
}

unsigned char analyse_parameters(SemaState *state, Fields *parameters) {
  for (size_t i = 0; i < parameters->count; i++) {
    Field *param = &parameters->elements[i];
    Identifier *ident = &state->program->identifiers.elements[param->ident_idx];
    ident->scope = state->scopes.elements[state->scopes.count - 1];

    // Always set array parameters to have -1 length so runtime bounds checks will be added
    if (param->d_type.d_kind == D_ARRAY) {
      param->d_type.d_union.a_type.length = -1;
      ident->d_type.d_union.a_type.length = -1;
    }
  }
  return 0;
}
