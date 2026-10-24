#include "parsing/program.h"

#include "dynamic_array.h"
#include "lexer.h"
#include <assert.h>
#include <stdio.h>

#define FUNCTION_ARRAY_LEN_FACTOR 0.1
#define USER_TYPES_ARRAY_LEN 10

unsigned char parse_program(Program *program, const Tokens *tokens) {
  assert(tokens != NULL && tokens->count > 0);

  size_t capacity = tokens->count * FUNCTION_ARRAY_LEN_FACTOR;
  // Guard against getting 0 capacity when token input size is very small
  if (capacity == 0) {
    capacity = 1;
  }

  dyn_array_init(&program->functions, sizeof(Function), capacity);
  dyn_array_init(&program->user_types, sizeof(UserType), USER_TYPES_ARRAY_LEN);

  size_t idx = 0;
  while (idx < tokens->count) {
    // idx + 2 should contain what type of construct this is (idx should be identifer, idx+1 should be ':')
    if (idx + 2 >= tokens->count) {
      fprintf(stderr, "Invalid global construct\n");
      return 1;
    }

    switch (tokens->elements[idx + 2].t_type) {
    case T_FUNCTION:
      Function func;
      if (parse_function(&func, tokens, &idx) != 0) {
        fprintf(stderr, "Failed to parse function\n");
        return 1;
      }
      dyn_array_insert(&program->functions, func);
      break;
    case T_STRUCT:
      UserType user_type;
      if (parse_user_type(&user_type, tokens, &idx) != 0) {
        fprintf(stderr, "Failed to parse function\n");
        return 1;
      }
      dyn_array_insert(&program->user_types, user_type);
      break;
    default:
      fprintf(stderr, "Unexpected token type: %s for global construct\n",
              t_type_to_string(tokens->elements[idx + 2].t_type));
      return 1;
    }
  }

  return 0;
}

void program_free(Program *program) {
  if (program == NULL) {
    return;
  }

  if (program->functions.elements != NULL) {
    for (size_t i = 0; i < program->functions.count; i++) {
      function_free(&program->functions.elements[i]);
    }
    dyn_array_free(&program->functions);
  }

  dyn_array_free(&program->user_types);
}
