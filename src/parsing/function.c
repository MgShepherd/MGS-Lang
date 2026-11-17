#include "parsing/function.h"

#include "dynamic_array.h"
#include "lexer.h"
#include "parsing/data_type.h"
#include "parsing/statement.h"
#include "parsing/type_definition.h"
#include "parsing/utils.h"
#include <stdio.h>

#define FUNCTION_STATEMENTS_LEN_ESTIMATE 10
#define PARAMETERS_LEN_ESTIMATE 5

unsigned char parse_function(Function *function, ParseState *state);

void function_free(Function *function) {
  data_type_free(&function->return_type);
  statements_free(&function->block.body);
  dyn_array_free(&function->parameters);
  if (function->param_types != NULL) {
    free(function->param_types);
  }
}

unsigned char parse_function(Function *function, ParseState *state) {
  const Token *ident_tok = expect_next(T_IDENTIFIER, state);
  if (ident_tok == NULL) {
    fprintf(stderr, "Failed to get identifier for function\n");
    return 1;
  }
  function->name = ident_tok->item;

  if (expect_next(T_COLON, state) == NULL) {
    fprintf(stderr, "Expected colon after function identifier\n");
    return 1;
  }

  if (expect_next(T_FUNCTION, state) == NULL) {
    fprintf(stderr, "Expected keyword token for function\n");
    return 1;
  }

  if (expect_next(T_LEFT_PAREN, state) == NULL) {
    fprintf(stderr, "Expected opening parenthesis in function\n");
    return 1;
  }

  dyn_array_init(&function->parameters, sizeof(Field), PARAMETERS_LEN_ESTIMATE);
  if (peek_index(state->tokens, state->idx) != T_RIGHT_PAREN && parse_fields(&function->parameters, state) != 0) {
    fprintf(stderr, "Failed to parse parameters\n");
    return 1;
  }

  if (expect_next(T_RIGHT_PAREN, state) == NULL) {
    fprintf(stderr, "Expected closing parenthesis in function\n");
    return 1;
  }

  if (expect_next(T_ARROW, state) == NULL) {
    fprintf(stderr, "Expected arrow in function\n");
    return 1;
  }

  if (parse_data_type(&function->return_type, state) != 0) {
    fprintf(stderr, "Unable to parse return type for function\n");
    return 1;
  }

  if (expect_next(T_LEFT_CURLY, state) == NULL) {
    fprintf(stderr, "Expected opening curly brace before function body\n");
    return 1;
  }

  dyn_array_init(&function->block.body, sizeof(Statement), FUNCTION_STATEMENTS_LEN_ESTIMATE);
  if (parse_statements(&function->block.body, state, T_RIGHT_CURLY) != 0) {
    fprintf(stderr, "Failed to process function body\n");
    return 1;
  }

  if (expect_next(T_RIGHT_CURLY, state) == NULL) {
    fprintf(stderr, "Expected closing curly brace after function body\n");
    return 1;
  }

  return 0;
}
