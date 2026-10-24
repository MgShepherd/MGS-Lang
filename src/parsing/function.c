#include "parsing/function.h"

#include "dynamic_array.h"
#include "lexer.h"
#include "parsing/data_type.h"
#include "parsing/statement.h"
#include "parsing/user_type.h"
#include "parsing/utils.h"
#include <stdio.h>

#define FUNCTION_STATEMENTS_LEN_ESTIMATE 10
#define PARAMETERS_LEN_ESTIMATE 5

unsigned char parse_function(Function *function, const Tokens *tokens, size_t *idx);

void function_free(Function *function) {
  statements_free(&function->block.body);
  dyn_array_free(&function->parameters);
}

unsigned char parse_function(Function *function, const Tokens *tokens, size_t *idx) {
  const Token *ident_tok = expect_next(T_IDENTIFIER, tokens, idx);
  if (ident_tok == NULL) {
    fprintf(stderr, "Failed to get identifier for function\n");
    return 1;
  }
  function->name = ident_tok->item;

  if (expect_next(T_COLON, tokens, idx) == NULL) {
    fprintf(stderr, "Expected colon after function identifier\n");
    return 1;
  }

  if (expect_next(T_FUNCTION, tokens, idx) == NULL) {
    fprintf(stderr, "Expected keyword token for function\n");
    return 1;
  }

  if (expect_next(T_LEFT_PAREN, tokens, idx) == NULL) {
    fprintf(stderr, "Expected opening parenthesis in function\n");
    return 1;
  }

  dyn_array_init(&function->parameters, sizeof(Field), PARAMETERS_LEN_ESTIMATE);
  if (peek_index(tokens, *idx) != T_RIGHT_PAREN && parse_fields(&function->parameters, tokens, idx) != 0) {
    fprintf(stderr, "Failed to parse parameters\n");
    return 1;
  }

  if (expect_next(T_RIGHT_PAREN, tokens, idx) == NULL) {
    fprintf(stderr, "Expected closing parenthesis in function\n");
    return 1;
  }

  if (expect_next(T_ARROW, tokens, idx) == NULL) {
    fprintf(stderr, "Expected arrow in function\n");
    return 1;
  }

  if (parse_data_type(&function->return_type, tokens, idx) != 0) {
    fprintf(stderr, "Unable to parse return type for function\n");
    return 1;
  }

  if (expect_next(T_LEFT_CURLY, tokens, idx) == NULL) {
    fprintf(stderr, "Expected opening curly brace before function body\n");
    return 1;
  }

  dyn_array_init(&function->block.body, sizeof(Statement), FUNCTION_STATEMENTS_LEN_ESTIMATE);
  if (parse_statements(&function->block.body, tokens, idx, T_RIGHT_CURLY) != 0) {
    fprintf(stderr, "Failed to process function body\n");
    return 1;
  }

  if (expect_next(T_RIGHT_CURLY, tokens, idx) == NULL) {
    fprintf(stderr, "Expected closing curly brace after function body\n");
    return 1;
  }

  return 0;
}
