#include "parsing/function.h"

#include "dynamic_array.h"
#include "parsing/statement.h"
#include "parsing/type.h"
#include "parsing/utils.h"
#include <stdio.h>

#define FUNCTION_STATEMENTS_LEN_ESTIMATE 10
#define PARAMETERS_LEN_ESTIMATE 5

unsigned char parse_function(Function *function, const Tokens *tokens, size_t *idx);
unsigned char parse_parameters(Parameters *parameters, const Tokens *tokens, size_t *idx);

unsigned char parse_functions(Functions *functions, const Tokens *tokens) {
  size_t idx = 0;
  while (idx < tokens->count) {
    if (tokens->elements[idx].t_type != T_FUNCTION) {
      fprintf(stderr, "Expected func token for function begin, got %s\n",
              t_type_to_string(tokens->elements[idx].t_type));
      return 1;
    }

    Function func;
    if (parse_function(&func, tokens, &idx) != 0) {
      fprintf(stderr, "Failed to parse function\n");
      return 1;
    }

    dyn_array_insert(functions, func);
  }

  return 0;
}

void functions_free(Functions *functions) {
  for (size_t i = 0; i < functions->count; i++) {
    statements_free(&functions->elements[i].statements);
    dyn_array_free(&functions->elements[i].parameters);
  }
  dyn_array_free(functions);
}

unsigned char parse_function(Function *function, const Tokens *tokens, size_t *idx) {
  if (expect_next(T_FUNCTION, tokens, idx) == NULL) {
    fprintf(stderr, "Expected keyword token for function\n");
    return 1;
  }

  const Token *ident_tok = expect_next(T_IDENTIFIER, tokens, idx);
  if (ident_tok == NULL) {
    fprintf(stderr, "Failed to get identifier for function\n");
    return 1;
  }
  function->name = ident_tok->item;

  if (expect_next(T_LEFT_PAREN, tokens, idx) == NULL) {
    fprintf(stderr, "Expected opening parenthesis in function\n");
    return 1;
  }

  dyn_array_init(&function->parameters, sizeof(Parameter), PARAMETERS_LEN_ESTIMATE);
  if (parse_parameters(&function->parameters, tokens, idx) != 0) {
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

  function->return_type = tok_to_data_type(tokens->elements[(*idx)++].t_type);
  if (function->return_type == D_NONE) {
    fprintf(stderr, "Expected return type for function to be valid data type\n");
    return 1;
  }

  if (expect_next(T_LEFT_CURLY, tokens, idx) == NULL) {
    fprintf(stderr, "Expected opening curly brace before function body\n");
    return 1;
  }

  dyn_array_init(&function->statements, sizeof(Statement), FUNCTION_STATEMENTS_LEN_ESTIMATE);
  if (parse_statements(&function->statements, tokens, idx, T_RIGHT_CURLY) != 0) {
    fprintf(stderr, "Failed to process function body\n");
    return 1;
  }

  if (expect_next(T_RIGHT_CURLY, tokens, idx) == NULL) {
    fprintf(stderr, "Expected closing curly brace after function body\n");
    return 1;
  }

  return 0;
}

unsigned char parse_parameters(Parameters *parameters, const Tokens *tokens, size_t *idx) {
  if (peek_index(tokens, *idx) == T_RIGHT_PAREN) {
    return 0;
  }

  while (true) {
    Parameter parameter;
    const Token *ident = expect_next(T_IDENTIFIER, tokens, idx);
    if (ident == NULL) {
      fprintf(stderr, "Expected identifier for parameter value\n");
      return 1;
    }
    assert(ident->item != NULL);
    parameter.term_ident.name = ident->item;

    if (expect_next(T_COLON, tokens, idx) == NULL) {
      fprintf(stderr, "Expected colon after parameter value\n");
      return 1;
    }

    parameter.d_type = tok_to_data_type(tokens->elements[(*idx)++].t_type);
    if (parameter.d_type == D_NONE) {
      fprintf(stderr, "Expected datatype for parameter\n");
      return 1;
    }

    dyn_array_insert(parameters, parameter);

    if (peek_index(tokens, *idx) != T_COMMA) {
      return 0;
    }
    *idx += 1;
  }
}
