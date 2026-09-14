#include "lexer.h"
#include "dynamic_array.h"
#include "utils.h"

#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TOKEN_ARRAY_LEN_FACTOR 0.6

unsigned char get_next_token(Token *tok, const char *input, size_t *i);
unsigned char process_next_token(Token *token, const char *input, size_t input_len, size_t *idx);
unsigned char process_word_token(Token *token, const char *input, size_t input_len, size_t *idx);
unsigned char process_number_token(Token *token, const char *input, size_t input_len, size_t *idx);
unsigned char process_symbol_token(Token *token, const char *input, size_t input_len, size_t *idx);

#define X(N)                                                                                                           \
  case N:                                                                                                              \
    return #N;

const char *t_type_to_string(TokenType t) {
  switch (t) {
    TOKEN_TYPES
  default:
    return "unknown";
  }
}
#undef X

typedef struct {
  const char *key;
  TokenType value;
} TokenMapping;

unsigned char lexer_process_tokens(Tokens *tokens, const char *data) {
  assert(data != NULL);

  tokens->elements = NULL;
  const size_t data_len = strlen(data);
  if (data_len == 0) {
    return 0;
  }

  size_t capacity = data_len * TOKEN_ARRAY_LEN_FACTOR;
  // Guard against getting 0 capacity when token input size is very small
  if (capacity == 0) {
    capacity = 1;
  }

  dyn_array_init(tokens, sizeof(Token), capacity);

  size_t idx = 0;
  while (idx < data_len) {
    Token tok;
    if (process_next_token(&tok, data, data_len, &idx) != 0) {
      fprintf(stderr, "Failed to process token starting from: %c\n", data[idx]);
      return 1;
    }

    if (tok.t_type == T_NONE) {
      continue;
    }

    dyn_array_insert(tokens, tok);
  }

  return 0;
}

unsigned char process_next_token(Token *token, const char *input, size_t input_len, size_t *idx) {
  assert(*idx < input_len);
  token->t_type = T_NONE;

  while (*idx < input_len && isspace(input[*idx])) {
    *idx += 1;
  }

  if (*idx == input_len) {
    return 0;
  }

  if (isalpha(input[*idx]) || input[*idx] == '_') {
    return process_word_token(token, input, input_len, idx);
  }

  if (isdigit(input[*idx])) {
    return process_number_token(token, input, input_len, idx);
  }

  return process_symbol_token(token, input, input_len, idx);
}

unsigned char process_word_token(Token *token, const char *input, size_t input_len, size_t *idx) {
  static const TokenMapping keyword_mappings[] = {
      {.key = "func", .value = T_FUNCTION}, {.key = "return", .value = T_RETURN}, {.key = "var", .value = T_VAR},
      {.key = "i32", .value = T_I32},       {.key = "bool", .value = T_BOOL},     {.key = "true", .value = T_TRUE},
      {.key = "false", .value = T_FALSE},   {.key = "if", .value = T_IF},         {.key = "else", .value = T_ELSE},
  };

  const size_t tok_start = *idx;
  *idx += 1;

  while (*idx < input_len && (isalnum(input[*idx]) || input[*idx] == '_')) {
    *idx += 1;
  }

  char *token_str = string_slice(input, tok_start, *idx);
  if (token_str == NULL) {
    return 1;
  }
  token->item = token_str;

  for (size_t i = 0; i < sizeof(keyword_mappings) / sizeof(TokenMapping); i++) {
    if (strcmp(keyword_mappings[i].key, token_str) == 0) {
      token->t_type = keyword_mappings[i].value;
      return 0;
    }
  }

  token->t_type = T_IDENTIFIER;
  return 0;
}

unsigned char process_number_token(Token *token, const char *input, size_t input_len, size_t *idx) {
  const size_t tok_start = *idx;
  *idx += 1;

  while (*idx < input_len && isdigit(input[*idx])) {
    *idx += 1;
  }

  char *token_str = string_slice(input, tok_start, *idx);
  if (token_str == NULL) {
    return 1;
  }
  token->item = token_str;
  token->t_type = T_NUMERIC_LIT;

  return 0;
}

unsigned char process_symbol_token(Token *token, const char *input, size_t input_len, size_t *idx) {
  switch (input[*idx]) {
  case '-':
    if (*idx + 1 < input_len && input[*idx + 1] == '>') {
      *idx += 1;
      token->t_type = T_ARROW;
      break;
    }
    token->t_type = T_MINUS;
    break;
  case '<':
    if (*idx + 1 < input_len && input[*idx + 1] == '=') {
      *idx += 1;
      token->t_type = T_LTE;
      break;
    }
    token->t_type = T_LT;
    break;
  case '>':
    if (*idx + 1 < input_len && input[*idx + 1] == '=') {
      *idx += 1;
      token->t_type = T_GTE;
      break;
    }
    token->t_type = T_GT;
    break;
  case '(':
    token->t_type = T_LEFT_PAREN;
    break;
  case ')':
    token->t_type = T_RIGHT_PAREN;
    break;
  case '{':
    token->t_type = T_LEFT_CURLY;
    break;
  case '}':
    token->t_type = T_RIGHT_CURLY;
    break;
  case '=':
    token->t_type = T_EQUALS;
    break;
  case ':':
    token->t_type = T_COLON;
    break;
  case '+':
    token->t_type = T_PLUS;
    break;
  case ';':
    token->t_type = T_SEMI;
    break;
  default:
    fprintf(stderr, "Unknown symbol token: %c\n", input[*idx]);
    return 1;
  }

  token->item = NULL;
  *idx += 1;
  return 0;
}
