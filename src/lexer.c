#include "lexer.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

const size_t MAX_LINE_LENGTH = 200;
const size_t MAX_TOKENS = 1000;
const size_t MAX_TOKEN_LENGTH = 1000;

const size_t NUM_TYPES = 2;
const char *TYPES[] = {"int", "float"};

const size_t NUM_OPERATORS = 4;
const char *OPERATORS[] = {"+", "-", "*", "/"};

enum L_TokenType { L_TYPE, L_OPERATOR, L_VALUE, L_SEMI };

typedef struct {
  enum L_TokenType type;
  const char *value;
} L_Token;

int convert_line_to_tokens(const char *line, L_Token *tokens,
                           size_t *current_index);
int add_token(const char *token_string, L_Token *tokens, size_t index);
bool array_contains(const char *element, const char **arr, size_t arr_len);

int lexer_process_file(const char *file_path) {
  FILE *file_ptr = fopen(file_path, "r");

  if (!file_ptr) {
    printf("Unable to open file: %s\n", file_path);
    return -1;
  }

  char line_buffer[MAX_LINE_LENGTH];
  L_Token *tokens = malloc(sizeof(L_Token) * MAX_TOKENS);
  size_t current_index = 0;

  while (fgets(line_buffer, sizeof(line_buffer), file_ptr) != NULL) {
    convert_line_to_tokens(line_buffer, tokens, &current_index);
  }

  if (fclose(file_ptr) != 0) {
    printf("There was a problem closing the file\n");
    return -1;
  }

  for (size_t i = 0; i < current_index; i++) {
    printf("%u\n", tokens[i].type);
  }
  return 0;
}

int convert_line_to_tokens(const char *line, L_Token *tokens,
                           size_t *current_index) {
  char current_token[MAX_TOKEN_LENGTH];
  size_t token_index = 0;
  for (size_t i = 0; i < strlen(line); i++) {
    switch (line[i]) {
    case ' ':
      current_token[token_index] = '\0';
      add_token(current_token, tokens, *current_index);
      (*current_index)++;
      token_index = 0;
      break;
    default:
      current_token[token_index++] = line[i];
    }
  }
  return 0;
}

int add_token(const char *token_string, L_Token *tokens, const size_t index) {
  L_Token new_token;
  new_token.value = token_string;

  if (array_contains(token_string, TYPES, NUM_TYPES)) {
    new_token.type = L_TYPE;
  } else if (array_contains(token_string, OPERATORS, NUM_OPERATORS)) {
    new_token.type = L_OPERATOR;
  } else {
    new_token.type = L_VALUE;
  }

  tokens[index] = new_token;
  return 0;
}

bool array_contains(const char *element, const char **arr,
                    const size_t arr_len) {
  for (size_t i = 0; i < arr_len; i++) {
    if (strcmp(element, arr[i]) == 0) {
      return true;
    }
  }
  return false;
}
