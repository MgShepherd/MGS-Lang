#include "parsing/utils.h"

#include <string.h>

const Token *expect_next(TokenType expected, const Tokens *tokens, size_t *idx) {
  if (*idx >= tokens->count) {
    return NULL;
  }

  const Token *next = &tokens->elements[*idx];
  if (next->t_type != expected) {
    return NULL;
  }

  *idx += 1;
  return next;
}

TokenType peek_index(const Tokens *tokens, size_t idx) {
  if (idx >= tokens->count) {
    return T_NONE;
  }

  return tokens->elements[idx].t_type;
}
