#include "parsing/utils.h"

#include <string.h>

const Token *expect_next(TokenType expected, ParseState *state) {
  if (state->idx >= state->tokens->count) {
    return NULL;
  }

  const Token *next = &state->tokens->elements[state->idx];
  if (next->t_type != expected) {
    return NULL;
  }

  state->idx += 1;
  return next;
}

TokenType peek_index(const Tokens *tokens, size_t idx) {
  if (idx >= tokens->count) {
    return T_NONE;
  }

  return tokens->elements[idx].t_type;
}
