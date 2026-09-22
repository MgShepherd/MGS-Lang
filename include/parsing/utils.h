#ifndef _PARSING_UTILS_H_
#define _PARSING_UTILS_H_

#include "lexer.h"

/*
 * expect_next will read the next token and check it matches the expected token type
 * If matches will return the token pointer, otherwise will return NULL
 * Will update the idx pointer to point at the next token when valid
 */
const Token *expect_next(TokenType expected, const Tokens *tokens, size_t *idx);

/*
 * peek_index will retrieve the type of element from the tokens array at a given index
 * Will return T_NONE if index is out of bounds
 */
TokenType peek_index(const Tokens *tokens, size_t idx);

#endif // _PARSING_UTILS_H_
