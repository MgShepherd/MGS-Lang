#ifndef _PARSING_STATEMENT_H_
#define _PARSING_STATEMENT_H_

#include "lexer.h"
#include "parsing/type.h"
#include <stdlib.h>

/*
 * Will parse a series of statements either until a token of type exit_token is reached
 * Starts processing elements from the provided idx and will update the idx pointer to end up at end of processed
 * elements Will output the parsed statements into the statements parameter Assumes statements already has an
 * allocated element array, with a count and capacity Will error if exit_token is not found before reaching the end of
 * the input Returns 0 on success, 1 if there was a failure
 */
unsigned char parse_statements(Statements *statements, const Tokens *tokens, size_t *idx, TokenType exit_token);

/*
 * Frees statement array and all associated data
 */
void statements_free(Statements *statements);

#endif // _PARSING_STATEMENT_H_
