#ifndef _PARSING_EXPRESSION_H_
#define _PARSING_EXPRESSION_H_

#include "lexer.h"
#include "parsing/type.h"
#include <stdint.h>

unsigned char parse_terminal_identifier(TerminalIdentifier *term_ident, const Tokens *tokens, size_t *idx);
unsigned char parse_struct_access(StructAccess *struct_acc, const Tokens *tokens, size_t *idx);
unsigned char parse_array_access(ArrayAccess *array_access, const Tokens *tokens, size_t *idx);

/*
 * parse_expression will attempt to convert the next tokens into an expression and output to provided expression
 * Will update the idx pointer to point at the next token when valid
 * Returns 0 on success, 1 on failure
 */
unsigned char parse_expression(Expression *expression, const Tokens *tokens, size_t *idx);

// TODO: We may want to restructure where some of these types and functions live - more a specific sort of utilty
/**
 * get_terminal_name will get a string representation of a terminal symbols name
 * @param term - A pointer to the terminal to get the name of
 * @return The string representation of the name - returns a copy so must be freed by the caller
 */
char *get_terminal_name(const Terminal *term);

/*
 * Frees a single expression
 */
void expression_free(Expression *expression);

#endif // _PARSING_EXPRESSION_H_
