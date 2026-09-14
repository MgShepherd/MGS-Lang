#ifndef _PARSING_STATEMENT_H_
#define _PARSING_STATEMENT_H_

#include "lexer.h"
#include "parsing/expression.h"
#include <stdlib.h>

typedef struct Statement Statement;

typedef struct {
  Statement *elements;
  size_t count;
  size_t capacity;
} Statements;

typedef struct {
  const char *lhs;
  bool variable;
  DataType d_type;
  Expression expr;
} DeclarationStatement;

typedef struct {
  const char *lhs;
  Expression expr;
  // d_type will be populated as part of semantic analysis
  DataType d_type;
} AssignmentStatement;

typedef struct {
  Expression expr;
  // d_type will be populated as part of semantic analysis
  DataType d_type;
} ReturnStatement;

typedef struct {
  Expression expr;
  Statements body;
  // else_body will have NULL elements and 0 count if no else provided
  Statements else_body;
} IfStatement;

typedef union {
  DeclarationStatement dec;
  ReturnStatement ret;
  AssignmentStatement assign;
  IfStatement if_cond;
} StatementUnion;

struct Statement {
  StatementType s_type;
  StatementUnion s_union;
};

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
