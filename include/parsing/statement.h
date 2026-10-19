#ifndef _PARSING_STATEMENT_H_
#define _PARSING_STATEMENT_H_

#include "lexer.h"
#include "parsing/data_type.h"
#include "parsing/expression.h"
#include "parsing/type.h"
#include <stdlib.h>

typedef struct Statement Statement;

typedef struct {
  Statement *elements;
  size_t count;
  size_t capacity;
} Statements;

typedef struct {
  TerminalIdentifier term_ident;
  DataType d_type;
  Expression expr;
} DeclarationStatement;

typedef union {
  TerminalIdentifier term_ident;
  ArrayAccess arr_element;
} AssignmentOutputUnion;

typedef struct {
  AssignmentType a_type;
  AssignmentOutputUnion a_union;
} AssignmentOutput;

typedef struct {
  AssignmentOutput a_output;
  Expression expr;
  // d_type will be populated as part of semantic analysis
  const DataType *d_type;
} AssignmentStatement;

typedef struct {
  Expression expr;
  // d_type will be populated as part of semantic analysis
  const DataType *d_type;
} ReturnStatement;

typedef struct {
  Expression expr;
} VoidStatement;

typedef struct {
  Statements body;
  // has_return will be populated as part of semantic analysis
  bool has_return;
} Block;

typedef struct {
  Expression expr;
  Block block;
} IfBranch;

typedef struct {
  IfBranch *elements;
  size_t count;
  size_t capacity;
} ElseIfBranches;

typedef struct {
  IfBranch if_branch;
  // else_if_branches will have 0 count if no else_if provided
  ElseIfBranches else_if_branches;
  // else_branch will have body with 0 elements if no else provided
  IfBranch else_branch;
} IfBlock;

typedef union {
  DeclarationStatement dec;
  ReturnStatement ret;
  AssignmentStatement assign;
  VoidStatement void_s;
  IfBlock if_block;
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
