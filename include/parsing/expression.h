#ifndef _PARSING_EXPRESSION_H_
#define _PARSING_EXPRESSION_H_

#include "lexer.h"
#include "parsing/data_type.h"
#include "parsing/type.h"
#include <stdint.h>

typedef struct Expression Expression;
typedef struct FieldAssignment FieldAssignment;
typedef struct TypeDefinition TypeDefinition;

typedef union {
  int8_t i8;
  int16_t i16;
  int32_t i32;
  int64_t i64;
  bool b;
} LiteralUnion;

typedef struct {
  LiteralUnion l_union;
  // d_type will be populated as part of semantic analysis
  const DataType *d_type;
} Literal;

typedef struct {
  Expression *elements;
  size_t count;
  size_t capacity;
} Expressions;

typedef struct {
  const char *name;
  Expressions parameters;
} FunctionCall;

typedef struct {
  const char *name;
  // Will be populated by sema
  const void *scope;
} TerminalIdentifier;

typedef struct {
  const Token *tok;
  // Will be populated by sema
  Literal literal;
} TerminalLiteral;

typedef struct {
  TerminalIdentifier term_ident;
  const char **field_names;
  size_t num_field_nestings;
  // type_def, field_idxs, variable and d_type will be populated as part of semantic analysis
  const TypeDefinition *type_def;
  size_t *field_idxs;
  const DataType *d_type;
  // TODO: Is there a better way to handle variable for StructAccess - current solution a bit hacky
  bool variable;
} StructAccess;

typedef struct {
  TerminalIdentifier term_ident;
  // TODO: Update this to be able to support nested arrays
  const Token *idx_tok;
  // d_type and idx will be populated as part of semantic analysis
  const DataType *d_type;
  size_t idx;
  bool variable;
} ArrayAccess;

typedef struct {
  FieldAssignment *elements;
  size_t count;
  size_t capacity;
} FieldAssignments;

typedef struct {
  FieldAssignments field_assignments;
  // d_type will be populated as part of semantic analysis
  const DataType *d_type;
} StructLiteral;

typedef struct {
  Expressions items;
  // d_type will be populated as part of semantic analysis
  const DataType *d_type;
} ArrayLiteral;

typedef struct {
  TerminalLiteral lit;
  TerminalIdentifier ident;
  FunctionCall func_call;
  StructLiteral struct_literal;
  StructAccess struct_access;
  ArrayLiteral array_literal;
  ArrayAccess array_access;
} TerminalUnion;

typedef struct {
  TerminalUnion t_union;
  TerminalType t_type;
} Terminal;

typedef struct {
  Terminal item;
  const Token *sign;
} TerminalExpr;

typedef struct {
  TerminalExpr lhs;
  OperatorType op;
  Expression *rhs;
} CompoundExpr;

typedef union {
  TerminalExpr term;
  CompoundExpr comp;
} ExpressionUnion;

struct Expression {
  ExpressionUnion e_union;
  ExpressionType e_type;
};

struct FieldAssignment {
  const char *name;
  Expression expression;
};

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
