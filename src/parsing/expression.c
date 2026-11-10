#include "parsing/expression.h"
#include "dynamic_array.h"
#include "lexer.h"
#include "parsing/data_type.h"
#include "parsing/type.h"
#include "parsing/utils.h"
#include "utils.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>

#define PARAMETER_LIST_LEN_ESTIMATE 5
#define STRUCT_FIELDS_ESTIMATE 5
#define ARRAY_LEN_ESTIMATE 5

unsigned char parse_terminal_expr(TerminalExpr *term, const Tokens *tokens, size_t *idx);
unsigned char parse_terminal_literal(TerminalLiteral *term_literal, const Tokens *tokens, size_t *idx);
unsigned char parse_func_call(FunctionCall *func_call, const Tokens *tokens, size_t *idx);
unsigned char parse_expression_list(Expressions *expressions, const Tokens *tokens, size_t *idx,
                                    const TokenType list_end_type);
unsigned char parse_field_assignments(FieldAssignments *field_assignments, const Tokens *tokens, size_t *idx);
unsigned char parse_struct_literal(StructLiteral *struct_lit, const Tokens *tokens, size_t *idx);
unsigned char parse_array_literal(ArrayLiteral *array_lit, const Tokens *tokens, size_t *idx);
unsigned char parse_array_access(ArrayAccess *array_access, const Tokens *tokens, size_t *idx);

char *get_terminal_name(const Terminal *term) {
  static const char *CALL_PREFIX = "call_";

  char *buffer = NULL;
  switch (term->t_type) {
  case TERM_IDENTIFIER:
    const TerminalIdentifier *ident = &term->t_union.ident;
    // plus 1 to account for null terminator
    buffer = malloc(sizeof(char) * strlen(ident->name) + 1);
    strcpy(buffer, ident->name);
    break;
  case TERM_FUNC_CALL:
    const FunctionCall *func_call = &term->t_union.func_call;
    const size_t name_len = strlen(CALL_PREFIX) + strlen(func_call->name->item) + 1;
    buffer = malloc(sizeof(char) * name_len);
    const size_t result = snprintf(buffer, name_len, "%s%s", CALL_PREFIX, func_call->name->item);
    assert(result > 0);
    break;
  case TERM_LITERAL:
    const TerminalLiteral *lit = &term->t_union.lit;
    // plus 1 to account for null terminator
    buffer = malloc(sizeof(char) * strlen(lit->tok->item) + 1);
    strcpy(buffer, lit->tok->item);
    break;
  case TERM_STRUCT_ACCESS:
    const StructAccess *access = &term->t_union.struct_access;
    buffer = join_strings(access->field_names, access->num_field_nestings, '.');
    break;
  case TERM_STRUCT_LITERAL:
    const StructLiteral *struct_lit = &term->t_union.struct_literal;
    buffer = malloc(sizeof(char) * strlen(struct_lit->d_type->d_union.s_type.name));
    break;
  case TERM_ARRAY_LITERAL:
    const char *array_name = "ARRAY";
    buffer = malloc(sizeof(char) * strlen(array_name));
    strcpy(buffer, array_name);
    break;
  case TERM_ARRAY_ACCESS:
    const char *array_access_name = "ARRAY_ACCESS";
    buffer = malloc(sizeof(char) * strlen(array_access_name));
    strcpy(buffer, array_access_name);
    break;
  case T_NONE:
    assert(false);
  }

  assert(buffer != NULL);
  return buffer;
}

unsigned char parse_expression(Expression *expression, const Tokens *tokens, size_t *idx) {
  if (*idx >= tokens->count) {
    fprintf(stderr, "Attempted to get value token but reached end of input\n");
    return 1;
  }

  TerminalExpr term;
  if (parse_terminal_expr(&term, tokens, idx) != 0) {
    return 1;
  }

  const OperatorType op = *idx >= tokens->count ? O_NONE : tok_to_op_type(tokens->elements[*idx].t_type);
  if (op == O_NONE) {
    expression->e_type = E_TERMINAL;
    expression->e_union.term = term;
    return 0;
  }
  *idx += 1;

  Expression rhs;
  if (parse_expression(&rhs, tokens, idx) != 0) {
    return 1;
  }

  Expression *rhs_ptr = malloc(sizeof(Expression));
  if (rhs_ptr == NULL) {
    fprintf(stderr, "Failed to allocate memory for Right hand side of expression\n");
    return 1;
  }
  *rhs_ptr = rhs;

  CompoundExpr comp = {
      .lhs = term,
      .op = op,
      .rhs = rhs_ptr,
  };

  expression->e_type = E_COMPOUND;
  expression->e_union.comp = comp;
  return 0;
}

unsigned char parse_terminal_identifier(TerminalIdentifier *term_ident, const Tokens *tokens, size_t *idx) {
  const Token *next = expect_next(T_IDENTIFIER, tokens, idx);
  if (next == NULL) {
    fprintf(stderr, "Expected identifier for terminal expression\n");
    return 1;
  }

  term_ident->name = next->item;
  term_ident->scope = NULL;
  return 0;
}

void expression_free(Expression *expression) {
  switch (expression->e_type) {
  case E_COMPOUND:
    expression_free(expression->e_union.comp.rhs);
    free(expression->e_union.comp.rhs);
    break;
  case E_TERMINAL:
    if (expression->e_union.term.item.t_type == TERM_FUNC_CALL) {
      FunctionCall *func_call = &expression->e_union.term.item.t_union.func_call;
      for (size_t i = 0; i < func_call->parameters.count; i++) {
        expression_free(&func_call->parameters.elements[i]);
      }
      dyn_array_free(&expression->e_union.term.item.t_union.func_call.parameters);
    } else if (expression->e_union.term.item.t_type == TERM_STRUCT_ACCESS) {
      StructAccess *struct_access = &expression->e_union.term.item.t_union.struct_access;
      if (struct_access->field_names != NULL) {
        free(struct_access->field_names);
      }

      if (struct_access->field_idxs != NULL) {
        free(struct_access->field_idxs);
      }
    } else if (expression->e_union.term.item.t_type == TERM_ARRAY_LITERAL) {
      ArrayLiteral *array_lit = &expression->e_union.term.item.t_union.array_literal;
      for (size_t i = 0; i < array_lit->items.count; i++) {
        expression_free(&array_lit->items.elements[i]);
      }
      dyn_array_free(&array_lit->items);
    }
    break;
  default:
    break;
  }
}

// TODO: We end up parsing the same tokens multiple times when checking ahead for the terminal type, can we improve
// this
unsigned char parse_terminal_expr(TerminalExpr *term, const Tokens *tokens, size_t *idx) {
  const Token *next = &tokens->elements[*idx];
  term->item.t_type = TERM_NONE;

  if (next->t_type == T_PLUS || next->t_type == T_MINUS) {
    term->sign = next;
    next = &tokens->elements[(*idx)++];
  } else {
    term->sign = NULL;
  }

  if (next->t_type == T_IDENTIFIER) {
    const TokenType after_type = peek_index(tokens, *idx + 1);
    if (after_type == T_LEFT_PAREN) {
      if (parse_func_call(&term->item.t_union.func_call, tokens, idx) != 0) {
        return 1;
      }
      term->item.t_type = TERM_FUNC_CALL;
      return 0;
    }

    if (after_type == T_DOT) {
      if (parse_struct_access(&term->item.t_union.struct_access, tokens, idx) != 0) {
        return 1;
      }
      term->item.t_type = TERM_STRUCT_ACCESS;
      return 0;
    }

    if (after_type == T_LEFT_SQUARE) {
      if (parse_array_access(&term->item.t_union.array_access, tokens, idx) != 0) {
        return 1;
      }
      term->item.t_type = TERM_ARRAY_ACCESS;
      return 0;
    }

    if (parse_terminal_identifier(&term->item.t_union.ident, tokens, idx) != 0) {
      return 1;
    }
    term->item.t_type = TERM_IDENTIFIER;
    return 0;
  } else if (next->t_type == T_LEFT_CURLY) {
    if (parse_struct_literal(&term->item.t_union.struct_literal, tokens, idx) != 0) {
      return 1;
    }
    term->item.t_type = TERM_STRUCT_LITERAL;
    return 0;
  } else if (next->t_type == T_LEFT_SQUARE) {
    if (parse_array_literal(&term->item.t_union.array_literal, tokens, idx) != 0) {
      return 1;
    }
    term->item.t_type = TERM_ARRAY_LITERAL;
    return 0;
  }

  if (parse_terminal_literal(&term->item.t_union.lit, tokens, idx) != 0) {
    return 1;
  }
  term->item.t_type = TERM_LITERAL;
  return 0;
}

unsigned char parse_expression_list(Expressions *expressions, const Tokens *tokens, size_t *idx,
                                    const TokenType list_end_type) {
  if (peek_index(tokens, *idx) == list_end_type) {
    return 0;
  }

  while (true) {
    Expression expression;
    if (parse_expression(&expression, tokens, idx) != 0) {
      fprintf(stderr, "Failed to parse expression for function parameter\n");
      return 1;
    }

    dyn_array_insert(expressions, expression);

    if (peek_index(tokens, *idx) != T_COMMA) {
      return 0;
    }
    *idx += 1;
  }

  return 0;
}

unsigned char parse_field_assignments(FieldAssignments *field_assignments, const Tokens *tokens, size_t *idx) {
  if (peek_index(tokens, *idx) == T_RIGHT_CURLY) {
    return 0;
  }

  while (true) {
    FieldAssignment assignment;
    const Token *ident_tok = expect_next(T_IDENTIFIER, tokens, idx);
    if (ident_tok == NULL) {
      fprintf(stderr, "Expected identifier for field name\n");
      return 1;
    }
    assert(ident_tok->item != NULL);
    assignment.name = ident_tok->item;

    if (expect_next(T_EQUALS, tokens, idx) == NULL) {
      fprintf(stderr, "Expected '=' for field assignment\n");
      return 1;
    }

    if (parse_expression(&assignment.expression, tokens, idx) != 0) {
      fprintf(stderr, "Failed to parse expression for field assignment\n");
      return 1;
    }

    dyn_array_insert(field_assignments, assignment);

    if (peek_index(tokens, *idx) != T_COMMA) {
      return 0;
    }
    *idx += 1;
  }

  return 0;
}

unsigned char parse_terminal_literal(TerminalLiteral *term_literal, const Tokens *tokens, size_t *idx) {
  static const TokenType LITERAL_TOKEN_TYPES[] = {T_NUMERIC_LIT, T_TRUE, T_FALSE};
  static const size_t LITERAL_TOKEN_LEN = sizeof(LITERAL_TOKEN_TYPES) / sizeof(TokenType);

  const Token *next = &tokens->elements[(*idx)++];

  for (size_t i = 0; i < LITERAL_TOKEN_LEN; i++) {
    if (next->t_type == LITERAL_TOKEN_TYPES[i]) {
      term_literal->tok = next;
      return 0;
    }
  }

  fprintf(stderr, "Token type %s is not a valid terminal literal\n", t_type_to_string(next->t_type));
  return 1;
}

unsigned char parse_func_call(FunctionCall *func_call, const Tokens *tokens, size_t *idx) {
  func_call->name = expect_next(T_IDENTIFIER, tokens, idx);
  if (func_call->name == NULL) {
    fprintf(stderr, "Expected identifier for start of function call\n");
    return 1;
  }

  if (expect_next(T_LEFT_PAREN, tokens, idx) == NULL) {
    fprintf(stderr, "Expected '(' for start of parameter list\n");
    return 1;
  }

  dyn_array_init(&func_call->parameters, sizeof(Expression), PARAMETER_LIST_LEN_ESTIMATE);
  if (parse_expression_list(&func_call->parameters, tokens, idx, T_RIGHT_PAREN) != 0) {
    fprintf(stderr, "Failed to process parameter list for function\n");
    return 1;
  }

  if (expect_next(T_RIGHT_PAREN, tokens, idx) == NULL) {
    fprintf(stderr, "Expected ')' for end of parameter list\n");
    return 1;
  }

  return 0;
}

unsigned char parse_struct_literal(StructLiteral *struct_lit, const Tokens *tokens, size_t *idx) {
  if (expect_next(T_LEFT_CURLY, tokens, idx) == NULL) {
    fprintf(stderr, "Expected '{' for start of struct literal, got %s\n",
            t_type_to_string(tokens->elements[*idx].t_type));
    return 1;
  }

  dyn_array_init(&struct_lit->field_assignments, sizeof(FieldAssignment), STRUCT_FIELDS_ESTIMATE);
  if (parse_field_assignments(&struct_lit->field_assignments, tokens, idx) != 0) {
    fprintf(stderr, "Failed to process field assignments for struct\n");
    return 1;
  }

  if (expect_next(T_RIGHT_CURLY, tokens, idx) == NULL) {
    fprintf(stderr, "Expected '}' for end of struct literal, got %s\n",
            t_type_to_string(tokens->elements[*idx].t_type));
    return 1;
  }

  return 0;
}

unsigned char parse_array_literal(ArrayLiteral *array_lit, const Tokens *tokens, size_t *idx) {
  // We can assert on next token as this should already be checked in calling function
  assert(expect_next(T_LEFT_SQUARE, tokens, idx) != NULL);

  dyn_array_init(&array_lit->items, sizeof(Expression), ARRAY_LEN_ESTIMATE);
  if (parse_expression_list(&array_lit->items, tokens, idx, T_RIGHT_SQUARE) != 0) {
    fprintf(stderr, "Failed to process expression list for array literal\n");
    return 1;
  }

  // We can assert on next token since it should already be checked by expression list parsing
  assert(expect_next(T_RIGHT_SQUARE, tokens, idx) != NULL);
  return 0;
}

unsigned char parse_array_access(ArrayAccess *array_access, const Tokens *tokens, size_t *idx) {
  const Token *ident_tok = expect_next(T_IDENTIFIER, tokens, idx);
  // We can assert on the indentifier and '[' tokens since these are already checked in calling function
  assert(ident_tok != NULL);
  assert(expect_next(T_LEFT_SQUARE, tokens, idx) != NULL);

  array_access->term_ident.name = ident_tok->item;

  array_access->idx_tok = expect_next(T_NUMERIC_LIT, tokens, idx);
  if (array_access->idx_tok == NULL) {
    fprintf(stderr, "Expected numeric literal for array access\n");
    return 1;
  }

  if (expect_next(T_RIGHT_SQUARE, tokens, idx) == NULL) {
    fprintf(stderr, "Expected ']' for array access\n");
    return 1;
  }

  return 0;
}

unsigned char parse_struct_access(StructAccess *struct_acc, const Tokens *tokens, size_t *idx) {
  // TODO: Should we change this to be more dynamic - realistically should be enough though
  static const size_t MAX_FIELD_NESTING = 10;

  struct_acc->field_idxs = NULL;
  struct_acc->field_names = NULL;

  const Token *ident_tok = expect_next(T_IDENTIFIER, tokens, idx);
  if (ident_tok == NULL) {
    fprintf(stderr, "Expected identifier for struct field access\n");
    return 1;
  }
  assert(ident_tok->item != NULL);
  struct_acc->term_ident.name = ident_tok->item;

  struct_acc->field_names = malloc(sizeof(char *) * MAX_FIELD_NESTING);
  size_t num_nestings = 0;
  while (peek_index(tokens, *idx) == T_DOT) {
    *idx += 1;

    if (num_nestings >= MAX_FIELD_NESTING - 1) {
      fprintf(stderr, "Maximum field nesting depth of %zu reached\n", MAX_FIELD_NESTING);
      return 1;
    }

    const Token *field_name_tok = expect_next(T_IDENTIFIER, tokens, idx);
    if (field_name_tok == NULL) {
      fprintf(stderr, "Expected identifier for struct field access\n");
      return 1;
    }
    struct_acc->field_names[num_nestings++] = field_name_tok->item;
  }

  if (num_nestings == 0) {
    fprintf(stderr, "Expected at least one '.' for struct field access\n");
    return 1;
  }
  struct_acc->num_field_nestings = num_nestings;

  return 0;
}
