#include "parsing/statement.h"

#include "dynamic_array.h"
#include "parsing/expression.h"
#include "parsing/utils.h"
#include <stdio.h>

#define IF_STATEMENTS_LEN_ESTIMATE 10
#define NUM_ELSE_IF_BRANCHES_ESTIMATE 3

unsigned char parse_dec_statement(Statement *statement, const Tokens *tokens, size_t *idx);
unsigned char parse_assign_statement(Statement *statement, const Tokens *tokens, size_t *idx);
unsigned char parse_ret_statement(Statement *statement, const Tokens *tokens, size_t *idx);
unsigned char parse_void_statement(Statement *statement, const Tokens *tokens, size_t *idx);
unsigned char parse_if_block(Statement *statement, const Tokens *tokens, size_t *idx);

unsigned char parse_if_branch(IfBranch *if_branch, const Tokens *tokens, size_t *idx);

void statement_free(Statement *statement);

unsigned char parse_statements(Statements *statements, const Tokens *tokens, size_t *idx, TokenType exit_token) {
  while (*idx < tokens->count && tokens->elements[*idx].t_type != exit_token) {
    Statement statement;
    unsigned char result = 1;

    switch (tokens->elements[*idx].t_type) {
    case T_IDENTIFIER:
      if (tokens->count <= *idx + 1) {
        fprintf(stderr, "Invalid tokens at end of input\n");
        return result;
      }
      if (tokens->elements[*idx + 1].t_type == T_COLON) {
        result = parse_dec_statement(&statement, tokens, idx);
      } else if (tokens->elements[*idx + 1].t_type == T_EQUALS) {
        result = parse_assign_statement(&statement, tokens, idx);
      } else {
        result = parse_void_statement(&statement, tokens, idx);
      }
      break;
    case T_RETURN:
      result = parse_ret_statement(&statement, tokens, idx);
      break;
    case T_IF:
      result = parse_if_block(&statement, tokens, idx);
      break;
    default:
      result = parse_void_statement(&statement, tokens, idx);
    }

    if (result != 0) {
      fprintf(stderr, "Failed to process next statement\n");
      return result;
    }

    dyn_array_insert(statements, statement);
  }

  if (*idx == tokens->count) {
    fprintf(stderr, "Reached end of input before finding block ending token\n");
    return 1;
  }

  return 0;
}

void statements_free(Statements *statements) {
  for (size_t j = 0; j < statements->count; j++) {
    statement_free(&statements->elements[j]);
  }
  dyn_array_free(statements);
}

void statement_free(Statement *statement) {
  switch (statement->s_type) {
  case S_DECLARATION:
    expression_free(&statement->s_union.dec.expr);
    break;
  case S_ASSIGNMENT:
    expression_free(&statement->s_union.assign.expr);
  case S_RETURN:
    expression_free(&statement->s_union.ret.expr);
    break;
  case S_IF:
    IfBlock *if_block = &statement->s_union.if_block;
    expression_free(&if_block->if_branch.expr);
    statements_free(&if_block->if_branch.body);
    for (size_t i = 0; i < if_block->else_if_branches.count; i++) {
      expression_free(&if_block->else_if_branches.elements[i].expr);
      statements_free(&if_block->else_if_branches.elements[i].body);
    }
    if (if_block->else_body.elements != NULL) {
      statements_free(&if_block->else_body);
    }
    break;
  default:
    break;
  }
}

unsigned char parse_dec_statement(Statement *statement, const Tokens *tokens, size_t *idx) {
  statement->s_type = S_NONE;
  DeclarationStatement dec;

  const Token *ident = expect_next(T_IDENTIFIER, tokens, idx);
  if (ident == NULL) {
    fprintf(stderr, "Failed to read identifier for declaration statement\n");
    return 1;
  }
  assert(ident->item != NULL);
  dec.lhs = ident->item;

  if (expect_next(T_COLON, tokens, idx) == NULL) {
    fprintf(stderr, "Failed to read colon for declaration statement\n");
    return 1;
  }

  dec.variable = expect_next(T_VAR, tokens, idx) != NULL;

  dec.d_type = tok_to_data_type(tokens->elements[(*idx)++].t_type);
  if (dec.d_type == D_NONE) {
    fprintf(stderr, "Failed to parse keyword into datatype\n");
    return 1;
  }

  if (expect_next(T_EQUALS, tokens, idx) == NULL) {
    fprintf(stderr, "Failed to read equals for declaration statement\n");
    return 1;
  }

  if (parse_expression(&dec.expr, tokens, idx) != 0) {
    fprintf(stderr, "Failed to parse expression for declaration statement\n");
    return 1;
  }

  if (expect_next(T_SEMI, tokens, idx) == NULL) {
    fprintf(stderr, "Failed to read semicolon for declaration statement\n");
    return 1;
  }

  statement->s_type = S_DECLARATION;
  statement->s_union.dec = dec;

  return 0;
}

unsigned char parse_assign_statement(Statement *statement, const Tokens *tokens, size_t *idx) {
  statement->s_type = S_NONE;
  AssignmentStatement assign;

  const Token *ident = expect_next(T_IDENTIFIER, tokens, idx);
  if (ident == NULL) {
    fprintf(stderr, "Failed to read identifier for assignment statement\n");
    return 1;
  }
  assert(ident->item != NULL);
  assign.lhs = ident->item;

  if (expect_next(T_EQUALS, tokens, idx) == NULL) {
    fprintf(stderr, "Failed to read equals for assignment statement\n");
    return 1;
  }

  if (parse_expression(&assign.expr, tokens, idx) != 0) {
    fprintf(stderr, "Failed to parse expression for assignment statement\n");
    return 1;
  }

  if (expect_next(T_SEMI, tokens, idx) == NULL) {
    fprintf(stderr, "Failed to read semicolon for assignment statement\n");
    return 1;
  }

  statement->s_type = S_ASSIGNMENT;
  statement->s_union.assign = assign;
  return 0;
}

unsigned char parse_ret_statement(Statement *statement, const Tokens *tokens, size_t *idx) {
  statement->s_type = S_NONE;
  ReturnStatement ret;

  if (expect_next(T_RETURN, tokens, idx) == NULL) {
    fprintf(stderr, "Expected keyword token for return statement\n");
    return 1;
  }

  if (parse_expression(&ret.expr, tokens, idx) != 0) {
    fprintf(stderr, "Failed to parse expression for return statement\n");
    return 1;
  }

  if (expect_next(T_SEMI, tokens, idx) == NULL) {
    fprintf(stderr, "Failed to read semicolon for return statement\n");
    return 1;
  }

  statement->s_type = S_RETURN;
  statement->s_union.ret = ret;

  return 0;
}

unsigned char parse_void_statement(Statement *statement, const Tokens *tokens, size_t *idx) {
  statement->s_type = S_NONE;
  VoidStatement void_s;

  if (parse_expression(&void_s.expr, tokens, idx) != 0) {
    fprintf(stderr, "Expected valid expression for void statement\n");
    return 1;
  }

  if (expect_next(T_SEMI, tokens, idx) == NULL) {
    fprintf(stderr, "Expected ';' at end of void statement\n");
    return 1;
  }

  statement->s_type = S_VOID;
  statement->s_union.void_s = void_s;

  return 0;
}

unsigned char parse_if_block(Statement *statement, const Tokens *tokens, size_t *idx) {
  statement->s_type = S_NONE;

  if (parse_if_branch(&statement->s_union.if_block.if_branch, tokens, idx) != 0) {
    return 1;
  }

  statement->s_type = S_IF;
  statement->s_union.if_block.else_body.elements = NULL;
  statement->s_union.if_block.else_body.count = 0;
  dyn_array_init(&statement->s_union.if_block.else_if_branches, sizeof(IfBranch), NUM_ELSE_IF_BRANCHES_ESTIMATE);

  while (peek_index(tokens, *idx) == T_ELSE && peek_index(tokens, *idx + 1) == T_IF) {
    *idx += 1;
    IfBranch else_if;
    if (parse_if_branch(&else_if, tokens, idx) != 0) {
      return 1;
    }

    dyn_array_insert(&statement->s_union.if_block.else_if_branches, else_if);
  }

  if (expect_next(T_ELSE, tokens, idx) == NULL) {
    return 0;
  }

  // TODO: Can we move the logic for processing a block to a common functions - used for functions, if and else
  if (expect_next(T_LEFT_CURLY, tokens, idx) == NULL) {
    fprintf(stderr, "Expected { after else keyword\n");
    return 1;
  }

  Statements else_body;
  dyn_array_init(&else_body, sizeof(Statement), IF_STATEMENTS_LEN_ESTIMATE);

  if (parse_statements(&else_body, tokens, idx, T_RIGHT_CURLY) != 0) {
    fprintf(stderr, "Failed to parse statements for else body\n");
    return 1;
  }

  if (expect_next(T_RIGHT_CURLY, tokens, idx) == NULL) {
    fprintf(stderr, "Expected closing curly brace after else statement body\n");
    return 1;
  }

  statement->s_union.if_block.else_body = else_body;

  return 0;
}

unsigned char parse_if_branch(IfBranch *if_branch, const Tokens *tokens, size_t *idx) {
  if (expect_next(T_IF, tokens, idx) == NULL) {
    fprintf(stderr, "Expected if keyword for if statment\n");
    return 1;
  }

  if (parse_expression(&if_branch->expr, tokens, idx) != 0) {
    fprintf(stderr, "Failed to parse expression for if statement\n");
    return 1;
  }

  if (expect_next(T_LEFT_CURLY, tokens, idx) == NULL) {
    fprintf(stderr, "Expected { after if condition\n");
    return 1;
  }

  Statements statements;
  dyn_array_init(&statements, sizeof(Statement), IF_STATEMENTS_LEN_ESTIMATE);

  if (parse_statements(&statements, tokens, idx, T_RIGHT_CURLY) != 0) {
    fprintf(stderr, "Failed to process if statement body\n");
    return 1;
  }
  if_branch->body = statements;

  if (expect_next(T_RIGHT_CURLY, tokens, idx) == NULL) {
    fprintf(stderr, "Expected closing curly brace after if statement body\n");
    return 1;
  }

  return 0;
}
