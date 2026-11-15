#include "parsing/statement.h"

#include "dynamic_array.h"
#include "lexer.h"
#include "parsing/data_type.h"
#include "parsing/expression.h"
#include "parsing/type.h"
#include "parsing/utils.h"
#include <stdio.h>

#define IF_STATEMENTS_LEN_ESTIMATE 10
#define NUM_ELSE_IF_BRANCHES_ESTIMATE 3

unsigned char parse_dec_statement(Statement *statement, ParseState *state);
unsigned char parse_assign_statement(Statement *statement, ParseState *state);
unsigned char parse_ret_statement(Statement *statement, ParseState *state);
unsigned char parse_void_statement(Statement *statement, ParseState *state);
unsigned char parse_if_block(Statement *statement, ParseState *state);

unsigned char parse_if_branch(IfBranch *if_branch, ParseState *state);

void statement_free(Statement *statement);

unsigned char parse_statements(Statements *statements, ParseState *state, TokenType exit_token) {
  while (state->idx < state->tokens->count && state->tokens->elements[state->idx].t_type != exit_token) {
    Statement statement;
    unsigned char result = 1;

    switch (state->tokens->elements[state->idx].t_type) {
    case T_IDENTIFIER:
      if (state->tokens->count <= state->idx + 1) {
        fprintf(stderr, "Invalid tokens at end of input\n");
        return result;
      }

      switch (state->tokens->elements[state->idx + 1].t_type) {
      case T_COLON:
        result = parse_dec_statement(&statement, state);
        break;
      case T_EQUALS:
      case T_DOT:
      case T_LEFT_SQUARE:
        result = parse_assign_statement(&statement, state);
        break;
      default:
        result = parse_void_statement(&statement, state);
      }
      break;
    case T_RETURN:
      result = parse_ret_statement(&statement, state);
      break;
    case T_IF:
      result = parse_if_block(&statement, state);
      break;
    default:
      result = parse_void_statement(&statement, state);
    }

    if (result != 0) {
      fprintf(stderr, "Failed to process next statement\n");
      return result;
    }

    dyn_array_insert(statements, statement);
  }

  if (state->idx == state->tokens->count) {
    fprintf(stderr, "Reached end of input before finding block ending token\n");
    return 1;
  }

  return 0;
}

void statements_free(Statements *statements) {
  if (statements->elements == NULL) {
    return;
  }

  for (size_t j = 0; j < statements->count; j++) {
    statement_free(&statements->elements[j]);
  }
  dyn_array_free(statements);
}

void statement_free(Statement *statement) {
  switch (statement->s_type) {
  case S_DECLARATION:
    data_type_free(&statement->s_union.dec.d_type);
    expression_free(&statement->s_union.dec.expr);
    break;
  case S_ASSIGNMENT:
    expression_free(&statement->s_union.assign.expr);
    break;
  case S_RETURN:
    expression_free(&statement->s_union.ret.expr);
    break;
  case S_IF:
    IfBlock *if_block = &statement->s_union.if_block;
    expression_free(&if_block->if_branch.expr);
    statements_free(&if_block->if_branch.block.body);
    for (size_t i = 0; i < if_block->else_if_branches.count; i++) {
      expression_free(&if_block->else_if_branches.elements[i].expr);
      statements_free(&if_block->else_if_branches.elements[i].block.body);
    }
    if (if_block->else_branch.block.body.count != 0) {
      statements_free(&if_block->else_branch.block.body);
    }
    break;
  default:
    break;
  }
}

unsigned char parse_dec_statement(Statement *statement, ParseState *state) {
  statement->s_type = S_NONE;
  DeclarationStatement dec;
  Identifier identifier = {
      .ptr = NULL,
      .scope = NULL,
  };

  const Token *ident = expect_next(T_IDENTIFIER, state);
  if (ident == NULL) {
    fprintf(stderr, "Failed to read identifier for declaration statement\n");
    return 1;
  }
  assert(ident->item != NULL);
  identifier.name = ident->item;

  if (expect_next(T_COLON, state) == NULL) {
    fprintf(stderr, "Failed to read colon for declaration statement\n");
    return 1;
  }

  if (parse_data_type(&dec.d_type, state) != 0) {
    fprintf(stderr, "Failed to parse datatype for declaration statement\n");
    return 1;
  }
  identifier.d_type = dec.d_type;

  if (expect_next(T_EQUALS, state) == NULL) {
    fprintf(stderr, "Failed to read equals for declaration statement\n");
    return 1;
  }

  if (parse_expression(&dec.expr, state) != 0) {
    fprintf(stderr, "Failed to parse expression for declaration statement\n");
    return 1;
  }

  if (expect_next(T_SEMI, state) == NULL) {
    fprintf(stderr, "Failed to read semicolon for declaration statement\n");
    return 1;
  }

  dyn_array_insert(&state->identifiers, identifier);
  dec.ident_idx = state->identifiers.count - 1;

  statement->s_type = S_DECLARATION;
  statement->s_union.dec = dec;

  return 0;
}

unsigned char parse_assign_statement(Statement *statement, ParseState *state) {
  statement->s_type = S_NONE;
  AssignmentStatement assign;
  assign.a_output.a_type = A_NONE;

  const TokenType next_type = peek_index(state->tokens, state->idx + 1);

  if (next_type == T_EQUALS) {
    if (parse_identifier_key(&assign.a_output.a_union.ident_key, state) != 0) {
      fprintf(stderr, "Failed to process identifier name for assignment statement\n");
      return 1;
    }
    assign.a_output.a_type = A_IDENTIFIER;
  } else if (next_type == T_DOT) {
    assign.a_output.a_type = A_STRUCT_FIELD;
    if (parse_struct_access(&assign.a_output.a_union.struct_field, state) != 0) {
      fprintf(stderr, "Failed to read struct access for assignment statement\n");
      return 1;
    }
  } else if (next_type == T_LEFT_SQUARE) {
    assign.a_output.a_type = A_ARRAY_INDEX;
    if (parse_array_access(&assign.a_output.a_union.array_index, state) != 0) {
      fprintf(stderr, "Failed to read array access for assignment statement\n");
      return 1;
    }
  } else {
    assert(false);
  }

  if (expect_next(T_EQUALS, state) == NULL) {
    fprintf(stderr, "Failed to read equals for assignment statement\n");
    return 1;
  }

  if (parse_expression(&assign.expr, state) != 0) {
    fprintf(stderr, "Failed to parse expression for assignment statement\n");
    return 1;
  }

  if (expect_next(T_SEMI, state) == NULL) {
    fprintf(stderr, "Failed to read semicolon for assignment statement\n");
    return 1;
  }

  statement->s_type = S_ASSIGNMENT;
  statement->s_union.assign = assign;
  return 0;
}

unsigned char parse_ret_statement(Statement *statement, ParseState *state) {
  statement->s_type = S_NONE;
  ReturnStatement ret;

  if (expect_next(T_RETURN, state) == NULL) {
    fprintf(stderr, "Expected keyword token for return statement\n");
    return 1;
  }

  if (parse_expression(&ret.expr, state) != 0) {
    fprintf(stderr, "Failed to parse expression for return statement\n");
    return 1;
  }

  if (expect_next(T_SEMI, state) == NULL) {
    fprintf(stderr, "Failed to read semicolon for return statement\n");
    return 1;
  }

  statement->s_type = S_RETURN;
  statement->s_union.ret = ret;

  return 0;
}

unsigned char parse_void_statement(Statement *statement, ParseState *state) {
  statement->s_type = S_NONE;
  VoidStatement void_s;

  if (parse_expression(&void_s.expr, state) != 0) {
    fprintf(stderr, "Expected valid expression for void statement\n");
    return 1;
  }

  if (expect_next(T_SEMI, state) == NULL) {
    fprintf(stderr, "Expected ';' at end of void statement\n");
    return 1;
  }

  statement->s_type = S_VOID;
  statement->s_union.void_s = void_s;

  return 0;
}

unsigned char parse_if_block(Statement *statement, ParseState *state) {
  statement->s_type = S_NONE;

  if (parse_if_branch(&statement->s_union.if_block.if_branch, state) != 0) {
    return 1;
  }

  statement->s_type = S_IF;
  statement->s_union.if_block.else_branch.block.body.count = 0;
  dyn_array_init(&statement->s_union.if_block.else_if_branches, sizeof(IfBranch), NUM_ELSE_IF_BRANCHES_ESTIMATE);

  while (peek_index(state->tokens, state->idx) == T_ELSE && peek_index(state->tokens, state->idx + 1) == T_IF) {
    state->idx += 1;
    IfBranch else_if;
    if (parse_if_branch(&else_if, state) != 0) {
      return 1;
    }

    dyn_array_insert(&statement->s_union.if_block.else_if_branches, else_if);
  }

  if (expect_next(T_ELSE, state) == NULL) {
    return 0;
  }

  // TODO: Can we move the logic for processing a block to a common functions - used for functions, if and else
  if (expect_next(T_LEFT_CURLY, state) == NULL) {
    fprintf(stderr, "Expected { after else keyword\n");
    return 1;
  }

  IfBranch else_branch;
  else_branch.expr.e_type = E_NONE;
  dyn_array_init(&else_branch.block.body, sizeof(Statement), IF_STATEMENTS_LEN_ESTIMATE);

  if (parse_statements(&else_branch.block.body, state, T_RIGHT_CURLY) != 0) {
    fprintf(stderr, "Failed to parse statements for else body\n");
    return 1;
  }

  if (expect_next(T_RIGHT_CURLY, state) == NULL) {
    fprintf(stderr, "Expected closing curly brace after else statement body\n");
    return 1;
  }

  statement->s_union.if_block.else_branch = else_branch;

  return 0;
}

unsigned char parse_if_branch(IfBranch *if_branch, ParseState *state) {
  if (expect_next(T_IF, state) == NULL) {
    fprintf(stderr, "Expected if keyword for if statment\n");
    return 1;
  }

  if (parse_expression(&if_branch->expr, state) != 0) {
    fprintf(stderr, "Failed to parse expression for if statement\n");
    return 1;
  }

  if (expect_next(T_LEFT_CURLY, state) == NULL) {
    fprintf(stderr, "Expected { after if condition\n");
    return 1;
  }

  Statements statements;
  dyn_array_init(&statements, sizeof(Statement), IF_STATEMENTS_LEN_ESTIMATE);

  if (parse_statements(&statements, state, T_RIGHT_CURLY) != 0) {
    fprintf(stderr, "Failed to process if statement body\n");
    return 1;
  }
  if_branch->block.body = statements;

  if (expect_next(T_RIGHT_CURLY, state) == NULL) {
    fprintf(stderr, "Expected closing curly brace after if statement body\n");
    return 1;
  }

  return 0;
}
