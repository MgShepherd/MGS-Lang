#ifndef _PARSING_PROGRAM_H_
#define _PARSING_PROGRAM_H_

#include "lexer.h"
#include "parsing/function.h"
#include "parsing/type_definition.h"

typedef struct {
  Function *elements;
  size_t count;
  size_t capacity;
} Functions;

typedef struct {
  TypeDefinition *elements;
  size_t count;
  size_t capacity;
} TypeDefinitions;

typedef struct {
  Functions functions;
  TypeDefinitions type_defs;
} Program;

/*
 * parse_program will convert the filled tokens into a Program Syntax Tree
 * Will output result into the program arguement - will overwrite any existing data
 * Allocates program on the heap so will need to be freed - use program_free for this
 * Note: Created program stores pointers to tokens, so tokens should not be freed until program can also be freed
 */
unsigned char parse_program(Program *program, const Tokens *tokens);

/*
 * Frees the memory associated with a program
 */
void program_free(Program *program);

#endif // _PARSING_PROGRAM_H_
