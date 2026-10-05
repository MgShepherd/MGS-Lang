#ifndef _PARSING_FUNCTION_H_
#define _PARSING_FUNCTION_H_

#include "parsing/statement.h"
#include "parsing/type.h"

typedef struct {
  const char *name;
  DataType d_type;
} Parameter;

typedef struct {
  Parameter *elements;
  size_t count;
  size_t capacity;
} Parameters;

typedef struct {
  const char *name;
  DataType return_type;
  Statements statements;
  Parameters parameters;
} Function;

typedef struct {
  Function *elements;
  size_t count;
  size_t capacity;
} Functions;

/*
 * Will parse a series of functions either until end of input to build a program
 * Starts processing from the first element in elements
 * Will output the parsed statements into the functions parameter
 * Assumes functions already has an allocated element array, with a count and capacity
 * Returns 0 on success, 1 if there was a failure
 */
unsigned char parse_functions(Functions *functions, const Tokens *tokens);

/*
 * Frees functions array as well as nested statement arrays
 */
void functions_free(Functions *functions);

#endif // _PARSING_FUNCTION_H_
