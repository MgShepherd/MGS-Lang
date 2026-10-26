#ifndef _PARSING_FUNCTION_H_
#define _PARSING_FUNCTION_H_

#include "parsing/statement.h"
#include "parsing/type_definition.h"

typedef struct {
  const char *name;
  DataType return_type;
  Fields parameters;
  Block block;
} Function;

unsigned char parse_function(Function *function, const Tokens *tokens, size_t *idx);

void function_free(Function *function);

#endif // _PARSING_FUNCTION_H_
