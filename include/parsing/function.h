#ifndef _PARSING_FUNCTION_H_
#define _PARSING_FUNCTION_H_

#include "parsing/type.h"

unsigned char parse_function(Function *function, const Tokens *tokens, size_t *idx);

void function_free(Function *function);

#endif // _PARSING_FUNCTION_H_
