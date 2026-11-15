#ifndef _PARSING_FUNCTION_H_
#define _PARSING_FUNCTION_H_

#include "parsing/type.h"

unsigned char parse_function(Function *function, ParseState *state);

void function_free(Function *function);

#endif // _PARSING_FUNCTION_H_
