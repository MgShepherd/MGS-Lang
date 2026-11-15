#ifndef _PARSING_TYPE_DEFINITION_H_
#define _PARSING_TYPE_DEFINITION_H_

#include "parsing/type.h"

unsigned char parse_type_definition(TypeDefinition *type_def, ParseState *state);

unsigned char parse_fields(Fields *fields, ParseState *state);

#endif // _PARSING_TYPE_DEFINITION_H_
