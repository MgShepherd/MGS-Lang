#ifndef _PARSING_TYPE_DEFINITION_H_
#define _PARSING_TYPE_DEFINITION_H_

#include "parsing/type.h"

unsigned char parse_type_definition(TypeDefinition *type_def, const Tokens *tokens, size_t *idx);

unsigned char parse_fields(Fields *fields, const Tokens *tokens, size_t *idx);

#endif // _PARSING_TYPE_DEFINITION_H_
