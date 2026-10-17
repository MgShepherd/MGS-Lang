#ifndef _PARSING_DATATYPE_H_
#define _PARSING_DATATYPE_H_

#include "parsing/type.h"

typedef struct {
  PrimitiveType p_type;
} DataTypeUnion;

typedef struct {
  DataTypeUnion d_union;
  DataTypeKind d_kind;
} DataType;

unsigned char parse_data_type(DataType *data_type, const Tokens *tokens, size_t *idx);

const char *d_type_to_string(const DataType *data_type);

bool do_data_types_match(const DataType *d_type_1, const DataType *d_type_2);

#endif // _PARSING_DATATYPE_H_
