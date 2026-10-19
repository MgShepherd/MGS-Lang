#ifndef _PARSING_DATATYPE_H_
#define _PARSING_DATATYPE_H_

#include "parsing/type.h"
#include <stdint.h>

typedef struct DataType DataType;

typedef struct {
  DataType *element_type;
  // length will be populated as part of sema
  uint64_t length;
} ArrayType;

typedef struct {
  PrimitiveType p_type;
  ArrayType a_type;
} DataTypeUnion;

struct DataType {
  DataTypeUnion d_union;
  DataTypeKind d_kind;
  bool variable;
};

unsigned char parse_data_type(DataType *data_type, const Tokens *tokens, size_t *idx);

void data_type_free(DataType *data_type);

const char *d_type_to_string(const DataType *data_type);

bool do_data_types_match(const DataType *d_type_1, const DataType *d_type_2);

#endif // _PARSING_DATATYPE_H_
