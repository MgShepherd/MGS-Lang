#ifndef _PARSING_DATATYPE_H_
#define _PARSING_DATATYPE_H_

#include "parsing/type.h"
#include <stdint.h>

typedef struct DataType DataType;

typedef struct {
  const char *name;
} StructType;

typedef struct {
  DataType *element_type;
  // length will be populated as part of sema - if length is unknown at compile time, -1 will be used here
  int64_t length;
} ArrayType;

typedef struct {
  PrimitiveType p_type;
  StructType s_type;
  ArrayType a_type;
} DataTypeUnion;

struct DataType {
  DataTypeUnion d_union;
  DataTypeKind d_kind;
  bool variable;
};

unsigned char parse_data_type(DataType *data_type, const Tokens *tokens, size_t *idx);

const char *d_type_to_string(const DataType *data_type);

bool do_data_types_match(const DataType *d_type_1, const DataType *d_type_2);

/**
 * data_type_free will free all allocated memory associated with a data_type.
 * This only currently has an array for array data types, as the element_type must be freed
 * For other data_types, calling this function is still valid, but is just a no-op
 */
void data_type_free(DataType *data_type);

#endif // _PARSING_DATATYPE_H_
