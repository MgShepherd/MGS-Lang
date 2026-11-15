#ifndef _PARSING_DATATYPE_H_
#define _PARSING_DATATYPE_H_

#include "parsing/type.h"
#include <stdint.h>

unsigned char parse_data_type(DataType *data_type, ParseState *state);

const char *d_type_to_string(const DataType *data_type);

bool do_data_types_match(const DataType *d_type_1, const DataType *d_type_2);

/**
 * data_type_free will free all allocated memory associated with a data_type.
 * This only currently has an array for array data types, as the element_type must be freed
 * For other data_types, calling this function is still valid, but is just a no-op
 */
void data_type_free(DataType *data_type);

#endif // _PARSING_DATATYPE_H_
