#ifndef _SEMA_DATA_TYPE_H_
#define _SEMA_DATA_TYPE_H_

#include "parsing/data_type.h"
#include "parsing/expression.h"

unsigned char analyse_data_type(DataType *d_type, const Expression *expr);

#endif // _SEMA_DATA_TYPE_H_
