#ifndef _SEMA_EXPRESSION_H_
#define _SEMA_EXPRESSION_H_

#include "sema/program.h"

unsigned char analyse_expression(SemaState *state, Expression *expr, const DataType *expr_type);

#endif // _SEMA_EXPRESSION_H_
