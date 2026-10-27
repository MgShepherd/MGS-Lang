#ifndef _SEMA_STATEMENT_H_
#define _SEMA_STATEMENT_H_

#include "sema/program.h"

unsigned char analyse_statements(SemaState *state, const Statements *statements, const DataType *func_type);

#endif // _SEMA_STATEMENT_H_
