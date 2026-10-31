#ifndef _SEMA_TERMINAL_H_
#define _SEMA_TERMINAL_H_

#include "sema/program.h"

unsigned char analyse_func_call(SemaState *state, const FunctionCall *func_call, const DataType *expr_type);

unsigned char analyse_array_access(SemaState *state, ArrayAccess *arr_access, const DataType *expr_type);

unsigned char analyse_identifier(SemaState *state, TerminalIdentifier *term_ident, const DataType *expr_type);

unsigned char analyse_literal(TerminalLiteral *term_lit, const Token *sign, const DataType *expr_type);

unsigned char analyse_array_literal(SemaState *state, ArrayLiteral *arr_lit, const DataType *expr_type);

unsigned char analyse_struct_literal(SemaState *state, StructLiteral *struct_lit, const DataType *expr_type);

unsigned char analyse_struct_access(SemaState *state, StructAccess *struct_access, const DataType *expr_type);

unsigned char analyse_numeric_literal(TerminalLiteral *term_lit, bool is_negative, const DataType *expr_type);

#endif // _SEMA_TERMINAL_H_
