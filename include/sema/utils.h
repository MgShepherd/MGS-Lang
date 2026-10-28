#ifndef _SEMA_UTILS_H_
#define _SEMA_UTILS_H_

#include "sema/program.h"

unsigned char insert_identifier(SemaState *state, const char *name, const DataType *d_type);

const Identifier *get_identifier(const SemaState *state, const char *name);

const Function *get_function(const Functions *functions, const char *name);

const TypeDefinition *get_type_def(const TypeDefinitions *type_defs, const char *name);

bool is_identifier_in_scope(const Scopes *scopes, const Identifier *identifier);

bool is_integer_type(const DataType *d_type);

bool is_number_in_range(long long input, long long min, long long max);

unsigned char tok_to_integer(long long *output_num, const Token *tok, bool is_negative);

bool is_void_type(const DataType *d_type);

#endif // _SEMA_UTILS_H_
