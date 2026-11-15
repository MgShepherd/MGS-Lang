#ifndef _SEMA_TYPE_DEFINITION_H_
#define _SEMA_TYPE_DEFINITION_H_

#include "parsing/type_definition.h"
#include "sema/program.h"

unsigned char analyse_type_definition(SemaState *state, TypeDefinition *type_def);

#endif // _SEMA_TYPE_DEFINITION_H_
