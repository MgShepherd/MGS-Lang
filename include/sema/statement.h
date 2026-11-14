#ifndef _SEMA_STATEMENT_H_
#define _SEMA_STATEMENT_H_

#include "sema/program.h"

/**
 * analyse_statements will analyse all the statements in a given Statements array
 * @param state      - The current IR state
 * @param block      - The block containing the statements that we want to analyse
 *                     Will update the block with whether there is a return statement
 * @param func_type  - The return type of the current function that we are processing
 */
unsigned char analyse_statements(SemaState *state, Block *block, const DataType *func_type);

#endif // _SEMA_STATEMENT_H_
