#ifndef _SEMA_DATA_TYPE_H_
#define _SEMA_DATA_TYPE_H_

#include "sema/program.h"

/**
 * analyse_data_type will analyse a provided data type and ensures that the provided expression is compatible with the
 * data type
 *
 * @param state      - The current semantic analysis state
 * @param d_type     - The data type that we are wanting to analyse
 * @param expression - The expression that will be being stored with the data type - should already have analysed
 * @return           - 0 for successful analysis, 1 on failure
 */
unsigned char analyse_data_type(SemaState *state, DataType *d_type, const Expression *expression);

#endif // _SEMA_DATA_TYPE_H_
