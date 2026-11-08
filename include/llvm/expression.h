#ifndef _LLVM_EXPRESSION_H_
#define _LLVM_EXPRESSION_H_

#include "parsing/expression.h"
#include "llvm/llvm.h"
#include <llvm-c/Core.h>

/**
 * build_expression will build an llvm expression from a provided analysed expression.
 * Will output a LLVMValueRef representing the stored value of the expression.
 *
 * @param state   The current state of the IR processing.
 * @param expr    The expression representation that we are converting into IR.
 * @param d_type  The LLVMTypeRef for which is associated with this expression.
 * @param store   Whether the result of the expression needs to be stored in memory, or just returned directly.
 *
 * @return        If store is true, will store expression value into memory and return a pointer to that memory.
 *                Otherwise will directly return the expression value
 */
LLVMValueRef build_expression(IRState *state, const Expression *expr, const LLVMTypeRef d_type, bool store);

#endif // _LLVM_EXPRESSION_H_
