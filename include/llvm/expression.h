#ifndef _LLVM_EXPRESSION_H_
#define _LLVM_EXPRESSION_H_

#include "parsing/expression.h"
#include "llvm/llvm.h"
#include <llvm-c/Core.h>

/**
 * build_expression will build an llvm expression from a provided analysed expression
 * Will output the built expression into the LLVMValueRef out_val
 *
 * @param out_ptr The LLVMValueRef containing the pointer to where we want to store the expression output
 * @param state   The current state of the IR processing
 * @param expr    The expression representation that we are converting into IR
 * @param d_type  The LLVMTypeRef for which is associated with this expression
 */
void build_expression(const LLVMValueRef out_ptr, const IRState *state, const Expression *expr,
                      const LLVMTypeRef d_type);

#endif // _LLVM_EXPRESSION_H_
