#ifndef _LLVM_GEP_H_
#define _LLVM_GEP_H_

#include "parsing/expression.h"
#include "llvm/llvm.h"

/**
 * Gets a pointer into a provided identifier - assumes the identifier can be indexed
 * @param state       - The current IR state
 * @param term_ident  - The identifier to index into
 * @param indices     - Array of indices used - each item refers to the next layer of nested elements
 * @param num_indices - Number of elements in the indices array
 * @return            - The LLVMValueRef for the pointer to the element
 */
LLVMValueRef get_element_ptr(IRState *state, const TerminalIdentifier *term_ident, const size_t *indices,
                             size_t num_indices);

/**
 * get_array_element_ptr will get a pointer to the requested indices in the array stored in term_ident.
 * Assumes that all indices are within the bounds of the array
 * This is an extension of get_element_ptr, since for arrays we need to index them differently
 * @param state         - The current IR state
 * @param term_ident    - The identifier to index into - should be an array type identifier
 * @param indices       - Array of indices used - multiple indices indicate multi-dimensional arrays
 * @param num_indices   - Number of elements in the indices array
 * @return              - The LLVMValueRef for the pointer to the element
 */
LLVMValueRef get_array_element_ptr(IRState *state, const TerminalIdentifier *term_ident, const size_t *indices,
                                   size_t num_indices);

#endif // _LLVM_GEP_H_
