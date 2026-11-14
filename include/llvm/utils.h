#ifndef _LLVM_UTILS_H_
#define _LLVM_UTILS_H_

#include "parsing/data_type.h"
#include "llvm/llvm.h"

/**
 * load_identifier will return the ValueRef struct which represents the provided term_ident
 * Finds ValueRef which has matching name and scope as requested identifier
 * @param values     - The dynamic array of ValueRefs which contain all defined values
 * @param term_ident - The identifier we want to load
 * @return             The ValueRef representation of the identifier - NULL if not found
 */
const ValueRef *load_identifier(const ValueRefs *values, const TerminalIdentifier *term_ident);

/**
 * load_function will return the LlvmFunc struct which matches the provided identifier string
 * @param funcs      - The dynamic array of funcs which contain all defined functions
 * @param identifier - The identifier of the function we want to load
 * @return             The LlvmFunc representation of the function - NULL if not found
 */
const LlvmFunc *load_function(const FuncRefs *funcs, const char *identifier);

/**
 * load_type_definition will return the TypeDefinitionRef which matches the provided identifier string
 * @param type_refs  - The dynamic array of type references which contain all type defs
 * @param identifier - The identifier of the type definition we want to load
 * @return             The TypeDefinitionRef representation of the type def - NULL if not found
 */
const TypeDefinitionRef *load_type_definition(const TypeDefinitionRefs *type_refs, const char *identifier);

/**
 * Gets the LLVM Type associated with a provided DataType
 * @param state  - The current IR state
 * @param d_type - The data type we are wanting the representation of
 * @return       - The LLVMTypeRef of the provided d_type
 */
LLVMTypeRef get_type(IRState *state, const DataType *d_type);

#endif // _LLVM_UTILS_H_
