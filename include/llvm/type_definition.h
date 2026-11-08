#ifndef _LLVM_TYPE_DEFINITION_H_
#define _LLVM_TYPE_DEFINITION_H_

#include "parsing/type_definition.h"
#include <llvm-c/Core.h>

// Using '!' at the start of type definition name to ensure we don't conflict with any user defined struct names
#define ARRAY_TYPE_DEFINITION_NAME "!ARRAY"
// 2 fields in a array internal struct representation - first is the pointer to the elements, second is the length
#define NUM_ARRAY_TYPE_DEF_FIELDS 2

typedef struct IRState IRState;

typedef struct {
  const char *name;
  LLVMTypeRef ref;
  LLVMTypeRef *field_types;
  size_t num_fields;
} TypeDefinitionRef;

typedef struct {
  TypeDefinitionRef *elements;
  size_t count;
  size_t capacity;
} TypeDefinitionRefs;

/**
 * build_type_definition creates the TypeRef for the provided type definition
 * Will append the newly built TypeRef into the states type_ref array
 * @param state      - The current state of the IR processing - will append the newly created TypeDefintionRef to
 *                     type_refs
 * @param type_def   - The defined type we need to create a reference for
 */
void build_type_definition(IRState *state, const TypeDefinition *type_def);

/**
 * build_array_type_definition creates the TypeDefinition to internally represent an array as a struct.
 * This should only need to be built once and can represent all array types due to the use of opaque pointers
 * The built TypeDefinition will contain an element pointer (for the start of the array) and a length (in that order)
 * @param state         - The current state of the IR processing - will append the newly created TypeDefintionRef to
 *                        type_refs
 * @return              - Reference to the new type definition - does not need to be freed as points into states
 *                        type_refs
 */
const TypeDefinitionRef *build_array_type_definition(IRState *state);

#endif // _LLVM_TYPE_DEFINITION_H_
