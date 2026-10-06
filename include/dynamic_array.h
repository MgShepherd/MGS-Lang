#ifndef _DYNAMIC_ARRAY_H_
#define _DYNAMIC_ARRAY_H_

#include <assert.h>
#include <stdlib.h>

#define ARRAY_REALLOC_FACTOR 2

/**
 * Dynamic Array structures should be user defined and must have the following structure:
 * typedef struct {
 * 	size_t count;
 * 	size_t capacity;
 * 	type *elements;
 * } MyArray;
 *
 * Where type is the datatype you are wanting to store - undefined behaviour if this structure is not followed
 * for all the macro definitions in this file
 *
 * Macros are used for all dynamic array functions to allow for supporting generic types
 * All macros in this file require a pointer to a dynamic array
 */

/*
 * Initialises the dynamic array memory with the provided capacity
 * On failure to allocate required extra memory, this function will cause an assertion failure
 * This is deemed acceptable as the compiler will likely be unable to function if out of memory, so crashing is okay
 */
#define dyn_array_init(dyn_arr, element_size, initial_cap)                                                             \
  {                                                                                                                    \
    (dyn_arr)->elements = NULL;                                                                                        \
    assert(initial_cap != 0);                                                                                          \
                                                                                                                       \
    (dyn_arr)->elements = malloc(initial_cap * element_size);                                                          \
    assert((dyn_arr)->elements != NULL && "Out of memory");                                                            \
    (dyn_arr)->capacity = initial_cap;                                                                                 \
    (dyn_arr)->count = 0;                                                                                              \
  }

/*
 * Inserts an element into a dynamic array - will resize the array if it does not have enough capacity
 * Undefined behaviour if typeof element does not match that of the array
 * On failure to allocate required extra memory, this function will cause an assertion failure
 * This is deemed acceptable as the compiler will likely be unable to function if out of memory, so crashing is okay
 */
#define dyn_array_insert(dyn_arr, element)                                                                             \
  {                                                                                                                    \
    assert((dyn_arr)->elements != NULL);                                                                               \
                                                                                                                       \
    if ((dyn_arr)->count >= (dyn_arr)->capacity) {                                                                     \
      (dyn_arr)->capacity = (dyn_arr)->capacity * ARRAY_REALLOC_FACTOR;                                                \
      (dyn_arr)->elements = realloc((dyn_arr)->elements, (dyn_arr)->capacity * sizeof((dyn_arr)->elements[0]));        \
      assert((dyn_arr)->elements != NULL && "Out of memory");                                                          \
    }                                                                                                                  \
                                                                                                                       \
    (dyn_arr)->elements[(dyn_arr)->count++] = element;                                                                 \
  }

/*
 * Removes the last element from a dynamic array
 * Will print to stderr but not crash the program if called on a 0 element array
 */
#define dyn_array_pop(dyn_arr)                                                                                         \
  {                                                                                                                    \
    assert((dyn_arr)->elements != NULL);                                                                               \
                                                                                                                       \
    if ((dyn_arr)->count <= 0) {                                                                                       \
      fprintf(stderr, "Attempted to remove element from empty array\n");                                               \
    }                                                                                                                  \
                                                                                                                       \
    (dyn_arr)->count--;                                                                                                \
  }

/*
 * Frees a dynamic array
 */
#define dyn_array_free(dyn_arr)                                                                                        \
  {                                                                                                                    \
    if ((dyn_arr) != NULL && (dyn_arr)->elements != NULL) {                                                            \
      free((dyn_arr)->elements);                                                                                       \
      (dyn_arr)->elements = NULL;                                                                                      \
      (dyn_arr)->count = 0;                                                                                            \
      (dyn_arr)->capacity = 0;                                                                                         \
    }                                                                                                                  \
  }

#endif //_DYNAMIC_ARRAY_H_
