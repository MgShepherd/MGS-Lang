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
 * Be aware: This function will return from the function it is used in the case of an error with allocating the
 * memory
 * Will return a 1 status code on error - ensure calling function uses this convention to be able to use
 */
#define dyn_array_init(dyn_arr, element_size, initial_cap)                                                             \
  {                                                                                                                    \
    (dyn_arr)->elements = NULL;                                                                                        \
    if (initial_cap == 0) {                                                                                            \
      fprintf(stderr, "Dynamic Array cannot be created with 0 capacity\n");                                            \
      return 1;                                                                                                        \
    }                                                                                                                  \
                                                                                                                       \
    (dyn_arr)->elements = malloc(initial_cap * element_size);                                                          \
    if ((dyn_arr)->elements == NULL) {                                                                                 \
      fprintf(stderr, "Failed to allocate required memory for dynamic array\n");                                       \
      return 1;                                                                                                        \
    }                                                                                                                  \
    (dyn_arr)->capacity = initial_cap;                                                                                 \
    (dyn_arr)->count = 0;                                                                                              \
  }

/*
 * Inserts an element into a dynamic array - will resize the array if it does not have enough capacity
 * Undefined behaviour if typeof element does not match that of the array
 * Be aware: This function will return from the function it is used in the case of an error with allocating the
 * memory
 * Will return a 1 status code on error - ensure calling function uses this convention to be able to use
 */
#define dyn_array_insert(dyn_arr, element)                                                                             \
  {                                                                                                                    \
    assert((dyn_arr)->elements != NULL);                                                                               \
                                                                                                                       \
    if ((dyn_arr)->count >= (dyn_arr)->capacity) {                                                                     \
      (dyn_arr)->capacity = (dyn_arr)->capacity * ARRAY_REALLOC_FACTOR;                                                \
      void *new_elements = realloc((dyn_arr)->elements, (dyn_arr)->capacity * sizeof((dyn_arr)->elements[0]));         \
      if (new_elements == NULL) {                                                                                      \
        fprintf(stderr, "Failed to allocate additional required space for dynamic array\n");                           \
        return 1;                                                                                                      \
      }                                                                                                                \
      (dyn_arr)->elements = new_elements;                                                                              \
    }                                                                                                                  \
                                                                                                                       \
    (dyn_arr)->elements[(dyn_arr)->count++] = element;                                                                 \
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
