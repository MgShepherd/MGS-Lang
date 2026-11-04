#ifndef _UTILS_H_
#define _UTILS_H_

#include <stdlib.h>

typedef struct {
  char *data;
  size_t count;
} FileData;

/*
 * Loads all the content of a file and return as a null terminated string
 * Allocates memory on the heap which will need to be freed
 * Returns NULL on failure
 */
char *read_file(const char *path);

/*
 * Returns a slice from within an existing string - slice_start inclusive, slice_end exclusive
 * Will ensure the slice is null terminated
 * Will allocate the string slice on the heap so this must be freed once finished with
 * Does not perform bound checks on the length of the string, so ensure slice_start and slice_end are within string
 * bounds - always assumes start is less than end
 * Will return NULL if unable to create the string slice
 */
char *string_slice(const char *input, size_t slice_start, size_t slice_end);

/*
 * Converts a provided object file into a native executable file
 * Returns 0 on success, 1 on failure
 */
unsigned char obj_to_executable(const char *input_file, const char *output_file);

/*
 * has_suffix will check that a provided string ends in a given series of characters
 */
bool has_suffix(const char *input, const char *suffix);

/*
 * Processes a file path and extracts the file name
 * File name will be the anything after the final '/' character up to the extension marker
 * Returned string will need to be freed when finished with
 */
char *file_name_from_path(const char *input);

/*
 * join_strings will join a provided array of strings (size num_strings) into one single string
 * The strings will be separated using the provided separator char
 * Resulting string is allocated on the heap so must be freed manually by caller
 */
char *join_strings(const char **strings, size_t num_strings, char separator);

#endif // _UTILS_H_
