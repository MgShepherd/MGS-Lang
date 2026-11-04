#include "utils.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *read_file(const char *path) {
  FILE *f_ptr = fopen(path, "r");
  if (f_ptr == NULL) {
    fprintf(stderr, "Failed to read file at path: %s\n", path);
    return NULL;
  }

  if (fseek(f_ptr, 0, SEEK_END) != 0) {
    fprintf(stderr, "Failed to jump to end of file in order to get size\n");
    fclose(f_ptr);
    return NULL;
  }

  const long file_size = ftell(f_ptr);
  if (file_size <= 0) {
    fprintf(stderr, "Failed to get the size of the input file\n");
    fclose(f_ptr);
    return NULL;
  }

  if (fseek(f_ptr, 0, SEEK_SET) != 0) {
    fprintf(stderr, "Failed to jump back to start of file after reading size\n");
    fclose(f_ptr);
    return NULL;
  }

  char *data = malloc(file_size + 1);
  if (data == NULL) {
    fprintf(stderr, "Failed to allocate required memory for data buffer\n");
    fclose(f_ptr);
    return NULL;
  }

  size_t read_bytes = fread(data, 1, file_size, f_ptr);
  if (read_bytes != (size_t)file_size) {
    fprintf(stderr, "Failed to copy file data to output buffer, read bytes %zu, but expected %zu\n", read_bytes,
            file_size);
    free(data);
    fclose(f_ptr);
    return NULL;
  }

  data[file_size] = '\0';

  fclose(f_ptr);
  return data;
}

char *string_slice(const char *input, size_t slice_start, size_t slice_end) {
  assert(input != NULL);
  assert(slice_start < slice_end);

  char *output = malloc(slice_end - slice_start + 1);
  if (output == NULL) {
    fprintf(stderr, "Failed to allocate required space for string slice");
    return NULL;
  }

  for (size_t i = slice_start; i < slice_end; i++) {
    output[i - slice_start] = input[i];
  }

  output[slice_end - slice_start] = '\0';
  return output;
}

unsigned char obj_to_executable(const char *input_file, const char *output_file) {
  assert(has_suffix(input_file, ".o"));

  size_t cmd_len = strlen("clang ") + strlen(input_file) + strlen(" -o ") + strlen(output_file) + 1;
  char *command = malloc(cmd_len * sizeof(char));

  if (command == NULL) {
    fprintf(stderr, "Failed to allocate enough memory for executable generation command\n");
    return 1;
  }

  if (snprintf(command, cmd_len, "clang %s -o %s", input_file, output_file) < 0) {
    fprintf(stderr, "Failed to build command for generating executable file\n");
    free(command);
    return 1;
  }

  if (system(command) != 0) {
    fprintf(stderr, "Failed to generate executable from object file\n");
    free(command);
    return 1;
  }

  free(command);
  return 0;
}

bool has_suffix(const char *input, const char *suffix) {
  assert(strlen(input) > 0);

  const size_t input_len = strlen(input);
  const size_t suffix_len = strlen(suffix);
  if (input_len < suffix_len) {
    return false;
  }

  for (size_t i = 0; i < suffix_len; i++) {
    if (input[input_len - suffix_len + i] != suffix[i]) {
      return false;
    }
  }

  return true;
}

char *file_name_from_path(const char *input) {
  assert(input != NULL);
  size_t name_start = strlen(input);
  size_t name_end = name_start;

  while (name_start >= 0 && input[name_start] != '/') {
    if (input[name_start] == '.') {
      name_end = name_start;
    }
    name_start--;
  }

  return string_slice(input, name_start, name_end);
}

char *join_strings(const char **strings, size_t num_strings, char separator) {
  // Additional space needed for ending null terminator
  size_t total_len = 1;
  for (size_t i = 0; i < num_strings; i++) {
    total_len += strlen(strings[i]);

    // Add space for separator
    if (i < num_strings - 1) {
      total_len++;
    }
  }

  char *joined = malloc(sizeof(char) * total_len);
  assert(joined != NULL);

  joined[total_len - 1] = '\0';

  size_t current_idx = 0;
  for (size_t i = 0; i < num_strings; i++) {
    for (size_t j = 0; j < strlen(strings[i]); j++) {
      joined[current_idx++] = strings[i][j];
    }

    if (i < num_strings - 1) {
      joined[current_idx++] = separator;
    }
  }

  return joined;
}
