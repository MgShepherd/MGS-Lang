#include "lexer.h"
#include <stdio.h>

int lexer_process_file(const char *file_path) {
  FILE *file_ptr = fopen(file_path, "r");

  if (!file_ptr) {
    printf("Unable to open file: %s\n", file_path);
    return -1;
  }
  printf("Successfully opened file %s\n", file_path);

  if (fclose(file_ptr) != 0) {
    printf("There was a problem closing the file\n");
    return -1;
  }
  return 0;
}
