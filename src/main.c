#include <stdio.h>
#include <unistd.h>

#include "lexer.h"

int main(int argc, char **argv) {
  int opt;
  const char *file_path = NULL;

  while ((opt = getopt(argc, argv, "f:")) != -1) {
    switch (opt) {
    case 'f':
      file_path = optarg;
      break;
    default:
      printf("Unknown argument passed: %c\n", opt);
    }
  }

  if (!file_path) {
    printf("Missing filepath argument -f\n");
    return -1;
  }

  return lexer_process_file(file_path);
}
