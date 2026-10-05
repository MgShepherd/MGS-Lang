#include "cmd_args.h"

#include "utils.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define MIN_ARGS_LEN 2
#define SRC_EXTENSION ".mgs"

bool is_valid_filepath(const char *filepath);
unsigned char process_arg_with_value(CmdArgs *args, const char *arg, const char *equals_location);

unsigned char cmd_args_process(CmdArgs *args, int argc, char **argv) {
  args->llvm_debug = false;
  args->output_folder = NULL;

  if (argc < MIN_ARGS_LEN) {
    fprintf(stderr, "Expected at least %d arguments, got %d\n", MIN_ARGS_LEN, argc);
    print_usage();
    return 1;
  }

  if (strcmp(argv[1], "--help") == 0 || strcmp(argv[1], "-h") == 0) {
    print_usage();
    return HELP_RESPONSE_CODE;
  }

  if (!has_suffix(argv[1], SRC_EXTENSION)) {
    fprintf(stderr, "Input file with extension " SRC_EXTENSION " must be provided as first argument\n");
    print_usage();
    return 1;
  }
  args->filepath = argv[1];

  for (int i = MIN_ARGS_LEN; i < argc; i++) {
    if (strcmp(argv[i], "--llvm-debug") == 0 && !args->llvm_debug) {
      args->llvm_debug = true;
      continue;
    }

    char *equals_location = strchr(argv[i], '=');
    if (equals_location != NULL && process_arg_with_value(args, argv[i], equals_location) == 0) {
      continue;
    }

    fprintf(stderr, "Invalid argument: %s\n", argv[i]);
    print_usage();
    return 1;
  }

  if (args->output_folder == NULL) {
    args->output_folder = "./";
  } else if (access(args->output_folder, F_OK) != 0) {
    fprintf(stderr, "Relative output location \"%s\" does not exist\n", args->output_folder);
    return 1;
  }

  return 0;
}

void print_usage() {
  printf("OVERVIEW: MGS Language Compiler\n\n");
  printf("USAGE: mgs filepath [--llvm-debug --output-folder=<PATH>]\n\n");
}

unsigned char process_arg_with_value(CmdArgs *args, const char *arg, const char *equals_location) {
  const size_t equals_idx = equals_location - arg;
  assert(arg[equals_idx] == '=');

  char arg_name[equals_idx + 1];
  arg_name[equals_idx] = '\0';

  for (size_t i = 0; i < equals_idx; i++) {
    arg_name[i] = arg[i];
  }

  if (strlen(arg) == equals_idx + 1) {
    return 1;
  }
  const char *arg_value = equals_location + 1;

  if (strcmp("--output-folder", arg_name) == 0) {
    args->output_folder = arg_value;
  } else {
    return 1;
  }

  return 0;
}
