#include "cmd_args.h"
#include "dynamic_array.h"
#include "lexer.h"
#include "llvm.h"
#include "parsing/program.h"
#include "sema.h"
#include "utils.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BUILD_FOLDER "build/"
#define OBJ_EXTENSION ".o"

int main(int argc, char **argv) {
  int response_code = 0;
  char *data = NULL;
  Tokens tokens;
  tokens.elements = NULL;
  Program program;
  program.functions.elements = NULL;
  Identifiers identifiers;
  identifiers.elements = NULL;
  char *file_name = NULL;
  char *obj_file_path = NULL;
  char *exe_file_path = NULL;

  CmdArgs args;
  unsigned char result = cmd_args_process(&args, argc, argv);
  if (result == HELP_RESPONSE_CODE) {
    return 0;
  }

  if (result != 0) {
    fprintf(stderr, "Failed to read command line arguments\n");
    response_code = 1;
    goto cleanup;
  }

  data = read_file(args.filepath);
  if (data == NULL) {
    fprintf(stderr, "Failed to read file %s\n", args.filepath);
    response_code = 1;
    goto cleanup;
  }

  if (lexer_process_tokens(&tokens, data) != 0) {
    fprintf(stderr, "Failed to convert file input into tokens\n");
    response_code = 1;
    goto cleanup;
  }

  if (parse_program(&program, &tokens) != 0) {
    fprintf(stderr, "Failed to convert processed tokens into a program\n");
    response_code = 1;
    goto cleanup;
  }

  if (analyse_program(&identifiers, &program) != 0) {
    fprintf(stderr, "Failed to analyse program structure\n");
    response_code = 1;
    goto cleanup;
  }

  file_name = file_name_from_path(args.filepath);
  if (file_name == NULL) {
    fprintf(stderr, "Failed to extract file name from path\n");
    response_code = 1;
    goto cleanup;
  }

  const size_t obj_file_len = strlen(BUILD_FOLDER) + strlen(file_name) + strlen(OBJ_EXTENSION) + 1;
  obj_file_path = malloc(obj_file_len * sizeof(char));
  if (obj_file_path == NULL) {
    fprintf(stderr, "Failed to allocate required memory for object file path\n");
    response_code = 1;
    goto cleanup;
  }

  if (snprintf(obj_file_path, obj_file_len, "%s%s%s", BUILD_FOLDER, file_name, OBJ_EXTENSION) < 0) {
    fprintf(stderr, "Failed to write object file path to string\n");
    response_code = 1;
    goto cleanup;
  }

  if (program_to_object_file(&program, obj_file_path) != 0) {
    fprintf(stderr, "Failed to build LLVM IR from program AST\n");
    response_code = 1;
    goto cleanup;
  }

  const size_t exe_file_len = strlen(BUILD_FOLDER) + strlen(file_name) + 1;
  exe_file_path = malloc(exe_file_len * sizeof(char));
  if (exe_file_path == NULL) {
    fprintf(stderr, "Failed to allocate required memory for executable file path\n");
    response_code = 1;
    goto cleanup;
  }

  if (snprintf(exe_file_path, exe_file_len, "%s%s", BUILD_FOLDER, file_name) < 0) {
    fprintf(stderr, "Failed to write executable file path to string\n");
    response_code = 1;
    goto cleanup;
  }

  if (obj_to_executable(obj_file_path, exe_file_path) != 0) {
    fprintf(stderr, "Failed to generate native executable from obj\n");
    response_code = 1;
    goto cleanup;
  }

  printf("Compilation succeeded\n");
cleanup:
  if (exe_file_path != NULL)
    free(exe_file_path);
  if (obj_file_path != NULL)
    free(obj_file_path);
  if (file_name != NULL)
    free(file_name);
  dyn_array_free(&identifiers);
  program_free(&program);
  dyn_array_free(&tokens);
  if (data != NULL)
    free(data);

  return response_code;
}
