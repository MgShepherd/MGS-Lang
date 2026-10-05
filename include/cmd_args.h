#ifndef _CMD_ARGS_H_
#define _CMD_ARGS_H_

#define HELP_RESPONSE_CODE 2

// TODO: Support writing output to a specified file path
typedef struct {
  const char *filepath;
  bool llvm_debug;
  const char *output_folder;
} CmdArgs;

/*
 * cmd_args_process will read all the arguments provided to the program and convert them to a CmdArgs struct
 * Output will be written to provided args parameter, will overwrite any existing data
 * Returns 0 on success, 1 on failure, 2 if help was printed (execution should be stopped, but no error)
 * CmdArgs does not need to be freed since all strings come from program arguments
 */
unsigned char cmd_args_process(CmdArgs *args, int argc, char **argv);

/*
 * Prints usage help to the console to explain the arguments that need to be provided
 */
void print_usage();

#endif // _CMD_ARGS_H_
