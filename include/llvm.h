#ifndef _LLVM_H_
#define _LLVM_H_

#include "parsing/program.h"

/*
 * program_to_object_file takes a built program AST and will generate a native obj file via LLVM
 * Generated obj will be output to provided file name
 * llvm_debug argument used to output the generated IR code to stdout
 * All LLVM state is cleaned up as part of this function, no additional cleanup neccessary
 * Returns 0 on success, 1 on error
 */
unsigned char program_to_object_file(const Program *program, const char *file_name, bool llvm_debug);

#endif // _LLVM_H_
