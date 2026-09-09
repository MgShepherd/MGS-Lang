#include "llvm.h"
#include "dynamic_array.h"
#include "lexer.h"
#include "parsing/type.h"
#include "llvm-c/Types.h"

#include <assert.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/Core.h>
#include <llvm-c/Target.h>
#include <llvm-c/TargetMachine.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MODULE_NAME "main"
#define VARIBLE_LEN_ESTIMATE 20
#define ARRAY_REALLOC_FACTOR 2

typedef struct {
  const char *name;
  LLVMValueRef ptr;
  bool variable;
} ValueRef;

typedef struct {
  ValueRef *elements;
  size_t count;
  size_t capacity;
} ValueRefs;

typedef struct {
  LLVMContextRef context;
  LLVMModuleRef module;
  LLVMBuilderRef builder;
  ValueRefs values;
} IRState;

unsigned char init_ir_state(IRState *state);

unsigned char build_function(IRState *state, const Function *func);
LLVMTypeRef get_type(const IRState *state, DataType d_type);

unsigned char build_statement(IRState *state, const Statement *statement);
// Declaration statement can only error due to failing to add ValueRef into array
unsigned char build_declaration_statement(IRState *state, const DeclarationStatement *dec);
void build_assignment_statement(IRState *state, const AssignmentStatement *assign);
void build_return_statement(const IRState *state, const ReturnStatement *ret);

/*
 * build_expression will build the required statements in order to get a single output
 * value which can be used in statements
 * Will produce output value as return value
 * Functions should never error assuming that all parser checks worked successfully
 */
LLVMValueRef build_expression(const IRState *state, const Expression *expr, const LLVMTypeRef d_type);
LLVMValueRef build_terminal_expr(const IRState *state, const TerminalExpr *term, const LLVMTypeRef d_type);
LLVMValueRef build_compound_expr(const IRState *state, const CompoundExpr *comp, const LLVMTypeRef d_type);

LLVMValueRef build_identifier(const IRState *state, const char *name, const LLVMTypeRef d_type);
LLVMValueRef build_literal(const Literal *literal, const LLVMTypeRef d_type);

unsigned char generate_object_file(const IRState *state, const char *file_name);

void dispose_ir_state(IRState *state);

LLVMValueRef load_identifier(const ValueRefs *values, const char *identifier);

unsigned char program_to_object_file(const Program *program, const char *file_name) {
  assert(program != NULL);

  IRState state;
  if (init_ir_state(&state) != 0) {
    fprintf(stderr, "Failed to initialise IR state\n");
    return 1;
  }

  for (size_t i = 0; i < program->functions.count; i++) {
    if (build_function(&state, &program->functions.elements[i]) != 0) {
      dispose_ir_state(&state);
      return 1;
    }
  }

  char *message;
  if (LLVMVerifyModule(state.module, LLVMReturnStatusAction, &message) != 0) {
    fprintf(stderr, "Failed to build function due to: %s\n", message);
    LLVMDisposeMessage(message);
    dispose_ir_state(&state);
    return 1;
  }

  if (generate_object_file(&state, file_name) != 0) {
    fprintf(stderr, "Failed to generated object file from LLVM IR\n");
    dispose_ir_state(&state);
    return 1;
  }

  dispose_ir_state(&state);
  return 0;
}

unsigned char init_ir_state(IRState *state) {
  dyn_array_init(&state->values, sizeof(ValueRef), VARIBLE_LEN_ESTIMATE);

  // TODO: This creation logic will need updating when supporting multiple source files
  state->context = LLVMContextCreate();
  state->module = LLVMModuleCreateWithNameInContext(MODULE_NAME, state->context);
  state->builder = LLVMCreateBuilderInContext(state->context);

  return 0;
}

unsigned char build_function(IRState *state, const Function *func) {
  LLVMTypeRef return_type = get_type(state, func->return_type);
  // TODO: Will need updating when we support function parameters
  LLVMTypeRef func_type = LLVMFunctionType(return_type, NULL, 0, false);
  LLVMValueRef llvm_func = LLVMAddFunction(state->module, func->name, func_type);

  LLVMBasicBlockRef block = LLVMAppendBasicBlockInContext(state->context, llvm_func, func->name);
  LLVMPositionBuilderAtEnd(state->builder, block);

  for (size_t i = 0; i < func->statements.count; i++) {
    if (build_statement(state, &func->statements.elements[i]) != 0) {
      fprintf(stderr, "Failed to build statement\n");
      return 1;
    }
  }

  if (LLVMVerifyFunction(llvm_func, LLVMPrintMessageAction) != 0) {
    fprintf(stderr, "Failed to build function\n");
    return 1;
  }
  return 0;
}

LLVMTypeRef get_type(const IRState *state, DataType d_type) {
  assert(state != NULL && d_type != D_NONE);

  switch (d_type) {
  case D_I32:
    return LLVMInt32TypeInContext(state->context);
  case D_BOOL:
    return LLVMInt1TypeInContext(state->context);
  default:
    abort();
  }
}

unsigned char build_statement(IRState *state, const Statement *statement) {
  assert(state != NULL && statement != NULL);
  switch (statement->s_type) {
  case S_RETURN:
    build_return_statement(state, &statement->s_union.ret);
    break;
  case S_DECLARATION:
    if (build_declaration_statement(state, &statement->s_union.dec) != 0) {
      return 1;
    }
    break;
  case S_ASSIGNMENT:
    build_assignment_statement(state, &statement->s_union.assign);
    break;
  default:
    abort();
    unreachable();
  }

  return 0;
}

void build_return_statement(const IRState *state, const ReturnStatement *ret) {
  assert(ret != NULL);

  const LLVMTypeRef statement_type = get_type(state, ret->d_type);
  const LLVMValueRef expr_output = build_expression(state, &ret->expr, statement_type);
  assert(expr_output != NULL);
  LLVMBuildRet(state->builder, expr_output);
}

unsigned char build_declaration_statement(IRState *state, const DeclarationStatement *dec) {
  assert(dec != NULL && dec->d_type != D_NONE);

  const LLVMTypeRef statement_type = get_type(state, dec->d_type);
  const LLVMValueRef var_ptr = LLVMBuildAlloca(state->builder, statement_type, dec->lhs);

  const LLVMValueRef expr_output = build_expression(state, &dec->expr, statement_type);
  assert(expr_output != NULL);
  LLVMBuildStore(state->builder, expr_output, var_ptr);

  ValueRef var = {
      .name = dec->lhs,
      .ptr = var_ptr,
      .variable = dec->variable,
  };
  dyn_array_insert(&state->values, var);

  return 0;
}

void build_assignment_statement(IRState *state, const AssignmentStatement *assign) {
  assert(assign != NULL && assign->d_type != D_NONE);

  const LLVMTypeRef statement_type = get_type(state, assign->d_type);
  const LLVMValueRef expr_output = build_expression(state, &assign->expr, statement_type);
  assert(expr_output != NULL);
  const LLVMValueRef assign_var = load_identifier(&state->values, assign->lhs);
  assert(assign_var != NULL);

  LLVMBuildStore(state->builder, expr_output, assign_var);
}

LLVMValueRef build_expression(const IRState *state, const Expression *expr, const LLVMTypeRef d_type) {
  assert(expr->e_type != E_NONE);

  switch (expr->e_type) {
  case E_TERMINAL:
    return build_terminal_expr(state, &expr->e_union.term, d_type);
  case E_COMPOUND:
    return build_compound_expr(state, &expr->e_union.comp, d_type);
  default:
    abort();
  }
}

// TODO: Re-add support for boolean terminals - add back once we have semantic analysis step
LLVMValueRef build_terminal_expr(const IRState *state, const TerminalExpr *term, const LLVMTypeRef d_type) {
  LLVMValueRef processed;

  if (term->tok->t_type == T_IDENTIFIER) {
    processed = build_identifier(state, term->tok->item, d_type);
  } else {
    processed = build_literal(&term->literal, d_type);
  }

  if (term->sign != NULL && term->sign->t_type == T_MINUS) {
    return LLVMBuildNeg(state->builder, processed, term->tok->item);
  }

  return processed;
}

LLVMValueRef build_identifier(const IRState *state, const char *name, const LLVMTypeRef d_type) {
  const LLVMValueRef value_ref = load_identifier(&state->values, name);
  assert(value_ref != NULL);

  return LLVMBuildLoad2(state->builder, d_type, value_ref, name);
}

LLVMValueRef build_literal(const Literal *literal, const LLVMTypeRef d_type) {
  assert(literal->l_type != L_NONE);

  switch (literal->l_type) {
  case L_NUM:
    return LLVMConstInt(d_type, literal->l_union.num, false);
  case L_BOOL:
    return LLVMConstInt(d_type, literal->l_union.b, false);
  default:
    abort();
  }
}

LLVMValueRef build_compound_expr(const IRState *state, const CompoundExpr *comp, const LLVMTypeRef d_type) {
  assert(comp != NULL && comp->op != O_NONE);

  LLVMValueRef lhs = build_terminal_expr(state, &comp->lhs, d_type);
  assert(lhs != NULL);
  LLVMValueRef rhs = build_expression(state, comp->rhs, d_type);
  assert(rhs != NULL);

  switch (comp->op) {
  case O_PLUS:
    return LLVMBuildAdd(state->builder, lhs, rhs, "ADD");
  case O_MINUS:
    return LLVMBuildSub(state->builder, lhs, rhs, "SUB");
  default:
    abort();
  }
}

unsigned char generate_object_file(const IRState *state, const char *file_name) {
  LLVMInitializeNativeTarget();
  LLVMInitializeNativeAsmParser();
  LLVMInitializeNativeAsmPrinter();

  char *target_triple = LLVMGetDefaultTargetTriple();

  LLVMTargetRef target;
  char *error_message;
  if (LLVMGetTargetFromTriple(target_triple, &target, &error_message) != 0) {
    fprintf(stderr, "Failed to get target machine, error: %s\n", error_message);
    LLVMDisposeMessage(error_message);
    LLVMDisposeMessage(target_triple);
    return 1;
  }

  LLVMTargetMachineRef target_machine = LLVMCreateTargetMachine(
      target, target_triple, "generic", "", LLVMCodeGenLevelDefault, LLVMRelocDefault, LLVMCodeModelDefault);
  LLVMTargetDataRef target_data = LLVMCreateTargetDataLayout(target_machine);
  LLVMSetModuleDataLayout(state->module, target_data);

  unsigned char status = 0;
  if (LLVMTargetMachineEmitToFile(target_machine, state->module, file_name, LLVMObjectFile, &error_message) != 0) {
    fprintf(stderr, "Failed to generate object file for machine, error: %s\n", error_message);
    LLVMDisposeMessage(error_message);
    status = 1;
  }

  LLVMDisposeMessage(target_triple);
  LLVMDisposeTargetData(target_data);
  LLVMDisposeTargetMachine(target_machine);
  return status;
}

void dispose_ir_state(IRState *state) {
  dyn_array_free(&state->values);

  LLVMDisposeBuilder(state->builder);
  LLVMDisposeModule(state->module);
  LLVMContextDispose(state->context);
}

LLVMValueRef load_identifier(const ValueRefs *values, const char *identifier) {
  for (size_t i = 0; i < values->count; i++) {
    if (strcmp(values->elements[i].name, identifier) == 0) {
      return values->elements[i].ptr;
    }
  }

  return NULL;
}
