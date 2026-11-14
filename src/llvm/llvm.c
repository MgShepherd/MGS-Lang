#include "llvm/llvm.h"
#include "dynamic_array.h"
#include "parsing/data_type.h"
#include "parsing/expression.h"
#include "parsing/type.h"
#include "llvm-c/Types.h"
#include "llvm/expression.h"
#include "llvm/type_definition.h"
#include "llvm/utils.h"

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
#define FUNC_LEN_ESTIMATE 5
#define TYPE_DEFINITIONS_LEN_ESTIMATE 5
#define ARRAY_REALLOC_FACTOR 2

unsigned char init_ir_state(IRState *state);

unsigned char build_function(IRState *state, const Function *func);

unsigned char build_statements(IRState *state, const Statements *statements);
unsigned char build_statement(IRState *state, const Statement *statement);
// Statements which return unsigned char can only error due to failing to add ValueRef into array
unsigned char build_declaration_statement(IRState *state, const DeclarationStatement *dec);
unsigned char build_if_block(IRState *state, const IfBlock *if_block);
void build_assignment_statement(IRState *state, const AssignmentStatement *assign);
void build_return_statement(IRState *state, const ReturnStatement *ret);
void build_void_statement(IRState *state, const VoidStatement *void_s);

unsigned char generate_object_file(const IRState *state, const char *file_name);

void dispose_ir_state(IRState *state);

unsigned char program_to_object_file(const Program *program, const char *file_name, bool llvm_debug) {
  assert(program != NULL);

  IRState state;
  if (init_ir_state(&state) != 0) {
    fprintf(stderr, "Failed to initialise IR state\n");
    return 1;
  }

  for (size_t i = 0; i < program->type_defs.count; i++) {
    build_type_definition(&state, &program->type_defs.elements[i]);
  }

  if (program->requires_array_type) {
    build_array_type_definition(&state);
  }

  // Add the exit function from libc
  LLVMTypeRef *param_types = malloc(sizeof(LLVMTypeRef) * 1);
  param_types[0] = LLVMInt32TypeInContext(state.context);
  LLVMTypeRef exit_type = LLVMFunctionType(LLVMVoidTypeInContext(state.context), param_types, 1, false);
  LLVMValueRef func_ptr = LLVMAddFunction(state.module, "exit", exit_type);
  const LlvmFunc exit_func = {
      .param_types = param_types,
      .num_params = 1,
      .ptr = func_ptr,
      .func_type = exit_type,
      .name = "exit",
  };
  dyn_array_insert(&state.funcs, exit_func);

  for (size_t i = 0; i < program->functions.count; i++) {
    if (build_function(&state, &program->functions.elements[i]) != 0) {
      dispose_ir_state(&state);
      return 1;
    }
  }

  if (llvm_debug) {
    fprintf(stdout, "%s\n", LLVMPrintModuleToString(state.module));
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
  dyn_array_init(&state->funcs, sizeof(LlvmFunc), FUNC_LEN_ESTIMATE);
  dyn_array_init(&state->type_refs, sizeof(TypeDefinitionRef), TYPE_DEFINITIONS_LEN_ESTIMATE);

  // TODO: This creation logic will need updating when supporting multiple source files
  state->context = LLVMContextCreate();
  state->module = LLVMModuleCreateWithNameInContext(MODULE_NAME, state->context);
  state->builder = LLVMCreateBuilderInContext(state->context);

  return 0;
}

unsigned char build_function(IRState *state, const Function *func) {
  LLVMTypeRef return_type = get_type(state, &func->return_type);

  LLVMTypeRef *param_types = malloc(sizeof(LLVMTypeRef) * func->parameters.count);
  if (param_types == NULL) {
    fprintf(stderr, "Failed to allocate required space for function parameters\n");
    return 1;
  }

  for (size_t i = 0; i < func->parameters.count; i++) {
    param_types[i] = get_type(state, &func->parameters.elements[i].d_type);
  }
  LLVMTypeRef func_type = LLVMFunctionType(return_type, param_types, func->parameters.count, false);
  LLVMValueRef llvm_func = LLVMAddFunction(state->module, func->name, func_type);

  LLVMBasicBlockRef block = LLVMAppendBasicBlockInContext(state->context, llvm_func, func->name);
  LLVMPositionBuilderAtEnd(state->builder, block);

  if (func->parameters.count > 0) {
    LLVMValueRef params[func->parameters.count];
    LLVMGetParams(llvm_func, params);

    for (size_t i = 0; i < func->parameters.count; i++) {
      const Field *param = &func->parameters.elements[i];
      const LLVMValueRef param_ptr = LLVMBuildAlloca(state->builder, param_types[i], param->term_ident.name);
      assert(param_ptr != NULL);
      LLVMBuildStore(state->builder, params[i], param_ptr);

      // TODO: ValueRef in LLVM is very simlar to Identifier in sema, can we combine these somehow?
      ValueRef param_val = {
          .name = param->term_ident.name,
          .ptr = param_ptr,
          .scope = param->term_ident.scope,
          .d_type = &param->d_type,
      };
      dyn_array_insert(&state->values, param_val);
    }
  }

  state->current_func = llvm_func;
  for (size_t i = 0; i < func->block.body.count; i++) {
    if (build_statement(state, &func->block.body.elements[i]) != 0) {
      fprintf(stderr, "Failed to build statement\n");
      return 1;
    }
  }

  // TODO: Void return type with early return
  if (func->return_type.d_kind == D_PRIMITIVE && func->return_type.d_union.p_type == P_VOID) {
    LLVMBuildRetVoid(state->builder);
  }

  if (LLVMVerifyFunction(llvm_func, LLVMPrintMessageAction) != 0) {
    fprintf(stderr, "Failed to build function\n");
    return 1;
  }

  const LlvmFunc func_llvm = {
      .param_types = param_types,
      .func_type = func_type,
      .num_params = func->parameters.count,
      .ptr = llvm_func,
      .name = func->name,
  };

  dyn_array_insert(&state->funcs, func_llvm);

  return 0;
}

unsigned char build_statements(IRState *state, const Statements *statements) {
  for (size_t i = 0; i < statements->count; i++) {
    unsigned char result = build_statement(state, &statements->elements[i]);
    if (result != 0) {
      return result;
    }
  }
  return 0;
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
  case S_IF:
    if (build_if_block(state, &statement->s_union.if_block) != 0) {
      return 1;
    }
    break;
  case S_VOID:
    build_void_statement(state, &statement->s_union.void_s);
    break;
  default:
    assert(false);
  }

  return 0;
}

void build_return_statement(IRState *state, const ReturnStatement *ret) {
  assert(ret != NULL);

  const LLVMTypeRef statement_type = get_type(state, ret->d_type);
  const LLVMValueRef expr_output = build_expression(state, &ret->expr, statement_type, false);
  LLVMBuildRet(state->builder, expr_output);
}

unsigned char build_declaration_statement(IRState *state, const DeclarationStatement *dec) {
  assert(dec != NULL && dec->d_type.d_kind != D_NONE);

  const LLVMTypeRef statement_type = get_type(state, &dec->d_type);
  assert(statement_type != NULL);
  const LLVMValueRef output_ptr = build_expression(state, &dec->expr, statement_type, true);

  ValueRef var = {
      .name = dec->term_ident.name,
      .ptr = output_ptr,
      .scope = dec->term_ident.scope,
      .d_type = &dec->d_type,
  };
  dyn_array_insert(&state->values, var);

  return 0;
}

void build_assignment_statement(IRState *state, const AssignmentStatement *assign) {
  assert(assign != NULL && assign->d_type->d_kind != D_NONE);

  LLVMValueRef out_ptr;
  switch (assign->a_output.a_type) {
  case A_IDENTIFIER:
    const ValueRef *assign_var = load_identifier(&state->values, &assign->a_output.a_union.term_ident);
    out_ptr = assign_var->ptr;
    break;
  case A_STRUCT_FIELD:
    const StructAccess *struct_field = &assign->a_output.a_union.struct_field;
    out_ptr =
        get_element_ptr(state, &struct_field->term_ident, struct_field->field_idxs, struct_field->num_field_nestings);
    break;
  case A_ARRAY_INDEX:
    const ArrayAccess *array_access = &assign->a_output.a_union.array_index;
    out_ptr = get_array_element_ptr(state, &array_access->term_ident, &array_access->idx, 1);
    break;
  default:
    assert(false);
  }

  const LLVMTypeRef statement_type = get_type(state, assign->d_type);
  const LLVMValueRef expr_output = build_expression(state, &assign->expr, statement_type, false);
  LLVMBuildStore(state->builder, expr_output, out_ptr);
}

unsigned char build_if_block(IRState *state, const IfBlock *if_block) {
  assert(if_block != NULL);

  const LLVMTypeRef bool_type = LLVMInt1TypeInContext(state->context);
  const LLVMValueRef function = state->current_func;

  const LLVMBasicBlockRef exit_if_block = LLVMAppendBasicBlockInContext(state->context, function, "if-exit");
  const LLVMBasicBlockRef then_if_block = LLVMAppendBasicBlockInContext(state->context, function, "if-then");
  LLVMBasicBlockRef next_branch_block = LLVMAppendBasicBlockInContext(state->context, function, "next");

  LLVMValueRef if_expr = build_expression(state, &if_block->if_branch.expr, bool_type, false);
  LLVMBuildCondBr(state->builder, if_expr, then_if_block, next_branch_block);
  LLVMPositionBuilderAtEnd(state->builder, then_if_block);

  if (build_statements(state, &if_block->if_branch.block.body) != 0) {
    return 1;
  }
  if (!if_block->if_branch.block.has_return) {
    LLVMBuildBr(state->builder, exit_if_block);
  }

  for (size_t i = 0; i < if_block->else_if_branches.count; i++) {
    const LLVMBasicBlockRef then_else_if_block =
        LLVMAppendBasicBlockInContext(state->context, state->current_func, "else-if-then");

    LLVMPositionBuilderAtEnd(state->builder, next_branch_block);
    next_branch_block = LLVMAppendBasicBlockInContext(state->context, function, "next");

    LLVMValueRef else_if_expr = build_expression(state, &if_block->else_if_branches.elements[i].expr, bool_type, false);
    LLVMBuildCondBr(state->builder, else_if_expr, then_else_if_block, next_branch_block);

    LLVMPositionBuilderAtEnd(state->builder, then_else_if_block);
    if (build_statements(state, &if_block->else_if_branches.elements[i].block.body) != 0) {
      return 1;
    }
    if (!if_block->else_if_branches.elements[i].block.has_return) {
      LLVMBuildBr(state->builder, exit_if_block);
    }
  }

  LLVMPositionBuilderAtEnd(state->builder, next_branch_block);
  if (if_block->else_branch.block.body.count == 0) {
    LLVMBuildBr(state->builder, exit_if_block);
  } else {
    if (build_statements(state, &if_block->else_branch.block.body) != 0) {
      return 1;
    }
    if (!if_block->else_branch.block.has_return) {
      LLVMBuildBr(state->builder, exit_if_block);
    }
  }

  LLVMPositionBuilderAtEnd(state->builder, exit_if_block);
  return 0;
}

void build_void_statement(IRState *state, const VoidStatement *void_s) {
  assert(void_s != NULL);

  const LLVMTypeRef statement_type = LLVMVoidTypeInContext(state->context);
  build_expression(state, &void_s->expr, statement_type, false);
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
  for (size_t i = 0; i < state->funcs.count; i++) {
    free(state->funcs.elements[i].param_types);
  }

  dyn_array_free(&state->funcs);
  for (size_t i = 0; i < state->type_refs.count; i++) {
    free(state->type_refs.elements[i].field_types);
  }
  dyn_array_free(&state->type_refs);

  LLVMDisposeBuilder(state->builder);
  LLVMDisposeModule(state->module);
  LLVMContextDispose(state->context);
}
