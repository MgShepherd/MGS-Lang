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
#define FUNC_LEN_ESTIMATE 5
#define ARRAY_REALLOC_FACTOR 2

typedef struct {
  const char *name;
  LLVMValueRef ptr;
  IdentifierType i_type;
  const void *scope;
} ValueRef;

typedef struct {
  ValueRef *elements;
  size_t count;
  size_t capacity;
} ValueRefs;

typedef struct {
  LLVMTypeRef *param_types;
  size_t num_params;
  LLVMTypeRef func_type;
  LLVMValueRef ptr;
  const char *name;
} LlvmFunc;

typedef struct {
  LlvmFunc *elements;
  size_t count;
  size_t capacity;
} FuncRefs;

typedef struct {
  LLVMContextRef context;
  LLVMModuleRef module;
  LLVMBuilderRef builder;
  ValueRefs values;
  FuncRefs funcs;
} IRState;

unsigned char init_ir_state(IRState *state);

unsigned char build_function(IRState *state, const Function *func);
LLVMTypeRef get_type(const IRState *state, DataType d_type);

unsigned char build_statements(IRState *state, const Statements *statements, const LLVMValueRef func);
unsigned char build_statement(IRState *state, const Statement *statement, const LLVMValueRef func);
// Statements which return unsigned char can only error due to failing to add ValueRef into array
unsigned char build_declaration_statement(IRState *state, const DeclarationStatement *dec);
unsigned char build_if_block(IRState *state, const IfBlock *if_block, const LLVMValueRef func);
void build_assignment_statement(IRState *state, const AssignmentStatement *assign);
void build_return_statement(const IRState *state, const ReturnStatement *ret);
void build_void_statement(const IRState *state, const VoidStatement *void_s);

/*
 * build_expression will build the required statements in order to get a single output
 * value which can be used in statements
 * Will produce output value as return value
 * Functions should never error assuming that all parser checks worked successfully
 */
LLVMValueRef build_expression(const IRState *state, const Expression *expr, const LLVMTypeRef d_type);
LLVMValueRef build_terminal_expr(const IRState *state, const TerminalExpr *term, const LLVMTypeRef d_type);
LLVMValueRef build_compound_expr(const IRState *state, const CompoundExpr *comp, const LLVMTypeRef d_type);
LLVMValueRef build_func_call(const IRState *state, const FunctionCall *func_call, const LLVMTypeRef d_type);

LLVMValueRef build_identifier(const IRState *state, const TerminalIdentifier *term_ident, const LLVMTypeRef d_type);
LLVMValueRef build_literal(const Literal *literal, const LLVMTypeRef d_type);

unsigned char generate_object_file(const IRState *state, const char *file_name);

void dispose_ir_state(IRState *state);

LLVMValueRef load_identifier(const ValueRefs *values, const TerminalIdentifier *term_ident);
const LlvmFunc *load_function(const FuncRefs *funcs, const char *identifier);

unsigned char program_to_object_file(const Program *program, const char *file_name, bool llvm_debug) {
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

  if (llvm_debug) {
    fprintf(stdout, "%s\n", LLVMPrintModuleToString(state.module));
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

  // TODO: This creation logic will need updating when supporting multiple source files
  state->context = LLVMContextCreate();
  state->module = LLVMModuleCreateWithNameInContext(MODULE_NAME, state->context);
  state->builder = LLVMCreateBuilderInContext(state->context);

  return 0;
}

unsigned char build_function(IRState *state, const Function *func) {
  LLVMTypeRef return_type = get_type(state, func->return_type);

  LLVMTypeRef *param_types = malloc(sizeof(LLVMTypeRef) * func->parameters.count);
  if (param_types == NULL) {
    fprintf(stderr, "Failed to allocate required space for function parameters\n");
    return 1;
  }

  for (size_t i = 0; i < func->parameters.count; i++) {
    param_types[i] = get_type(state, func->parameters.elements[i].d_type);
  }
  LLVMTypeRef func_type = LLVMFunctionType(return_type, param_types, func->parameters.count, false);
  LLVMValueRef llvm_func = LLVMAddFunction(state->module, func->name, func_type);

  LLVMBasicBlockRef block = LLVMAppendBasicBlockInContext(state->context, llvm_func, func->name);
  LLVMPositionBuilderAtEnd(state->builder, block);

  if (func->parameters.count > 0) {
    LLVMValueRef params[func->parameters.count];
    LLVMGetParams(llvm_func, params);

    for (size_t i = 0; i < func->parameters.count; i++) {
      const Parameter *param = &func->parameters.elements[i];
      const LLVMValueRef param_ptr = LLVMBuildAlloca(state->builder, param_types[i], param->term_ident.name);
      assert(param_ptr != NULL);
      LLVMBuildStore(state->builder, params[i], param_ptr);

      // TODO: ValueRef in LLVM is very simlar to Identifier in sema, can we combine these somehow?
      ValueRef param_val = {
          .name = param->term_ident.name,
          .ptr = param_ptr,
          .i_type = I_CONST,
          .scope = param->term_ident.scope,
      };
      dyn_array_insert(&state->values, param_val);
    }
  }

  for (size_t i = 0; i < func->statements.count; i++) {
    if (build_statement(state, &func->statements.elements[i], llvm_func) != 0) {
      fprintf(stderr, "Failed to build statement\n");
      return 1;
    }
  }

  if (func->return_type == D_VOID) {
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

LLVMTypeRef get_type(const IRState *state, DataType d_type) {
  assert(state != NULL && d_type != D_NONE);

  switch (d_type) {
  case D_I32:
    return LLVMInt32TypeInContext(state->context);
  case D_BOOL:
    return LLVMInt1TypeInContext(state->context);
  case D_VOID:
    return LLVMVoidTypeInContext(state->context);
  default:
    assert(false);
  }
}

unsigned char build_statements(IRState *state, const Statements *statements, const LLVMValueRef func) {
  for (size_t i = 0; i < statements->count; i++) {
    unsigned char result = build_statement(state, &statements->elements[i], func);
    if (result != 0) {
      return result;
    }
  }
  return 0;
}

unsigned char build_statement(IRState *state, const Statement *statement, const LLVMValueRef func) {
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
    if (build_if_block(state, &statement->s_union.if_block, func) != 0) {
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
  const LLVMValueRef var_ptr = LLVMBuildAlloca(state->builder, statement_type, dec->term_ident.name);

  const LLVMValueRef expr_output = build_expression(state, &dec->expr, statement_type);
  assert(expr_output != NULL);
  LLVMBuildStore(state->builder, expr_output, var_ptr);

  const IdentifierType i_type = dec->variable ? I_VARIABLE : I_CONST;
  ValueRef var = {
      .name = dec->term_ident.name,
      .ptr = var_ptr,
      .i_type = i_type,
      .scope = dec->term_ident.scope,
  };
  dyn_array_insert(&state->values, var);

  return 0;
}

void build_assignment_statement(IRState *state, const AssignmentStatement *assign) {
  assert(assign != NULL && assign->d_type != D_NONE);

  const LLVMTypeRef statement_type = get_type(state, assign->d_type);
  const LLVMValueRef expr_output = build_expression(state, &assign->expr, statement_type);
  assert(expr_output != NULL);
  const LLVMValueRef assign_var = load_identifier(&state->values, &assign->term_ident);
  assert(assign_var != NULL);

  LLVMBuildStore(state->builder, expr_output, assign_var);
}

// TODO: Clean up repeated and messy code in this function
unsigned char build_if_block(IRState *state, const IfBlock *if_block, const LLVMValueRef func) {
  assert(if_block != NULL);

  const LLVMTypeRef bool_type = get_type(state, D_BOOL);

  const LLVMBasicBlockRef exit_if_block = LLVMAppendBasicBlockInContext(state->context, func, "if-exit");
  const LLVMBasicBlockRef then_if_block = LLVMAppendBasicBlockInContext(state->context, func, "if-then");
  LLVMBasicBlockRef next_branch_block = LLVMAppendBasicBlockInContext(state->context, func, "next");

  const LLVMValueRef if_expr = build_expression(state, &if_block->if_branch.expr, bool_type);
  assert(if_expr != NULL);
  LLVMBuildCondBr(state->builder, if_expr, then_if_block, next_branch_block);

  LLVMPositionBuilderAtEnd(state->builder, then_if_block);
  if (build_statements(state, &if_block->if_branch.body, func) != 0) {
    return 1;
  }
  LLVMBuildBr(state->builder, exit_if_block);

  for (size_t i = 0; i < if_block->else_if_branches.count; i++) {
    const LLVMBasicBlockRef then_else_if_block = LLVMAppendBasicBlockInContext(state->context, func, "else-if-then");

    LLVMPositionBuilderAtEnd(state->builder, next_branch_block);
    next_branch_block = LLVMAppendBasicBlockInContext(state->context, func, "next");

    const LLVMValueRef else_if_expr = build_expression(state, &if_block->else_if_branches.elements[i].expr, bool_type);
    assert(else_if_expr != NULL);
    LLVMBuildCondBr(state->builder, else_if_expr, then_else_if_block, next_branch_block);

    LLVMPositionBuilderAtEnd(state->builder, then_else_if_block);
    if (build_statements(state, &if_block->else_if_branches.elements[i].body, func) != 0) {
      return 1;
    }
    LLVMBuildBr(state->builder, exit_if_block);
  }

  LLVMPositionBuilderAtEnd(state->builder, next_branch_block);
  if (if_block->else_body.elements != NULL && build_statements(state, &if_block->else_body, func) != 0) {
    return 1;
  }
  LLVMBuildBr(state->builder, exit_if_block);

  LLVMPositionBuilderAtEnd(state->builder, exit_if_block);
  return 0;
}

void build_void_statement(const IRState *state, const VoidStatement *void_s) {
  assert(void_s != NULL);

  const LLVMTypeRef statement_type = get_type(state, D_VOID);
  const LLVMValueRef expr_output = build_expression(state, &void_s->expr, statement_type);
  assert(expr_output != NULL);
}

LLVMValueRef build_expression(const IRState *state, const Expression *expr, const LLVMTypeRef d_type) {
  assert(expr->e_type != E_NONE);

  switch (expr->e_type) {
  case E_TERMINAL:
    return build_terminal_expr(state, &expr->e_union.term, d_type);
  case E_COMPOUND:
    return build_compound_expr(state, &expr->e_union.comp, d_type);
  default:
    assert(false);
  }
}

LLVMValueRef build_terminal_expr(const IRState *state, const TerminalExpr *term, const LLVMTypeRef d_type) {
  LLVMValueRef processed;

  switch (term->item.t_type) {
  case TERM_IDENTIFIER:
    processed = build_identifier(state, &term->item.t_union.ident, d_type);
    break;
  case TERM_LITERAL:
    processed = build_literal(&term->item.t_union.lit.literal, d_type);
    break;
  case TERM_FUNC_CALL:
    processed = build_func_call(state, &term->item.t_union.func_call, d_type);
    break;
  default:
    assert(false);
  }

  if (term->sign != NULL && term->sign->t_type == T_MINUS) {
    return LLVMBuildNeg(state->builder, processed, "negative-term");
  }

  return processed;
}

LLVMValueRef build_func_call(const IRState *state, const FunctionCall *func_call, const LLVMTypeRef d_type) {
  const LlvmFunc *func = load_function(&state->funcs, func_call->name->item);
  assert(func != NULL);

  LLVMValueRef params[func->num_params];
  for (size_t i = 0; i < func->num_params; i++) {
    if (func->param_types[i] != LLVMInt32TypeInContext(state->context)) {
      LLVMDumpType(func->param_types[i]);
    }
    params[i] = build_expression(state, &func_call->parameters.elements[i], func->param_types[i]);
    assert(params[i] != NULL);
  }

  const char *name = func_call->name->item;
  if (d_type != LLVMVoidTypeInContext(state->context)) {
    return LLVMBuildCall2(state->builder, func->func_type, func->ptr, params, func->num_params, name);
  }
  return LLVMBuildCall2(state->builder, func->func_type, func->ptr, params, func->num_params, "");
}

LLVMValueRef build_identifier(const IRState *state, const TerminalIdentifier *term_ident, const LLVMTypeRef d_type) {
  const LLVMValueRef value_ref = load_identifier(&state->values, term_ident);
  assert(value_ref != NULL);

  return LLVMBuildLoad2(state->builder, d_type, value_ref, term_ident->name);
}

LLVMValueRef build_literal(const Literal *literal, const LLVMTypeRef d_type) {
  assert(literal->l_type != L_NONE);

  switch (literal->l_type) {
  case L_NUM:
    return LLVMConstInt(d_type, literal->l_union.num, false);
  case L_BOOL:
    return LLVMConstInt(d_type, literal->l_union.b, false);
  default:
    assert(false);
  }
}

LLVMValueRef build_compound_expr(const IRState *state, const CompoundExpr *comp, const LLVMTypeRef d_type) {
  assert(comp != NULL && comp->op != O_NONE);

  LLVMTypeRef term_type = d_type;
  if (term_type == LLVMInt1TypeInContext(state->context)) {
    term_type = LLVMInt32TypeInContext(state->context);
  }

  LLVMValueRef lhs = build_terminal_expr(state, &comp->lhs, term_type);
  assert(lhs != NULL);
  LLVMValueRef rhs;
  if (comp->rhs->e_type == E_TERMINAL) {
    rhs = build_terminal_expr(state, &comp->rhs->e_union.term, term_type);
  } else {
    rhs = build_compound_expr(state, &comp->rhs->e_union.comp, d_type);
  }
  assert(rhs != NULL);

  switch (comp->op) {
  case O_PLUS:
    return LLVMBuildAdd(state->builder, lhs, rhs, "ADD");
  case O_MINUS:
    return LLVMBuildSub(state->builder, lhs, rhs, "SUB");
  case O_LT:
    return LLVMBuildICmp(state->builder, LLVMIntSLT, lhs, rhs, "LT");
  case O_GT:
    return LLVMBuildICmp(state->builder, LLVMIntSGT, lhs, rhs, "GT");
  case O_LTE:
    return LLVMBuildICmp(state->builder, LLVMIntSLE, lhs, rhs, "LTE");
  case O_GTE:
    return LLVMBuildICmp(state->builder, LLVMIntSGE, lhs, rhs, "GTE");
  default:
    assert(false);
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
  for (size_t i = 0; i < state->funcs.count; i++) {
    free(state->funcs.elements[i].param_types);
  }
  dyn_array_free(&state->funcs);

  LLVMDisposeBuilder(state->builder);
  LLVMDisposeModule(state->module);
  LLVMContextDispose(state->context);
}

LLVMValueRef load_identifier(const ValueRefs *values, const TerminalIdentifier *term_ident) {
  for (size_t i = 0; i < values->count; i++) {
    if (strcmp(values->elements[i].name, term_ident->name) != 0) {
      continue;
    }

    if (term_ident->scope != values->elements[i].scope) {
      continue;
    }

    return values->elements[i].ptr;
  }

  return NULL;
}

const LlvmFunc *load_function(const FuncRefs *funcs, const char *identifier) {
  for (size_t i = 0; i < funcs->count; i++) {
    if (strcmp(funcs->elements[i].name, identifier) == 0) {
      return &funcs->elements[i];
    }
  }

  return NULL;
}
