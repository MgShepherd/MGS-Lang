#ifndef _PARSING_TYPE_H_
#define _PARSING_TYPE_H_

#include "lexer.h"
#include <llvm-c/Core.h>

#define STATEMENT_TYPES                                                                                                \
  X(S_NONE)                                                                                                            \
  X(S_DECLARATION)                                                                                                     \
  X(S_ASSIGNMENT)                                                                                                      \
  X(S_IF)                                                                                                              \
  X(S_VOID)                                                                                                            \
  X(S_RETURN)

#define EXPRESSION_TYPES                                                                                               \
  X(E_NONE)                                                                                                            \
  X(E_COMPOUND)                                                                                                        \
  X(E_TERMINAL)

#define PRIMITIVE_TYPES                                                                                                \
  X(P_NONE)                                                                                                            \
  X(P_BOOL)                                                                                                            \
  X(P_VOID)                                                                                                            \
  X(P_I8)                                                                                                              \
  X(P_I16)                                                                                                             \
  X(P_I32)                                                                                                             \
  X(P_I64)

#define OPERATOR_TYPES                                                                                                 \
  X(O_NONE)                                                                                                            \
  X(O_LT)                                                                                                              \
  X(O_LTE)                                                                                                             \
  X(O_GT)                                                                                                              \
  X(O_GTE)                                                                                                             \
  X(O_PLUS)                                                                                                            \
  X(O_MINUS)

#define LITERAL_TYPES                                                                                                  \
  X(L_NONE)                                                                                                            \
  X(L_NUM)                                                                                                             \
  X(L_BOOL)

#define TERMINAL_TYPES                                                                                                 \
  X(TERM_NONE)                                                                                                         \
  X(TERM_LITERAL)                                                                                                      \
  X(TERM_IDENTIFIER)                                                                                                   \
  X(TERM_STRUCT_LITERAL)                                                                                               \
  X(TERM_STRUCT_ACCESS)                                                                                                \
  X(TERM_ARRAY_LITERAL)                                                                                                \
  X(TERM_ARRAY_ACCESS)                                                                                                 \
  X(TERM_FUNC_CALL)

#define IDENTIFIER_TYPES                                                                                               \
  X(I_NONE)                                                                                                            \
  X(I_VARIABLE)                                                                                                        \
  X(I_CONST)

#define DATATYPE_KINDS                                                                                                 \
  X(D_NONE)                                                                                                            \
  X(D_STRUCT)                                                                                                          \
  X(D_ARRAY)                                                                                                           \
  X(D_PRIMITIVE)

#define ASSIGNMENT_TYPES                                                                                               \
  X(A_NONE)                                                                                                            \
  X(A_IDENTIFIER)                                                                                                      \
  X(A_ARRAY_INDEX)                                                                                                     \
  X(A_STRUCT_FIELD)

#define TYPE_DEFINITION_KINDS                                                                                          \
  X(TD_NONE)                                                                                                           \
  X(TD_STRUCT)

#define X(N) N,
typedef enum { STATEMENT_TYPES } StatementType;
typedef enum { EXPRESSION_TYPES } ExpressionType;
typedef enum { PRIMITIVE_TYPES } PrimitiveType;
typedef enum { OPERATOR_TYPES } OperatorType;
typedef enum { LITERAL_TYPES } LiteralType;
typedef enum { TERMINAL_TYPES } TerminalType;
typedef enum { DATATYPE_KINDS } DataTypeKind;
typedef enum { ASSIGNMENT_TYPES } AssignmentType;
typedef enum { TYPE_DEFINITION_KINDS } TypeDefinitionKind;
#undef X

const char *s_type_to_string(StatementType s);
const char *e_type_to_string(ExpressionType e);
const char *p_type_to_string(PrimitiveType p);
const char *o_type_to_string(OperatorType o);
const char *l_type_to_string(LiteralType l);
const char *term_type_to_string(TerminalType t);
const char *td_kind_to_string(TypeDefinitionKind td);

// TODO: Decided whether we want this approach of having all the structs defined in one file
// If so, implement the same for sema and llvm stages for consistency
// Also, may want to reorder existing stucts to make order clearer
typedef struct Expression Expression;
typedef struct FieldAssignment FieldAssignment;
typedef struct TypeDefinition TypeDefinition;
typedef struct DataType DataType;
typedef struct Statement Statement;

typedef union {
  int8_t i8;
  int16_t i16;
  int32_t i32;
  int64_t i64;
  bool b;
} LiteralUnion;

typedef struct {
  const char *name;
} StructType;

typedef struct {
  DataType *element_type;
  // length will be populated as part of sema - if length is unknown at compile time, -1 will be used here
  int64_t length;
} ArrayType;

typedef struct {
  PrimitiveType p_type;
  StructType s_type;
  ArrayType a_type;
} DataTypeUnion;

struct DataType {
  DataTypeUnion d_union;
  DataTypeKind d_kind;
  bool variable;
};

typedef struct {
  Expression *elements;
  size_t count;
  size_t capacity;
} Expressions;

typedef struct {
  const char *name;
  Expressions parameters;
} FunctionCall;

typedef struct {
  const char *name;
  DataType d_type;
  // Will be populated by sema
  const void *scope;
  // Will be populated by llvm
  LLVMValueRef ptr;
} Identifier;

typedef struct {
  const Token *tok;
  // l_union and d_type will be populated as part of semantic analysis
  LiteralUnion l_union;
  const DataType *d_type;
} Literal;

// IdentifierKey is used to allow us to retrieve identifiers from the Identifier array
typedef struct {
  const char *name;
  // scope will be populated as part of sema
  const void *scope;
} IdentifierKey;

typedef struct {
  IdentifierKey ident_key;
  const char **field_names;
  size_t num_field_nestings;
  // type_def, field_idxs, variable and d_type will be populated as part of semantic analysis
  const TypeDefinition *type_def;
  size_t *field_idxs;
  const DataType *d_type;
  // TODO: Is there a better way to handle variable for StructAccess - current solution a bit hacky
  bool variable;
} StructAccess;

typedef struct {
  IdentifierKey ident_key;
  // TODO: Update this to be able to support nested arrays
  const Token *idx_tok;
  // d_type and idx will be populated as part of semantic analysis
  const DataType *d_type;
  size_t idx;
  bool variable;
} ArrayAccess;

typedef struct {
  FieldAssignment *elements;
  size_t count;
  size_t capacity;
} FieldAssignments;

typedef struct {
  FieldAssignments field_assignments;
  // d_type will be populated as part of semantic analysis
  const DataType *d_type;
} StructLiteral;

typedef struct {
  Expressions items;
  // d_type will be populated as part of semantic analysis
  const DataType *d_type;
} ArrayLiteral;

typedef struct {
  Literal lit;
  IdentifierKey ident_key;
  FunctionCall func_call;
  StructLiteral struct_literal;
  StructAccess struct_access;
  ArrayLiteral array_literal;
  ArrayAccess array_access;
} TerminalUnion;

typedef struct {
  TerminalUnion t_union;
  TerminalType t_type;
} Terminal;

typedef struct {
  Terminal item;
  const Token *sign;
} TerminalExpr;

typedef struct {
  TerminalExpr lhs;
  OperatorType op;
  Expression *rhs;
} CompoundExpr;

typedef union {
  TerminalExpr term;
  CompoundExpr comp;
} ExpressionUnion;

struct Expression {
  ExpressionUnion e_union;
  ExpressionType e_type;
};

struct FieldAssignment {
  const char *name;
  Expression expression;
};

typedef struct {
  size_t ident_idx;
  // TODO: Name is stored here so that we can compare fields properly, is there a better approach?
  const char *name;
  // TODO: We may not need d_type here now that the identifier contains the data_type as well
  DataType d_type;
} Field;

typedef struct {
  Field *elements;
  size_t count;
  size_t capacity;
} Fields;

typedef struct {
  const char *name;
  Fields fields;
} StructDefinition;

typedef struct {
  StructDefinition s_def;
} TypeDefinitionUnion;

struct TypeDefinition {
  TypeDefinitionKind td_kind;
  TypeDefinitionUnion td_union;
};
typedef struct {
  Statement *elements;
  size_t count;
  size_t capacity;
} Statements;

typedef struct {
  size_t ident_idx;
  // TODO: Do we need d_type here now that the identifier contains the data_type as well
  DataType d_type;
  Expression expr;
} DeclarationStatement;

typedef union {
  IdentifierKey ident_key;
  StructAccess struct_field;
  ArrayAccess array_index;
} AssignmentOutputUnion;

typedef struct {
  AssignmentType a_type;
  AssignmentOutputUnion a_union;
} AssignmentOutput;

typedef struct {
  AssignmentOutput a_output;
  Expression expr;
  // d_type will be populated as part of semantic analysis
  const DataType *d_type;
} AssignmentStatement;

typedef struct {
  Expression expr;
  // d_type will be populated as part of semantic analysis
  const DataType *d_type;
} ReturnStatement;

typedef struct {
  Expression expr;
} VoidStatement;

typedef struct {
  Statements body;
  // has_return will be populated as part of semantic analysis
  bool has_return;
} Block;

typedef struct {
  Expression expr;
  Block block;
} IfBranch;

typedef struct {
  IfBranch *elements;
  size_t count;
  size_t capacity;
} ElseIfBranches;

typedef struct {
  IfBranch if_branch;
  // else_if_branches will have 0 count if no else_if provided
  ElseIfBranches else_if_branches;
  // else_branch will have body with 0 elements if no else provided
  IfBranch else_branch;
} IfBlock;

typedef union {
  DeclarationStatement dec;
  ReturnStatement ret;
  AssignmentStatement assign;
  VoidStatement void_s;
  IfBlock if_block;
} StatementUnion;

struct Statement {
  StatementType s_type;
  StatementUnion s_union;
};

typedef struct {
  const char *name;
  DataType return_type;
  Fields parameters;
  Block block;
  // Will be filled is as part of llvm
  LLVMTypeRef *param_types;
  LLVMTypeRef func_type;
  LLVMValueRef ptr;
} Function;

typedef struct {
  Function *elements;
  size_t count;
  size_t capacity;
} Functions;

typedef struct {
  TypeDefinition *elements;
  size_t count;
  size_t capacity;
} TypeDefinitions;

typedef struct {
  Identifier *elements;
  size_t count;
  size_t capacity;
} Identifiers;

typedef struct {
  // TODO: Really these should become fixed arrays that they they can't be accidently re-allocated after parsing
  Functions functions;
  TypeDefinitions type_defs;
  Identifiers identifiers;
  // requires_array_type will be filled in during sema
  // This is needed since we have a single global array type, which we only want to define if the program actually uses
  // arrays
  bool requires_array_type;
} Program;

typedef struct {
  const Tokens *tokens;
  size_t idx;
  Identifiers identifiers;
} ParseState;

/*
 * Converts a provided token type into the matching primitive type
 * Will return P_NONE if invalid primitive
 */
PrimitiveType tok_to_primitive_type(TokenType t_type);

/*
 * Converts a provided token type into the matching operator type
 * Will return O_NONE if invalid datatype
 */
OperatorType tok_to_op_type(TokenType t_type);

#endif // _PARSING_TYPE_H_
