open Common

type expr =
  | ExprArithmetic of Token.token * expr * expr
  | ExprComparison of Token.token * expr * expr
  | ExprToken of Token.token

type statement =
  | DeclarationStatement of Token.token * Token.token * Token.token * expr
  | AssignmentStatement of Token.token * Token.token * expr
  | IfStatement of expr list * statement list list
  | WhileStatement of expr * statement list
  | PrintStatement of Token.token

type program = Program of statement list

val create_tree : Token.token list -> program
