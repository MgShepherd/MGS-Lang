open Parser

let create_start_function = ".global _start\n_start:\n"

let process_arithmetic_operator = function
  | "+" -> "ADD"
  | "-" -> "SUB"
  | _ -> ""

let rec process_expression current_reg expr =
  if current_reg > 30 then raise (Failure "Too many operands in expression")
  else
    match expr with
    | ExprToken (T_VALUE x) -> Printf.sprintf "\tMOV X%d, #%s\n" current_reg x
    | ExprArithmetic (T_ARITHMETIC operator, left, right) ->
        Printf.sprintf "%s%s\t%s X%d, X%d, X%d\n"
          (process_expression (current_reg + 1) left)
          (process_expression (current_reg + 2) right)
          (process_arithmetic_operator operator)
          current_reg (current_reg + 1) (current_reg + 2)
    | _ -> ""

let process_statement = function
  | AssignmentStatement (_, _, _, expr) -> process_expression 0 expr

let rec process_statements acc = function
  | [] -> acc
  | x :: xs -> process_statements (acc ^ process_statement x) xs

let create_exit_function = "\tMOV X8, #93\n\tSVC 0\n"

let generate_assembly = function
  | Program statements ->
      create_start_function
      ^ process_statements "" statements
      ^ create_exit_function
