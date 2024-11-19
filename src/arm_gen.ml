let create_start_function = ".global _start\n_start:\n"

let rec process_expression = function
  | Parser.ExprToken (T_VALUE x) -> Printf.sprintf "#%s" x
  | Parser.ExprArithmetic (T_ARITHMETIC "+", left, right) ->
      Printf.sprintf "\tMOV X1, %s\n\tADD X0, X1, %s\n"
        (process_expression left) (process_expression right)
  | _ -> ""

let process_statement = function
  | Parser.AssignmentStatement (_, _, _, expr) -> process_expression expr

let rec process_statements acc = function
  | [] -> acc
  | x :: xs -> process_statements (acc ^ process_statement x) xs

let create_exit_function = "\tMOV X8, #93\n\tSVC 0\n"

let generate_assembly = function
  | Parser.Program statements ->
      create_start_function
      ^ process_statements "" statements
      ^ create_exit_function
