open Parser

let sys_exit = 93
let sys_write = 64
let create_start_function = ".global _start\n_start:\n"

let process_arithmetic_operator = function
  | "+" -> "ADD"
  | "-" -> "SUB"
  | _ -> ""

let process_comparison_operator = function "<" -> "LT" | ">" -> "GT" | _ -> ""

let rec process_expression current_reg expr =
  if current_reg > 30 then raise (Failure "Too many operands in expression")
  else
    match expr with
    | ExprToken (T_NUMBER x) -> Printf.sprintf "\tMOV X%d, #%s\n" current_reg x
    | ExprArithmetic (T_ARITHMETIC operator, left, right) ->
        Printf.sprintf "%s%s\t%s X%d, X%d, X%d\n"
          (process_expression (current_reg + 1) left)
          (process_expression (current_reg + 2) right)
          (process_arithmetic_operator operator)
          current_reg (current_reg + 1) (current_reg + 2)
    (*** Ignoring left for now due to hardcoding comparison with register X0***)
    | ExprComparison (T_COMPARISON operator, _left, right) ->
        Printf.sprintf "%s\tCMP X%d, X%d\n\tB.%s _ifbody\n\tB _endif\n"
          (process_expression (current_reg + 1) right)
          current_reg (current_reg + 1)
          (process_comparison_operator operator)
    | _ -> ""

(*** For now can only assign 16 bit values***)
let process_assignment current_reg expr =
  Printf.sprintf "%s\tSUB SP, SP, #16\n\tSTR W%d, [SP]\n"
    (process_expression 0 expr)
    current_reg

let rec process_statement data = function
  | AssignmentStatement (T_TYPE _t, T_VARIABLE _v, _, expr) ->
      (data, process_assignment 0 expr)
  | IfStatement (comparison, body) ->
      let new_data, statements = process_statements data "" body in
      ( new_data,
        Printf.sprintf "%s_ifbody:\n%s_endif:\n"
          (process_expression 0 comparison)
          statements )
  | PrintStatement (T_STRING x) ->
      let var_name = Printf.sprintf "V%d" (List.length data) in
      ( (var_name, x) :: data,
        Printf.sprintf
          "\tMOV X0, #1\n\tLDR X1, =%s\n\tMOV X2, #%d\n\tMOV X8, #%d\n\tSVC 0\n"
          var_name (String.length x) sys_write )
  | _ -> (data, "")

and process_statements data acc = function
  | [] -> (data, acc)
  | x :: xs ->
      let new_data, statement = process_statement data x in
      process_statements new_data (acc ^ statement) xs

let create_exit_function =
  Printf.sprintf "\tMOV X0, #0\n\tMOV X8, #%d\n\tSVC 0\n" sys_exit

let rec create_data_elements acc = function
  | [] -> acc
  | (k, v) :: xs ->
      create_data_elements (acc ^ Printf.sprintf "%s: .asciz %s\n" k v) xs

let create_data_section data =
  Printf.sprintf ".data\n%s" (create_data_elements "" data)

let generate_assembly = function
  | Program statements ->
      let data, statements = process_statements [] "" statements in
      create_start_function ^ statements ^ create_exit_function
      ^ create_data_section data
