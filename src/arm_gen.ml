open Parser
module StringMap = Map.Make (String)

let sys_exit = 93
let sys_write = 64
let stack_alignment = 16
let frame_pointer_register = 29

let create_start_function =
  Printf.sprintf ".global _start\n_start:\n\tMOV X%d, SP\n"
    frame_pointer_register

let process_arithmetic_operator = function
  | "+" -> "ADD"
  | "-" -> "SUB"
  | _ -> ""

let process_comparison_operator = function "<" -> "LT" | ">" -> "GT" | _ -> ""

let get_value_from_map map key =
  try StringMap.find key map
  with _ ->
    let error_message = Printf.sprintf "Unable to find key %s in map\n" key in
    raise (Failure error_message)

let process_variable current_reg var_name stack_vars =
  let value = get_value_from_map stack_vars var_name in
  Printf.sprintf "\tLDR W%d, [X%d, #-%d]\n" current_reg frame_pointer_register
    value

let rec process_expression current_reg expr stack_vars =
  if current_reg > 30 then raise (Failure "Too many operands in expression")
  else
    match expr with
    | ExprToken (T_NUMBER x) -> Printf.sprintf "\tMOV X%d, #%s\n" current_reg x
    | ExprToken (T_VARIABLE x) -> process_variable current_reg x stack_vars
    | ExprArithmetic (T_ARITHMETIC operator, left, right) ->
        Printf.sprintf "%s%s\t%s X%d, X%d, X%d\n"
          (process_expression (current_reg + 1) left stack_vars)
          (process_expression (current_reg + 2) right stack_vars)
          (process_arithmetic_operator operator)
          current_reg (current_reg + 1) (current_reg + 2)
    | ExprComparison (T_COMPARISON operator, left, right) ->
        Printf.sprintf "%s%s\tCMP X%d, X%d\n\tB.%s _ifbody\n\tB _endif\n"
          (process_expression current_reg left stack_vars)
          (process_expression (current_reg + 1) right stack_vars)
          current_reg (current_reg + 1)
          (process_comparison_operator operator)
    | _ -> ""

(*** For now can only assign 16 bit values***)
let process_assignment stack_vars var_name expr =
  let new_stack =
    StringMap.add var_name
      ((StringMap.cardinal stack_vars + 1) * stack_alignment)
      stack_vars
  in
  ( new_stack,
    Printf.sprintf "%s\tSUB SP, SP, #16\n\tSTR W0, [SP]\n"
      (process_expression 0 expr stack_vars) )

let rec process_statement constants stack_vars = function
  | AssignmentStatement (T_TYPE _t, T_VARIABLE v, _, expr) ->
      let new_stack, statements = process_assignment stack_vars v expr in
      (constants, new_stack, statements)
  | IfStatement (comparison, body) ->
      let new_data, new_stack, statements =
        process_statements constants stack_vars "" body
      in
      ( new_data,
        new_stack,
        Printf.sprintf "%s_ifbody:\n%s_endif:\n"
          (process_expression 0 comparison stack_vars)
          statements )
  | PrintStatement (T_STRING x) ->
      let const_name = Printf.sprintf "V%d" (List.length constants) in
      ( (const_name, x) :: constants,
        stack_vars,
        Printf.sprintf
          "\tMOV X0, #1\n\tLDR X1, =%s\n\tMOV X2, #%d\n\tMOV X8, #%d\n\tSVC 0\n"
          const_name (String.length x) sys_write )
  | _ -> (constants, stack_vars, "")

and process_statements constants stack_vars acc = function
  | [] -> (constants, stack_vars, acc)
  | x :: xs ->
      let new_constants, new_stack, statement =
        process_statement constants stack_vars x
      in
      process_statements new_constants new_stack (acc ^ statement) xs

let create_exit_function =
  Printf.sprintf "\tMOV X0, #0\n\tMOV X8, #%d\n\tSVC 0\n" sys_exit

let rec create_data_elements acc = function
  | [] -> acc
  | (k, v) :: xs ->
      create_data_elements (acc ^ Printf.sprintf "%s: .asciz %s\n" k v) xs

let create_data_section constants =
  Printf.sprintf ".data\n%s" (create_data_elements "" constants)

let generate_assembly = function
  | Program statements ->
      let constants, stack_vars, statements =
        process_statements [] StringMap.empty "" statements
      in
      Printer.display_map stack_vars;
      create_start_function ^ statements ^ create_exit_function
      ^ create_data_section constants
