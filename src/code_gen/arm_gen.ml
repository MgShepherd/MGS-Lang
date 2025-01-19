open Parser
open Common.Logger
open Common.Token
module StringMap = Map.Make (String)

let sys_exit = 93
let sys_write = 64
let stack_alignment = 16
let frame_pointer_register = 29

let create_start_function =
  Printf.sprintf ".global _start\n_start:\n\tMOV X%d, SP\n"
    frame_pointer_register

let process_arithmetic_operator = function
  | '+' -> "ADD"
  | '-' -> "SUB"
  | _ -> ""

let process_comparison_operator = function
  | "<" -> "LT"
  | ">" -> "GT"
  | "==" -> "EQ"
  | ">=" -> "GE"
  | "<=" -> "LE"
  | _ -> ""

let get_value_from_map map var_token =
  try StringMap.find var_token.t_str map
  with _ ->
    let msg = Printf.sprintf "Undefined variable: %s" var_token.t_str in
    fatal_err_with_line msg var_token.line_num

let process_variable current_reg var_token stack_vars =
  let value = get_value_from_map stack_vars var_token in
  Printf.sprintf "\tLDR W%d, [X%d, #-%d]\n" current_reg frame_pointer_register
    value

let rec process_expression current_reg expr stack_vars label_num block_num =
  match expr with
  | ExprToken x -> (
      match x.t_type with
      | T_NUMBER -> Printf.sprintf "\tMOV X%d, #%s\n" current_reg x.t_str
      | T_VARIABLE -> process_variable current_reg x stack_vars
      | T_ELSE -> Printf.sprintf "\tB _%dblock%d\n" block_num label_num
      | _ -> "")
  | ExprArithmetic (operator, left, right) ->
      Printf.sprintf "%s%s\t%s X%d, X%d, X%d\n"
        (process_expression (current_reg + 1) left stack_vars label_num
           block_num)
        (process_expression (current_reg + 2) right stack_vars label_num
           block_num)
        (process_arithmetic_operator operator.t_str.[0])
        current_reg (current_reg + 1) (current_reg + 2)
  | ExprComparison (operator, left, right) ->
      Printf.sprintf "%s%s\tCMP X%d, X%d\n\tB.%s _%dblock%d\n"
        (process_expression current_reg left stack_vars label_num block_num)
        (process_expression (current_reg + 1) right stack_vars label_num
           block_num)
        current_reg (current_reg + 1)
        (process_comparison_operator operator.t_str)
        block_num label_num

(*** For now can only assign 16 bit values***)
let process_declaration stack_vars var_name label_num expr =
  let new_stack =
    StringMap.add var_name
      ((StringMap.cardinal stack_vars + 1) * stack_alignment)
      stack_vars
  in
  ( new_stack,
    Printf.sprintf "%s\tSUB SP, SP, #16\n\tSTR W0, [SP]\n"
      (process_expression 0 expr stack_vars label_num 0) )

let process_assignment stack_vars var_token label_num expr =
  let processed_expr = process_expression 0 expr stack_vars label_num 0 in
  let variable_location = get_value_from_map stack_vars var_token in
  let store_var =
    Printf.sprintf "\tSTR W0, [X%d, #-%d]\n" frame_pointer_register
      variable_location
  in
  processed_expr ^ store_var

let process_compound_assignment stack_vars var_token label_num expr op_str =
  let processed_expr = process_expression 1 expr stack_vars label_num 0 in
  let variable_location = get_value_from_map stack_vars var_token in
  let load_variable =
    Printf.sprintf "\tLDR W0, [X%d, #-%d]\n" frame_pointer_register
      variable_location
  in
  let op_val = process_arithmetic_operator op_str.[0] in
  let compound_update = Printf.sprintf "\t%s W0, W0, W1\n" op_val in
  let store_variable =
    Printf.sprintf "\tSTR W0, [X%d, #-%d]\n" frame_pointer_register
      variable_location
  in
  processed_expr ^ load_variable ^ compound_update ^ store_variable

let rec process_if_comparisions index stack_vars label_num processed_comparisons
    num_blocks = function
  | x :: xs ->
      let comp = process_expression 0 x stack_vars label_num index in
      process_if_comparisions (index + 1) stack_vars label_num
        (processed_comparisons ^ comp)
        num_blocks xs
  | [] ->
      Printf.sprintf "%s\tB _%dblock%d\n" processed_comparisons num_blocks
        label_num

let rec process_if_statements index constants stack_vars label_num num_blocks
    processed_statements = function
  | x :: xs ->
      let new_constants, _, statement =
        process_statements constants stack_vars (label_num + 1) "" x
      in
      let if_statement =
        Printf.sprintf "_%dblock%d:\n%s\tB _%dblock%d\n" index label_num
          statement num_blocks label_num
      in
      process_if_statements (index + 1) new_constants stack_vars label_num
        num_blocks
        (processed_statements ^ if_statement)
        xs
  | [] -> (constants, processed_statements)

and process_if_blocks constants stack_vars label_num blocks =
  let num_blocks = List.length blocks in
  let comparisions = List.map (fun (comp, _) -> comp) blocks in
  let statements = List.map (fun (_, statement) -> statement) blocks in
  let processed_comparisions =
    process_if_comparisions 0 stack_vars label_num "" num_blocks comparisions
  in
  let new_constants, processed_statements =
    process_if_statements 0 constants stack_vars label_num num_blocks ""
      statements
  in
  ( new_constants,
    stack_vars,
    label_num,
    Printf.sprintf "%s%s_%dblock%d:\n" processed_comparisions
      processed_statements num_blocks label_num )

and process_while_block constants stack_vars label_num expr statements =
  let processed_comp = process_expression 0 expr stack_vars label_num 1 in
  let new_constants, _, processed_statements =
    process_statements constants stack_vars (label_num + 1) "" statements
  in
  let while_loop =
    Printf.sprintf
      "_0block%d:\n%s\tB _2block%d\n_1block%d:\n%s\tB _0block%d\n_2block%d:\n"
      label_num processed_comp label_num label_num processed_statements
      label_num label_num
  in
  (new_constants, stack_vars, label_num, while_loop)

and handle_assignment stack_vars var_token label_num expr op =
  match op.t_type with
  | T_COMPOUND_ASSIGNMENT ->
      process_compound_assignment stack_vars var_token label_num expr op.t_str
  | _ -> process_assignment stack_vars var_token label_num expr

and process_statement constants stack_vars label_num = function
  | DeclarationStatement (_, v, _, expr) ->
      let new_stack, statements =
        process_declaration stack_vars v.t_str label_num expr
      in
      (constants, new_stack, label_num, statements)
  | AssignmentStatement (v, op, expr) ->
      let statements = handle_assignment stack_vars v label_num expr op in
      (constants, stack_vars, label_num, statements)
  | IfStatement blocks ->
      process_if_blocks constants stack_vars label_num blocks
  | WhileStatement (expr, statements) ->
      process_while_block constants stack_vars label_num expr statements
  | PrintStatement x ->
      let const_name = Printf.sprintf "V%d" (List.length constants) in
      ( (const_name, x.t_str) :: constants,
        stack_vars,
        label_num,
        Printf.sprintf
          "\tMOV X0, #1\n\tLDR X1, =%s\n\tMOV X2, #%d\n\tMOV X8, #%d\n\tSVC 0\n"
          const_name (String.length x.t_str) sys_write )

and process_statements constants stack_vars label_num acc = function
  | [] -> (constants, stack_vars, acc)
  | x :: xs ->
      let new_constants, new_stack, new_label_num, statement =
        process_statement constants stack_vars label_num x
      in
      process_statements new_constants new_stack new_label_num (acc ^ statement)
        xs

let create_exit_function =
  Printf.sprintf "\tMOV X0, #0\n\tMOV X8, #%d\n\tSVC 0\n" sys_exit

let rec create_data_elements acc = function
  | [] -> acc
  | (k, v) :: xs ->
      create_data_elements
        (acc ^ Printf.sprintf "%s: .asciz %s\n.align 8\n" k v)
        xs

let create_data_section constants =
  Printf.sprintf ".data\n%s" (create_data_elements "" constants)

let generate_assembly = function
  | Program statements ->
      let constants, _, statements =
        process_statements [] StringMap.empty 0 "" statements
      in
      create_start_function ^ statements ^ create_exit_function
      ^ create_data_section constants
