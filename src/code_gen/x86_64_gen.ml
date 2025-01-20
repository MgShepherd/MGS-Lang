module StringMap = Map.Make (String)
open Parser
open Program_state

let create_start_function = ".global _start\n_start:\n"

let create_exit_function status =
  Printf.sprintf "\tMOV $60, %%rax\n\tMOV $%d, %%rdi\n\tsyscall\n" status

let process_statement p_state = function
  | DeclarationStatement (_, _v, _, _expr) -> (p_state, "\tDeclStatement\n")
  | AssignmentStatement (_v, _op, _expr) -> (p_state, "\tAssignmentStatement\n")
  | IfStatement _blocks -> (p_state, "\tIfStatement\n")
  | WhileStatement (_expr, _statements) -> (p_state, "\tWhileStatement\n")
  | PrintStatement _x -> (p_state, "\tPrintStatement\n")

let rec process_statements p_state acc = function
  | [] -> (p_state, acc)
  | x :: xs ->
      let new_state, statement = process_statement p_state x in
      process_statements new_state (acc ^ statement) xs

let generate_assembly = function
  | Program statements ->
      let start_state =
        { stack = StringMap.empty; label_num = 0; constants = [] }
      in
      let _p_state, str_statements =
        process_statements start_state "" statements
      in
      create_start_function ^ str_statements ^ create_exit_function 0
