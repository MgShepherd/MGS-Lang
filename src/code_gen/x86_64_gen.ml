module StringMap = Map.Make (String)
open Parser
open Program_state
open Common.Token

let create_start_function = {|
.global _start
_start:
|}

let create_exit_function status =
  Printf.sprintf {|
  MOV $60, %%rax
  MOV $%d, %%rdi
  syscall
|} status

let process_print_statement p_state tok =
  let c_name = Printf.sprintf "V%d" (List.length p_state.constants) in
  let u_state =
    { p_state with constants = (c_name, tok.t_str) :: p_state.constants }
  in
  let pr_string =
    Printf.sprintf
      {|
  MOV $1, %%rdi
  MOV $%s, %%rsi
  MOV $%d, %%rdx
  MOV $1, %%rax
  syscall
|}
      c_name (String.length tok.t_str)
  in
  (u_state, pr_string)

let process_statement p_state = function
  | DeclarationStatement (_, _v, _, _expr) -> (p_state, "\tDeclStatement\n")
  | AssignmentStatement (_v, _op, _expr) -> (p_state, "\tAssignmentStatement\n")
  | IfStatement _blocks -> (p_state, "\tIfStatement\n")
  | WhileStatement (_expr, _statements) -> (p_state, "\tWhileStatement\n")
  | PrintStatement x -> process_print_statement p_state x

let rec process_statements p_state acc = function
  | [] -> (p_state, acc)
  | x :: xs ->
      let new_state, statement = process_statement p_state x in
      process_statements new_state (acc ^ statement) xs

let rec define_constants acc = function
  | (c_name, c_val) :: xs ->
      let con_def = Printf.sprintf "%s: .ascii %s\n" c_name c_val in
      define_constants (con_def ^ acc) xs
  | [] -> acc

let create_data_sec constants = ".data\n" ^ define_constants "" constants

let generate_assembly = function
  | Program statements ->
      let start_state =
        { stack = StringMap.empty; label_num = 0; constants = [] }
      in
      let p_state, str_statements =
        process_statements start_state "" statements
      in
      create_start_function ^ str_statements ^ create_exit_function 0
      ^ create_data_sec p_state.constants
