module StringMap = Map.Make (String)
open Parser
open Program_state
open Common.Token

let reg_order = [| "rax"; "rbx"; "rcx"; "rdx"; "r8"; "r9"; "r10"; "r11" |]

let create_start_function = {|
.global _start
_start:
  MOV %rsp, %rbp
|}

let create_exit_function status =
  Printf.sprintf {|
  MOV $60, %%rax
  MOV $%d, %%rdi
  syscall
|} status

let process_tok_expression reg_num tok =
  match tok.t_type with
  | T_NUMBER -> Printf.sprintf "\tMOV $%s, %%%s\n" tok.t_str reg_order.(reg_num)
  | _x -> ""

let rec process_arith_expression p_state reg_num _op left right =
  let left_expr = process_expression p_state reg_num left in
  let right_expr = process_expression p_state (reg_num + 1) right in
  let add_inst =
    Printf.sprintf "\tADD %%%s, %%%s\n"
      reg_order.(reg_num + 1)
      reg_order.(reg_num)
  in
  Printf.sprintf "%s%s%s" left_expr right_expr add_inst

and process_expression p_state reg_num = function
  | ExprToken x -> process_tok_expression reg_num x
  | ExprArithmetic (op, left, right) ->
      process_arith_expression p_state reg_num op left right
  | ExprComparison (_op, _left, _right) -> ""

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

let process_dec_statement p_state tok ex =
  let push_instr = Printf.sprintf "\tPUSH %%%s\n" reg_order.(0) in
  let p_expr = process_expression p_state 0 ex in
  let n_state = add_to_stack p_state tok.t_str in
  (n_state, p_expr ^ push_instr)

let process_statement p_state = function
  | DeclarationStatement (_, v, _, ex) -> process_dec_statement p_state v ex
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
