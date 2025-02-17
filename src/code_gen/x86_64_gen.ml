module StringMap = Map.Make (String)
open Parser
open Program_state
open Common.Token

let reg_order_8 = [| "al"; "bl"; "cl"; "dl"; "r8b"; "r9b"; "r10b"; "r11b" |]
let reg_order_16 = [| "ax"; "bx"; "cx"; "dx"; "r8w"; "r9w"; "r10w"; "r11w" |]
let mem_align = 16

let create_start_function = {|
.global _start
_start:
  MOV %rsp, %rbp
|}

let create_exit_function status =
  Printf.sprintf {|  MOV $60, %%rax
  MOV $%d, %%rdi
  syscall
|} status

let process_comp_operator = function
  | "<" -> "L"
  | ">" -> "G"
  | "==" -> "E"
  | ">=" -> "GE"
  | "<=" -> "LE"
  | _ -> ""

let get_reg reg_num = function
  | I_8 -> reg_order_8.(reg_num)
  | I_16 -> reg_order_16.(reg_num)

let get_instr_suffix = function I_8 -> "B" | I_16 -> "W"

let process_variable p_state reg_num tok =
  let offset = get_stack_var p_state tok in
  let v_type = StringMap.find tok.t_str p_state.v_table in
  let reg = get_reg reg_num v_type in
  let suffix = get_instr_suffix v_type in
  Printf.sprintf {|  MOV%s -%d(%%rbp), %%%s
|} suffix offset reg

let process_tok_expression p_state reg_num tok block_num =
  match tok.t_type with
  | T_NUMBER ->
      Printf.sprintf {|  MOV $%s, %%%s
|} tok.t_str reg_order_16.(reg_num)
  | T_ELSE -> Printf.sprintf {|  JMP _%dblock%d
|} p_state.label_num block_num
  | T_VARIABLE -> process_variable p_state reg_num tok
  | _x -> ""

let rec process_arith_expression p_state reg_num _op left right block_num =
  let left_expr = process_expression p_state reg_num block_num left in
  let right_expr = process_expression p_state (reg_num + 1) block_num right in
  let add_inst =
    Printf.sprintf "\tADD %%%s, %%%s\n"
      reg_order_16.(reg_num + 1)
      reg_order_16.(reg_num)
  in
  Printf.sprintf "%s%s%s" left_expr right_expr add_inst

and process_comp_expression p_state reg_num op left right block_num =
  let left_expr = process_expression p_state reg_num block_num left in
  let right_expr = process_expression p_state (reg_num + 1) block_num right in
  let cmp_block =
    Printf.sprintf {|  CMP %%%s, %%%s
  J%s _%dblock%d
|}
      reg_order_16.(reg_num + 1)
      reg_order_16.(reg_num)
      (process_comp_operator op.t_str)
      p_state.label_num block_num
  in
  Printf.sprintf "%s%s%s" left_expr right_expr cmp_block

and process_expression p_state reg_num block_num = function
  | ExprToken x -> process_tok_expression p_state reg_num x block_num
  | ExprArithmetic (op, left, right) ->
      process_arith_expression p_state reg_num op left right block_num
  | ExprComparison (op, left, right) ->
      process_comp_expression p_state reg_num op left right block_num

let rec process_if_conds p_state acc_conds block_num = function
  | x :: xs ->
      let cond = process_expression p_state 0 block_num x in
      process_if_conds p_state (acc_conds ^ cond) (block_num + 1) xs
  | [] ->
      let jump_always =
        Printf.sprintf {|  JMP _%dblock%d
|} p_state.label_num block_num
      in
      acc_conds ^ jump_always

let process_print_statement p_state tok =
  let c_name = Printf.sprintf "V%d" (List.length p_state.constants) in
  let u_state =
    { p_state with constants = (c_name, tok.t_str) :: p_state.constants }
  in
  let pr_string =
    Printf.sprintf
      {|  MOV $1, %%rdi
  MOV $%s, %%rsi
  MOV $%d, %%rdx
  MOV $1, %%rax
  syscall
|}
      c_name (String.length tok.t_str)
  in
  (u_state, pr_string)

let process_dec_statement p_state tok ex =
  let push_instr = Printf.sprintf {|  PUSH %%r%s
|} reg_order_16.(0) in
  let p_expr = process_expression p_state 0 0 ex in
  let n_state = add_to_stack p_state tok.t_str in
  (n_state, p_expr ^ push_instr)

let rec process_statement p_state = function
  | DeclarationStatement (_, v, _, ex) -> process_dec_statement p_state v ex
  | AssignmentStatement (_v, _op, _expr) -> (p_state, "\tAssignmentStatement\n")
  | IfStatement (conds, blocks) -> process_if_statement p_state conds blocks
  | WhileStatement (_expr, _statements) -> (p_state, "\tWhileStatement\n")
  | PrintStatement x -> process_print_statement p_state x

and process_statements p_state acc = function
  | [] -> (p_state, acc)
  | x :: xs ->
      let new_state, statement = process_statement p_state x in
      process_statements new_state (acc ^ statement) xs

and process_if_blocks p_state acc_blocks block_num num_blocks = function
  | x :: xs ->
      let label = Printf.sprintf "_%dblock%d:\n" p_state.label_num block_num in
      let u_state =
        {
          p_state with
          label_num = p_state.next_label;
          next_label = p_state.next_label + 1;
        }
      in
      let block_state, statements = process_statements u_state "" x in
      let jump_state =
        Printf.sprintf {|  JMP _%dblock%d
|} p_state.label_num num_blocks
      in
      let n_blocks = acc_blocks ^ label ^ statements ^ jump_state in
      let n_state =
        {
          p_state with
          constants = block_state.constants;
          next_label = block_state.next_label;
        }
      in
      process_if_blocks n_state n_blocks (block_num + 1) num_blocks xs
  | [] ->
      let end_label =
        Printf.sprintf "_%dblock%d:\n" p_state.label_num block_num
      in
      (p_state, acc_blocks ^ end_label)

and process_if_statement p_state conds blocks =
  let num_blocks = List.length blocks in
  let p_conds = process_if_conds p_state "" 0 conds in
  let if_state, p_blocks = process_if_blocks p_state "" 0 num_blocks blocks in
  let n_state =
    {
      p_state with
      label_num = if_state.next_label;
      next_label = if_state.next_label + 1;
      constants = if_state.constants;
    }
  in
  (n_state, p_conds ^ p_blocks)

let rec define_constants acc = function
  | (c_name, c_val) :: xs ->
      let con_def =
        Printf.sprintf {|%s: .ascii %s
.align %d
|} c_name c_val mem_align
      in
      define_constants (con_def ^ acc) xs
  | [] -> acc

let create_data_sec constants = ".data\n" ^ define_constants "" constants

let generate_assembly v_table = function
  | Program statements ->
      let start_state =
        {
          stack = StringMap.empty;
          label_num = 0;
          next_label = 1;
          constants = [];
          v_table;
        }
      in
      let p_state, str_statements =
        process_statements start_state "" statements
      in
      create_start_function ^ str_statements ^ create_exit_function 0
      ^ create_data_sec p_state.constants
