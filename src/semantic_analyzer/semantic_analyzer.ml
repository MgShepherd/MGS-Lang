open Parser
open Common.Token
open Common.Logger
module StringMap = Map.Make (String)

let convert_to_data_type tok =
  match tok.t_str with
  | "i8" -> I_8
  | "i16" -> I_16
  | x ->
      fatal_err_with_line
        (Printf.sprintf "Invalid variable type %s" x)
        tok.line_num

let add_var_to_map type_tok v_tok v_table =
  let v_type = convert_to_data_type type_tok in
  StringMap.add v_tok.t_str v_type v_table

let check_token_validity v_table tok =
  match tok.t_type with
  | T_VARIABLE ->
      if StringMap.find_opt tok.t_str v_table == None then
        fatal_err_with_line
          (Printf.sprintf "Undefined variable %s" tok.t_str)
          tok.line_num
  | _ -> ()

let rec check_expression_validity v_table = function
  | ExprArithmetic (_op, left, right) | ExprComparison (_op, left, right) ->
      check_expression_validity v_table left;
      check_expression_validity v_table right
  | ExprToken tok -> check_token_validity v_table tok

let rec check_if_expressions v_table = function
  | x :: xs ->
      check_expression_validity v_table x;
      check_if_expressions v_table xs
  | [] -> ()

let rec check_if_statements v_table = function
  | x :: xs ->
      let n_v_table = check_statements v_table x in
      check_if_statements n_v_table xs
  | [] -> v_table

and check_statement v_table = function
  | DeclarationStatement (t_type, t_var, _t_operator, ex) ->
      check_expression_validity v_table ex;
      add_var_to_map t_type t_var v_table
  | AssignmentStatement (_t_var, _t_operator, ex) ->
      check_expression_validity v_table ex;
      v_table
  | IfStatement (exprs, stmnts) ->
      let n_v_table = check_if_statements v_table stmnts in
      check_if_expressions v_table exprs;
      n_v_table
  | WhileStatement (ex, stmnts) ->
      let n_v_table = check_statements v_table stmnts in
      check_expression_validity v_table ex;
      n_v_table
  | PrintStatement _val -> v_table

and check_statements v_table = function
  | x :: xs ->
      let new_v_table = check_statement v_table x in
      check_statements new_v_table xs
  | [] -> v_table

let run_analyzer = function
  | Program statements -> check_statements StringMap.empty statements
