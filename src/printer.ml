open Token
open Parser

let rec display_expression indent = function
  | ExprArithmetic (T_ARITHMETIC op, x, y) ->
      let new_indent = "\t" ^ indent in
      Printf.printf "%sArithmetic %s ->\n%s" indent op new_indent;
      display_expression new_indent x;
      Printf.printf "\n%s" new_indent;
      display_expression new_indent y;
      print_endline ""
  | ExprToken x -> Printf.printf "%s " (get_token_string x)
  | _ -> ()

let display_statement indent = function
  | AssignmentStatement (T_TYPE t, T_VALUE v, _, expression) ->
      Printf.printf "%sStatement -> %s %s ->\n" indent t v;
      display_expression ("\t" ^ indent) expression
  | _ -> ()

let display_program = function
  | Program statements ->
      Printf.printf "Program ->\n";
      List.iter (fun x -> display_statement "\t" x) statements;
      print_endline ""

let display_tree tree = display_program tree
