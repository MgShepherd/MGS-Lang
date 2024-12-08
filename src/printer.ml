open Token
open Parser

let rec display_expression indent = function
  | ExprArithmetic (T_ARITHMETIC op, x, y) ->
      let new_indent = "\t" ^ indent in
      Printf.printf "%sArithmetic %s ->\n%s" indent op new_indent;
      display_expression new_indent x;
      Printf.printf "\n%s" new_indent;
      display_expression new_indent y;
      Printf.printf "\n\t"
  | ExprComparison (T_COMPARISON op, x, y) ->
      let new_indent = "\t" ^ indent in
      Printf.printf "%sComparison %s ->\n%s" indent op new_indent;
      display_expression new_indent x;
      Printf.printf "\n%s" new_indent;
      display_expression new_indent y;
      Printf.printf "\n\t"
  | ExprToken x -> Printf.printf "%s " (get_token_string x)
  | _ -> ()

let rec display_statement indent = function
  | AssignmentStatement (T_TYPE t, T_VARIABLE v, _, expression) ->
      Printf.printf "%sStatement -> %s %s ->\n\t" indent t v;
      display_expression ("\t" ^ indent) expression
  | IfStatement (comparison, body) ->
      let new_indent = "\t\t" ^ indent in
      Printf.printf "IfStatement -> \n";
      display_expression new_indent comparison;
      Printf.printf "\n%sBody ->\n\t" new_indent;
      List.iter (fun x -> display_statement new_indent x) body
  | PrintStatement x ->
      Printf.printf "PrintStatement -> \n\t\t";
      display_expression ("\t" ^ indent) (ExprToken x)
  | _ -> ()

let display_program = function
  | Program statements ->
      Printf.printf "Program ->\n";
      List.iter (fun x -> display_statement "\t" x) statements;
      print_endline ""

let display_tree tree = display_program tree
