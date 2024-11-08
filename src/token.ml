type token_type = T_OPERATOR | T_VALUE | T_TYPE | T_SEMI | T_BRACKET
type token = { value : string; t_type : token_type }

let token_type_to_str = function
  | T_OPERATOR -> "Operator"
  | T_VALUE -> "Value"
  | T_TYPE -> "Type"
  | T_SEMI -> "Semicolon"
  | T_BRACKET -> "Bracket"

let rec display_tokens = function
  | [] -> ()
  | x :: xs ->
      print_endline (x.value ^ " " ^ token_type_to_str x.t_type);
      display_tokens xs
