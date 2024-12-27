type token_type =
  | T_ARITHMETIC
  | T_COMPARISON
  | T_STRING
  | T_NUMBER
  | T_VARIABLE
  | T_EQUALS
  | T_TYPE
  | T_OPEN_PAREN
  | T_CLOSE_PAREN
  | T_SEMI
  | T_IF
  | T_ELIF
  | T_ELSE
  | T_OPEN_BLOCK
  | T_CLOSE_BLOCK
  | T_PRINT_FUNCTION

type token = { t_type : token_type; t_str : string; line_num : int }

let get_token_string t =
  match t.t_type with
  | T_STRING -> Printf.sprintf "(STRING:%s)" t.t_str
  | T_NUMBER -> Printf.sprintf "(NUMBER:%s)" t.t_str
  | T_VARIABLE -> Printf.sprintf "(VARIABLE:%s)" t.t_str
  | T_ARITHMETIC -> Printf.sprintf "(ARITHMETIC:%s)" t.t_str
  | T_COMPARISON -> Printf.sprintf "(COMPARISION:%s)" t.t_str
  | T_TYPE -> Printf.sprintf "(TYPE:%s)" t.t_str
  | T_SEMI -> "(SEMI)"
  | T_OPEN_PAREN -> "(OPEN_PAREN)"
  | T_CLOSE_PAREN -> "(CLOSE_PAREN)"
  | T_EQUALS -> "(EQUALS)"
  | T_IF -> "(IF)"
  | T_ELIF -> "(ELIF)"
  | T_ELSE -> "(ELSE)"
  | T_OPEN_BLOCK -> "(T_OPEN_BLOCK)"
  | T_CLOSE_BLOCK -> "(T_CLOSE_BLOCK)"
  | T_PRINT_FUNCTION -> "(T_PRINT_FUNCTION)"

let rec display_tokens = function
  | [] -> Printf.printf "\n"
  | x :: xs ->
      Printf.printf "%s" (get_token_string x);
      display_tokens xs
