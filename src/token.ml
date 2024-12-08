type token =
  | T_ARITHMETIC of string
  | T_COMPARISON of string
  | T_STRING of string
  | T_NUMBER of string
  | T_VARIABLE of string
  | T_EQUALS
  | T_TYPE of string
  | T_OPEN_PAREN
  | T_CLOSE_PAREN
  | T_SEMI
  | T_IF
  | T_OPEN_BLOCK
  | T_CLOSE_BLOCK
  | T_PRINT_FUNCTION

let get_token_string = function
  | T_STRING x -> Printf.sprintf "(STRING:%s)" x
  | T_NUMBER x -> Printf.sprintf "(NUMBER:%s)" x
  | T_VARIABLE x -> Printf.sprintf "(VARIABLE:%s)" x
  | T_ARITHMETIC x -> Printf.sprintf "(ARITHMETIC:%s)" x
  | T_COMPARISON x -> Printf.sprintf "(COMPARISION:%s)" x
  | T_TYPE x -> Printf.sprintf "(TYPE:%s)" x
  | T_SEMI -> "(SEMI)"
  | T_OPEN_PAREN -> "(OPEN_PAREN)"
  | T_CLOSE_PAREN -> "(CLOSE_PAREN)"
  | T_EQUALS -> "(EQUALS)"
  | T_IF -> "(IF)"
  | T_OPEN_BLOCK -> "(T_OPEN_BLOCK)"
  | T_CLOSE_BLOCK -> "(T_CLOSE_BLOCK)"
  | T_PRINT_FUNCTION -> "(T_PRINT_FUNCTION)"

let rec display_tokens = function
  | [] -> Printf.printf "\n"
  | x :: xs ->
      Printf.printf "%s" (get_token_string x);
      display_tokens xs
