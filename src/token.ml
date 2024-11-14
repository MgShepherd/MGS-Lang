type token =
  | T_ARITHMETIC of string
  | T_VALUE of string
  | T_EQUALS
  | T_TYPE of string
  | T_OPEN_PAREN
  | T_CLOSE_PAREN
  | T_SEMI

let get_token_string = function
  | T_VALUE x -> Printf.sprintf "(VALUE:%s)" x
  | T_ARITHMETIC x -> Printf.sprintf "(ARITHMETIC:%s)" x
  | T_TYPE x -> Printf.sprintf "(TYPE:%s)" x
  | T_SEMI -> "(SEMI)"
  | T_OPEN_PAREN -> "(OPEN_PAREN)"
  | T_CLOSE_PAREN -> "(CLOSE_PAREN)"
  | T_EQUALS -> "(EQUALS)"

let rec display_tokens = function
  | [] -> Printf.printf "\n"
  | x :: xs ->
      Printf.printf "%s" (get_token_string x);
      display_tokens xs
