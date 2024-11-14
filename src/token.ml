type token =
  | T_ARITHMETIC of string
  | T_VALUE of string
  | T_OPEN_PAREN
  | T_CLOSE_PAREN
  | T_SEMI

let get_token_string = function
  | T_ARITHMETIC x | T_VALUE x -> x
  | T_SEMI -> ";"
  | T_OPEN_PAREN -> "("
  | T_CLOSE_PAREN -> ")"

let rec display_tokens = function
  | [] -> ()
  | x :: xs ->
      Printf.printf "%s" (get_token_string x);
      display_tokens xs
