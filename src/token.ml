type token =
  | T_ARITHMETIC of char
  | T_DIGIT of int
  | T_OPEN_PAREN
  | T_CLOSE_PAREN
  | T_SEMI

let get_token_string = function
  | T_ARITHMETIC x -> Printf.sprintf "%c" x
  | T_DIGIT x -> Printf.sprintf "%d" x
  | T_SEMI -> ";"
  | T_OPEN_PAREN -> "("
  | T_CLOSE_PAREN -> ")"

let rec display_tokens = function
  | [] -> ()
  | x :: xs ->
      Printf.printf "%s" (get_token_string x);
      display_tokens xs
