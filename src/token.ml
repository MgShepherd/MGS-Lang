type token = T_EQUALS | T_ADD | T_DIGIT of int | T_SEMI

let get_token_string = function
  | T_EQUALS -> "="
  | T_ADD -> "+"
  | T_DIGIT x -> Printf.sprintf "%d" x
  | T_SEMI -> ";"

let rec display_tokens = function
  | [] -> ()
  | x :: xs ->
      Printf.printf "%s" (get_token_string x);
      display_tokens xs
