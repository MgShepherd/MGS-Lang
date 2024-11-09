type token = T_EQUALS | T_ADD | T_DIGIT of int

let get_token_string = function
  | T_EQUALS -> "="
  | T_ADD -> "+"
  | T_DIGIT x -> Printf.sprintf "%d" x

let rec display_tokens = function
  | [] -> ()
  | x :: xs ->
      Printf.printf "%s" (get_token_string x);
      display_tokens xs
