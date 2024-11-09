type token = T_EQUALS | T_ADD | T_DIGIT of int

let print_token = function
  | T_EQUALS -> print_endline "="
  | T_ADD -> print_endline "+"
  | T_DIGIT x -> Printf.printf "%d" x

let rec display_tokens = function
  | [] -> ()
  | x :: xs ->
      print_token x;
      display_tokens xs
