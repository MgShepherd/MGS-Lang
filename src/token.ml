type token_type = T_DIGIT | T_OPERATOR
type token = { value : char; t_type : token_type }

let token_type_to_str = function T_OPERATOR -> "Operator" | T_DIGIT -> "Digit"

let print_token x =
  Printf.printf "(%s: %c)" (token_type_to_str x.t_type) x.value

let rec display_tokens = function
  | [] -> ()
  | x :: xs ->
      print_token x;
      display_tokens xs
