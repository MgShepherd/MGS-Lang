type token_type = T_DIGIT | T_OPERATOR
type token = { value : char; t_type : token_type }

let token_type_to_str = function T_OPERATOR -> "Operator" | T_DIGIT -> "Digit"

let rec display_tokens = function
  | [] -> ()
  | x :: xs ->
      Printf.printf "%c: %s\n" x.value (token_type_to_str x.t_type);
      display_tokens xs
