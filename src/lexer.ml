open Token

let parse_token token =
  match token with
  | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0' ->
      { value = token; t_type = T_DIGIT }
  | '+' | '-' | '*' | '/' -> { value = token; t_type = T_OPERATOR }
  | _ -> raise (Failure "Unknown token type")

let rec process_tokens tokens file =
  let char = input_char file in
  try
    match char with
    | '\n' | ' ' -> process_tokens tokens file
    | x -> process_tokens (parse_token x :: tokens) file
  with _ -> tokens

let process_file file = List.rev (process_tokens [] file)
