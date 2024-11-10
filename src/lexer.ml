open Token

let parse_token token =
  match token with
  | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0' ->
      T_DIGIT (int_of_char token - int_of_char '0')
  | '+' -> T_ADD
  | ';' -> T_SEMI
  | '(' -> T_OPEN_PAREN
  | ')' -> T_CLOSE_PAREN
  | x ->
      let error_message =
        Printf.sprintf "Lexer Error: Unknown Token Type %c\n" x
      in
      raise (Failure error_message)

let rec process_tokens tokens file =
  let char = input_char file in
  try
    match char with
    | '\n' | ' ' -> process_tokens tokens file
    | x -> process_tokens (parse_token x :: tokens) file
  with
  | End_of_file -> tokens
  | e -> raise e

let process_file file = List.rev (process_tokens [] file)
