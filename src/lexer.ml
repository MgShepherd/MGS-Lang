open Token

let parse_token token =
  let token_str = String.of_seq (List.to_seq token) in
  match token_str with
  | "+" | "-" | "*" | "/" -> T_ARITHMETIC token_str
  | ">" | ">=" | "<=" | "<" -> T_COMPARISON token_str
  | "i8" -> T_TYPE token_str
  | "if" -> T_IF
  | ";" -> T_SEMI
  | "(" -> T_OPEN_PAREN
  | ")" -> T_CLOSE_PAREN
  | "{" -> T_OPEN_BLOCK
  | "}" -> T_CLOSE_BLOCK
  | "=" -> T_EQUALS
  | "" -> raise (Failure "Empty Token\n")
  | x -> T_VALUE x

let rec process_tokens acc_token tokens file =
  let char = input_char file in
  try
    match char with
    | '\n' | ' ' | '\t' ->
        if List.length acc_token > 0 then
          process_tokens [] (parse_token (List.rev acc_token) :: tokens) file
        else process_tokens [] tokens file
    | ';' | '(' | ')' ->
        if List.length acc_token > 0 then
          process_tokens []
            (parse_token [ char ] :: parse_token (List.rev acc_token) :: tokens)
            file
        else process_tokens [] (parse_token [ char ] :: tokens) file
    | x -> process_tokens (x :: acc_token) tokens file
  with
  | End_of_file ->
      if List.length acc_token > 0 then
        parse_token (List.rev acc_token) :: tokens
      else tokens
  | e -> raise e

let process_file file = List.rev (process_tokens [] [] file)
