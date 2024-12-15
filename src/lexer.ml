open Token

let parse_value x =
  if Str.string_match (Str.regexp "[0-9]+") x 0 then T_NUMBER x
  else if Str.string_match (Str.regexp "\".*\"") x 0 then T_STRING x
  else T_VARIABLE x

let parse_token token =
  let token_str = String.of_seq (List.to_seq token) in
  match token_str with
  | "+" | "-" | "*" | "/" -> T_ARITHMETIC token_str
  | ">" | ">=" | "<=" | "<" | "==" -> T_COMPARISON token_str
  | "i16" -> T_TYPE token_str
  | "if" -> T_IF
  | ";" -> T_SEMI
  | "(" -> T_OPEN_PAREN
  | ")" -> T_CLOSE_PAREN
  | "{" -> T_OPEN_BLOCK
  | "}" -> T_CLOSE_BLOCK
  | "=" -> T_EQUALS
  | "print" -> T_PRINT_FUNCTION
  | "" -> raise (Failure "Empty Token\n")
  | x -> parse_value x

let rec process_double_quotes acc_chars file =
  let char = input_char file in
  try
    match char with
    | '"' -> List.rev ('"' :: acc_chars)
    | x -> process_double_quotes (x :: acc_chars) file
  with _ -> raise (Failure "Unable to process quote string")

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
    | '"' ->
        let str_val = process_double_quotes [ '"' ] file in
        process_tokens [] (parse_token str_val :: tokens) file
    | x -> process_tokens (x :: acc_token) tokens file
  with
  | End_of_file ->
      if List.length acc_token > 0 then
        parse_token (List.rev acc_token) :: tokens
      else tokens
  | e -> raise e

let process_file file = List.rev (process_tokens [] [] file)
