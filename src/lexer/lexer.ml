open Common.Token

let parse_value x =
  if Str.string_match (Str.regexp "[0-9]+") x 0 then T_NUMBER
  else if Str.string_match (Str.regexp "\".*\"") x 0 then T_STRING
  else T_VARIABLE

let get_t_type t_str =
  match t_str with
  | "+" | "-" | "*" | "/" -> T_ARITHMETIC
  | "+=" | "-=" -> T_COMPOUND_ASSIGNMENT
  | ">" | ">=" | "<=" | "<" | "==" -> T_COMPARISON
  | "i16" -> T_TYPE
  | "if" -> T_IF
  | "elif" -> T_ELIF
  | "else" -> T_ELSE
  | ";" -> T_SEMI
  | "(" -> T_OPEN_PAREN
  | ")" -> T_CLOSE_PAREN
  | "{" -> T_OPEN_BLOCK
  | "}" -> T_CLOSE_BLOCK
  | "=" -> T_EQUALS
  | "print" -> T_PRINT_FUNCTION
  | "while" -> T_WHILE
  | "" -> raise (Failure "Empty Token\n")
  | x -> parse_value x

let parse_token t line_num =
  let t_str = String.of_seq (List.to_seq t) in
  let t_type = get_t_type t_str in
  { t_type; t_str; line_num }

let rec process_double_quotes acc_chars file =
  let char = input_char file in
  try
    match char with
    | '"' -> List.rev ('"' :: acc_chars)
    | x -> process_double_quotes (x :: acc_chars) file
  with _ -> raise (Failure "Unable to process quote string")

let rec process_tokens acc_token tokens line_num file =
  let char = input_char file in
  try
    match char with
    | ' ' | '\t' ->
        if List.length acc_token > 0 then
          process_tokens []
            (parse_token (List.rev acc_token) line_num :: tokens)
            line_num file
        else process_tokens [] tokens line_num file
    | '\n' -> process_tokens acc_token tokens (line_num + 1) file
    | ';' | '(' | ')' ->
        if List.length acc_token > 0 then
          process_tokens []
            (parse_token [ char ] line_num
            :: parse_token (List.rev acc_token) line_num
            :: tokens)
            line_num file
        else
          process_tokens []
            (parse_token [ char ] line_num :: tokens)
            line_num file
    | '"' ->
        let str_val = process_double_quotes [ '"' ] file in
        process_tokens [] (parse_token str_val line_num :: tokens) line_num file
    | x -> process_tokens (x :: acc_token) tokens line_num file
  with
  | End_of_file ->
      if List.length acc_token > 0 then
        parse_token (List.rev acc_token) line_num :: tokens
      else tokens
  | e -> raise e

let process_file file = List.rev (process_tokens [] [] 1 file)
