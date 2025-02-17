open Common.Token
open Common.Logger
open Common.Utils

let parse_value x =
  if Str.string_match (Str.regexp "[0-9]+") x 0 then T_NUMBER
  else if Str.string_match (Str.regexp "\".*\"") x 0 then T_STRING
  else T_VARIABLE

let get_t_type t_str =
  match t_str with
  | "+" | "-" | "*" | "/" -> T_ARITHMETIC
  | "+=" | "-=" -> T_COMPOUND_ASSIGNMENT
  | ">" | ">=" | "<=" | "<" | "==" -> T_COMPARISON
  | "i16" | "i8" -> T_TYPE
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
  | x -> parse_value x

let parse_token t line_num =
  let t_str = String.of_seq (List.to_seq t) in
  let t_type = get_t_type t_str in
  { t_type; t_str; line_num }

let rec process_double_quotes acc_chars = function
  | '"' :: xs -> (List.rev ('"' :: acc_chars), xs)
  | x :: xs -> process_double_quotes (x :: acc_chars) xs
  | [] -> fatal_err "Unclosed string quotes"

let rec process_tokens_impl acc_token tokens line_num = function
  | char :: xs -> (
      match char with
      | ' ' | '\t' ->
          if List.length acc_token > 0 then
            let n_tokens =
              parse_token (List.rev acc_token) line_num :: tokens
            in
            process_tokens_impl [] n_tokens line_num xs
          else process_tokens_impl [] tokens line_num xs
      | '\n' -> process_tokens_impl acc_token tokens (line_num + 1) xs
      | ';' | '(' | ')' ->
          if List.length acc_token > 0 then
            let n_tokens =
              parse_token [ char ] line_num
              :: parse_token (List.rev acc_token) line_num
              :: tokens
            in
            process_tokens_impl [] n_tokens line_num xs
          else
            let n_tokens = parse_token [ char ] line_num :: tokens in
            process_tokens_impl [] n_tokens line_num xs
      | '"' ->
          let str_val, remaining = process_double_quotes [ '"' ] xs in
          let n_tokens = parse_token str_val line_num :: tokens in
          process_tokens_impl [] n_tokens line_num remaining
      | x -> process_tokens_impl (x :: acc_token) tokens line_num xs)
  | [] ->
      if List.length acc_token > 0 then
        parse_token (List.rev acc_token) line_num :: tokens
      else tokens

let process_tokens tokens = List.rev (process_tokens_impl [] [] 1 tokens)

let process_file file =
  let chars = read_file_to_chars file in
  process_tokens chars
