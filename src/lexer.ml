open Token

let get_token_string token = String.of_seq (List.to_seq (List.rev token))

let parse_token token =
  let token_str = get_token_string token in
  match token_str with
  | "int" -> { value = token_str; t_type = T_TYPE }
  | ";" -> { value = token_str; t_type = T_SEMI }
  | "=" -> { value = token_str; t_type = T_OPERATOR }
  | ")" | "(" -> { value = token_str; t_type = T_BRACKET }
  | _ -> { value = token_str; t_type = T_VALUE }

let add_token_to_list new_token tokens =
  if List.length new_token > 0 then parse_token new_token :: tokens else tokens

let rec process_quote_string acc_token file =
  let char = input_char file in
  try
    match char with
    | '"' -> parse_token acc_token
    | x -> process_quote_string (x :: acc_token) file
  with _ -> parse_token acc_token

let rec process_tokens tokens acc_token file =
  let char = input_char file in
  try
    match char with
    | '\n' | ' ' -> process_tokens (add_token_to_list acc_token tokens) [] file
    | ';' | '(' | ')' ->
        process_tokens
          (parse_token [ char ] :: add_token_to_list acc_token tokens)
          [] file
    | '"' ->
        process_tokens
          (process_quote_string [] file :: add_token_to_list acc_token tokens)
          [] file
    | x -> process_tokens tokens (x :: acc_token) file
  with _ -> tokens

let process_file file = List.rev (process_tokens [] [] file)
