type token_type = T_OPERATOR | T_VALUE | T_TYPE | T_SEMI
type token = { value : string; t_type : token_type }

let token_type_to_str = function
  | T_OPERATOR -> "Operator"
  | T_VALUE -> "Value"
  | T_TYPE -> "Type"
  | T_SEMI -> "Semicolon"

let get_token_string token = String.of_seq (List.to_seq (List.rev token))

let parse_token token =
  let token_str = get_token_string token in
  match token_str with
  | "int" -> { value = token_str; t_type = T_TYPE }
  | ";" -> { value = token_str; t_type = T_SEMI }
  | "=" -> { value = token_str; t_type = T_OPERATOR }
  | _ -> { value = token_str; t_type = T_VALUE }

let add_token_to_list new_token tokens =
  if List.length new_token > 0 then parse_token new_token :: tokens else tokens

let rec process_tokens tokens acc_token file =
  try
    match input_char file with
    | '\n' | ' ' -> process_tokens (add_token_to_list acc_token tokens) [] file
    | ';' ->
        process_tokens
          (parse_token [ ';' ] :: add_token_to_list acc_token tokens)
          [] file
    | x -> process_tokens tokens (x :: acc_token) file
  with _ -> tokens

let rec display_tokens = function
  | [] -> ()
  | x :: xs ->
      print_endline (x.value ^ " " ^ token_type_to_str x.t_type);
      display_tokens xs

let process_file file = display_tokens (List.rev (process_tokens [] [] file))
