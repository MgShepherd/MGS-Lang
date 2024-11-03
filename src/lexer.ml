let get_token_string token = String.of_seq (List.to_seq (List.rev token))

let rec process_tokens tokens acc_token file =
  try
    match input_char file with
    | '\n' | ' ' ->
        process_tokens (get_token_string acc_token :: tokens) [] file
    | ';' ->
        process_tokens (";" :: get_token_string acc_token :: tokens) [] file
    | x -> process_tokens tokens (x :: acc_token) file
  with _ -> tokens

let rec display_tokens = function
  | [] -> ()
  | x :: xs ->
      print_endline x;
      display_tokens xs

let process_file file = display_tokens (List.rev (process_tokens [] [] file))
