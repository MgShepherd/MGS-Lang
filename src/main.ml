let file_name = "examples/basic.mgs"

let () =
  let file = open_in file_name in
  try
    Token.display_tokens (Lexer.process_file file);
    close_in file
  with e ->
    close_in_noerr file;
    raise e
