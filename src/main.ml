let file_name = "examples/basic.mgs"

open Parser

let () =
  let file = open_in file_name in
  try
    let tokens = Lexer.process_file file in
    print_endline "\nParse Tree output:";
    let tree = create_tree tokens in
    display_tree tree;
    close_in file
  with e ->
    close_in_noerr file;
    raise e
