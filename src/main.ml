let file_name = "examples/basic.mgs"

open Token
open Parse_tree

let () =
  let file = open_in file_name in
  try
    display_tokens (Lexer.process_file file);
    print_endline "\nParse Tree output:";
    display_tree
      (Node
         ( T_DIGIT 1,
           [
             Node (T_DIGIT 9, [ Leaf (T_DIGIT 7); Leaf (T_DIGIT 4) ]);
             Leaf T_ADD;
           ] ));
    close_in file
  with e ->
    close_in_noerr file;
    raise e
