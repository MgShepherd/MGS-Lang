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
         ( { t_type = T_DIGIT; value = '1' },
           [
             Node
               ( { t_type = T_DIGIT; value = '9' },
                 [
                   Leaf { t_type = T_DIGIT; value = '7' };
                   Leaf { t_type = T_DIGIT; value = '4' };
                 ] );
             Leaf { t_type = T_OPERATOR; value = '+' };
           ] ));
    close_in file
  with e ->
    close_in_noerr file;
    raise e
