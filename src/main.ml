let file_name = "examples/basic.mgs"

open Parser

let process_file file =
  try
    let tokens = Lexer.process_file file in
    Token.display_tokens tokens;
    let tree = create_tree tokens in
    print_endline "\nParse Tree output:";
    display_tree tree;
    close_in file
  with Failure e -> Printf.printf "Unexpected Exception Occurred\n%s" e

let () =
  let file = try Some (open_in file_name) with _ -> None in
  match file with
  | Some x -> process_file x
  | None -> Printf.printf "Unable to open file: %s\n" file_name
