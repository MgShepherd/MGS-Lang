let file_name = "examples/basic"

let process_file file =
  try
    let tokens = Lexer.process_file file in
    Token.display_tokens tokens;
    close_in file;
    Parser.create_tree tokens
  with Failure e ->
    raise (Failure (Printf.sprintf "Unexpected Exception Occurred\n%s" e))

let write_string contents =
  let oc = open_out (file_name ^ ".s") in
  Printf.fprintf oc "%s\n" contents;
  close_out oc

let () =
  let file = try Some (open_in (file_name ^ ".mgs")) with _ -> None in
  match file with
  | Some x -> write_string (Arm_gen.generate_assembly (process_file x))
  | None -> Printf.printf "Unable to open file: %s\n" file_name
