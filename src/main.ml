let input_file_name = "examples/basic.mgs"
let output_dir = "build"
let output_file_name = output_dir ^ "/basic.s"

let process_file file =
  try
    let tokens = Lexer.process_file file in
    Token.display_tokens tokens;
    close_in file;
    let tree = Parser.create_tree tokens in
    Printer.display_tree tree;
    tree
  with Failure e ->
    raise (Failure (Printf.sprintf "Unexpected Exception Occurred\n%s" e))

let write_string contents =
  if not (Sys.file_exists output_dir) then Sys.mkdir output_dir 0o755;
  let oc = open_out output_file_name in
  Printf.fprintf oc "%s\n" contents;
  close_out oc

let () =
  let file = try Some (open_in input_file_name) with _ -> None in
  match file with
  | Some x -> write_string (Arm_gen.generate_assembly (process_file x))
  | None -> Printf.printf "Unable to open file: %s\n" input_file_name
