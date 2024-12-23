let output_dir = "build/"
let assembly_extension = ".s"
let input_file_path = ref ""
let arglist = [ ("-f", Arg.Set_string input_file_path, "File to be compiled") ]
let anon_arg_func _ = ()
let arg_usage_msg = Printf.sprintf "Usage: ./%smain -f <input_file>" output_dir

let rec get_last_element = function
  | x :: [] -> x
  | _ :: xs -> get_last_element xs
  | [] -> raise (Failure "Attempted to get last element of empty list")

let get_file_name path =
  let name_with_ext = get_last_element (String.split_on_char '/' path) in
  match String.split_on_char '.' name_with_ext with
  | x :: _ -> x
  | [] -> raise (Failure "Unable to remove file extension")

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
  let oc =
    open_out (output_dir ^ get_file_name !input_file_path ^ assembly_extension)
  in
  Printf.fprintf oc "%s\n" contents;
  close_out oc

let () =
  Arg.parse arglist anon_arg_func arg_usage_msg;
  let file = try Some (open_in !input_file_path) with _ -> None in
  match file with
  | Some x -> write_string (Arm_gen.generate_assembly (process_file x))
  | None -> Printf.printf "Unable to open file: %s\n" !input_file_path
