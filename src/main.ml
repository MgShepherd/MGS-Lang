open Common.Logger
open Code_gen.Native_gen

let output_dir = "build/"
let assembly_extension = ".s"
let object_extension = ".o"
let input_file_path = ref ""
let arglist = [ ("-f", Arg.Set_string input_file_path, "File to be compiled") ]
let anon_arg_func _ = ()
let arg_usage_msg = Printf.sprintf "Usage: ./%smain -f <input_file>" output_dir

let rec get_last_element = function
  | x :: [] -> x
  | _ :: xs -> get_last_element xs
  | [] -> fatal_err "Provided file path is empty"

let get_file_name path =
  let name_with_ext = get_last_element (String.split_on_char '/' path) in
  match String.split_on_char '.' name_with_ext with
  | x :: _ -> x
  | [] ->
      fatal_err
        "Please ensure that a valid file with the .mgs extension is provided"

let process_file file =
  let tokens = Lexer.process_file file in
  Common.Token.display_tokens tokens;
  close_in file;
  let tree = Parser.create_tree tokens in
  let v_table = Semantic_analyzer.run_analyzer tree in
  (tree, v_table)

let write_string file_name contents =
  if not (Sys.file_exists output_dir) then Sys.mkdir output_dir 0o755;
  let oc = open_out (output_dir ^ file_name ^ assembly_extension) in
  Printf.fprintf oc "%s\n" contents;
  close_out oc

let run_command command =
  match Sys.command command with
  | 0 -> ()
  | _ ->
      fatal_err
        (Printf.sprintf
           "Unable to run following command when generating executable:\n%s"
           command)

let produce_executable file_name =
  let obj_file = output_dir ^ file_name ^ object_extension in
  let as_file = output_dir ^ file_name ^ assembly_extension in
  let exe_file = output_dir ^ file_name in
  run_command (Printf.sprintf "as -o %s %s -g" obj_file as_file);
  run_command (Printf.sprintf "ld -o %s %s" exe_file obj_file);
  run_command (Printf.sprintf "rm %s" obj_file);
  run_command (Printf.sprintf "rm %s" as_file)

let () =
  try
    Arg.parse arglist anon_arg_func arg_usage_msg;
    let file = try Some (open_in !input_file_path) with _ -> None in
    let file_name = get_file_name !input_file_path in
    match file with
    | Some x ->
        let program, v_table = process_file x in
        write_string file_name (generate_native_assembly program v_table);
        produce_executable file_name
    | None ->
        fatal_err
          (Printf.sprintf "Unable to open file with name: \"%s\"" file_name)
  with CompilerError msg -> display_compiler_error msg
