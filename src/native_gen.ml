open Logger

let is_os_type_supported os_type = os_type = "Unix"

let get_processor_architecture =
  let command = "uname -p" in
  let process = Unix.open_process_in command in
  let output = In_channel.input_lines process in
  In_channel.close process;
  match output with
  | p_arch :: _ -> p_arch
  | [] -> fatal_err "Unable to determine processor architecture"

let generate_assembly_for_arch program = function
  | "x86_64" -> X86_64_gen.generate_assembly program
  | "arm64" -> Arm_gen.generate_assembly program
  | x ->
      fatal_err
        (Printf.sprintf "CPU Architecture %s not currently supported\n" x)

let generate_native_assembly program =
  let os_type = Sys.os_type in
  if not (is_os_type_supported os_type) then
    fatal_err
      (Printf.sprintf "Operating System Type %s is not yet supported\n" os_type)
  else
    let p_arch = get_processor_architecture in
    generate_assembly_for_arch program p_arch
