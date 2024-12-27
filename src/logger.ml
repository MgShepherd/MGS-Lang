let display_error_line line = Printf.printf "\027[31mERROR: %s\n" line
let reset_stdout_color = Printf.printf "\027[0m"

let fatal_err msg =
  let lines = String.split_on_char '\n' msg in
  List.iter (fun line -> display_error_line line) lines;
  reset_stdout_color;
  exit 1

let fatal_err_with_line msg line_num =
  let new_msg = Printf.sprintf "Syntax error on line %d\n%s" line_num msg in
  fatal_err new_msg
