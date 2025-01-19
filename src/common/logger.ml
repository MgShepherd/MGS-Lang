let display_error_line line = Printf.printf "\027[31m[ERROR]:\027[0m %s\n" line

let fatal_err msg =
  let lines = String.split_on_char '\n' msg in
  List.iter (fun line -> display_error_line line) lines;
  exit 1

let fatal_err_with_line msg line_num =
  let new_msg = Printf.sprintf "Syntax error on line %d\n%s" line_num msg in
  fatal_err new_msg
