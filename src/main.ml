let file_name = "examples/basic.mgs"

let () =
  let file = open_in file_name in
  try
    let line = input_line file in
    prerr_endline line;
    close_in file
  with e ->
    close_in_noerr file;
    raise e
