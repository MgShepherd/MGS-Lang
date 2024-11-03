let process_file file =
  let line = input_line file in
  print_endline "Running from lexer";
  prerr_endline line
