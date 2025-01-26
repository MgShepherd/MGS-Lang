let rec read_file_impl acc file =
  let char = input_char file in
  try read_file_impl (char :: acc) file with End_of_file -> List.rev acc

let read_file_to_chars file = read_file_impl [] file
