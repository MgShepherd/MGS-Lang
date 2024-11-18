let create_start_function = ".global _start\n_start:\n"
let process_expression _expr = "Parsing a statement\n"

let process_statement = function
  | Parser.AssignmentStatement (_, _, _, expr) -> process_expression expr

let rec process_statements acc = function
  | [] -> acc
  | x :: xs -> process_statements (acc ^ process_statement x) xs

let generate_assembly = function
  | Parser.Program statements ->
      create_start_function ^ process_statements "" statements
