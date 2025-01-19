let create_start_function = ".global _start\n_start:\n"

let create_exit_function status =
  Printf.sprintf "\tMOV $60, %%rax\n\tMOV $%d, %%rdi\n\tsyscall\n" status

let generate_assembly _program = create_start_function ^ create_exit_function 0
