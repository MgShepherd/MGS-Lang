exception CompilerError of string

val fatal_err : string -> 'a
val display_compiler_error : string -> unit
val fatal_err_with_line : string -> int -> 'a
