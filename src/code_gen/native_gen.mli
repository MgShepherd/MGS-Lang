open Common.Token
module StringMap : module type of Map.Make (String)

val generate_native_assembly : Parser.program -> data_type StringMap.t -> string
