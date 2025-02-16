open Common.Token
module StringMap : module type of Map.Make (String)

val generate_assembly : data_type StringMap.t -> Parser.program -> string
