open Parser
open Common.Token
module StringMap : module type of Map.Make (String)

val run_analyzer : program -> data_type StringMap.t
