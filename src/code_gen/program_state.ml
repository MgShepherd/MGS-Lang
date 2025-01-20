module StringMap = Map.Make (String)

type program_state = {
  stack : int StringMap.t;
  constants : (string * string) list;
  label_num : int;
}
