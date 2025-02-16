module StringMap = Map.Make (String)
open Common.Token

let stack_alignment = 8

type program_state = {
  stack : int StringMap.t;
  constants : (string * string) list;
  label_num : int;
  next_label : int;
}

let add_to_stack p_state var_name =
  let stack =
    StringMap.add var_name
      ((StringMap.cardinal p_state.stack + 1) * stack_alignment)
      p_state.stack
  in
  { p_state with stack }

let get_stack_var p_state tok = StringMap.find tok.t_str p_state.stack
