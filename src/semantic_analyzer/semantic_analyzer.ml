open Parser
module StringMap = Map.Make (String)

let check_statement = function
  | DeclarationStatement (_t_type, _t_var, _t_operator, _ex) -> ()
  | AssignmentStatement (_t_var, _t_operator, _ex) -> ()
  | IfStatement (_exprs, _stmnts) -> ()
  | WhileStatement (_expr, _stmnts) -> ()
  | PrintStatement _val -> ()

let rec check_statements = function
  | x :: xs ->
      check_statement x;
      check_statements xs
  | [] -> ()

let run_analyzer = function Program statements -> check_statements statements
