open Token

(***
     Language Context Free Grammar:
     Start Symbol: Start
     Non Terminals: Expression, Operator, Digit

     Production Rules:
     <Start> ::= <Expression>
     <Expression> ::= <Expression><Operator><Expression> | <Digit>
     <Digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
     <Operator> ::= + | - | / | *
  ***)

type expr = ExprAdd of expr * expr | ExprToken of token

let parse_expression = function
  | [ T_DIGIT x; T_ADD; T_DIGIT y ] ->
      ExprAdd (ExprToken (T_DIGIT x), ExprToken (T_DIGIT y))
  | _ -> raise (Failure "Unprocessable tokens")

let create_tree tokens = parse_expression tokens

let display_tree_aux = function
  | ExprAdd (ExprToken x, ExprToken y) ->
      Printf.printf "+ ->\n\t";
      print_token x;
      Printf.printf "\n\t";
      print_token y;
      Printf.printf "\n"
  | ExprToken x ->
      print_token x;
      Printf.printf "\n"
  | _ -> ()

let display_tree tree = display_tree_aux tree
