open Token

(***
     Language Context Free Grammar:
     Start Symbol: Program
     Non Terminals: Expression, Operator, Digit

     Production Rules:
     <Program> ::= <Expression> | <Expression><Program>
     <Expression> ::= <Expression><Operator><Expression> | <Digit>
     <Digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
     <Operator> ::= + | - | / | *
  ***)

type expr =
  | ExprProgram of expr list
  | ExprAdd of expr * expr
  | ExprToken of token

let rec parse_tokens = function
  | T_DIGIT x :: T_ADD :: T_DIGIT y :: remaining ->
      ExprAdd (ExprToken (T_DIGIT x), ExprToken (T_DIGIT y))
      :: parse_tokens remaining
  | [] -> []
  | _ -> raise (Failure "Unprocessable tokens")

let create_tree tokens = ExprProgram (parse_tokens tokens)

let rec display_tree_aux indent = function
  | ExprProgram elements ->
      Printf.printf "Program ->\n";
      List.iter (fun x -> display_tree_aux "\t" x) elements
  | ExprAdd (ExprToken x, ExprToken y) ->
      Printf.printf "%s+ -> %s %s\n" indent (get_token_string x)
        (get_token_string y)
  | ExprToken x -> Printf.printf "%s\n" (get_token_string x)
  | _ -> ()

let display_tree tree = display_tree_aux "" tree
