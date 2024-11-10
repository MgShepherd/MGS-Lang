open Token

(***
     Language Context Free Grammar:
     Start Symbol: Program
     Non Terminals: Expression, Operator, Digit

     Production Rules:
     <Program> ::= <Expression> | <Expression><Program>
     <Expression> ::= <Expression><Operator><Expression>;  | (<Expression>) | <Digit>
     <Digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
     <Operator> ::= + | - | / | *
  ***)

type expr =
  | ExprProgram of expr list
  | ExprAdd of expr * expr
  | ExprToken of token

let unprocessable_token_error token =
  let error_message =
    Printf.sprintf "Parser Error: Unprocessable Token: %s\n"
      (get_token_string token)
  in
  raise (Failure error_message)

let rec parse_expression prev = function
  | T_DIGIT x :: xs -> (
      match prev with
      | None -> parse_expression (Some (ExprToken (T_DIGIT x))) xs
      | Some _ -> raise (Failure "Unproccessed Elements\n"))
  | T_ADD :: xs -> (
      match prev with
      | Some x -> ExprAdd (x, parse_expression None xs)
      | None -> raise (Failure "No left operand of addition\n"))
  | [] -> (
      match prev with
      | Some (ExprToken x) -> ExprToken x
      | _ -> raise (Failure "Unprocessed Elements\n"))
  | _ -> raise (Failure "Unrecogonised Token\n")

let rec parse_program current_expr = function
  | T_SEMI :: xs ->
      parse_expression None (List.rev current_expr) :: parse_program [] xs
  | x :: xs -> parse_program (x :: current_expr) xs
  | [] ->
      if List.length current_expr = 0 then []
      else raise (Failure "Unprocessed Input at end of file\n")

let create_tree tokens = ExprProgram (parse_program [] tokens)

let rec display_tree_aux indent = function
  | ExprProgram elements ->
      Printf.printf "Program ->";
      List.iter (fun x -> display_tree_aux "\t" x) elements;
      print_endline ""
  | ExprAdd (x, y) ->
      Printf.printf "\n%sExprAdd -> " indent;
      display_tree_aux ("\t" ^ indent) x;
      display_tree_aux ("\t" ^ indent) y
  | ExprToken x -> Printf.printf "%s " (get_token_string x)

let display_tree tree = display_tree_aux "" tree
