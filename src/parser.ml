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
  | ExprArithmetic of token * expr * expr
  | ExprToken of token

let unprocessable_token_error token =
  let error_message =
    Printf.sprintf "Parser Error: Unprocessable Token: %s\n"
      (get_token_string token)
  in
  raise (Failure error_message)

let rec parse_open_paren nesting xs =
  let result, remaining = parse_expression None (T_OPEN_PAREN :: nesting) xs in
  parse_expression (Some result) nesting remaining

and parse_close_paren prev nesting xs =
  match prev with
  | None -> raise (Failure "Empty Paratheses\n")
  | Some x -> (
      match nesting with
      | T_OPEN_PAREN :: _ -> (x, xs)
      | _ -> raise (Failure "Unbalanced Parentheses\n"))

and parse_value x prev nesting xs =
  match prev with
  | Some _ -> raise (Failure "Invalid value location\n")
  | None -> parse_expression (Some (ExprToken (T_VALUE x))) nesting xs

and parse_arithmetic x prev nesting xs =
  match prev with
  | None -> raise (Failure "Invalid addition operator\n")
  | Some left ->
      let right, remaining = parse_expression None nesting xs in
      (ExprArithmetic (T_ARITHMETIC x, left, right), remaining)

and handle_empty_tokens prev =
  match prev with
  | Some x -> (x, [])
  | _ -> raise (Failure "Unproccessed Elements\n")

and parse_expression prev nesting = function
  | T_OPEN_PAREN :: xs -> parse_open_paren nesting xs
  | T_CLOSE_PAREN :: xs -> parse_close_paren prev nesting xs
  | T_VALUE x :: xs -> parse_value x prev nesting xs
  | T_ARITHMETIC x :: xs -> parse_arithmetic x prev nesting xs
  | [] -> handle_empty_tokens prev
  | _ -> raise (Failure "Unrecognised Token\n")

let rec parse_program current_expr = function
  | T_SEMI :: xs ->
      let result, remaining =
        parse_expression None [] (List.rev current_expr)
      in
      if remaining = [] then result :: parse_program [] xs
      else raise (Failure "Not all tokens processed\n")
  | x :: xs -> parse_program (x :: current_expr) xs
  | [] ->
      if List.length current_expr = 0 then []
      else raise (Failure "Unprocessed Input at end of file\n")

let create_tree tokens = ExprProgram (parse_program [] tokens)

let rec display_tree_aux indent = function
  | ExprProgram elements ->
      Printf.printf "Program ->\n\t";
      List.iter (fun x -> display_tree_aux "\t" x) elements;
      print_endline ""
  | ExprArithmetic (T_ARITHMETIC op, x, y) ->
      let new_indent = "\t" ^ indent in
      Printf.printf "Arithmetic %s ->\n%s" op new_indent;
      display_tree_aux new_indent x;
      Printf.printf "\n%s" new_indent;
      display_tree_aux new_indent y;
      print_endline ""
  | ExprToken x -> Printf.printf "%s " (get_token_string x)
  | _ -> ()

let display_tree tree = display_tree_aux "" tree
