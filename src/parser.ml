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

type expr = ExprArithmetic of token * expr * expr | ExprToken of token
type statement = AssignmentStatement of token * token * token * expr
type program = Program of statement list

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

let parse_statement = function
  | T_TYPE x :: T_VALUE y :: T_EQUALS :: xs ->
      let expression, remaining = parse_expression None [] xs in
      if List.length remaining > 0 then
        raise (Failure "Unproccessed tokens in statement\n")
      else AssignmentStatement (T_TYPE x, T_VALUE y, T_EQUALS, expression)
  | _ -> raise (Failure "Invalid statement format\n")

let rec parse_program current_statement = function
  | T_SEMI :: xs ->
      let statement = parse_statement (List.rev current_statement) in
      statement :: parse_program [] xs
  | x :: xs -> parse_program (x :: current_statement) xs
  | [] ->
      if List.length current_statement = 0 then []
      else raise (Failure "Unprocessed Input at end of file\n")

let create_tree tokens = Program (parse_program [] tokens)
