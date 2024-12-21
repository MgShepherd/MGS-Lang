open Token

type expr =
  | ExprArithmetic of token * expr * expr
  | ExprComparison of token * expr * expr
  | ExprToken of token

type statement =
  | AssignmentStatement of token * token * token * expr
  | IfStatement of (expr * statement list) list
  | PrintStatement of token

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
  | None -> parse_expression (Some (ExprToken x)) nesting xs

and parse_arithmetic x prev nesting xs =
  match prev with
  | None -> raise (Failure "Invalid arithmetic operator\n")
  | Some left ->
      let right, remaining = parse_expression None nesting xs in
      (ExprArithmetic (T_ARITHMETIC x, left, right), remaining)

and parse_comparision x prev nesting xs =
  match prev with
  | None -> raise (Failure "Invalid comparision operator\n")
  | Some left ->
      let right, remaining = parse_expression None nesting xs in
      (ExprComparison (T_COMPARISON x, left, right), remaining)

and handle_empty_tokens prev =
  match prev with
  | Some x -> (x, [])
  | _ -> raise (Failure "Unproccessed Elements\n")

and parse_expression prev nesting = function
  | T_OPEN_PAREN :: xs -> parse_open_paren nesting xs
  | T_CLOSE_PAREN :: xs -> parse_close_paren prev nesting xs
  | T_STRING x :: xs -> parse_value (T_STRING x) prev nesting xs
  | T_NUMBER x :: xs -> parse_value (T_NUMBER x) prev nesting xs
  | T_VARIABLE x :: xs -> parse_value (T_VARIABLE x) prev nesting xs
  | T_ARITHMETIC x :: xs -> parse_arithmetic x prev nesting xs
  | T_COMPARISON x :: xs -> parse_comparision x prev nesting xs
  | [ T_ELSE ] -> (ExprToken T_ELSE, [])
  | [] -> handle_empty_tokens prev
  | x :: _ ->
      let error_message =
        Printf.sprintf "Unrecognised Token %s\n" (get_token_string x)
      in
      raise (Failure error_message)

let rec parse_statement = function
  | T_TYPE x :: T_VARIABLE y :: T_EQUALS :: xs ->
      let expression, remaining = parse_expression None [] xs in
      if List.length remaining > 0 then
        raise (Failure "Unproccessed tokens in statement\n")
      else AssignmentStatement (T_TYPE x, T_VARIABLE y, T_EQUALS, expression)
  | T_IF :: xs -> parse_if_block [] [] true xs
  | [ T_PRINT_FUNCTION; T_STRING x ] -> PrintStatement (T_STRING x)
  | _ -> raise (Failure "Invalid statement format\n")

and parse_if_block acc_condition acc_segments has_valid_start = function
  | T_OPEN_BLOCK :: xs ->
      if has_valid_start then
        let block, remaining = parse_block 0 [] xs in
        let condition, expr_remaining =
          parse_expression None [] (List.rev acc_condition)
        in
        if List.length expr_remaining > 0 then
          raise (Failure "Invalid if statement condition")
        else
          parse_if_block [] ((condition, block) :: acc_segments) false remaining
      else raise (Failure "Invalid if statement condition location")
  | T_ELIF :: xs ->
      if has_valid_start then
        raise (Failure "Invalid location for elif statement")
      else parse_if_block [] acc_segments true xs
  | T_ELSE :: xs ->
      if has_valid_start then
        raise (Failure "Invalid location for else statement")
      else parse_if_block [ T_ELSE ] acc_segments true xs
  | x :: xs ->
      parse_if_block (x :: acc_condition) acc_segments has_valid_start xs
  | [] -> IfStatement (List.rev acc_segments)

and parse_statements acc_statements current_statement nested_level = function
  | T_SEMI :: xs ->
      if nested_level = 0 then
        parse_statements
          (parse_statement (List.rev current_statement) :: acc_statements)
          [] nested_level xs
      else
        parse_statements acc_statements
          (T_SEMI :: current_statement)
          nested_level xs
  | T_OPEN_BLOCK :: xs ->
      parse_statements acc_statements
        (T_OPEN_BLOCK :: current_statement)
        (nested_level + 1) xs
  | T_CLOSE_BLOCK :: xs ->
      parse_statements acc_statements
        (T_CLOSE_BLOCK :: current_statement)
        (nested_level - 1) xs
  | x :: xs ->
      parse_statements acc_statements (x :: current_statement) nested_level xs
  | [] ->
      if List.length current_statement > 0 then
        raise (Failure "Invalid end of statements")
      else List.rev acc_statements

and parse_block nested_level current_block = function
  | T_CLOSE_BLOCK :: xs ->
      if nested_level = 0 then
        (parse_statements [] [] 0 (List.rev current_block), xs)
      else parse_block (nested_level - 1) (T_CLOSE_BLOCK :: current_block) xs
  | T_OPEN_BLOCK :: xs ->
      parse_block (nested_level + 1) (T_OPEN_BLOCK :: current_block) xs
  | x :: xs -> parse_block nested_level (x :: current_block) xs
  | [] -> raise (Failure "Unclosed block")

let create_tree = function
  | T_OPEN_BLOCK :: xs ->
      let statements, remaining = parse_block 0 [] xs in
      if List.length remaining > 0 then
        raise (Failure "Unprocessed tokens at end of program")
      else Program statements
  | _ -> raise (Failure "All programs must be inside a block")
