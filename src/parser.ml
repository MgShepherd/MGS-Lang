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
      (ExprArithmetic (x, left, right), remaining)

and parse_comparision x prev nesting xs =
  match prev with
  | None -> raise (Failure "Invalid comparision operator\n")
  | Some left ->
      let right, remaining = parse_expression None nesting xs in
      (ExprComparison (x, left, right), remaining)

and handle_empty_tokens prev =
  match prev with
  | Some x -> (x, [])
  | _ -> raise (Failure "Unproccessed Elements\n")

and parse_expression prev nesting = function
  | x :: xs -> (
      match x.t_type with
      | T_OPEN_PAREN -> parse_open_paren nesting xs
      | T_CLOSE_PAREN -> parse_close_paren prev nesting xs
      | T_STRING -> parse_value x prev nesting xs
      | T_NUMBER -> parse_value x prev nesting xs
      | T_VARIABLE -> parse_value x prev nesting xs
      | T_ARITHMETIC -> parse_arithmetic x prev nesting xs
      | T_COMPARISON -> parse_comparision x prev nesting xs
      | T_ELSE -> (ExprToken x, [])
      | _ ->
          let error_message =
            Printf.sprintf "Unrecognised Token %s\n" (get_token_string x)
          in
          raise (Failure error_message))
  | [] -> handle_empty_tokens prev

let is_assignment_statement x y z =
  x.t_type = T_TYPE && y.t_type = T_VARIABLE && z.t_type = T_EQUALS

let is_if_statement x = x.t_type = T_IF
let is_print_statement x y = x.t_type = T_PRINT_FUNCTION && y.t_type = T_STRING

let rec parse_statement = function
  | x :: y :: z :: xs when is_assignment_statement x y z ->
      let expression, remaining = parse_expression None [] xs in
      if List.length remaining > 0 then
        raise (Failure "Unproccessed tokens in statement\n")
      else AssignmentStatement (x, y, z, expression)
  | x :: xs when is_if_statement x -> parse_if_block [] [] true xs
  | [ x; y ] when is_print_statement x y -> PrintStatement y
  | _ -> raise (Failure "Invalid statement format\n")

and parse_if_block acc_condition acc_segments has_valid_start = function
  | x :: xs -> (
      match x.t_type with
      | T_OPEN_BLOCK ->
          if has_valid_start then
            let block, remaining = parse_block 0 [] xs in
            let condition, expr_remaining =
              parse_expression None [] (List.rev acc_condition)
            in
            if List.length expr_remaining > 0 then
              raise (Failure "Invalid if statement condition")
            else
              parse_if_block []
                ((condition, block) :: acc_segments)
                false remaining
          else raise (Failure "Invalid if statement condition location")
      | T_ELIF ->
          if has_valid_start then
            raise (Failure "Invalid location for elif statement")
          else parse_if_block [] acc_segments true xs
      | T_ELSE ->
          if has_valid_start then
            raise (Failure "Invalid location for else statement")
          else parse_if_block [ x ] acc_segments true xs
      | _ -> parse_if_block (x :: acc_condition) acc_segments has_valid_start xs
      )
  | [] -> IfStatement (List.rev acc_segments)

and parse_statements acc_statements current_statement nested_level = function
  | x :: xs -> (
      match x.t_type with
      | T_SEMI ->
          if nested_level = 0 then
            parse_statements
              (parse_statement (List.rev current_statement) :: acc_statements)
              [] nested_level xs
          else
            parse_statements acc_statements (x :: current_statement)
              nested_level xs
      | T_OPEN_BLOCK ->
          parse_statements acc_statements (x :: current_statement)
            (nested_level + 1) xs
      | T_CLOSE_BLOCK ->
          parse_statements acc_statements (x :: current_statement)
            (nested_level - 1) xs
      | _ ->
          parse_statements acc_statements (x :: current_statement) nested_level
            xs)
  | [] ->
      if List.length current_statement > 0 then
        raise (Failure "Invalid end of statements")
      else List.rev acc_statements

and parse_block nested_level current_block = function
  | x :: xs -> (
      match x.t_type with
      | T_CLOSE_BLOCK ->
          if nested_level = 0 then
            (parse_statements [] [] 0 (List.rev current_block), xs)
          else parse_block (nested_level - 1) (x :: current_block) xs
      | T_OPEN_BLOCK -> parse_block (nested_level + 1) (x :: current_block) xs
      | _ -> parse_block nested_level (x :: current_block) xs)
  | [] -> raise (Failure "Unclosed block")

let create_tree = function
  | { t_type = T_OPEN_BLOCK; _ } :: xs ->
      let statements, remaining = parse_block 0 [] xs in
      if List.length remaining > 0 then
        raise (Failure "Unprocessed tokens at end of program")
      else Program statements
  | _ -> raise (Failure "All programs must be inside a block")
