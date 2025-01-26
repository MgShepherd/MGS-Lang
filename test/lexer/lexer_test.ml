open Alcotest
open Common.Token

let token_type_to_str = function
  | T_ARITHMETIC -> "ARITHMETIC"
  | T_COMPOUND_ASSIGNMENT -> "COMPOUND_ASSIGNMENT"
  | T_COMPARISON -> "COMPARISON"
  | T_STRING -> "STRING"
  | T_NUMBER -> "NUMBER"
  | T_VARIABLE -> "VARIABLE"
  | T_EQUALS -> "EQUALS"
  | T_TYPE -> "TYPE"
  | T_OPEN_PAREN -> "OPEN_PAREN"
  | T_CLOSE_PAREN -> "CLOSE_PAREN"
  | T_SEMI -> "SEMI"
  | T_IF -> "IF"
  | T_ELIF -> "ELIF"
  | T_ELSE -> "ELSE"
  | T_OPEN_BLOCK -> "OPEN_BLOCK"
  | T_CLOSE_BLOCK -> "CLOSE_BLOCK"
  | T_PRINT_FUNCTION -> "PRINT"
  | T_WHILE -> "WHILE"

let string_to_chars str = List.of_seq (String.to_seq str)

let rec map_to_token_types acc_types = function
  | x :: xs ->
      let n_types = token_type_to_str x.t_type :: acc_types in
      map_to_token_types n_types xs
  | [] -> List.rev acc_types

let test_assignment () =
  Alcotest.(check (list string))
    "same tokens"
    [ "TYPE"; "VARIABLE"; "EQUALS"; "NUMBER"; "SEMI" ]
    (map_to_token_types []
       (Lexer.process_tokens (string_to_chars "i16 x = 10;")))

let () =
  run "Lexer Tests"
    [ ("single_statements", [ test_case "" `Quick test_assignment ]) ]
