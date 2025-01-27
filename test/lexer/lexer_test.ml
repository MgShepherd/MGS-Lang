open Alcotest
open Common.Token
open Common.Logger

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

let token_cases =
  [
    ("i16", [ "TYPE" ]);
    ("+", [ "ARITHMETIC" ]);
    ("-", [ "ARITHMETIC" ]);
    ("*", [ "ARITHMETIC" ]);
    ("/", [ "ARITHMETIC" ]);
    ("+=", [ "COMPOUND_ASSIGNMENT" ]);
    ("-=", [ "COMPOUND_ASSIGNMENT" ]);
    (">=", [ "COMPARISON" ]);
    (">", [ "COMPARISON" ]);
    ("<", [ "COMPARISON" ]);
    ("<=", [ "COMPARISON" ]);
    ("==", [ "COMPARISON" ]);
    ("i16", [ "TYPE" ]);
    ("if", [ "IF" ]);
    ("elif", [ "ELIF" ]);
    ("else", [ "ELSE" ]);
    (";", [ "SEMI" ]);
    ("(", [ "OPEN_PAREN" ]);
    (")", [ "CLOSE_PAREN" ]);
    ("{", [ "OPEN_BLOCK" ]);
    ("}", [ "CLOSE_BLOCK" ]);
    ("=", [ "EQUALS" ]);
    ("print", [ "PRINT" ]);
    ("while", [ "WHILE" ]);
  ]

let statement_cases =
  [
    ( "\ti16 x = (10 + 2) - 3;",
      [
        "TYPE";
        "VARIABLE";
        "EQUALS";
        "OPEN_PAREN";
        "NUMBER";
        "ARITHMETIC";
        "NUMBER";
        "CLOSE_PAREN";
        "ARITHMETIC";
        "NUMBER";
        "SEMI";
      ] );
    ("print \"Hello World!\"", [ "PRINT"; "STRING" ]);
  ]

let error_cases = [ ("print \"Hello World", "Unclosed string quotes") ]

let perform_checks cases =
  List.iter
    (fun (input, expected) ->
      Alcotest.(check (list string))
        "Lists equal"
        (map_to_token_types [] (Lexer.process_tokens (string_to_chars input)))
        expected)
    cases

let perform_error_checks cases =
  List.iter
    (fun (input, expected) ->
      Alcotest.check_raises "Throws exception" (CompilerError expected)
        (fun () ->
          let _ = Lexer.process_tokens (string_to_chars input) in
          ()))
    cases

let test_token_types () = perform_checks token_cases
let test_statements () = perform_checks statement_cases
let test_errors () = perform_error_checks error_cases

let () =
  run "Lexer Tests"
    [
      ( "Valid",
        [
          test_case "Tokens" `Quick test_token_types;
          test_case "Statements" `Quick test_statements;
        ] );
      ("Invalid", [ test_case "Errors" `Quick test_errors ]);
    ]
