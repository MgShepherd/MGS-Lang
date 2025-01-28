open Alcotest
open Parser
open Common.Token

let get_expr_string _ex = "EXPR:"

let get_statement_string = function
  | DeclarationStatement (_t_type, _t_var, _t_op, ex) ->
      Printf.sprintf "DECL: TOKEN VAR OP %s" (get_expr_string ex)
  | AssignmentStatement (_t_var, _t_op, ex) ->
      Printf.sprintf "ASSI: VAR OP %s" (get_expr_string ex)
  | IfStatement (_exprs, _blocks) -> "IF"
  | WhileStatement (_ex, _block) -> "WHILE"
  | PrintStatement t_val -> Printf.sprintf "PRNT: %s" t_val.t_str

let rec convert_statements_to_string acc_statements = function
  | x :: xs ->
      let statement = get_statement_string x in
      convert_statements_to_string (statement :: acc_statements) xs
  | [] -> List.rev acc_statements

let convert_program_to_string program =
  match program with
  | Program statements -> convert_statements_to_string [] statements

let test_token tok_type tok_str =
  { t_type = tok_type; t_str = tok_str; line_num = 1 }

let print_cases =
  [
    ( [
        test_token T_OPEN_BLOCK "";
        test_token T_PRINT_FUNCTION "";
        test_token T_STRING "Hello World";
        test_token T_SEMI "";
        test_token T_CLOSE_BLOCK "";
      ],
      [ "PRNT: Hello World" ] );
  ]

let decl_cases =
  [
    ( [
        test_token T_OPEN_BLOCK "";
        test_token T_TYPE "i16";
        test_token T_VARIABLE "x";
        test_token T_EQUALS "";
        test_token T_NUMBER "10";
        test_token T_SEMI "";
        test_token T_CLOSE_BLOCK "";
      ],
      [ "DECL: TOKEN VAR OP EXPR:" ] );
  ]

let assi_cases =
  [
    ( [
        test_token T_OPEN_BLOCK "";
        test_token T_VARIABLE "x";
        test_token T_COMPOUND_ASSIGNMENT "+=";
        test_token T_NUMBER "10";
        test_token T_SEMI "";
        test_token T_CLOSE_BLOCK "";
      ],
      [ "ASSI: VAR OP EXPR:" ] );
  ]

let if_cases =
  [
    ( [
        test_token T_OPEN_BLOCK "";
        test_token T_IF "";
        test_token T_VARIABLE "x";
        test_token T_COMPARISON ">";
        test_token T_NUMBER "10";
        test_token T_OPEN_BLOCK "";
        test_token T_PRINT_FUNCTION "";
        test_token T_STRING "Hello";
        test_token T_SEMI "";
        test_token T_CLOSE_BLOCK "";
        test_token T_SEMI "";
        test_token T_CLOSE_BLOCK "";
      ],
      [ "IF" ] );
  ]

let while_cases =
  [
    ( [
        test_token T_OPEN_BLOCK "";
        test_token T_WHILE "";
        test_token T_VARIABLE "x";
        test_token T_COMPARISON ">";
        test_token T_NUMBER "10";
        test_token T_OPEN_BLOCK "";
        test_token T_PRINT_FUNCTION "";
        test_token T_STRING "Hello";
        test_token T_SEMI "";
        test_token T_CLOSE_BLOCK "";
        test_token T_SEMI "";
        test_token T_CLOSE_BLOCK "";
      ],
      [ "WHILE" ] );
  ]

let perform_checks cases =
  List.iter
    (fun (input, expected) ->
      Alcotest.(check (list string))
        "Lists equal"
        (convert_program_to_string (Parser.create_tree input))
        expected)
    cases

let test_print_statement () = perform_checks print_cases
let test_decl_statement () = perform_checks decl_cases
let test_assi_statement () = perform_checks assi_cases
let test_if_statement () = perform_checks if_cases
let test_while_statement () = perform_checks while_cases

let () =
  run "Parser Tests"
    [
      ( "Valid",
        [
          test_case "Print Statements" `Quick test_print_statement;
          test_case "Declaration Statements" `Quick test_decl_statement;
          test_case "Assignment Statements" `Quick test_assi_statement;
          test_case "If Statements" `Quick test_if_statement;
          test_case "While Statements" `Quick test_while_statement;
        ] );
    ]
