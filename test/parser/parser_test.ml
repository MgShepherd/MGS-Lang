open Alcotest
open Parser
open Common.Token

let rec get_expr_string = function
  | ExprArithmetic (t_op, left, right) ->
      Printf.sprintf "%s %s %s" (get_expr_string left) (get_token_string t_op)
        (get_expr_string right)
  | ExprComparison (t_op, left, right) ->
      Printf.sprintf "%s %s %s" (get_expr_string left) (get_token_string t_op)
        (get_expr_string right)
  | ExprToken tok -> get_token_string tok

let rec get_cond_block_string acc_str = function
  | (cond, block) :: xs ->
      get_cond_block_string
        (acc_str ^ get_expr_string cond ^ "; "
        ^ convert_statements_to_string "" block
        ^ " ")
        xs
  | [] -> acc_str

and get_statement_string = function
  | DeclarationStatement (t_type, t_var, t_op, ex) ->
      Printf.sprintf "DECL: %s %s %s %s;" (get_token_string t_type)
        (get_token_string t_var) (get_token_string t_op) (get_expr_string ex)
  | AssignmentStatement (t_var, t_op, ex) ->
      Printf.sprintf "ASSI: %s %s %s;" (get_token_string t_var)
        (get_token_string t_op) (get_expr_string ex)
  | IfStatement (exprs, blocks) ->
      get_cond_block_string "IF: " (List.combine exprs blocks)
  | WhileStatement (ex, block) ->
      get_cond_block_string "WHILE: " [ (ex, block) ]
  | PrintStatement t_val -> Printf.sprintf "PRNT: %s;" t_val.t_str

and convert_statements_to_string acc_statements = function
  | x :: xs ->
      let statement = get_statement_string x in
      convert_statements_to_string (acc_statements ^ statement) xs
  | [] -> acc_statements

let convert_program_to_string program =
  match program with
  | Program statements -> convert_statements_to_string "" statements

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
      "PRNT: Hello World;" );
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
      "DECL: (TYPE:i16) (VARIABLE:x) (EQUALS) (NUMBER:10);" );
    ( [
        test_token T_OPEN_BLOCK "";
        test_token T_TYPE "i16";
        test_token T_VARIABLE "x";
        test_token T_EQUALS "";
        test_token T_NUMBER "10";
        test_token T_ARITHMETIC "+";
        test_token T_NUMBER "20";
        test_token T_SEMI "";
        test_token T_CLOSE_BLOCK "";
      ],
      "DECL: (TYPE:i16) (VARIABLE:x) (EQUALS) (NUMBER:10) (ARITHMETIC:+) \
       (NUMBER:20);" );
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
      "ASSI: (VARIABLE:x) (COMPOUND_ASSIGNMENT:+=) (NUMBER:10);" );
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
      "IF: (VARIABLE:x) (COMPARISON:>) (NUMBER:10); PRNT: Hello; " );
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
        test_token T_ELSE "";
        test_token T_OPEN_BLOCK "";
        test_token T_PRINT_FUNCTION "";
        test_token T_STRING "World";
        test_token T_SEMI "";
        test_token T_CLOSE_BLOCK "";
        test_token T_SEMI "";
        test_token T_CLOSE_BLOCK "";
      ],
      "IF: (VARIABLE:x) (COMPARISON:>) (NUMBER:10); PRNT: Hello; (ELSE); PRNT: \
       World; " );
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
        test_token T_ELIF "";
        test_token T_VARIABLE "y";
        test_token T_COMPARISON "<=";
        test_token T_NUMBER "20";
        test_token T_OPEN_BLOCK "";
        test_token T_PRINT_FUNCTION "";
        test_token T_STRING "There";
        test_token T_SEMI "";
        test_token T_CLOSE_BLOCK "";
        test_token T_ELSE "";
        test_token T_OPEN_BLOCK "";
        test_token T_PRINT_FUNCTION "";
        test_token T_STRING "World";
        test_token T_SEMI "";
        test_token T_CLOSE_BLOCK "";
        test_token T_SEMI "";
        test_token T_CLOSE_BLOCK "";
      ],
      "IF: (VARIABLE:x) (COMPARISON:>) (NUMBER:10); PRNT: Hello; (VARIABLE:y) \
       (COMPARISON:<=) (NUMBER:20); PRNT: There; (ELSE); PRNT: World; " );
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
      "WHILE: (VARIABLE:x) (COMPARISON:>) (NUMBER:10); PRNT: Hello; " );
  ]

let perform_checks cases =
  List.iter
    (fun (input, expected) ->
      Alcotest.(check string)
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
