open Code_gen.X86_64_gen
open Common.Token
open Alcotest
open Parser

let test_token tok_type tok_str =
  { t_type = tok_type; t_str = tok_str; line_num = 1 }

let valid_statements =
  [
    ( Program [],
      {|
.global _start
_start:
  MOV %rsp, %rbp
  MOV $60, %rax
  MOV $0, %rdi
  syscall
.data
|}
    );
    ( Program [ PrintStatement (test_token T_STRING "Hello") ],
      {|
.global _start
_start:
  MOV %rsp, %rbp
  MOV $1, %rdi
  MOV $V0, %rsi
  MOV $5, %rdx
  MOV $1, %rax
  syscall
  MOV $60, %rax
  MOV $0, %rdi
  syscall
.data
V0: .ascii Hello
.align 16
|}
    );
    ( Program
        [
          DeclarationStatement
            ( test_token T_TYPE "i16",
              test_token T_VARIABLE "x",
              test_token T_EQUALS "",
              ExprToken (test_token T_NUMBER "10") );
        ],
      {|
.global _start
_start:
  MOV %rsp, %rbp
  MOV $10, %ax
  PUSH %rax
  MOV $60, %rax
  MOV $0, %rdi
  syscall
.data
|}
    );
  ]

let perform_checks cases =
  List.iter
    (fun (input, expected) ->
      Alcotest.(check string) "Output equal" (generate_assembly input) expected)
    cases

let test_valid_statements () = perform_checks valid_statements

let () =
  run "Code Gen Tests"
    [ ("Valid", [ test_case "Valid Statements" `Quick test_valid_statements ]) ]
