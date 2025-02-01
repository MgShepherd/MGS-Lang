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
    ( Program
        [
          DeclarationStatement
            ( test_token T_TYPE "i16",
              test_token T_VARIABLE "x",
              test_token T_EQUALS "",
              ExprToken (test_token T_NUMBER "7") );
          IfStatement
            ( [
                ExprComparison
                  ( test_token T_COMPARISON "<",
                    ExprToken (test_token T_VARIABLE "x"),
                    ExprToken (test_token T_NUMBER "10") );
              ],
              [ [ PrintStatement (test_token T_STRING "Hello") ] ] );
        ],
      {|
.global _start
_start:
  MOV %rsp, %rbp
  MOV $7, %ax
  PUSH %rax
  MOVW -8(%rbp), %ax
  MOV $10, %bx
  CMP %bx, %ax
  JL _0block0
  JMP _0block1
_0block0:
  MOV $1, %rdi
  MOV $V0, %rsi
  MOV $5, %rdx
  MOV $1, %rax
  syscall
  JMP _0block1
_0block1:
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
              ExprToken (test_token T_NUMBER "7") );
          IfStatement
            ( [
                ExprComparison
                  ( test_token T_COMPARISON "<",
                    ExprToken (test_token T_VARIABLE "x"),
                    ExprToken (test_token T_NUMBER "10") );
                ExprToken (test_token T_ELSE "");
              ],
              [
                [ PrintStatement (test_token T_STRING "Hello") ];
                [ PrintStatement (test_token T_STRING "World") ];
              ] );
        ],
      {|
.global _start
_start:
  MOV %rsp, %rbp
  MOV $7, %ax
  PUSH %rax
  MOVW -8(%rbp), %ax
  MOV $10, %bx
  CMP %bx, %ax
  JL _0block0
  JMP _0block1
  JMP _0block2
_0block0:
  MOV $1, %rdi
  MOV $V0, %rsi
  MOV $5, %rdx
  MOV $1, %rax
  syscall
  JMP _0block2
_0block1:
  MOV $1, %rdi
  MOV $V1, %rsi
  MOV $5, %rdx
  MOV $1, %rax
  syscall
  JMP _0block2
_0block2:
  MOV $60, %rax
  MOV $0, %rdi
  syscall
.data
V0: .ascii Hello
.align 16
V1: .ascii World
.align 16
|}
    );
    ( Program
        [
          DeclarationStatement
            ( test_token T_TYPE "i16",
              test_token T_VARIABLE "x",
              test_token T_EQUALS "",
              ExprToken (test_token T_NUMBER "7") );
          IfStatement
            ( [
                ExprComparison
                  ( test_token T_COMPARISON "<",
                    ExprToken (test_token T_VARIABLE "x"),
                    ExprToken (test_token T_NUMBER "10") );
                ExprComparison
                  ( test_token T_COMPARISON ">",
                    ExprToken (test_token T_VARIABLE "x"),
                    ExprToken (test_token T_NUMBER "7") );
                ExprToken (test_token T_ELSE "");
              ],
              [
                [ PrintStatement (test_token T_STRING "Hello") ];
                [ PrintStatement (test_token T_STRING "There") ];
                [ PrintStatement (test_token T_STRING "World") ];
              ] );
        ],
      {|
.global _start
_start:
  MOV %rsp, %rbp
  MOV $7, %ax
  PUSH %rax
  MOVW -8(%rbp), %ax
  MOV $10, %bx
  CMP %bx, %ax
  JL _0block0
  MOVW -8(%rbp), %ax
  MOV $7, %bx
  CMP %bx, %ax
  JG _0block1
  JMP _0block2
  JMP _0block3
_0block0:
  MOV $1, %rdi
  MOV $V0, %rsi
  MOV $5, %rdx
  MOV $1, %rax
  syscall
  JMP _0block3
_0block1:
  MOV $1, %rdi
  MOV $V1, %rsi
  MOV $5, %rdx
  MOV $1, %rax
  syscall
  JMP _0block3
_0block2:
  MOV $1, %rdi
  MOV $V2, %rsi
  MOV $5, %rdx
  MOV $1, %rax
  syscall
  JMP _0block3
_0block3:
  MOV $60, %rax
  MOV $0, %rdi
  syscall
.data
V0: .ascii Hello
.align 16
V1: .ascii There
.align 16
V2: .ascii World
.align 16
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
