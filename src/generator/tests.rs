use crate::parser::Operator;

use super::*;

const PRELUDE: &str = ".section .text\n.global _start\n_start:\n  mov x29, sp\n";
const POSTLUDE: &str = "  mov x0, #0\n  mov x8, #93\n  svc #0\n";

#[test]
fn should_generate_empty_assembly_program() {
    let output = generate(&Target::ARM64, Program { statements: vec![] }).unwrap();
    starts_with_prelude(&output);
    ends_with_postlude(&output)
}

#[test]
fn should_generate_declaration_statement() {
    let output = generate(
        &Target::ARM64,
        Program {
            statements: vec![Statement::DeclarationStatement {
                v_name: String::from("x"),
                expr: Expression::ValExpr(String::from("10")),
            }],
        },
    )
    .unwrap();

    starts_with_prelude(&output);
    contains_body(&output, "  mov x0, #10\n  str x0, [sp, #-16]!\n");
    ends_with_postlude(&output)
}

#[test]
fn should_generate_multiple_declarations() {
    let output = generate(
        &Target::ARM64,
        Program {
            statements: vec![
                Statement::DeclarationStatement {
                    v_name: String::from("x"),
                    expr: Expression::ValExpr(String::from("10")),
                },
                Statement::DeclarationStatement {
                    v_name: String::from("y"),
                    expr: Expression::ValExpr(String::from("32")),
                },
            ],
        },
    )
    .unwrap();

    starts_with_prelude(&output);
    contains_body(
        &output,
        "  mov x0, #10\n  str x0, [sp, #-16]!\n  mov x0, #32\n  str x0, [sp, #-16]!\n",
    );
    ends_with_postlude(&output)
}

#[test]
fn should_generate_assignment_statement() {
    let output = generate(
        &Target::ARM64,
        Program {
            statements: vec![
                Statement::DeclarationStatement {
                    v_name: String::from("x"),
                    expr: Expression::ValExpr(String::from("10")),
                },
                Statement::AssignmentStatement {
                    v_name: String::from("x"),
                    expr: Expression::ValExpr(String::from("32")),
                },
            ],
        },
    )
    .unwrap();

    starts_with_prelude(&output);
    contains_body(
        &output,
        "  mov x0, #10\n  str x0, [sp, #-16]!\n  mov x0, #32\n  str x0, [x29, #-16]\n",
    );
    ends_with_postlude(&output)
}

#[test]
fn should_support_variable_expressions() {
    let output = generate(
        &Target::ARM64,
        Program {
            statements: vec![
                Statement::DeclarationStatement {
                    v_name: String::from("x"),
                    expr: Expression::ValExpr(String::from("10")),
                },
                Statement::DeclarationStatement {
                    v_name: String::from("y"),
                    expr: Expression::VarExpr(String::from("x")),
                },
            ],
        },
    )
    .unwrap();

    starts_with_prelude(&output);
    contains_body(
        &output,
        "  mov x0, #10\n  str x0, [sp, #-16]!\n  ldr x0, [x29, #-16]\n  str x0, [sp, #-16]!\n",
    );
    ends_with_postlude(&output)
}

#[test]
fn should_support_simple_arithmetic_expressions() {
    let output = generate(
        &Target::ARM64,
        Program {
            statements: vec![Statement::DeclarationStatement {
                v_name: String::from("x"),
                expr: Expression::ArithmeticExpr(
                    Box::from(Expression::ValExpr(String::from("10"))),
                    Operator::Add,
                    Box::from(Expression::ValExpr(String::from("7"))),
                ),
            }],
        },
    )
    .unwrap();

    starts_with_prelude(&output);
    contains_body(
        &output,
        "  mov x1, #10\n  mov x2, #7\n  add x0, x1, x2\n  str x0, [sp, #-16]!\n",
    );
    ends_with_postlude(&output)
}

#[test]
fn should_support_chained_arithmetic_expressions() {
    let output = generate(
        &Target::ARM64,
        Program {
            statements: vec![Statement::DeclarationStatement {
                v_name: String::from("x"),
                expr: Expression::ArithmeticExpr(
                    Box::from(Expression::ValExpr(String::from("10"))),
                    Operator::Add,
                    Box::from(Expression::ArithmeticExpr(
                        Box::from(Expression::ValExpr(String::from("20"))),
                        Operator::Sub,
                        Box::from(Expression::ValExpr(String::from("12"))),
                    )),
                ),
            }],
        },
    )
    .unwrap();

    starts_with_prelude(&output);
    contains_body(
        &output,
        "  mov x1, #10\n  mov x3, #20\n  mov x4, #12\n  sub x2, x3, x4\n  add x0, x1, x2\n  str x0, [sp, #-16]!\n",
    );
    ends_with_postlude(&output)
}

#[test]
fn should_return_err_when_assigning_to_undefined_var() {
    let output = generate(
        &Target::ARM64,
        Program {
            statements: vec![Statement::AssignmentStatement {
                v_name: String::from("x"),
                expr: Expression::ValExpr(String::from("32")),
            }],
        },
    )
    .unwrap_err();

    assert_eq!(
        output.to_string(),
        "Unexpected error occurred during program generation: Undefined variable: x"
    );
}

fn starts_with_prelude(output: &str) {
    assert_eq!(&output[0..PRELUDE.len()], PRELUDE);
}

fn ends_with_postlude(output: &str) {
    assert_eq!(&output[(output.len() - POSTLUDE.len())..], POSTLUDE);
}

fn contains_body(output: &str, expected_body: &str) {
    assert_eq!(
        &output[PRELUDE.len()..(output.len() - POSTLUDE.len())],
        expected_body
    );
}
