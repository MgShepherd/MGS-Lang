use super::*;
use crate::lexer;

#[test]
fn test_valid_declaration_statement() {
    let statement = "int x = 10;";
    let tokens = lexer::parse_text(&statement).unwrap();
    let program = parse_program(tokens).unwrap();

    assert!(program.statements.len() == 1);
    match &program.statements[0] {
        Statement::DeclarationStatement { v_name, expr } => {
            assert_eq!(*v_name, String::from("x"));
            match expr {
                Expression::ValExpr(x) => assert_eq!(*x, String::from("10")),
                x => panic!("Unexpected expression: {}", x),
            }
        }
        x => panic!("Unexpected statement: {}", x),
    }
}

#[test]
fn test_valid_assignment_statement() {
    let statement = "int x = 10;\nx = 20;";
    let tokens = lexer::parse_text(&statement).unwrap();
    let program = parse_program(tokens).unwrap();

    assert!(program.statements.len() == 2);
    match &program.statements[0] {
        Statement::DeclarationStatement {
            v_name,
            expr: Expression::ValExpr(x),
        } => {
            assert_eq!(*v_name, String::from("x"));
            assert_eq!(*x, String::from("10"));
        }
        x => panic!("Unexpected statement: {}", x),
    }
    match &program.statements[1] {
        Statement::AssignmentStatement {
            v_name,
            expr: Expression::ValExpr(x),
        } => {
            assert_eq!(*v_name, String::from("x"));
            assert_eq!(*x, String::from("20"));
        }
        x => panic!("Unexpected statement: {}", x),
    }
}

#[test]
fn test_valid_variable_expression() {
    let statement = "int x = 10;int y = x;";
    let tokens = lexer::parse_text(&statement).unwrap();
    let program = parse_program(tokens).unwrap();

    assert!(program.statements.len() == 2);
    match &program.statements[1] {
        Statement::DeclarationStatement { v_name, expr } => {
            assert_eq!(*v_name, String::from("y"));
            match expr {
                Expression::VarExpr(x) => assert_eq!(*x, String::from("x")),
                x => panic!("Unexpected expression: {}", x),
            }
        }
        x => panic!("Unexpected statement: {}", x),
    }
}

#[test]
fn test_valid_arithmetic_expression() {
    let statement = "int x = 10 + 8 - 4;";
    let tokens = lexer::parse_text(&statement).unwrap();
    let program = parse_program(tokens).unwrap();

    assert!(program.statements.len() == 1);
    match &program.statements[0] {
        Statement::DeclarationStatement { v_name, expr } => {
            assert_eq!(*v_name, String::from("x"));
            if let Expression::ArithmeticExpr(x, op, y) = expr.clone() {
                if let Expression::ValExpr(v) = *x {
                    assert_eq!(v, String::from("10"));
                    assert_eq!(op, Operator::Add);
                    if let Expression::ArithmeticExpr(a, op2, b) = *y {
                        if let Expression::ValExpr(c) = *a {
                            assert_eq!(c, String::from("8"));
                            assert_eq!(op2, Operator::Sub);
                        } else {
                            panic!("Expected Value expression, but got {}", a);
                        }
                        if let Expression::ValExpr(d) = *b {
                            assert_eq!(d, String::from("4"));
                        } else {
                            panic!("Expected Value expression, but got {}", b);
                        }
                    }
                } else {
                    panic!("Expected Value expression, but got {}", x);
                }
            } else {
                panic!("Expected Arithmetic expression, but got {}", expr);
            }
        }
        x => panic!("Unexpected statement: {}", x),
    }
}

#[test]
fn test_declaration_should_error_for_redefined_var() {
    let statements = "int x = 20;int x = 100;";
    let tokens = lexer::parse_text(&statements).unwrap();
    let e = parse_program(tokens).unwrap_err();
    assert_eq!(
        e.to_string(),
        String::from("Attempted to redeclare variable: [(Variable: x), Line: 1, Col: 16]")
    );
}

#[test]
fn test_assignment_should_error_for_undefined_var() {
    let statement = "x = 20;";
    let tokens = lexer::parse_text(&statement).unwrap();
    let e = parse_program(tokens).unwrap_err();
    assert_eq!(
        e.to_string(),
        String::from("Undefined variable: [(Variable: x), Line: 1, Col: 1]")
    );
}

#[test]
fn test_should_error_for_undefined_var_in_expr() {
    let statement = "int x = y;";
    let tokens = lexer::parse_text(&statement).unwrap();
    let e = parse_program(tokens).unwrap_err();
    assert_eq!(
        e.to_string(),
        String::from("Undefined variable: [(Variable: y), Line: 1, Col: 9]")
    );
}

#[test]
fn test_should_error_for_empty_statement() {
    let statement = "int x = 10;;";
    let tokens = lexer::parse_text(&statement).unwrap();
    let e = parse_program(tokens).unwrap_err();

    assert_eq!(
        e.to_string(),
        String::from("Found empty statement after token [(Semicolon: ;), Line: 1, Col: 12]")
    );
}

#[test]
fn test_should_error_for_unrecognised_statement() {
    let statement = "= is not a statement;";
    let tokens = lexer::parse_text(&statement).unwrap();
    let e = parse_program(tokens).unwrap_err();

    assert_eq!(
        e.to_string(),
        String::from(
            "Unable to parse statement starting from token [(Equals: =), Line: 1, Col: 1]"
        )
    );
}

#[test]
fn test_should_error_for_unexpected_token() {
    let statement = "int = 10;";
    let tokens = lexer::parse_text(&statement).unwrap();
    let e = parse_program(tokens).unwrap_err();

    assert_eq!(
        e.to_string(),
        String::from(
            "Unable to parse statement starting from token [(Integer: int), Line: 1, Col: 1]"
        )
    );
}

#[test]
fn test_should_error_for_missing_semicolon() {
    let statement = "int x = 10 int y = 20;";
    let tokens = lexer::parse_text(&statement).unwrap();
    let e = parse_program(tokens).unwrap_err();

    assert_eq!(
        e.to_string(),
        String::from(
            "Unable to parse expression starting from token [(Value: 10), Line: 1, Col: 9]"
        )
    );
}
