/*
* MGS Language Current Grammar (EBNF format)
* This grammar representation should always be kept up to date with what has been implemented in
* the parser
*
* Terminal symbols (tokens) are provided in all upper case, anything else is a non-terminal
*
* Program = { Statement, SEMI }
* Statement = DeclarationStatement | AssignmentStatement
* DeclarationStatement = INT, VARIABLE, EQ, VALUE
* AssignmentStatement = VARIABLE, EQ, VALUE
*
*/

/*
* The aim of the parser is to take the tokens provided from the lexer and output a Parsed
* representation of the program
*/

use std::collections::HashMap;

use crate::{
    constants,
    token::{Token, TokenType},
};

const NUM_TOKENS_IN_DECLARATION: usize = 4;
const NUM_TOKENS_IN_ASSIGNMENT: usize = 3;

#[derive(Debug)]
pub enum ParseError {
    InvalidStatement(Token),
    MissingSemicolon(Token),
    EmptyStatement(Token),
    UnexpectedToken(Token, TokenType),
    ExtraToken(Token),
    RedeclaringVariable(Token),
    UndefinedVariable(Token),
    InvalidExpression(Token),
}

impl std::error::Error for ParseError {}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::InvalidStatement(x) => {
                write!(f, "Unable to parse statement starting from token {}", x)
            }
            ParseError::EmptyStatement(x) => {
                write!(f, "Found empty statement after token {}", x)
            }
            ParseError::UnexpectedToken(x, expected_type) => {
                write!(
                    f,
                    "Encountered unexpected token: {}, expected token with type: {}",
                    x, expected_type
                )
            }
            ParseError::MissingSemicolon(x) => {
                write!(f, "No semicolon found after statement starting with: {}", x)
            }
            ParseError::ExtraToken(x) => {
                write!(f, "Unexpected token found at end of statement {}", x)
            }
            ParseError::RedeclaringVariable(x) => {
                write!(f, "Attempted to redeclare variable: {}", x)
            }
            ParseError::UndefinedVariable(x) => {
                write!(f, "Undefined variable: {}", x)
            }
            ParseError::InvalidExpression(x) => {
                write!(f, "Unable to parse expression starting from token {}", x)
            }
        }
    }
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Program: ")?;
        for statement in &self.statements {
            writeln!(f, "{}", statement)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub enum Expression {
    ValExpr(String),
    VarExpr(String),
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::ValExpr(x) => write!(f, "{}", x),
            Expression::VarExpr(x) => write!(f, "{}", x),
        }
    }
}

#[derive(Debug)]
pub enum Statement {
    DeclarationStatement { v_name: String, expr: Expression },
    AssignmentStatement { v_name: String, expr: Expression },
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::DeclarationStatement { v_name, expr } => {
                write!(f, "Declaring {} with value {}", v_name, expr)
            }
            Statement::AssignmentStatement { v_name, expr } => {
                write!(f, "Assigning {} to value {}", v_name, expr)
            }
        }
    }
}

pub fn parse_program(tokens: Vec<Token>) -> Result<Program, ParseError> {
    let statements = parse_statements(tokens)?;
    Ok(Program {
        statements: statements,
    })
}

fn parse_statements(tokens: Vec<Token>) -> Result<Vec<Statement>, ParseError> {
    let mut start_idx: usize = 0;
    let mut end_idx: usize = 0;
    let mut statements: Vec<Statement> = Vec::new();
    let mut v_table: HashMap<String, i32> = HashMap::new();

    while end_idx < tokens.len() {
        match tokens[end_idx].t_type {
            TokenType::Semi => {
                if start_idx == end_idx {
                    return Err(ParseError::EmptyStatement(tokens[end_idx].clone()));
                }
                let statement = parse_statement(&tokens[start_idx..end_idx], &mut v_table)?;
                statements.push(statement);
                end_idx += 1;
                start_idx = end_idx;
            }
            _ => end_idx += 1,
        }
    }

    if start_idx != end_idx {
        Err(ParseError::MissingSemicolon(tokens[start_idx].clone()))
    } else {
        Ok(statements)
    }
}

fn parse_statement(
    tokens: &[Token],
    v_table: &mut HashMap<String, i32>,
) -> Result<Statement, ParseError> {
    match tokens[0].t_type {
        TokenType::Int => parse_declaration_statement(tokens, v_table),
        TokenType::Variable => parse_assignment_statement(tokens, v_table),
        _ => Err(ParseError::InvalidStatement(tokens[0].clone())),
    }
}

fn parse_declaration_statement(
    tokens: &[Token],
    v_table: &mut HashMap<String, i32>,
) -> Result<Statement, ParseError> {
    expect_token_type(&tokens[0], TokenType::Int)?;
    expect_token_type(&tokens[1], TokenType::Variable)?;
    expect_token_type(&tokens[2], TokenType::Eq)?;
    let expr = expect_expression(&tokens[3..], v_table)?;

    if v_table.contains_key(&tokens[1].value) {
        return Err(ParseError::RedeclaringVariable(tokens[1].clone()));
    }

    if tokens.len() > NUM_TOKENS_IN_DECLARATION {
        Err(ParseError::ExtraToken(
            tokens[NUM_TOKENS_IN_DECLARATION].clone(),
        ))
    } else {
        v_table.insert(tokens[1].value.clone(), 1);
        Ok(Statement::DeclarationStatement {
            v_name: tokens[1].value.clone(),
            expr,
        })
    }
}

fn parse_assignment_statement(
    tokens: &[Token],
    v_table: &mut HashMap<String, i32>,
) -> Result<Statement, ParseError> {
    expect_token_type(&tokens[0], TokenType::Variable)?;
    expect_token_type(&tokens[1], TokenType::Eq)?;
    let expr = expect_expression(&tokens[2..], v_table)?;

    if !v_table.contains_key(&tokens[0].value) {
        return Err(ParseError::UndefinedVariable(tokens[0].clone()));
    }

    if tokens.len() > NUM_TOKENS_IN_ASSIGNMENT {
        Err(ParseError::ExtraToken(
            tokens[NUM_TOKENS_IN_ASSIGNMENT].clone(),
        ))
    } else {
        Ok(Statement::AssignmentStatement {
            v_name: tokens[0].value.clone(),
            expr,
        })
    }
}

fn expect_token_type(actual: &Token, expected: TokenType) -> Result<(), ParseError> {
    if actual.t_type != expected {
        Err(ParseError::UnexpectedToken(actual.clone(), expected))
    } else {
        Ok(())
    }
}

fn expect_expression(
    tokens: &[Token],
    v_table: &HashMap<String, i32>,
) -> Result<Expression, ParseError> {
    match &tokens[0] {
        x if constants::VALUE_REGEX.is_match(&x.value) => Ok(Expression::ValExpr(x.value.clone())),
        x if constants::VARIABLE_REGEX.is_match(&x.value) => {
            if v_table.contains_key(&x.value) {
                Ok(Expression::VarExpr(x.value.clone()))
            } else {
                Err(ParseError::UndefinedVariable(x.clone()))
            }
        }
        x => Err(ParseError::InvalidExpression(x.clone())),
    }
}

#[cfg(test)]
mod tests {
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
    fn test_declaration_should_error_for_redefined_var() {
        let statements = "int x = 20;int x = 100;";
        let tokens = lexer::parse_text(&statements).unwrap();
        let e = parse_program(tokens).unwrap_err();
        assert_eq!(
            e.to_string(),
            String::from("Attempted to redeclare variable: [(Variable: x), Line: 1, Col: 17]")
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
            String::from("Found empty statement after token [(Semicolon: ;), Line: 1, Col: 13]")
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
    fn test_should_error_for_too_many_tokens_in_declaration() {
        let statement = "int x = 10 20;";
        let tokens = lexer::parse_text(&statement).unwrap();
        let e = parse_program(tokens).unwrap_err();

        assert_eq!(
            e.to_string(),
            String::from(
                "Unexpected token found at end of statement [(Value: 20), Line: 1, Col: 12]"
            )
        );
    }

    #[test]
    fn test_should_error_for_too_many_tokens_in_assignment() {
        let statement = "int x = 5;x = 10 int;";
        let tokens = lexer::parse_text(&statement).unwrap();
        let e = parse_program(tokens).unwrap_err();

        assert_eq!(
            e.to_string(),
            String::from(
                "Unexpected token found at end of statement [(Integer: int), Line: 1, Col: 19]"
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
                "Encountered unexpected token: [(Equals: =), Line: 1, Col: 5], expected token with type: Variable"
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
                "Unexpected token found at end of statement [(Integer: int), Line: 1, Col: 12]"
            )
        );
    }
}
