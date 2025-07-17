/*
* MGS Language Current Grammar (EBNF format)
* This grammar representation should always be kept up to date with what has been implemented in
* the parser
*
* Terminal symbols (tokens) are provided in all upper case, anything else is a non-terminal
*
* Program = { Statement, SEMI }
* Statement = AssignmentStatement
* AssignmentStatement = INT, VALUE, EQ, VALUE
*
*/

/*
* The aim of the parser is to take the tokens provided from the lexer and output a Parsed
* representation of the program
*/

use crate::token::{Token, TokenType};

const NUM_TOKENS_IN_ASSIGNMENT: usize = 4;

#[derive(Debug)]
pub enum ParseError {
    InvalidStatement(Token),
    MissingSemicolon(Token),
    EmptyStatement(Token),
    UnexpectedToken(Token, TokenType),
    ExtraToken(Token),
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
pub enum Statement {
    AssignmentStatement { v_name: String, value: String },
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::AssignmentStatement { v_name, value } => {
                write!(f, "Assigning {} to value {}", v_name, value)
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

    while end_idx < tokens.len() {
        match tokens[end_idx].t_type {
            TokenType::Semi => {
                if start_idx == end_idx {
                    return Err(ParseError::EmptyStatement(tokens[end_idx].clone()));
                }
                let statement = parse_statement(&tokens[start_idx..end_idx])?;
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

fn parse_statement(tokens: &[Token]) -> Result<Statement, ParseError> {
    match tokens[0].t_type {
        TokenType::Int => parse_assignment_statement(tokens),
        _ => Err(ParseError::InvalidStatement(tokens[0].clone())),
    }
}

fn parse_assignment_statement(tokens: &[Token]) -> Result<Statement, ParseError> {
    expect_token_type(&tokens[0], TokenType::Int)?;
    expect_token_type(&tokens[1], TokenType::Value)?;
    expect_token_type(&tokens[2], TokenType::Eq)?;
    expect_token_type(&tokens[3], TokenType::Value)?;

    if tokens.len() > NUM_TOKENS_IN_ASSIGNMENT {
        Err(ParseError::ExtraToken(
            tokens[NUM_TOKENS_IN_ASSIGNMENT].clone(),
        ))
    } else {
        Ok(Statement::AssignmentStatement {
            v_name: tokens[1].value.clone(),
            value: tokens[3].value.clone(),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer;

    #[test]
    fn test_valid_assignment_statement() {
        let statement = "int x = 10;";
        let tokens = lexer::parse_text(&statement);
        let program = parse_program(tokens).unwrap();

        assert!(program.statements.len() == 1);
        match &program.statements[0] {
            Statement::AssignmentStatement { v_name, value } => {
                assert_eq!(*v_name, String::from("x"));
                assert_eq!(*value, String::from("10"));
            }
        }
    }

    #[test]
    fn test_multiple_valid_statements() {
        let statement = "int x = 10; int y = 20;";
        let tokens = lexer::parse_text(&statement);
        let program = parse_program(tokens).unwrap();

        assert!(program.statements.len() == 2);
        match &program.statements[0] {
            Statement::AssignmentStatement { v_name, value } => {
                assert_eq!(*v_name, String::from("x"));
                assert_eq!(*value, String::from("10"));
            }
        }
        match &program.statements[1] {
            Statement::AssignmentStatement { v_name, value } => {
                assert_eq!(*v_name, String::from("y"));
                assert_eq!(*value, String::from("20"));
            }
        }
    }

    #[test]
    fn test_should_error_for_empty_statement() {
        let statement = "int x = 10;;";
        let tokens = lexer::parse_text(&statement);
        let e = parse_program(tokens).unwrap_err();

        assert_eq!(
            e.to_string(),
            String::from("Found empty statement after token [(Semicolon: ;), Line: 1, Col: 13]")
        );
    }

    #[test]
    fn test_should_error_for_unrecognised_statement() {
        let statement = "This is not a statement;";
        let tokens = lexer::parse_text(&statement);
        let e = parse_program(tokens).unwrap_err();

        assert_eq!(
            e.to_string(),
            String::from(
                "Unable to parse statement starting from token [(Value: This), Line: 1, Col: 1]"
            )
        );
    }

    #[test]
    fn test_should_error_for_too_many_tokens_in_assignment() {
        let statement = "int x = 10 20;";
        let tokens = lexer::parse_text(&statement);
        let e = parse_program(tokens).unwrap_err();

        assert_eq!(
            e.to_string(),
            String::from(
                "Unexpected token found at end of statement [(Value: 20), Line: 1, Col: 12]"
            )
        );
    }

    #[test]
    fn test_should_error_for_unexpected_token_in_assigment() {
        let statement = "int = 10;";
        let tokens = lexer::parse_text(&statement);
        let e = parse_program(tokens).unwrap_err();

        assert_eq!(
            e.to_string(),
            String::from(
                "Encountered unexpected token: [(Equals: =), Line: 1, Col: 5], expected token with type: Value"
            )
        );
    }

    #[test]
    fn test_should_error_for_missing_semicolon() {
        let statement = "int x = 10 int y = 20;";
        let tokens = lexer::parse_text(&statement);
        let e = parse_program(tokens).unwrap_err();

        assert_eq!(
            e.to_string(),
            String::from(
                "Unexpected token found at end of statement [(Integer: int), Line: 1, Col: 12]"
            )
        );
    }
}
