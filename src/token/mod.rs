#[cfg(test)]
mod tests;

use std::fmt;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenType {
    Value,
    Variable,
    Int,
    Eq,
    Semi,
    Unknown,
    ArithmeticOp,
    BooleanOp,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::Value => write!(f, "Value"),
            TokenType::Variable => write!(f, "Variable"),
            TokenType::Int => write!(f, "Integer"),
            TokenType::Eq => write!(f, "Equals"),
            TokenType::Semi => write!(f, "Semicolon"),
            TokenType::ArithmeticOp => write!(f, "Arithmetic Operator"),
            TokenType::BooleanOp => write!(f, "Boolean Operator"),
            TokenType::Unknown => write!(f, "Unknown"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct TextLocation {
    pub line_num: usize,
    pub col_num: usize,
}

impl TextLocation {
    pub fn new() -> Self {
        Self {
            line_num: 1,
            col_num: 0,
        }
    }
}

impl fmt::Display for TextLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Line: {}, Col: {}", self.line_num, self.col_num)
    }
}

#[derive(Clone, Debug)]
pub struct Token {
    pub t_type: TokenType,
    pub value: String,
    pub location: TextLocation,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[({}: {}), {}]", self.t_type, self.value, self.location)
    }
}
