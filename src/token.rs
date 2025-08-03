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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_display_token_types() {
        let test_cases = [
            (TokenType::Value, "Value"),
            (TokenType::Variable, "Variable"),
            (TokenType::Int, "Integer"),
            (TokenType::Eq, "Equals"),
            (TokenType::Semi, "Semicolon"),
            (TokenType::ArithmeticOp, "Arithmetic Operator"),
            (TokenType::BooleanOp, "Boolean Operator"),
            (TokenType::Unknown, "Unknown"),
        ];

        for (t_type, output) in test_cases {
            assert_eq!(format!("{}", t_type), output);
        }
    }

    #[test]
    fn test_display_text_location() {
        let loc = TextLocation {
            line_num: 10,
            col_num: 5,
        };

        assert_eq!(format!("{}", loc), "Line: 10, Col: 5");
    }

    #[test]
    fn test_display_token() {
        let token = Token {
            location: TextLocation {
                line_num: 10,
                col_num: 5,
            },
            value: String::from("test"),
            t_type: TokenType::Value,
        };

        assert_eq!(format!("{}", token), "[(Value: test), Line: 10, Col: 5]");
    }
}
