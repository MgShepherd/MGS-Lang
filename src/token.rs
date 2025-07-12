use std::fmt;

#[derive(Debug, PartialEq, Eq)]
pub enum TokenType {
    Variable(String),
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::Variable(x) => write!(f, "Var: {}", x),
        }
    }
}

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

pub struct Token {
    pub t_type: TokenType,
    pub location: TextLocation,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[({}), {}]", self.t_type, self.location)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_display_token_types() {
        let test_cases = [(TokenType::Variable(String::from("myVar")), "Var: myVar")];

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
            t_type: TokenType::Variable(String::from("testVar")),
        };

        assert_eq!(format!("{}", token), "[(Var: testVar), Line: 10, Col: 5]");
    }
}
