use regex::Regex;

use crate::token::{TextLocation, Token, TokenType};

const VARIABLE_PATTERN: &'static str = r"^[_a-zA-Z](?:[\w\-]*[a-zA-Z0-9])?$";
const VALUE_PATTERN: &'static str = r#"^(?:\d+(?:\.\d+)?|".*")$"#;

#[derive(Debug)]
pub enum LexError {
    InvalidToken(Token),
}

impl std::error::Error for LexError {}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexError::InvalidToken(x) => write!(f, "Unable to parse token: {}", x),
        }
    }
}

struct LexState {
    location: TextLocation,
    t_start_idx: usize,
    t_end_idx: usize,
    tokens: Vec<Token>,
    variable_regex: Regex,
    value_regex: Regex,
}

impl LexState {
    fn new() -> Self {
        let variable_regex = Regex::new(VARIABLE_PATTERN)
            .expect("Regex should be able to be created from variable pattern");
        let value_regex = Regex::new(VALUE_PATTERN)
            .expect("Regex should be able to be created from value pattern");

        LexState {
            location: TextLocation::new(),
            t_start_idx: 0,
            t_end_idx: 0,
            tokens: Vec::new(),
            variable_regex: variable_regex,
            value_regex: value_regex,
        }
    }
}

pub fn parse_text(contents: &str) -> Result<Vec<Token>, LexError> {
    let mut state = LexState::new();

    for (i, c) in contents.chars().enumerate() {
        state.location.col_num += 1;
        match c {
            '\n' => {
                process_token(&mut state, contents, Some(i))?;
                state.location.line_num += 1;
                state.location.col_num = 0;
            }
            ';' => {
                process_token(&mut state, contents, None)?;
                state.location.col_num += 1;
                state.t_start_idx = i;
                state.t_end_idx = i + 1;
                process_token(&mut state, contents, Some(i))?;
            }
            _ if c.is_ascii_whitespace() => process_token(&mut state, contents, Some(i))?,
            _ => state.t_end_idx += 1,
        }
    }
    state.location.col_num += 1;
    process_token(&mut state, contents, None)?;

    Ok(state.tokens)
}

fn process_token(
    state: &mut LexState,
    in_str: &str,
    current_idx: Option<usize>,
) -> Result<(), LexError> {
    if state.t_end_idx != state.t_start_idx {
        let t_str = &in_str[state.t_start_idx..state.t_end_idx];
        let token = get_token(t_str, state)?;
        state.tokens.push(token);
    }
    if let Some(idx) = current_idx {
        state.t_start_idx = idx + 1;
        state.t_end_idx = state.t_start_idx;
    }
    Ok(())
}

fn get_token(t_str: &str, state: &LexState) -> Result<Token, LexError> {
    let mut is_unknown_token = false;
    let t_type = match t_str {
        "int" => TokenType::Int,
        "=" => TokenType::Eq,
        ";" => TokenType::Semi,
        x if state.variable_regex.is_match(x) => TokenType::Variable,
        x if state.value_regex.is_match(x) => TokenType::Value,
        _ => {
            is_unknown_token = true;
            TokenType::Unknown
        }
    };

    let token = Token {
        t_type: t_type,
        value: String::from(t_str),
        location: TextLocation {
            line_num: state.location.line_num,
            col_num: state.location.col_num - (state.t_end_idx - state.t_start_idx),
        },
    };

    if is_unknown_token {
        Err(LexError::InvalidToken(token))
    } else {
        Ok(token)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex_empty_tokens() {
        let test_cases = ["", "\n\n", "   "];
        for t in test_cases {
            let tokens = parse_text(t).unwrap();
            assert_eq!(tokens.len(), 0);
        }
    }

    #[test]
    fn test_lex_variable_line() {
        let input = "int x = 10;";
        let tokens = parse_text(input).unwrap();

        let token_cols: Vec<usize> = tokens.iter().map(|x| x.location.col_num).collect();
        let expected_types: Vec<TokenType> = vec![
            TokenType::Int,
            TokenType::Variable,
            TokenType::Eq,
            TokenType::Value,
            TokenType::Semi,
        ];

        assert_eq!(tokens.len(), 5);
        assert_eq!(token_cols, vec![1, 5, 7, 9, 11]);

        for (i, t) in tokens.iter().enumerate() {
            assert_eq!(t.t_type, expected_types[i]);
        }
    }

    #[test]
    fn test_lex_multi_lines() {
        let input = "int x = 10\nint x = 10";
        let tokens = parse_text(input).unwrap();

        let token_cols: Vec<usize> = tokens.iter().map(|x| x.location.col_num).collect();
        let token_lines: Vec<usize> = tokens.iter().map(|x| x.location.line_num).collect();

        assert_eq!(tokens.len(), 8);
        assert_eq!(token_cols, vec![1, 5, 7, 9, 1, 5, 7, 9]);
        assert_eq!(token_lines, vec![1, 1, 1, 1, 2, 2, 2, 2]);
    }

    #[test]
    fn test_should_error_for_invalid_token() {
        let input = "int 1hello = 10\n";
        let result = parse_text(input).unwrap_err();

        assert_eq!(
            result.to_string(),
            String::from("Unable to parse token: [(Unknown: 1hello), Line: 1, Col: 5]")
        );
    }
}
