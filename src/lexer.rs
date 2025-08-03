use crate::{
    constants,
    token::{TextLocation, Token, TokenType},
};

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
}

impl LexState {
    fn new() -> Self {
        LexState {
            location: TextLocation::new(),
            t_start_idx: 0,
            t_end_idx: 0,
            tokens: Vec::new(),
        }
    }
}

pub fn parse_text(contents: &str) -> Result<Vec<Token>, LexError> {
    let mut state = LexState::new();

    for (i, c) in contents.chars().enumerate() {
        handle_next_char(&mut state, contents, c, i)?;
    }
    handle_next_char(&mut state, contents, ' ', contents.len())?;

    Ok(state.tokens)
}

fn handle_next_char(
    state: &mut LexState,
    contents: &str,
    curr: char,
    idx: usize,
) -> Result<(), LexError> {
    state.location.col_num += 1;
    match curr {
        '\n' => {
            process_token(state, contents, Some(idx))?;
            state.location.line_num += 1;
            state.location.col_num = 0;
        }
        ';' => {
            process_token(state, contents, None)?;
            state.location.col_num += 1;
            state.t_start_idx = idx;
            state.t_end_idx = idx + 1;
            process_token(state, contents, Some(idx))?;
            state.location.col_num -= 1;
        }
        _ if curr.is_ascii_whitespace() => process_token(state, contents, Some(idx))?,
        _ => state.t_end_idx += 1,
    }
    Ok(())
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
        "+" | "-" => TokenType::ArithmeticOp,
        x if constants::VARIABLE_REGEX.is_match(x) => TokenType::Variable,
        x if constants::VALUE_REGEX.is_match(x) => TokenType::Value,
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
        let input = "int x = 10 + -; -";
        let tokens = parse_text(input).unwrap();

        let token_cols: Vec<usize> = tokens.iter().map(|x| x.location.col_num).collect();
        let expected_types: Vec<TokenType> = vec![
            TokenType::Int,
            TokenType::Variable,
            TokenType::Eq,
            TokenType::Value,
            TokenType::ArithmeticOp,
            TokenType::ArithmeticOp,
            TokenType::Semi,
            TokenType::ArithmeticOp,
        ];

        assert_eq!(tokens.len(), 8);
        assert_eq!(token_cols, vec![1, 5, 7, 9, 12, 14, 15, 17]);

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
