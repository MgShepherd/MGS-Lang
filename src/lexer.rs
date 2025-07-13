use crate::token::{TextLocation, Token, TokenType};

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

pub fn parse_text(contents: &str) -> Vec<Token> {
    let mut state = LexState::new();

    for (i, c) in contents.chars().enumerate() {
        state.location.col_num += 1;
        match c {
            '\n' => {
                process_token(&mut state, contents, Some(i));
                state.location.line_num += 1;
                state.location.col_num = 0;
            }
            ';' => {
                process_token(&mut state, contents, None);
                state.location.col_num += 1;
                state.t_start_idx = i;
                state.t_end_idx = i + 1;
                process_token(&mut state, contents, Some(i));
            }
            _ if c.is_ascii_whitespace() => process_token(&mut state, contents, Some(i)),
            _ => state.t_end_idx += 1,
        }
    }
    state.location.col_num += 1;
    process_token(&mut state, contents, None);

    state.tokens
}

fn process_token(state: &mut LexState, in_str: &str, current_idx: Option<usize>) {
    if state.t_end_idx != state.t_start_idx {
        let t_str = &in_str[state.t_start_idx..state.t_end_idx];
        state.tokens.push(get_token(t_str, state));
    }
    if let Some(idx) = current_idx {
        state.t_start_idx = idx + 1;
        state.t_end_idx = state.t_start_idx;
    }
}

fn get_token(t_str: &str, state: &LexState) -> Token {
    let t_type = match t_str {
        "int" => TokenType::Int,
        "=" => TokenType::Eq,
        ";" => TokenType::Semi,
        _ => TokenType::Value,
    };

    Token {
        t_type: t_type,
        value: String::from(t_str),
        location: TextLocation {
            line_num: state.location.line_num,
            col_num: state.location.col_num - (state.t_end_idx - state.t_start_idx),
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex_empty_tokens() {
        let test_cases = ["", "\n\n", "   "];
        for t in test_cases {
            let tokens = parse_text(t);
            assert_eq!(tokens.len(), 0);
        }
    }

    #[test]
    fn test_lex_variable_line() {
        let input = "int x = 10;";
        let tokens = parse_text(input);

        let token_cols: Vec<usize> = tokens.iter().map(|x| x.location.col_num).collect();
        let expected_types: Vec<TokenType> = vec![
            TokenType::Int,
            TokenType::Value,
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
        let tokens = parse_text(input);

        let token_cols: Vec<usize> = tokens.iter().map(|x| x.location.col_num).collect();
        let token_lines: Vec<usize> = tokens.iter().map(|x| x.location.line_num).collect();

        assert_eq!(tokens.len(), 8);
        assert_eq!(token_cols, vec![1, 5, 7, 9, 1, 5, 7, 9]);
        assert_eq!(token_lines, vec![1, 1, 1, 1, 2, 2, 2, 2]);
    }
}
