use core::fmt;

enum TokenType {
    Variable(String),
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::Variable(x) => write!(f, "Var: {}", x),
        }
    }
}

struct TextLocation {
    line_num: usize,
    col_num: usize,
}

impl fmt::Display for TextLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Line: {}, Col: {}", self.line_num, self.col_num)
    }
}

pub struct Token {
    t_type: TokenType,
    location: TextLocation,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[({}), {}]", self.t_type, self.location)
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
            location: TextLocation {
                line_num: 1,
                col_num: 0,
            },
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
                process_whitespace(&mut state, contents, Some(i));
                state.location.line_num += 1;
                state.location.col_num = 0;
            }
            _ if c.is_ascii_whitespace() => process_whitespace(&mut state, contents, Some(i)),
            _ => state.t_end_idx += 1,
        }
    }
    process_whitespace(&mut state, contents, None);
    state.tokens
}

pub fn display_tokens(tokens: &Vec<Token>) {
    for token in tokens {
        println!("{}", token);
    }
}

fn process_whitespace(state: &mut LexState, in_str: &str, current_idx: Option<usize>) {
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
    Token {
        t_type: TokenType::Variable(String::from(t_str)),
        location: TextLocation {
            line_num: state.location.line_num,
            col_num: state.location.col_num - (state.t_end_idx - state.t_start_idx),
        },
    }
}
