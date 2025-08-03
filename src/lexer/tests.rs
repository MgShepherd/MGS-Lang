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
    let input = "int x = 10 + -; >";
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
        TokenType::BooleanOp,
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
