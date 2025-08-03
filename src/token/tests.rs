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
