mod lexer;

fn main() {
    let tokens = lexer::parse_text("Hello  World This is my new\nprogramming language");
    lexer::display_tokens(&tokens);
}
