use super::*;

fn nth_token_eq(program: &str, i: i32, token: Token) {
    let mut lexer = Reader::new(program, "".to_string());
    for _ in 0..i {
        lexer.next_token().unwrap();
    }
    assert_eq!(token, lexer.next_token().unwrap().tok);
}

fn nth_token_neq(program: &str, i: i32, token: Token) {
    let mut lexer = Reader::new(program, "".to_string());
    for _ in 0..i {
        lexer.next_token().unwrap();
    }
    assert!(token != lexer.next_token().unwrap().tok);
}

fn nth_token_err(program: &str, i: i32, error_kind: ErrorKind) {
    let mut lexer = Reader::new(program, "".to_string());
    for _ in 0..i {
        lexer.next_token().unwrap();
    }
    assert_eq!(error_kind, lexer.next_token().unwrap_err().kind);
}

#[test]
fn test_token_eof() {
    nth_token_eq("", 0, Token::EOF);
    nth_token_eq("test", 1, Token::EOF);

    nth_token_neq("test", 0, Token::EOF);
}

#[test]
fn test_token_identifier() {
    nth_token_eq("test_identifier00", 0, Token::Identifier("test_identifier00".to_string()));

    nth_token_neq("00invalid_identifier", 0, Token::Identifier("00invalid_identifier".to_string()));
}

#[test]
fn test_token_keyword() {
    nth_token_eq("import", 0, Token::Keyword(Keyword::Import));
    nth_token_eq("package", 0, Token::Keyword(Keyword::Package));
    nth_token_eq("func", 0, Token::Keyword(Keyword::Func));
    nth_token_eq("var", 0, Token::Keyword(Keyword::Var));
    nth_token_eq("if", 0, Token::Keyword(Keyword::If));
    nth_token_eq("else", 0, Token::Keyword(Keyword::Else));
    nth_token_eq("while", 0, Token::Keyword(Keyword::While));
    nth_token_eq("struct", 0, Token::Keyword(Keyword::Struct));
    nth_token_eq("return", 0, Token::Keyword(Keyword::Return));
    nth_token_eq("for", 0, Token::Keyword(Keyword::For));
    nth_token_eq("in", 0, Token::Keyword(Keyword::In));
    nth_token_eq("new", 0, Token::Keyword(Keyword::New));
}

#[test]
fn test_token_string_literal() {
    nth_token_eq("\"test string\"", 0, Token::StringLiteral("test string".to_string()));
    nth_token_eq("\"\\n\"", 0, Token::StringLiteral("\n".to_string()));
    nth_token_eq("\"\n\"", 0, Token::StringLiteral("\n".to_string()));

    nth_token_err("\"\\.\"", 0, ErrorKind::UnknownEscapeChar('.'));
    nth_token_err("\"", 0, ErrorKind::InvalidString);
}

#[test]
fn test_token_char_literal() {
    nth_token_eq("'t'", 0, Token::CharLiteral('t'));
    // TODO: add tests on escaping once it is improved for char

    nth_token_err("'te'", 0, ErrorKind::InvalidChar);
    nth_token_err("'t", 0, ErrorKind::InvalidChar);
    nth_token_err("'", 0, ErrorKind::InvalidChar);
}

#[test]
fn test_token_integer_literal() {
    nth_token_eq("42", 0, Token::IntegerLiteral(42));
}

#[test]
fn test_token_float_literal() {
    nth_token_eq("42.42", 0, Token::FloatLiteral(42.42));
    nth_token_eq(".42", 0, Token::FloatLiteral(0.42));

    nth_token_err("42.42.42", 0, ErrorKind::UnexpectedChar('.'));
}

#[test]
fn test_token_bool_literal() {
    nth_token_eq("true", 0, Token::BoolLiteral(true));
    nth_token_eq("false", 0, Token::BoolLiteral(false));
}

#[test]
fn test_token_symbol_literal() {
    nth_token_eq("(", 0, Token::Symbol(Symbol::LeftParenthesis));
    nth_token_eq(")", 0, Token::Symbol(Symbol::RightParenthesis));
    nth_token_eq("[", 0, Token::Symbol(Symbol::LeftBracket));
    nth_token_eq("]", 0, Token::Symbol(Symbol::RightBracket));
    nth_token_eq("{", 0, Token::Symbol(Symbol::LeftBrace));
    nth_token_eq("}", 0, Token::Symbol(Symbol::RightBrace));
    nth_token_eq("&", 0, Token::Symbol(Symbol::Amp));
    nth_token_eq("@", 0, Token::Symbol(Symbol::At));
    nth_token_eq("\n", 0, Token::Symbol(Symbol::NewLine));
    nth_token_eq(".", 0, Token::Symbol(Symbol::Dot));
    nth_token_eq(",", 0, Token::Symbol(Symbol::Comma));
    nth_token_eq(":", 0, Token::Symbol(Symbol::Colon));
    nth_token_eq("::", 0, Token::Symbol(Symbol::ColonColon));
    nth_token_eq("=", 0, Token::Symbol(Symbol::Equal));
    nth_token_eq("==", 0, Token::Symbol(Symbol::EqualEqual));
    nth_token_eq("+", 0, Token::Symbol(Symbol::Plus));
    nth_token_eq("++", 0, Token::Symbol(Symbol::PlusPlus));
    nth_token_eq("-", 0, Token::Symbol(Symbol::Minus));
    nth_token_eq("->", 0, Token::Symbol(Symbol::Return));
    nth_token_eq("*", 0, Token::Symbol(Symbol::Star));
    nth_token_eq("/", 0, Token::Symbol(Symbol::Over));
    nth_token_eq("%", 0, Token::Symbol(Symbol::Modulo));
    nth_token_eq("!=", 0, Token::Symbol(Symbol::NotEqual));
    nth_token_eq("#", 0, Token::Symbol(Symbol::Hash));
    nth_token_eq("<", 0, Token::Symbol(Symbol::Less));
    nth_token_eq("<=", 0, Token::Symbol(Symbol::LessOrEqual));
    nth_token_eq("<>", 0, Token::Symbol(Symbol::Concat));
    nth_token_eq(">", 0, Token::Symbol(Symbol::More));
    nth_token_eq(">=", 0, Token::Symbol(Symbol::MoreOrEqual));

    nth_token_err("!", 0, ErrorKind::InvalidSymbol);
    nth_token_err("$", 0, ErrorKind::InvalidSymbol);
}
