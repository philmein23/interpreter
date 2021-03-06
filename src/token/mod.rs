#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    ILLEGAL,
    EOF,
    STRING(String),
    IDENT(String),
    INT(String),
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    COMMA,
    COLON,
    SEMICOLON,
    LT,
    GT,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    LBRACKET,
    RBRACKET,
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
    EQ,
    NOT_EQ,
}

pub fn lookup_identifier(identifier: &str) -> Option<Token> {
    match identifier {
        "fn" => Some(Token::FUNCTION),
        "let" => Some(Token::LET),
        "if" => Some(Token::IF),
        "else" => Some(Token::ELSE),
        "return" => Some(Token::RETURN),
        "true" => Some(Token::TRUE),
        "false" => Some(Token::FALSE),
        _ => None,
    }
}
