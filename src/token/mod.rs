#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    ILLEGAL,
    EOF,
    IDENT,
    INT,
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    COMMA,
    SEMICOLON,
    LT,
    GT,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
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

// #[derive(Debug)]
// pub struct Token {
//     pub token_type: TokenType,
//     pub literal: String,
// }

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
