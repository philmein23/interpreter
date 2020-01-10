#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
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

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

pub fn lookup_identifier(identifier: &str) -> Option<TokenType> {
    match identifier {
        "fn" => Some(TokenType::FUNCTION),
        "let" => Some(TokenType::LET),
        "if" => Some(TokenType::IF),
        "else" => Some(TokenType::ELSE),
        "return" => Some(TokenType::RETURN),
        "true" => Some(TokenType::TRUE),
        "false" => Some(TokenType::FALSE),
        _ => None,
    }
}
