#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    ILLEGAL,
    EOF,
    IDENT,
    INT,
    ASSIGN,
    PLUS,
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    FUNCTION,
    LET,
}

pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

pub fn lookup_identifier(identifier: &str) -> Option<TokenType> {
    match identifier {
        "fn" => Some(TokenType::FUNCTION),
        "let" => Some(TokenType::LET),
        _ => None,
    }
}
