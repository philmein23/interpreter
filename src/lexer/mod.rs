use crate::token::{lookup_identifier, Token, TokenType};
use std::char;

#[derive(Debug)]
pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut lexer = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: '\u{0}',
        };

        lexer.readChar();

        lexer
    }

    pub fn readChar(&mut self) {
        let chars = self.input.chars().collect::<Vec<char>>();
        self.ch = *chars.get(self.read_position).unwrap_or_else(|| &'\u{0}');

        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn read_identifier(&mut self) -> &str {
        let position = self.position;
        while is_letter(self.ch) {
            self.readChar();
        }

        &self.input[position..self.position]
    }

    pub fn read_number(&mut self) -> &str {
        let position = self.position;
        while is_digit(self.ch) {
            self.readChar();
        }

        &self.input[position..self.position]
    }

    pub fn skip_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\n' || self.ch == '\t' || self.ch == '\r' {
            self.readChar();
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let token = match self.ch {
            '=' => Token {
                token_type: TokenType::ASSIGN,
                literal: self.ch.to_string(),
            },
            '+' => Token {
                token_type: TokenType::PLUS,
                literal: self.ch.to_string(),
            },
            ',' => Token {
                token_type: TokenType::COMMA,
                literal: self.ch.to_string(),
            },
            ';' => Token {
                token_type: TokenType::SEMICOLON,
                literal: self.ch.to_string(),
            },
            '(' => Token {
                token_type: TokenType::LPAREN,
                literal: self.ch.to_string(),
            },
            ')' => Token {
                token_type: TokenType::RPAREN,
                literal: self.ch.to_string(),
            },
            '{' => Token {
                token_type: TokenType::LBRACE,
                literal: self.ch.to_string(),
            },
            '}' => Token {
                token_type: TokenType::RBRACE,
                literal: self.ch.to_string(),
            },
            _ => {
                if is_letter(self.ch) {
                    let literal = self.read_identifier();
                    return Token {
                        literal: literal.to_string(),
                        token_type: lookup_identifier(literal).unwrap_or_else(|| TokenType::IDENT),
                    };
                } else if is_digit(self.ch) {
                    let literal = self.read_number();
                    return Token {
                        literal: literal.to_string(),
                        token_type: TokenType::INT,
                    };
                } else {
                    return Token {
                        token_type: TokenType::EOF,
                        literal: self.ch.to_string(),
                    };
                };
            }
        };

        self.readChar();

        token
    }
}

fn is_letter(ch: char) -> bool {
    ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || (ch == '_')
}

fn is_digit(ch: char) -> bool {
    '0' <= ch && ch <= '9'
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = String::from("=+(){},;");
        let expected = vec![
            TokenType::ASSIGN,
            TokenType::PLUS,
            TokenType::LPAREN,
            TokenType::RPAREN,
            TokenType::LBRACE,
            TokenType::RBRACE,
            TokenType::COMMA,
            TokenType::SEMICOLON,
        ];

        let mut lexer = Lexer::new(input);

        for token in expected.iter() {
            let tok = lexer.next_token();
            assert_eq!(&tok.token_type, token);
        }
    }

    #[test]
    fn test_next_token_2() {
        let input = String::from(
            "let five = 5;
        let ten = 10;

        let add = fn(x,y) {
            x + y;
        };
        
        let result = add(five, ten);",
        );

        let expected = vec![
            TokenType::LET,
            TokenType::IDENT,
            TokenType::ASSIGN,
            TokenType::INT,
            TokenType::SEMICOLON,
            TokenType::LET,
            TokenType::IDENT,
            TokenType::ASSIGN,
            TokenType::INT,
            TokenType::SEMICOLON,
            TokenType::LET,
            TokenType::IDENT,
            TokenType::ASSIGN,
            TokenType::FUNCTION,
            TokenType::LPAREN,
            TokenType::IDENT,
            TokenType::COMMA,
            TokenType::IDENT,
            TokenType::RPAREN,
            TokenType::LBRACE,
            TokenType::IDENT,
            TokenType::PLUS,
            TokenType::IDENT,
            TokenType::SEMICOLON,
            TokenType::RBRACE,
            TokenType::SEMICOLON,
            TokenType::LET,
            TokenType::IDENT,
            TokenType::ASSIGN,
            TokenType::IDENT,
            TokenType::LPAREN,
            TokenType::IDENT,
            TokenType::COMMA,
            TokenType::IDENT,
            TokenType::RPAREN,
            TokenType::SEMICOLON,
            TokenType::EOF,
        ];

        let mut lexer = Lexer::new(input);

        for token in expected.iter() {
            let tok = lexer.next_token();
            assert_eq!(&tok.token_type, token);
        }
    }
}
