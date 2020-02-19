use crate::token::{lookup_identifier, Token};
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

        lexer.read_char();

        lexer
    }

    pub fn read_char(&mut self) {
        let chars = self.input.chars().collect::<Vec<char>>();
        self.ch = *chars.get(self.read_position).unwrap_or_else(|| &'\u{0}');

        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn peek_char(&self) -> char {
        let chars = self.input.chars().collect::<Vec<char>>();
        *chars.get(self.read_position).unwrap_or_else(|| &'\u{0}')
    }

    pub fn read_identifier(&mut self) -> &str {
        let position = self.position;
        while is_letter(self.ch) {
            self.read_char();
        }

        &self.input[position..self.position]
    }

    pub fn read_number(&mut self) -> &str {
        let position = self.position;
        while is_digit(self.ch) {
            self.read_char();
        }

        &self.input[position..self.position]
    }

    pub fn skip_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\n' || self.ch == '\t' || self.ch == '\r' {
            self.read_char();
        }
    }

    pub fn read_string(&mut self) -> &str {
        let position = self.position + 1;
        loop {
            self.read_char();
            if self.ch == '"' || self.ch == '\u{0}' {
                break;
            }
        }
        &self.input[position..self.position]
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let token = match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::EQ
                } else {
                    Token::ASSIGN
                }
            }
            '+' => Token::PLUS,

            ',' => Token::COMMA,

            ':' => Token::COLON,

            ';' => Token::SEMICOLON,

            '(' => Token::LPAREN,

            ')' => Token::RPAREN,

            '{' => Token::LBRACE,

            '}' => Token::RBRACE,

            '[' => Token::LBRACKET,

            ']' => Token::RBRACKET,

            '-' => Token::MINUS,

            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::NOT_EQ
                } else {
                    Token::BANG
                }
            }
            '*' => Token::ASTERISK,

            '/' => Token::SLASH,

            '>' => Token::GT,

            '<' => Token::LT,
            '"' => {
                let val = self.read_string();
                Token::STRING(val.to_string())
            }
            _ => {
                if is_letter(self.ch) {
                    let literal = self.read_identifier();
                    return lookup_identifier(literal)
                        .unwrap_or_else(|| Token::IDENT(literal.to_string()));
                } else if is_digit(self.ch) {
                    let literal = self.read_number();
                    return Token::INT(literal.to_string());
                } else {
                    return Token::EOF;
                };
            }
        };

        self.read_char();
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
            Token::ASSIGN,
            Token::PLUS,
            Token::LPAREN,
            Token::RPAREN,
            Token::LBRACE,
            Token::RBRACE,
            Token::COMMA,
            Token::SEMICOLON,
        ];

        let mut lexer = Lexer::new(input);

        for token in expected.iter() {
            let tok = lexer.next_token();
            assert_eq!(&tok, token);
        }
    }

    #[test]
    fn test_next_token_2() {
        let input = String::from(
            r#"let five = 5;
        let ten = 10;

        let add = fn(x,y) {
            x + y;
        };
        
        let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;
        
        if (5 < 10) {
            return true;
        } else {
            return false;
        }
        10 == 10;
        10 != 9;
        "foobar"
        "foo bar" 
        [1, 2];
        { "foo": "bar" }
        "#,
        );

        let expected = vec![
            Token::LET,
            Token::IDENT("five".to_string()),
            Token::ASSIGN,
            Token::INT("5".to_string()),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("ten".to_string()),
            Token::ASSIGN,
            Token::INT("10".to_string()),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("add".to_string()),
            Token::ASSIGN,
            Token::FUNCTION,
            Token::LPAREN,
            Token::IDENT("x".to_string()),
            Token::COMMA,
            Token::IDENT("y".to_string()),
            Token::RPAREN,
            Token::LBRACE,
            Token::IDENT("x".to_string()),
            Token::PLUS,
            Token::IDENT("y".to_string()),
            Token::SEMICOLON,
            Token::RBRACE,
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("result".to_string()),
            Token::ASSIGN,
            Token::IDENT("add".to_string()),
            Token::LPAREN,
            Token::IDENT("five".to_string()),
            Token::COMMA,
            Token::IDENT("ten".to_string()),
            Token::RPAREN,
            Token::SEMICOLON,
            Token::BANG,
            Token::MINUS,
            Token::SLASH,
            Token::ASTERISK,
            Token::INT("5".to_string()),
            Token::SEMICOLON,
            Token::INT("5".to_string()),
            Token::LT,
            Token::INT("10".to_string()),
            Token::GT,
            Token::INT("5".to_string()),
            Token::SEMICOLON,
            Token::IF,
            Token::LPAREN,
            Token::INT("5".to_string()),
            Token::LT,
            Token::INT("10".to_string()),
            Token::RPAREN,
            Token::LBRACE,
            Token::RETURN,
            Token::TRUE,
            Token::SEMICOLON,
            Token::RBRACE,
            Token::ELSE,
            Token::LBRACE,
            Token::RETURN,
            Token::FALSE,
            Token::SEMICOLON,
            Token::RBRACE,
            Token::INT("10".to_string()),
            Token::EQ,
            Token::INT("10".to_string()),
            Token::SEMICOLON,
            Token::INT("10".to_string()),
            Token::NOT_EQ,
            Token::INT("9".to_string()),
            Token::SEMICOLON,
            Token::STRING("foobar".to_string()),
            Token::STRING("foo bar".to_string()),
            Token::LBRACKET,
            Token::INT("1".to_string()),
            Token::COMMA,
            Token::INT("2".to_string()),
            Token::RBRACKET,
            Token::SEMICOLON,
            Token::LBRACE,
            Token::STRING("foo".to_string()),
            Token::COLON,
            Token::STRING("bar".to_string()),
            Token::RBRACE,
            Token::EOF,
        ];

        let mut lexer = Lexer::new(input);

        for token in expected.iter() {
            let tok = lexer.next_token();
            assert_eq!(&tok, token);
        }
    }
}
