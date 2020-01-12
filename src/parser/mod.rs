use crate::ast::{self, Expression, Statement};
use crate::lexer::Lexer;
use crate::token::Token;
use std::error::Error;
use std::mem;

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
}

impl Parser {
    fn new(lexer: Lexer) -> Parser {
        let mut p = Parser {
            lexer,
            current_token: Token::ILLEGAL,
            peek_token: Token::ILLEGAL,
        };

        p.next_token();
        p.next_token();
        p
    }

    fn next_token(&mut self) {
        self.current_token = mem::replace(&mut self.peek_token, self.lexer.next_token());
    }

    fn parse_program(&mut self) -> ast::Program {
        let mut statements = vec![];

        while self.current_token != Token::EOF {
            match self.parse_statement() {
                Ok(statement) => {
                    statements.push(statement);
                }
                Err(error) => {
                    panic!("Error with parsing");
                }
            }

            self.next_token();
        }

        ast::Program { statements }
    }

    fn parse_statement(&mut self) -> Result<Statement, Box<dyn Error>> {
        let name;
        if let Token::IDENT(iden) = self.peek_token.clone() {
            name = iden;
            self.next_token();
        } else {
            panic!("No identifier located");
        }

        if self.expectPeek(Token::ASSIGN) {
            self.next_token();
        } else {
            panic!("There is no ASSIGN (=) token");
        }

        // TODO: remove method call after implementing expression parsing
        self.next_token();

        if self.expectPeek(Token::SEMICOLON) {
            self.next_token();
        }

        Ok(Statement::Let { name })
    }

    fn expectPeek(&self, expected: Token) -> bool {
        if self.peek_token == expected {
            true
        } else {
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_let_statements() {
        let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
        ";

        let mut lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        if program.statements.len() != 3 {
            panic!("Program statements does not contain 3 statements");
        }

        let expected = vec![
            Statement::Let {
                name: "x".to_string(),
                // value: Expression::IntegerLiteral(5),
            },
            Statement::Let {
                name: "y".to_string(),
                // value: Expression::IntegerLiteral(10),
            },
            Statement::Let {
                name: "foobar".to_string(),
                // value: Expression::IntegerLiteral(838383),
            },
        ];

        assert_eq!(expected, program.statements);
    }
}
