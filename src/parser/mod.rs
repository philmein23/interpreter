use crate::ast::{self, Expression, Statement};
use crate::lexer::Lexer;
use crate::token::Token;
use std::error::Error;
use std::mem;

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl Parser {
    fn new(lexer: Lexer) -> Parser {
        let mut p = Parser {
            lexer,
            current_token: Token::ILLEGAL,
            peek_token: Token::ILLEGAL,
            errors: vec![],
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
                    panic!("Error with parsing: {:?}", error);
                }
            }

            self.next_token();
        }

        ast::Program { statements }
    }

    fn parse_statement(&mut self) -> Result<Statement, &'static str> {
        match self.current_token {
            Token::LET => self.parse_let_statement(),
            Token::RETURN => self.parse_return_statement(),
            _ => Err(""),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, &'static str> {
        let name;
        if let Token::IDENT(iden) = self.peek_token.clone() {
            name = iden;
            self.next_token();
        } else {
            return Err("No identifier located");
        }

        if self.expectPeek(Token::ASSIGN) {
            self.next_token();
        } else {
            return Err("There is no ASSIGN (=) token");
        }

        // TODO: remove method call after implementing expression parsing
        self.next_token();

        if self.expectPeek(Token::SEMICOLON) {
            self.next_token();
        }

        Ok(Statement::Let { name })
    }

    fn parse_return_statement(&mut self) -> Result<Statement, &'static str> {
        let value;
        if let Token::INT(v) = self.peek_token.clone() {
            value = v.parse::<i32>().unwrap();
            self.next_token();
        } else {
            return Err("no expression found");
        }

        if self.expectPeek(Token::SEMICOLON) {
            self.next_token();
        }

        Ok(Statement::Return(Some(Expression::IntegerLiteral(value))))
    }

    fn expectPeek(&mut self, expected: Token) -> bool {
        if self.peek_token == expected {
            true
        } else {
            self.peekError(expected);
            false
        }
    }

    fn peekError(&mut self, token: Token) {
        let error_msg = format!(
            "Expected next token to be {:?} but instead got {:?}",
            token, self.peek_token
        );

        self.errors.push(error_msg);
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
        check_parse_errors(parser);

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

    #[test]
    fn test_return_statements() {
        let input = "
        return 5;
        return 10;
        return 993322;
        ";

        let mut lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_errors(parser);

        if program.statements.len() != 3 {
            panic!("Program statements does not contain 3 statements");
        }

        let expected = vec![
            Statement::Return(Some(Expression::IntegerLiteral(5))),
            Statement::Return(Some(Expression::IntegerLiteral(10))),
            Statement::Return(Some(Expression::IntegerLiteral(993322))),
        ];

        assert_eq!(program.statements, expected);
    }

    fn check_parse_errors(parser: Parser) {
        let errors = parser.errors;
        if errors.len() > 0 {
            for error in errors {
                panic!("{:?}", error);
            }
        }
    }
}
