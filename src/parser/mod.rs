use crate::ast::{self, Expression, Statement};
use crate::lexer::Lexer;
use crate::token::Token;
use std::mem;

pub enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // <>
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(X)
}

type PrefixParseFn = fn(&mut Parser) -> Result<Expression, &'static str>;
type InfixParseFn = fn(&mut Parser, Expression) -> Result<Expression, &'static str>;
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
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, &'static str> {
        let name;
        if let Token::IDENT(iden) = self.peek_token.clone() {
            // current_token: 'let'
            name = iden;
            self.next_token();
        // current_token: the identifier
        } else {
            return Err("No identifier located");
        }

        if self.expectPeek(Token::ASSIGN) {
            self.next_token();
        // current_token: '='
        } else {
            return Err("There is no ASSIGN (=) token");
        }
        self.next_token();
        // current_token: first token of the value expression
        let expression = self.parse_expression(Precedence::Lowest)?;

        if self.expectPeek(Token::SEMICOLON) {
            self.next_token();
        }

        Ok(Statement::Let {
            name,
            value: expression,
        })
    }

    fn parse_return_statement(&mut self) -> Result<Statement, &'static str> {
        // current_token: 'return'
        self.next_token();
        // current_token: 'is either ; or the first token of theexpression'

        if self.current_token == Token::SEMICOLON {
            return Ok(Statement::Return(None));
        }

        let expression = self.parse_expression(Precedence::Lowest)?;
        // current_token: last token of the expression;

        if self.expectPeek(Token::SEMICOLON) {
            self.next_token();
            // current_token: ;
        }

        Ok(Statement::Return(Some(expression)))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, &'static str> {
        let expression = self.parse_expression(Precedence::Lowest);

        if self.expectPeek(Token::SEMICOLON) {
            self.next_token();
        }

        expression.map(Statement::Expression)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, &'static str> {
        let prefix = self
            .parse_prefix_fn()
            .ok_or_else(|| "No associated prefix parsing fn")?;
        let mut left_exp = prefix(self);

        left_exp
    }

    fn parse_prefix_fn(&self) -> Option<PrefixParseFn> {
        match self.current_token {
            Token::IDENT(_) => Some(Parser::parse_identifier),
            Token::INT(_) => Some(Parser::parse_integer_literal),
            _ => None,
        }
    }

    fn parse_identifier(&mut self) -> Result<Expression, &'static str> {
        let ident;
        if let Token::IDENT(i) = &self.current_token {
            ident = i.to_string();
        } else {
            return Err("no identitier found");
        }

        Ok(ident).map(Expression::Identifier)
    }

    fn parse_integer_literal(&mut self) -> Result<Expression, &'static str> {
        let value;
        if let Token::INT(v) = &self.current_token {
            value = v.parse::<i64>().unwrap();
        } else {
            return Err("no expression found");
        }

        Ok(value).map(Expression::IntegerLiteral)
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
                value: Expression::IntegerLiteral(5),
            },
            Statement::Let {
                name: "y".to_string(),
                value: Expression::IntegerLiteral(10),
            },
            Statement::Let {
                name: "foobar".to_string(),
                value: Expression::IntegerLiteral(838383),
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

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let mut lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_errors(parser);

        let expected = vec![Statement::Expression(Expression::Identifier(
            "foobar".to_string(),
        ))];

        assert_eq!(expected, program.statements);
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
