use crate::ast::{self, Expression, Infix, Prefix, Statement};
use crate::lexer::Lexer;
use crate::token::Token;
use std::mem;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // <>
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(X)
    Index,       // []
}

type PrefixParseFn = fn(&mut Parser) -> Result<Expression, &'static str>;
type InfixParseFn = fn(&mut Parser, Expression) -> Result<Expression, &'static str>;
pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    pub errors: Vec<String>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
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

    pub fn parse_program(&mut self) -> ast::Program {
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

        self.expectPeek(Token::ASSIGN);
        // current_token: '='
        self.next_token();
        // current_token: first token of the value expression
        let expression = self.parse_expression(Precedence::Lowest)?;
        // current_token: last token of value expression

        self.expectPeek(Token::SEMICOLON);
        // current_token: is ;

        Ok(Statement::Let {
            name,
            value: expression,
        })
    }

    fn parse_return_statement(&mut self) -> Result<Statement, &'static str> {
        // current_token: 'return'
        self.next_token();
        // current_token: 'is either ; or the first token of the expression'

        if self.current_token == Token::SEMICOLON {
            return Ok(Statement::Return(None));
        }

        let expression = self.parse_expression(Precedence::Lowest)?;
        // current_token: last token of the expression;

        self.expectPeek(Token::SEMICOLON);
        // current_token: ;

        Ok(Statement::Return(Some(expression)))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, &'static str> {
        let expression = self.parse_expression(Precedence::Lowest);

        self.expectPeek(Token::SEMICOLON);
        // current_token: is ;

        expression.map(Statement::Expression)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, &'static str> {
        let prefix = self
            .parse_prefix_fn()
            .ok_or_else(|| "No associated prefix parsing fn")?;
        let mut left_exp = prefix(self);

        while self.peek_token != Token::SEMICOLON
            && precedence < self.lookup_precedence(&self.peek_token).0
        {
            if let Some(infix) = self.parse_infix_fn() {
                self.next_token();
                // current_token: infix operator token;

                left_exp = infix(self, left_exp?);
            } else {
                return Ok(left_exp?);
            }
        }

        left_exp
    }

    fn parse_prefix_fn(&self) -> Option<PrefixParseFn> {
        match self.current_token {
            Token::LBRACE => Some(Parser::parse_hash_literal),
            Token::LBRACKET => Some(Parser::parse_array_literal),
            Token::IDENT(_) => Some(Parser::parse_identifier),
            Token::INT(_) => Some(Parser::parse_integer_literal),
            Token::BANG => Some(Parser::parse_prefix_expression),
            Token::STRING(_) => Some(Parser::parse_string_literal),
            Token::MINUS => Some(Parser::parse_prefix_expression),
            Token::TRUE => Some(Parser::parse_boolean),
            Token::FALSE => Some(Parser::parse_boolean),
            Token::LPAREN => Some(Parser::parse_grouped_expressions),
            Token::IF => Some(Parser::parse_if_expression),
            Token::FUNCTION => Some(Parser::parse_function_literal_expression),
            _ => None,
        }
    }

    fn parse_infix_fn(&self) -> Option<InfixParseFn> {
        match self.peek_token {
            Token::PLUS => Some(Parser::parse_infix_expressions),
            Token::MINUS => Some(Parser::parse_infix_expressions),
            Token::SLASH => Some(Parser::parse_infix_expressions),
            Token::ASTERISK => Some(Parser::parse_infix_expressions),
            Token::EQ => Some(Parser::parse_infix_expressions),
            Token::NOT_EQ => Some(Parser::parse_infix_expressions),
            Token::LT => Some(Parser::parse_infix_expressions),
            Token::GT => Some(Parser::parse_infix_expressions),
            Token::LPAREN => Some(Parser::parse_call_expression),
            Token::LBRACKET => Some(Parser::parse_index_expression),
            _ => None,
        }
    }

    fn parse_hash_literal(&mut self) -> Result<Expression, &'static str> {
        let mut pairs = vec![];

        while !self.expectPeek(Token::RBRACE) {
            self.next_token();
            // current_token: Key - expression (Token::STRING)
            let key = self.parse_expression(Precedence::Lowest)?;

            if self.expectPeek(Token::COLON) {
                self.next_token();
            // current_token: Token::COLON
            } else {
                return Err("Missing colon in hash");
            }

            self.next_token();
            // current_token:  Value - expression (Token::STRING)
            let value = self.parse_expression(Precedence::Lowest)?;

            pairs.push((key, value));

            if self.expectPeek(Token::COMMA) {
                self.next_token();
                // current_token: Token::COMMA
            }
        }

        if self.expectPeek(Token::RBRACE) {
            self.next_token();
        //current_token: Token::RBRACE
        } else {
            return Err("No right brace in hash");
        }

        Ok(Expression::Hash(pairs))
    }

    fn parse_array_literal(&mut self) -> Result<Expression, &'static str> {
        let expression_list = self.parse_expression_list()?;

        Ok(Expression::Array(expression_list))
    }

    fn parse_expression_list(&mut self) -> Result<Vec<Expression>, &'static str> {
        let mut expression_list = vec![];

        if self.expectPeek(Token::RBRACKET) {
            self.next_token();
            // current_token: RBRACKET
            return Ok(expression_list);
        }

        self.next_token();
        // current_token: first token of expression of within array
        expression_list.push(self.parse_expression(Precedence::Lowest)?);

        while self.expectPeek(Token::COMMA) {
            self.next_token();
            // current_token: COMMA

            self.next_token();
            // current_token: expression value

            expression_list.push(self.parse_expression(Precedence::Lowest)?);
        }

        if self.expectPeek(Token::RBRACKET) {
            self.next_token()
        // current_token: Token::RBRACKET
        } else {
            return Err("expecting right bracket");
        }

        Ok(expression_list)
    }

    fn parse_index_expression(&mut self, left: Expression) -> Result<Expression, &'static str> {
        self.next_token();
        // curren_token: expression after left bracket

        let index = self.parse_expression(Precedence::Lowest)?;
        if self.expectPeek(Token::RBRACKET) {
            self.next_token();
        // current_token: Token::RBRACKET
        } else {
            return Err("expecting right bracket");
        }

        Ok(Expression::Index(Box::new(left), Box::new(index)))
    }

    fn parse_call_expression(&mut self, left: Expression) -> Result<Expression, &'static str> {
        // current_token: Token::LPAREN
        let arguments = self.parse_call_arguments()?;
        Ok(Expression::Call(Box::new(left), Box::new(arguments)))
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<Expression>, &'static str> {
        let mut arguments = vec![];

        if self.expectPeek(Token::RPAREN) {
            // current_token: Token::RPAREN
            return Ok(arguments);
        }

        self.next_token();
        // current_token: first token of expression of arguments

        arguments.push(self.parse_expression(Precedence::Lowest)?);
        while self.peek_token == Token::COMMA {
            self.next_token();
            // current_token: Token::COMMA
            self.next_token();
            // current_token: Token:: next token of expression in arguments

            arguments.push(self.parse_expression(Precedence::Lowest)?);
        }

        self.expectPeek(Token::RPAREN);
        // current_token: Token::RPAREN

        Ok(arguments)
    }

    fn parse_function_literal_expression(&mut self) -> Result<Expression, &'static str> {
        self.expectPeek(Token::LPAREN);
        // current_token: Token::LPAREN

        let parameters = self.parse_function_parameters()?;

        self.expectPeek(Token::LBRACE);
        let block_statement = self.parse_block_statement()?;
        // current_token: Token::RBRACE

        Ok(Expression::FunctionLiteral(parameters, block_statement))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<String>, &'static str> {
        let mut params = vec![];

        if self.expectPeek(Token::RPAREN) {
            // current_token: Token::RPAREN
            return Ok(vec![]);
        };

        self.next_token();
        // current_token: first expression identifier token in parameters
        params.push(self.parse_expression_identifiers()?);
        while self.peek_token == Token::COMMA {
            self.next_token();
            // current_token: Token::COMMA
            self.next_token();
            // current_token: Token::second expression identifier token in parameters
            params.push(self.parse_expression_identifiers()?);
        }

        self.expectPeek(Token::RPAREN);
        // current_token: Token::RPAREN
        Ok(params)
    }

    fn parse_expression_identifiers(&self) -> Result<String, &'static str> {
        let ident;

        if let Token::IDENT(i) = &self.current_token {
            ident = i.to_string();
        } else {
            return Err("no identitier found");
        }

        Ok(ident)
    }

    fn parse_if_expression(&mut self) -> Result<Expression, &'static str> {
        self.expectPeek(Token::LPAREN);
        // current_token: Token::LPAREN;
        self.next_token();
        // current_token: first token of expression

        let condition = self.parse_expression(Precedence::Lowest)?;

        self.expectPeek(Token::RPAREN);

        self.expectPeek(Token::LBRACE);
        let consequence = self.parse_block_statement()?;

        // current_token: Token::RBRACE

        if self.expectPeek(Token::ELSE) {
            // current_token: Token::ELSE

            self.expectPeek(Token::LBRACE);
            //current_token: Token::LBRACE

            let alternative = self.parse_block_statement()?;
            // current_token: Token::RBRACE

            return Ok(Expression::If(
                Box::new(condition),
                consequence,
                Some(alternative),
            ));
        }

        Ok(Expression::If(Box::new(condition), consequence, None))
    }

    fn parse_block_statement(&mut self) -> Result<ast::BlockStatement, &'static str> {
        let mut statements = vec![];

        self.next_token();
        // current_token: whatever token type within a statement

        while self.current_token != Token::RBRACE && self.current_token != Token::EOF {
            statements.push(self.parse_statement()?);

            self.next_token();
        }

        Ok(ast::BlockStatement { statements })
    }

    fn parse_string_literal(&mut self) -> Result<Expression, &'static str> {
        if let Token::STRING(literal) = &self.current_token {
            return Ok(Expression::StringLiteral(literal.to_string()));
        } else {
            return Err("No string literal found");
        }
    }

    fn parse_infix_expressions(&mut self, left: Expression) -> Result<Expression, &'static str> {
        let (current_precedence, infix) = self.lookup_precedence(&self.current_token);
        let i = infix.ok_or_else(|| "Error: expected infixed operator")?;
        // current_token: infix operator

        self.next_token();

        // current_token: expression right after infix operator
        let right = self.parse_expression(current_precedence)?;

        Ok(Expression::Infix(i, Box::new(left), Box::new(right)))
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

    fn parse_prefix_expression(&mut self) -> Result<Expression, &'static str> {
        // current_token: either ! or -
        let p = self.prefix_token(&self.current_token)?;
        self.next_token();
        // current_token will be the first token of the expression based on context (5, 'foobar', add(5))
        let expression = self.parse_expression(Precedence::Prefix)?;
        // current_token will be the last token of the expression
        Ok(Expression::Prefix(p, Box::new(expression)))
    }

    fn prefix_token(&self, token: &Token) -> Result<Prefix, &'static str> {
        let result = match token {
            Token::BANG => Ok(Prefix::BANG),
            Token::MINUS => Ok(Prefix::MINUS),
            _ => Err("Expected Prefix token"),
        };

        result
    }

    fn parse_boolean(&mut self) -> Result<Expression, &'static str> {
        let result = match self.current_token {
            Token::TRUE => Ok(Expression::Boolean(true)),
            Token::FALSE => Ok(Expression::Boolean(false)),
            _ => Err("Expected boolean token"),
        };

        result
    }

    fn parse_grouped_expressions(&mut self) -> Result<Expression, &'static str> {
        // current_token: Token::LPAREN
        self.next_token();
        // current_token: first token of value expression
        let expression = self.parse_expression(Precedence::Lowest);

        self.expectPeek(Token::RPAREN);
        expression
    }

    fn expectPeek(&mut self, expected: Token) -> bool {
        if self.peek_token == expected {
            self.next_token();
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

    fn lookup_precedence(&self, token: &Token) -> (Precedence, Option<Infix>) {
        match token {
            Token::EQ => (Precedence::Equals, Some(Infix::EQ)),
            Token::NOT_EQ => (Precedence::Equals, Some(Infix::NOT_EQ)),
            Token::LT => (Precedence::LessGreater, Some(Infix::LT)),
            Token::GT => (Precedence::LessGreater, Some(Infix::GT)),
            Token::PLUS => (Precedence::Sum, Some(Infix::PLUS)),
            Token::MINUS => (Precedence::Sum, Some(Infix::MINUS)),
            Token::SLASH => (Precedence::Product, Some(Infix::SLASH)),
            Token::ASTERISK => (Precedence::Product, Some(Infix::ASTERISK)),
            Token::LPAREN => (Precedence::Call, None),
            Token::LBRACKET => (Precedence::Index, None),
            _ => (Precedence::Lowest, None),
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

    #[test]
    fn test_prefix_operator_expression() {
        let input = "
        -5;
        -15;
        !true;
        !false;
        ";

        let mut lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_errors(parser);

        let expected = vec![
            Statement::Expression(Expression::Prefix(
                Prefix::MINUS,
                Box::new(Expression::IntegerLiteral(5)),
            )),
            Statement::Expression(Expression::Prefix(
                Prefix::MINUS,
                Box::new(Expression::IntegerLiteral(15)),
            )),
            Statement::Expression(Expression::Prefix(
                Prefix::BANG,
                Box::new(Expression::Boolean(true)),
            )),
            Statement::Expression(Expression::Prefix(
                Prefix::BANG,
                Box::new(Expression::Boolean(false)),
            )),
        ];

        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_infix_operator_expression() {
        let input = "
        5 + 5;
        5 - 5;
        5 * 5;
        5 / 5;
        5 > 5;
        5 < 5;
        5 == 5;
        5 != 5;
        ";

        let mut lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_errors(parser);

        let expected = vec![
            Statement::Expression(Expression::Infix(
                Infix::PLUS,
                Box::new(Expression::IntegerLiteral(5)),
                Box::new(Expression::IntegerLiteral(5)),
            )),
            Statement::Expression(Expression::Infix(
                Infix::MINUS,
                Box::new(Expression::IntegerLiteral(5)),
                Box::new(Expression::IntegerLiteral(5)),
            )),
            Statement::Expression(Expression::Infix(
                Infix::ASTERISK,
                Box::new(Expression::IntegerLiteral(5)),
                Box::new(Expression::IntegerLiteral(5)),
            )),
            Statement::Expression(Expression::Infix(
                Infix::SLASH,
                Box::new(Expression::IntegerLiteral(5)),
                Box::new(Expression::IntegerLiteral(5)),
            )),
            Statement::Expression(Expression::Infix(
                Infix::GT,
                Box::new(Expression::IntegerLiteral(5)),
                Box::new(Expression::IntegerLiteral(5)),
            )),
            Statement::Expression(Expression::Infix(
                Infix::LT,
                Box::new(Expression::IntegerLiteral(5)),
                Box::new(Expression::IntegerLiteral(5)),
            )),
            Statement::Expression(Expression::Infix(
                Infix::EQ,
                Box::new(Expression::IntegerLiteral(5)),
                Box::new(Expression::IntegerLiteral(5)),
            )),
            Statement::Expression(Expression::Infix(
                Infix::NOT_EQ,
                Box::new(Expression::IntegerLiteral(5)),
                Box::new(Expression::IntegerLiteral(5)),
            )),
        ];

        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_infix_boolean_expressions() {
        let input = "
         true == true;
         !true == false;
        ";

        let mut lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_errors(parser);

        let expected = [
            Statement::Expression(Expression::Infix(
                Infix::EQ,
                Box::new(Expression::Boolean(true)),
                Box::new(Expression::Boolean(true)),
            )),
            Statement::Expression(Expression::Infix(
                Infix::EQ,
                Box::new(Expression::Prefix(
                    Prefix::BANG,
                    Box::new(Expression::Boolean(true)),
                )),
                Box::new(Expression::Boolean(false)),
            )),
        ];

        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_if_expressions() {
        let input = vec![
            ("if (x < y) { x }", "if (x < y) { x; };"),
            (
                "if (x < y) { x } else { y }",
                "if (x < y) { x; } else { y; };",
            ),
            ("fn(x, y) { x + y; }", "fn(x, y) { (x + y); };"),
            (
                "fn(x, y, z) { x + y + z; }",
                "fn(x, y, z) { ((x + y) + z); };",
            ),
            (
                "fn(x, y, z) {
                    return x + y + z;
                }",
                "fn(x, y, z) { return ((x + y) + z); };",
            ),
            ("add(1, 2 * 3, 4 + 5)", "add(1, (2 * 3), (4 + 5));"),
        ];

        test_parsing(input);
    }

    #[test]
    fn test_array_expression() {
        let input = vec![
            ("[1, 2, 3, 4, 5]", "[1, 2, 3, 4, 5];"),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d);",
            ),
        ];

        test_parsing(input);
    }

    #[test]
    fn test_string_literals() {
        let input = vec![
            ("\"hello world\"", "\"hello world\";"),
            ("let s = \"hello world\"", "let s = \"hello world\";"),
        ];

        test_parsing(input);
    }

    #[test]
    fn hash() {
        test_parsing(vec![
            ("{}", "{};"),
            ("{1: 2, 2: 3}", "{1: 2, 2: 3};"),
            ("{true: 3}", "{true: 3};"),
            (
                r#"{"one": 1, "two": 2, "three": 3}"#,
                r#"{"one": 1, "two": 2, "three": 3};"#,
            ),
            // Duplicated entries
            (
                r#"{"one": 1, "one": 1, "two": 2}"#,
                r#"{"one": 1, "one": 1, "two": 2};"#,
            ),
        ]);
    }

    fn test_parsing(tests: Vec<(&str, &str)>) {
        for (input, expected) in tests {
            let mut lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            check_parse_errors(parser);

            assert_eq!(program.to_string(), expected);
        }
    }

    fn check_parse_errors(parser: Parser) {
        let errors = parser.errors;
        if errors.len() > 0 {
            for error in errors {
                println!("{:?}", error);
            }
        }
    }
}
