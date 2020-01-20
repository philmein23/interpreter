use crate::ast::{BlockStatement, Expression, Infix, Prefix, Program, Statement};
use crate::object::Object;

pub fn eval(program: &Program) -> Result<Object, &'static str> {
    let mut result = Object::Null;
    for statement in &program.statements {
        result = eval_statement(&statement)?;
    }

    Ok(result)
}

fn eval_statement(statement: &Statement) -> Result<Object, &'static str> {
    match statement {
        Statement::Expression(exp) => eval_expression(exp),
        _ => Err("No existence of statement"),
    }
}

fn eval_expression(expression: &Expression) -> Result<Object, &'static str> {
    match expression {
        Expression::IntegerLiteral(value) => Ok(Object::Integer(*value)),
        Expression::Boolean(value) => Ok(Object::Boolean(*value)),
        _ => Err("no existence of expression"),
    }
}

#[cfg(test)]
mod tests {
    use crate::eval::eval;
    use crate::lexer::Lexer;
    use crate::object::Object;
    use crate::parser::Parser;

    #[test]
    fn test_integer_literal() {
        let input = vec![
            ("5", "5"),
            ("123", "123"),
            ("true", "true"),
            ("false", "false"),
        ];

        expect_values(input);
    }

    fn expect_values(tests: Vec<(&str, &str)>) {
        for (input, expected) in tests {
            match eval_input(input) {
                Ok(obj) => assert_eq!(obj.to_string(), expected.to_string(), "for `{}`", input),
                Err(err) => {
                    panic!(
                        "expected `{}`, but got error=`{}` for `{}`",
                        expected, err, input
                    );
                }
            }
        }
    }

    fn eval_input(input: &str) -> Result<Object, &'static str> {
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        eval(&program)
    }
}
