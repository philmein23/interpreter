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
        Expression::Prefix(prefix, operand) => eval_prefix_expression(prefix, operand),
        _ => Err("no existence of expression"),
    }
}

fn eval_prefix_expression(
    prefix: &Prefix,
    operand: &Box<Expression>,
) -> Result<Object, &'static str> {
    let operand = eval_expression(operand)?;
    match prefix {
        Prefix::BANG => eval_bang_operator_expression(operand),
        Prefix::MINUS => eval_minus_operator_expression(operand),
        _ => Ok(Object::Null),
    }
}

fn eval_bang_operator_expression(operand: Object) -> Result<Object, &'static str> {
    match operand {
        Object::Boolean(true) => Ok(Object::Boolean(false)),
        Object::Boolean(false) => Ok(Object::Boolean(true)),
        Object::Null => Ok(Object::Boolean(true)),
        _ => Ok(Object::Boolean(false)),
    }
}

fn eval_minus_operator_expression(operand: Object) -> Result<Object, &'static str> {
    match operand {
        Object::Integer(value) => Ok(Object::Integer(-value)),
        _ => Ok(Object::Null),
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
            ("-3", "-3"),
            ("-5", "-5"),
        ];

        expect_values(input);
    }

    #[test]
    fn test_prefix_expression() {
        let input = vec![
            ("!true", "false"),
            ("!false", "true"),
            ("!5", "false"),
            ("!!true", "true"),
            ("!!false", "false"),
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
