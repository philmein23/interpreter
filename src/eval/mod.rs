use crate::ast::{BlockStatement, Expression, Infix, Prefix, Program, Statement};
use crate::object::environment::Environment;
use crate::object::{EvalError, EvalResult, Object};
use std::cell::RefCell;
use std::rc::Rc;

pub fn eval(program: &Program, env: Rc<RefCell<Environment>>) -> EvalResult {
    let mut result = Object::Null;
    for statement in &program.statements {
        result = eval_statement(&statement, Rc::clone(&env))?;
        if result.type_name() == "RETURN" {
            return Ok(result);
        }
    }

    Ok(result)
}

fn eval_statement(statement: &Statement, env: Rc<RefCell<Environment>>) -> EvalResult {
    match statement {
        Statement::Let { name, value } => {
            let result = eval_expression(value, Rc::clone(&env))?;

            env.borrow_mut().set(name, result.clone());

            Ok(result)
        }
        Statement::Expression(exp) => eval_expression(exp, env),
        Statement::Return(value) => {
            if let Some(exp) = value {
                let result = eval_expression(exp, env)?;
                Ok(Object::Return(Box::new(result)))
            } else {
                Ok(Object::Null)
            }
        }
        _ => Err(EvalError::General("no existence of statement".to_string())),
    }
}

fn eval_expression(expression: &Expression, env: Rc<RefCell<Environment>>) -> EvalResult {
    match expression {
        Expression::IntegerLiteral(value) => Ok(Object::Integer(*value)),
        Expression::Boolean(value) => Ok(Object::Boolean(*value)),
        Expression::Prefix(prefix, operand) => eval_prefix_expression(prefix, operand, env),
        Expression::Infix(infix, operand_one, operand_two) => {
            eval_infix_expression(infix, operand_one, operand_two, env)
        }
        Expression::If(condition, consequence, alternative) => {
            eval_if_expression(condition, consequence, alternative, env)
        }
        Expression::Identifier(name) => eval_identifier(name, env),
        Expression::FunctionLiteral(params, body) => eval_function(params, body, env),
        Expression::Call(exp, args) => {
            let func = eval_expression(exp, Rc::clone(&env))?;
            let arguments = eval_expressions(args, env)?;
            apply_function(func, arguments)
        }
        _ => Err(EvalError::General("No existence of expression".to_string())),
    }
}

fn apply_function(func: Object, args: Vec<Object>) -> EvalResult {
    if let Object::Function(params, body, env) = func {
        let extended_env = extend_function_env(params, args, env);
        let evaluated = eval_block_statement(&body, extended_env)?;
        unwrap_return_values(evaluated)
    } else {
        return Err(EvalError::General("Not a function".to_string()));
    }
}

fn extend_function_env(
    params: Vec<String>,
    args: Vec<Object>,
    env: Rc<RefCell<Environment>>,
) -> Rc<RefCell<Environment>> {
    let extended_env = Rc::new(RefCell::new(Environment::extend(env)));

    for (idx, param) in params.iter().enumerate() {
        extended_env.borrow_mut().set(
            param,
            args.get(idx).cloned().unwrap_or_else(|| Object::Null),
        );
    }

    extended_env
}

fn unwrap_return_values(obj: Object) -> EvalResult {
    if let Object::Return(val) = obj {
        return Ok(*val);
    }

    Ok(obj)
}

fn eval_expressions(
    args: &[Expression],
    env: Rc<RefCell<Environment>>,
) -> Result<Vec<Object>, EvalError> {
    let mut result = vec![];

    for exp in args.iter() {
        if let Ok(val) = eval_expression(exp, Rc::clone(&env)) {
            result.push(val);
        }
    }

    Ok(result)
}

fn eval_function(
    params: &Vec<String>,
    body: &BlockStatement,
    env: Rc<RefCell<Environment>>,
) -> EvalResult {
    Ok(Object::Function(params.clone(), body.clone(), env))
}

fn eval_identifier(name: &str, env: Rc<RefCell<Environment>>) -> EvalResult {
    if let Some(obj) = env.borrow().get(name) {
        return Ok(obj);
    }

    Err(EvalError::IdentifierNotFound(name.to_string()))
}

fn eval_if_expression(
    condition: &Box<Expression>,
    consequence: &BlockStatement,
    alternative: &Option<BlockStatement>,
    env: Rc<RefCell<Environment>>,
) -> EvalResult {
    let condition = eval_expression(condition, Rc::clone(&env))?;

    match is_truthy(condition) {
        true => eval_block_statement(consequence, env),
        false => {
            if let Some(alt) = alternative {
                eval_block_statement(alt, env)
            } else {
                Ok(Object::Null)
            }
        }
        _ => Ok(Object::Null),
    }
}

fn is_truthy(condition: Object) -> bool {
    match condition {
        Object::Boolean(true) => true,
        Object::Boolean(false) => false,
        Object::Null => false,
        _ => true,
    }
}

pub fn eval_block_statement(
    block_statement: &BlockStatement,
    env: Rc<RefCell<Environment>>,
) -> EvalResult {
    let mut result = Object::Null;
    for statement in &block_statement.statements {
        result = eval_statement(&statement, Rc::clone(&env))?;

        if result.type_name() == "RETURN" {
            return Ok(result);
        }
    }

    Ok(result)
}

fn eval_infix_expression(
    infix: &Infix,
    operand_one: &Box<Expression>,
    operand_two: &Box<Expression>,
    env: Rc<RefCell<Environment>>,
) -> EvalResult {
    let operand_one = eval_expression(operand_one, Rc::clone(&env))?;
    let operand_two = eval_expression(operand_two, env)?;
    match (operand_one, operand_two) {
        (Object::Integer(left), Object::Integer(right)) => {
            eval_integer_infix_expression(infix, left, right)
        }
        (Object::Boolean(left), Object::Boolean(right)) => {
            eval_boolean_infix_expression(infix, left, right)
        }
        (left, right) => Err(EvalError::TypeMismatch(infix.clone(), left, right)),
    }
}

fn eval_boolean_infix_expression(infix: &Infix, left: bool, right: bool) -> EvalResult {
    match infix {
        Infix::NOT_EQ => Ok(Object::Boolean(left != right)),
        Infix::EQ => Ok(Object::Boolean(left == right)),
        _ => Err(EvalError::UnknownInfixOperator(
            infix.clone(),
            Object::Boolean(left),
            Object::Boolean(right),
        )),
    }
}

fn eval_integer_infix_expression(infix: &Infix, operand_one: i64, operand_two: i64) -> EvalResult {
    match infix {
        Infix::PLUS => Ok(Object::Integer(operand_one + operand_two)),
        Infix::MINUS => Ok(Object::Integer(operand_one - operand_two)),
        Infix::ASTERISK => Ok(Object::Integer(operand_one * operand_two)),
        Infix::SLASH => Ok(Object::Integer(operand_one / operand_two)),
        Infix::GT => Ok(Object::Boolean(operand_one > operand_two)),
        Infix::LT => Ok(Object::Boolean(operand_one < operand_two)),
        Infix::NOT_EQ => Ok(Object::Boolean(operand_one != operand_two)),
        Infix::EQ => Ok(Object::Boolean(operand_one == operand_two)),
    }
}

fn eval_prefix_expression(
    prefix: &Prefix,
    operand: &Box<Expression>,
    env: Rc<RefCell<Environment>>,
) -> EvalResult {
    let operand = eval_expression(operand, env)?;
    match prefix {
        Prefix::BANG => eval_bang_operator_expression(operand),
        Prefix::MINUS => match operand {
            Object::Integer(value) => Ok(Object::Integer(-value)),
            _ => Err(EvalError::UnknownPrefixOperator(prefix.clone(), operand)),
        },
    }
}

fn eval_bang_operator_expression(operand: Object) -> EvalResult {
    match operand {
        Object::Boolean(true) => Ok(Object::Boolean(false)),
        Object::Boolean(false) => Ok(Object::Boolean(true)),
        Object::Null => Ok(Object::Boolean(true)),
        _ => Ok(Object::Boolean(false)),
    }
}

#[cfg(test)]
mod tests {
    use crate::eval::eval;
    use crate::lexer::Lexer;
    use crate::object::environment::Environment;
    use crate::object::EvalResult;
    use crate::parser::Parser;
    use std::cell::RefCell;
    use std::rc::Rc;

    #[test]
    fn test_integer_literal() {
        let input = vec![
            ("5", "5"),
            ("123", "123"),
            ("true", "true"),
            ("false", "false"),
            ("-3", "-3"),
            ("-5", "-5"),
            ("5 + 5 + 5 + 5 -10", "10"),
            ("2 * 2 * 2 * 2 * 2", "32"),
            ("-50 + 100 + -50", "0"),
            ("50 / 2 * 2 + 10", "60"),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", "50"),
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
            ("1 < 2", "true"),
            ("1 > 2", "false"),
            ("1 < 1", "false"),
            ("1 == 1", "true"),
            ("1 != 3", "true"),
            ("true == true", "true"),
            ("true != true", "false"),
            ("true != false", "true"),
            ("(1 > 2) == false", "true"),
        ];

        expect_values(input);
    }

    #[test]
    fn test_if_else_expression() {
        let input = vec![
            ("if (true) { 10 }", "10"),
            ("if (false) { 10 }", "null"),
            ("if (1) { 10 }", "10"),
            ("if (1 > 2) { 10 } else { 20 }", "20"),
        ];
        expect_values(input);
    }

    #[test]
    fn test_return_statement() {
        let input = vec![
            ("return 10;", "10"),
            ("9; return 2 * 5; 9;", "10"),
            ("return 2 * 5; 9;", "10"),
            (
                "if (10 > 1) {
                if (10 > 1) {
                  return 10;
           }
           return 1; }",
                "10",
            ),
        ];

        expect_values(input);
    }

    #[test]
    fn test_error_handling() {
        let input = vec![
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
                "if (10 > 1) { true + false; }; 8;",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            ("(1 + true) * 4;", "type mismatch: INTEGER + BOOLEAN"),
            ("3 - (true * 2);", "type mismatch: BOOLEAN * INTEGER"),
            (
                "(3 + false) - (true * 2);",
                "type mismatch: INTEGER + BOOLEAN",
            ),
            ("!(1 + true);", "type mismatch: INTEGER + BOOLEAN"),
            ("return (1 + true) * 4;", "type mismatch: INTEGER + BOOLEAN"),
            (
                "if (3 == true) { 1 } else { 2 }",
                "type mismatch: INTEGER == BOOLEAN",
            ),
            // ("foobar", "identifier not found: foobar"),
        ];

        expect_errors(input);
    }

    #[test]
    fn let_statement() {
        expect_values(vec![
            ("let a = 5; a;", "5"),
            ("let a = 5 * 5; a;", "25"),
            ("let a = 5; let b = a; b;", "5"),
            ("let a = 5; let b = a; let c = a + b + 5; c;", "15"),
        ]);
    }

    #[test]
    fn function_object() {
        expect_values(vec![("fn(x) { x + 2; }", "fn(x) {\n{ (x + 2); }\n}")]);
    }

    #[test]
    fn function_application() {
        expect_values(vec![
            ("let identity = fn(x) { x; }; identity(5);", "5"),
            ("let identity = fn(x) { return x; }; identity(5);", "5"),
            ("let double = fn(x) { x * 2; }; double(5);", "10"),
            ("let add = fn(x, y) { x + y; }; add(10, 23);", "33"),
            (
                "let add = fn(x, y) { x + y; }; add(3 + 4, add(10, 23));",
                "40",
            ),
            ("fn(x) { x; }(5);", "5"),
            ("fn(x) { x; }(1); 5;", "5"),
        ]);
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

    fn eval_input(input: &str) -> EvalResult {
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        let env = Rc::new(RefCell::new(Environment::new()));

        eval(&program, env)
    }

    fn expect_errors(tests: Vec<(&str, &str)>) {
        for (input, expected_message) in &tests {
            match eval_input(input) {
                Ok(obj) => {
                    panic!("no error object returned. got=`{}` for `{}`", obj, input);
                }
                Err(err) => {
                    assert_eq!(&err.to_string(), expected_message, "for `{}`", input);
                }
            }
        }
    }
}
