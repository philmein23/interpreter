mod ast;
mod lexer;
mod parser;
mod repl;
mod token;

use repl::start;

fn main() {
    // start();

    #[derive(Debug)]
    pub enum Statement {
        Let { name: String, value: Expression },
    }

    #[derive(Debug)]
    pub enum Expression {
        Const(i32),
    }

    let let_statement = Statement::Let {
        name: String::from("x"),
        value: Expression::Const(2),
    };

    println!("{:?}", let_statement);
}
