mod ast;
mod lexer;
mod parser;
mod repl;
mod token;

use repl::start;

fn main() {
    start();
}
