mod lexer;
mod token;

fn main() {
    let chars = String::from("hello");
    let slice = &chars[0..2];

    println!("{:?}", slice);
}
