extern crate docopt;
mod lexer;
mod parser;
mod interpreter;
use std::fs::File;
use std::path::Path;
use std::io::prelude::*;

static USAGE: &'static str = "
Usage: ion <src>
";

fn main() {
    let args = docopt::Docopt::new(USAGE)
                              .and_then(|d| d.parse())
                              .unwrap_or_else(|e| e.exit());

    let path = Path::new(args.get_str("<src>"));
    let mut file = match File::open(&path) {
        Ok(file) => file,
        Err(_) => panic!(),
    };

    let mut s = String::new();
    file.read_to_string(&mut s);
    let mut reader = lexer::Reader::new(s.as_ref());

    let mut parser = parser::Parser::new(&mut reader);
    let ast = parser.parse();

    let mut interpreter = interpreter::Interpreter::new(ast);
    interpreter.execute();
}
