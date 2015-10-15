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
    let res = file.read_to_string(&mut s);
    if let Some(err) = res.err() {
        panic!(err)
    }

    let mut reader = lexer::Reader::new(s.as_ref());

    let mut parser = parser::Parser::new(&mut reader);
    let ast_res = parser.parse();
    if let Some(err) = ast_res.clone().err() {
        panic!(err)
    }
    let ast = ast_res.ok().unwrap();

    let mut interpreter = interpreter::Interpreter::new(ast);
    let res2 = interpreter.execute();
    if let Some(err) = res2.err() {
        panic!(err)
    }
}
