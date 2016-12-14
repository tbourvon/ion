#![feature(plugin)]
#![plugin(clippy)]

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
        println!("{}", err)
    }

    let mut reader = lexer::Reader::new(s.as_ref(), args.get_str("<src>").to_string());

    let parser = parser::Parser::new(&mut reader);
    let ast_res = parser.parse();
    if let Some(err) = ast_res.clone().err() {
        println!("{}", err)
    }
    let mut ast = ast_res.ok().unwrap();

    {
        let mut importer = parser::importer::Importer::new(&mut ast);
        importer.import().unwrap();
    }

    {
        let mut path_resolver = parser::path_resolver::PathResolver::new(&mut ast);
        path_resolver.resolve().unwrap();
    }

    //println!("{:#?}", ast);

    let mut interpreter = interpreter::Interpreter::new(&ast);
    let res2 = interpreter.execute();
    if let Some(err) = res2.err() {
        println!("{}", err)
    }
}
