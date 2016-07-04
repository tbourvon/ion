use parser::ast::*;
use lexer::Span;
use interpreter::*;
use std::io;
use std::io::prelude::*;

impl<'a> Interpreter<'a> {
	pub fn builtin_print(&'a self, context: *mut InterpreterContext<'a>, args: &[Box<Expression>], span: Span) -> Result<Value> {
		if args.len() != 1 {
			return Err(Error { kind: ErrorKind::InvalidArgCount, span: span })
		};

		match try!(self.value_from_expression(context, args.get(0).unwrap())) {
			Value::String(s) => print!("{}", s),
			Value::Integer(i) => print!("{}", i),
			Value::Bool(b) => print!("{}", b),
			Value::Char(c) => print!("{}", c),
			Value::Struct(_, s) => print!("{:?}", s),
			Value::Array(_, a) => print!("{:?}", a),
			Value::Map(_, _, m) => print!("{:?}", m),
			Value::Reference(v) => print!("ref {:?}", v),
			Value::MutReference(v) => print!("mutref {:?}", v),
			Value::Func(_, f) => print!("{:?}", f),
			Value::Nil => print!("nil"),
		};

		Ok(Value::Nil)
	}

	pub fn builtin_readln(&self, args: &[Box<Expression>], span: Span) -> Result<Value> {
		if args.len() != 0 {
			return Err(Error { kind: ErrorKind::InvalidArgCount, span: span })
		};

		let mut line = String::new();
	    let stdin = io::stdin();
	    stdin.lock().read_line(&mut line).unwrap();
		let size = line.len();
		line.remove(size - 1);
		if line.ends_with('\r') {
			line.remove(size - 2);
		}

    	Ok(Value::String(
			line
		))
	}
}
