pub mod builtin;

use parser::ast::*;
use std;
use lexer;
use lexer::Span;
use parser;
use std::fs::File;
use std::io::prelude::*;
use std::borrow::Borrow;
use std::borrow::BorrowMut;
use std::hash::*;
use std::fmt::Display;
use std::fmt;
use std::error::Error as BaseError;

#[derive(Debug)]
pub struct Error<'a> {
    pub kind: ErrorKind<'a>,
    pub span: Span,
}

#[derive(Debug)]
pub enum ErrorKind<'a> {
    IO(std::io::Error),
    Parser(parser::Error),
	CannotInferTypeEmptyArray,
	CannotInferTypeEmptyMap,
	CannotIterateOver(Value<'a>),
	UnexpectedExprReturn,
	ExpectedExprReturn,
	MismatchedTypes(Type, Type),
	CannotCallNonFunction,
	ExpectedArgument(String),
	CannotMutablyRefFunction,
	UnknownVariable(std::vec::Vec<SpannedString>),
	IndexOutOfBounds,
	UnknownIndex(Value<'a>),
	CannotIndexNonIndexable,
	CannotPushToUntypedArray,
	CannotPushToNonArray,
	UnknownStructField(String),
	CannotAccessFieldOnNonStruct,
	CannotDerefConstRefInMutContext,
	CannotDerefNonRef,
	CannotGetMutRef(Expression_),
	CannotGetRef(Expression_),
	HeterogeneousTypesInArray,
	HeterogeneousTypesInMap,
	UnknownStruct(std::vec::Vec<SpannedString>),
	MissingStructField(String),
	CannotCountNonCountable,
	NoDefaultValue(Type),
    InvalidArgCount,
}

impl<'a> Display for Error<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let ErrorKind::IO(ref e) = self.kind {
            e.fmt(f)
        } else if let ErrorKind::Parser(ref e) = self.kind {
            e.fmt(f)
        } else {
            write!(f, "{}: {}", self.span,
                match self.kind {
                    ErrorKind::CannotInferTypeEmptyArray |
                    ErrorKind::CannotInferTypeEmptyMap |
                    ErrorKind::UnexpectedExprReturn |
                    ErrorKind::ExpectedExprReturn |
                    ErrorKind::CannotCallNonFunction |
                    ErrorKind::CannotMutablyRefFunction |
                    ErrorKind::IndexOutOfBounds |
                    ErrorKind::CannotIndexNonIndexable |
                    ErrorKind::CannotPushToUntypedArray |
                    ErrorKind::CannotPushToNonArray |
                    ErrorKind::CannotAccessFieldOnNonStruct |
                    ErrorKind::CannotDerefConstRefInMutContext |
                    ErrorKind::CannotDerefNonRef |
                    ErrorKind::HeterogeneousTypesInArray |
                    ErrorKind::HeterogeneousTypesInMap |
                    ErrorKind::InvalidArgCount |
                    ErrorKind::CannotCountNonCountable => self.description().to_string(),
    				ErrorKind::CannotIterateOver(ref v) => format!("cannot iterate over {:?}", v),
    				ErrorKind::MismatchedTypes(ref e, ref g) => format!("mismatched types (expected {:?}, got {:?})", e, g),
                    ErrorKind::ExpectedArgument(ref s) => format!("expected argument {}", s),
                    ErrorKind::UnknownVariable(ref parts) => format!("unknown variable {:?}", parts),
                    ErrorKind::UnknownIndex(ref v) => format!("unknown index {:?}", v),
                    ErrorKind::UnknownStructField(ref s) => format!("unknown struct field {}", s),
                    ErrorKind::CannotGetMutRef(ref e) => format!("cannot get mutable reference for {:?}", e),
                    ErrorKind::CannotGetRef(ref e) => format!("cannot get reference for {:?}", e),
                    ErrorKind::UnknownStruct(ref parts) => format!("unknown struct {:?}", parts),
                    ErrorKind::MissingStructField(ref s) => format!("missing field {} in struct init", s),
                    ErrorKind::NoDefaultValue(ref t) => format!("no default value for type {:?}", t),
                    _ => self.description().to_string(),
                }
            )
        }
    }
}

impl<'a> BaseError for Error<'a> {
    fn description(&self) -> &str {
        match self.kind {
            ErrorKind::IO(ref e) => e.description(),
            ErrorKind::Parser(ref e) => e.description(),
            ErrorKind::CannotInferTypeEmptyArray => "cannot infer type for empty array",
            ErrorKind::CannotInferTypeEmptyMap => "cannot infer type for empty map",
            ErrorKind::UnexpectedExprReturn => "unexpected expression for return",
            ErrorKind::ExpectedExprReturn => "exprected an expression for return",
            ErrorKind::CannotCallNonFunction => "cannot call a non-function",
            ErrorKind::CannotMutablyRefFunction => "cannot mutably reference a function",
            ErrorKind::IndexOutOfBounds => "index out of bounds",
            ErrorKind::CannotIndexNonIndexable => "cannot index a non-indexable",
            ErrorKind::CannotPushToUntypedArray => "cannot push to untyped array",
            ErrorKind::CannotPushToNonArray => "cannot push to non array",
            ErrorKind::CannotAccessFieldOnNonStruct => "cannot access field on non-struct",
            ErrorKind::CannotDerefConstRefInMutContext => "cannot dereference const reference in mutable context",
            ErrorKind::CannotDerefNonRef => "cannot dereference a non-reference",
            ErrorKind::HeterogeneousTypesInArray => "heterogeneous types in array",
            ErrorKind::HeterogeneousTypesInMap => "heterogeneous types in map",
            ErrorKind::CannotCountNonCountable => "cannot count non-countable",
            ErrorKind::InvalidArgCount => "invalid argument count",
            ErrorKind::CannotIterateOver(_) => "cannot iterate over value",
            ErrorKind::MismatchedTypes(_, _) => "mismatched types",
            ErrorKind::ExpectedArgument(_) => "expected an argument",
            ErrorKind::UnknownVariable(_) => "unknown variable",
            ErrorKind::UnknownIndex(_) => "unknown index",
            ErrorKind::UnknownStructField(_) => "unknown struct field",
            ErrorKind::CannotGetMutRef(_) => "cannot get mutable reference",
            ErrorKind::CannotGetRef(_) => "cannot get reference",
            ErrorKind::UnknownStruct(_) => "unknown struct",
            ErrorKind::MissingStructField(_) => "missing field in struct init",
            ErrorKind::NoDefaultValue(_) => "no default value for type",
        }
    }

    fn cause(&self) -> Option<&std::error::Error> {
        None
    }
}

impl<'a> From<parser::Error> for Error<'a> {
    fn from(error: parser::Error) -> Error<'a> {
        Error { kind: ErrorKind::Parser(error.clone()), span: error.span }
    }
}

pub type Result<'a, T> = std::result::Result<T, Error<'a>>;

pub struct Interpreter<'a> {
	ast: &'a Ast,
	funcs: std::collections::HashMap<Path, Value<'a>>,
	structs: std::collections::HashMap<Path, StructDeclData>,
}

#[derive(Debug)]
pub struct InterpreterContext<'a> {
	vars: std::collections::HashMap<String, Variable<'a>>,
	current_path: Path,
}

#[derive(Debug)]
pub struct Variable<'a> {
	name: String,
	var_type: Type,
	value: Value<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Value<'a> {
	Nil,
	String(String),
	Integer(i64),
	Bool(bool),
	Char(char),
	Struct(Path, StructValue<'a>),
	Array(Type, std::vec::Vec<Value<'a>>),
	Map(Type, Type, MapValue<'a>),
	Reference(*const Value<'a>),
	MutReference(*mut Value<'a>),
	Func(Path, FuncDeclData),
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MapValue<'a> {
	map: std::collections::HashMap<Value<'a>, Value<'a>>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructValue<'a> {
	map: std::collections::HashMap<String, Value<'a>>
}

#[allow(derive_hash_xor_eq)]
impl<'a> Hash for MapValue<'a> {
	fn hash<H>(&self, state: &mut H) where H: Hasher {
		for (key, value) in &self.map {
			key.hash(state);
			value.hash(state);
		}
	}
}

#[allow(derive_hash_xor_eq)]
impl<'a> Hash for StructValue<'a> {
	fn hash<H>(&self, state: &mut H) where H: Hasher {
		for (key, value) in &self.map {
			key.hash(state);
			value.hash(state);
		}
	}
}

impl<'a> Interpreter<'a> {
	pub fn new(ast: &'a Ast) -> Self {
		Interpreter {
			ast: ast,
			funcs: std::collections::HashMap::new(),
			structs: std::collections::HashMap::new(),
		}
	}

	fn inject_builtin_funcs(&mut self) {
		let mut inject_func = |name: String| {
			self.funcs.insert(
				Path {
					span: Span::nil_span(),
					parts: vec![SpannedString {
						span: Span::nil_span(),
						ident: name.clone(),
					}],
				},
				Value::Func(
					Path {
						span: Span::nil_span(),
						parts: vec![],
					},
					FuncDeclData {
						span: Span::nil_span(),
						name: name,
						return_type: Type::None,
						parameters: vec![],
						statements: vec![],
					}
				),
			)
		};

		inject_func("print".to_string());
		inject_func("readln".to_string());
	}

	pub fn execute(&'a mut self) -> Result<()> {
		self.inject_builtin_funcs();

		let initial_path = Path {
			span: Span::nil_span(),
			parts: vec![],
		};

        /*
            We are forced to use unsafe instead of a standard try!() because the borrow checker has a bug:
            When a loop contains a return, the borrow checker does not recognize it as an early lifetime end, and therefore imposes
            additional constraints which make the code unwritable safely.
        */
		for statement in &self.ast.statements {
            unsafe {
    			try!((*(self as *mut Self)).execute_statement(statement, initial_path.clone()))
            }
		}

		let main_func_call_expr = Expression {
			span: Span::nil_span(),
			expr: Expression_::FuncCall(
				Box::new(Expression {
					span: Span::nil_span(),
					expr: Expression_::Variable(Path {
						span: Span::nil_span(),
						parts: vec![
							SpannedString {
								span: Span::nil_span(),
								ident: "main".to_string(),
							}
						]
					})
				}),
				vec![],
			),
		};

		let mut context = InterpreterContext {
			vars: std::collections::HashMap::new(),
			current_path: Path {
				span: Span::nil_span(),
				parts: vec![],
			}
		};

		try!(self.value_from_expression(&mut context, &main_func_call_expr));

		Ok(())
	}

	fn execute_statement(&mut self, statement: &Statement, current_path: Path) -> Result<()> {
		match *statement {
			Statement::FuncDecl(ref fd) => {
				let mut new_path = current_path.clone();
				new_path.parts.push(SpannedString {
					span: fd.span.clone(),
					ident: fd.name.clone(),
				});

				self.funcs.insert(new_path, Value::Func(current_path.clone(), *fd.clone()));
			},
			Statement::Import(ref i) => {
				if !current_path.parts.is_empty() {
					let mut import_path = current_path.parts.clone();
					import_path.pop();
					try!(self.execute_import(i, Path { span: current_path.span.clone(), parts: import_path }));
				} else {
					try!(self.execute_import(i, current_path));
				}

			}
			Statement::StructDecl(ref sd) => {
				let mut new_path = current_path.clone();
				new_path.parts.push(SpannedString {
					span: sd.span.clone(),
					ident: sd.name.clone(),
				});
				self.structs.insert(new_path, *sd.clone());
			},
			_ => ()
		};

		Ok(())
	}

	fn type_from_value(value: *const Value, span: Span) -> Result<Type> {
		unsafe {
			match *value {
				Value::Array(ref t, _) => match *t {
					Type::None => Err(Error { kind: ErrorKind::CannotInferTypeEmptyArray, span: span}),
					ref array_type => Ok(Type::Array(
						Box::new((*array_type).clone())
					)),
				},
				Value::Bool(_) => Ok(Type::Bool),
				Value::Char(_) => Ok(Type::Char),
				Value::String(_) => Ok(Type::String),
				Value::Integer(_) => Ok(Type::Int),
				Value::Struct(ref t, _) => Ok(Type::Struct((*t).clone())),
				Value::Reference(ref r) => Ok(Type::Reference(Box::new(try!(Self::type_from_value(*r, span))))),
				Value::MutReference(ref r) => Ok(Type::MutReference(Box::new(try!(Self::type_from_value(*r, span))))),
				Value::Func(_, ref fd) => {
					let mut param_types: std::vec::Vec<Box<Type>> = vec![];
					for parameter in &fd.parameters {
						param_types.push(Box::new(parameter.param_type.clone()));
					}
					Ok(Type::Func(Box::new(fd.return_type.clone()), param_types))
				},
				Value::Map(ref t1, ref t2, _) => match ((*t1).clone(), (*t2).clone()) {
					(Type::None, Type::None) |
					(_, Type::None) |
					(Type::None, _) => Err(Error { kind: ErrorKind::CannotInferTypeEmptyMap, span: span}),
					(map_type1, map_type2) => Ok(Type::Map(
						Box::new(map_type1),
						Box::new(map_type2)
					)),
				},
				Value::Nil => Ok(Type::None),
			}
		}
	}

	fn execute_import(&mut self, import_data: &ImportData, current_path: Path) -> Result<()> { // TODO: rework that for more safety and non-naive handling
		let path_string = current_path.parts.iter().fold("".to_string(), |mut acc, ref item| { acc.push_str(item.ident.as_ref()); acc.push_str("/"); acc }) + import_data.path.as_ref() + ".ion";
	    let path = std::path::Path::new(AsRef::<str>::as_ref(&path_string[..]));
	    let mut file = match File::open(&path) {
	        Ok(file) => file,
	        Err(err) => return Err(Error { kind: ErrorKind::IO(err), span: Span::nil_span()}),
	    };

	    let mut s = String::new();
		let res = file.read_to_string(&mut s);
	    if let Some(err) = res.err() {
	        return Err(Error { kind: ErrorKind::IO(err), span: Span::nil_span()})
	    }
	    let mut reader = lexer::Reader::new(s.as_ref(), path_string.clone());

	    let mut parser = parser::Parser::new(&mut reader);
	    let ast = try!(parser.parse());

		let mut new_path = current_path.clone();
		for path_part in import_data.path.split('/') {
			new_path.parts.push(SpannedString {
				span: import_data.span.clone(),
				ident: path_part.to_string(),
			});
		}

		for statement in &ast.statements {
            unsafe {
    			try!((*(self as *mut Self)).execute_statement(statement, new_path.clone()))
            }
		}

		Ok(())
	}

	fn execute_block_statement(&'a self, context: *mut InterpreterContext<'a>, block_statement: &'a BlockStatement) -> Result<Value> {
		match *block_statement {
			BlockStatement::Expression(ref e) => { try!(self.value_from_expression(context, e)); Ok(Value::Nil) },
			BlockStatement::VarDecl(ref vd) => { try!(self.execute_var_decl(context, vd)); Ok(Value::Nil) },
			BlockStatement::VarAssignment(ref lhs, ref rhs) => { try!(self.execute_var_assignment(context, lhs, rhs)); Ok(Value::Nil) },
			BlockStatement::If(ref i) => self.execute_if(context, i),
			BlockStatement::While(ref w) => self.execute_while(context, w),
			BlockStatement::ForIn(ref fi) => self.execute_forin(context, fi),
			BlockStatement::Return(ref r) => self.execute_return(context, r),
		}
	}

	fn execute_forin(&'a self, context: *mut InterpreterContext<'a>, forin_data: &'a ForInData) -> Result<Value> {
		let coll_value = try!(self.value_from_expression(context, &forin_data.collection));
		match coll_value {
			Value::Array(t, a) => {
				for elem in a {
					unsafe {
						(*context).vars.insert(
							forin_data.element_name.clone(),
							Variable {
								name: forin_data.element_name.clone(),
								var_type: t.clone(),
								value: elem,
							}
						);
					}

					for statement in &forin_data.statements {
						let v = try!(self.execute_block_statement(context, statement));
						if v != Value::Nil {
							unsafe {
								(*context).vars.remove(AsRef::<str>::as_ref(&forin_data.element_name[..]));
							}

							return Ok(v)
						}
					}

					unsafe {
						(*context).vars.remove(AsRef::<str>::as_ref(&forin_data.element_name[..]));
					}
				}
			},
			Value::String(s) => {
				for c in s.chars() {
					unsafe {
						(*context).vars.insert(
							forin_data.element_name.clone(),
							Variable {
								name: forin_data.element_name.clone(),
								var_type: Type::Char,
								value: Value::Char(c),
							}
						);
					}

					for statement in &forin_data.statements {
						let v = try!(self.execute_block_statement(context, statement));
						if v != Value::Nil {
							unsafe {
								(*context).vars.remove(AsRef::<str>::as_ref(&forin_data.element_name[..]));
							}

							return Ok(v)
						}
					}

					unsafe {
						(*context).vars.remove(AsRef::<str>::as_ref(&forin_data.element_name[..]));
					}
				}
			},
			other => return Err(Error { kind: ErrorKind::CannotIterateOver(other), span: forin_data.collection.span.clone()}),
		};

		Ok(Value::Nil)
	}

	fn execute_return(&'a self, context: *mut InterpreterContext<'a>, return_data: &ReturnData) -> Result<Value> { // TODO: detect ref to local in return expr
		match return_data.expected_type {
			Type::None => {
				if let Some(_) = return_data.value {
					return Err(Error { kind: ErrorKind::UnexpectedExprReturn, span: return_data.span.clone()})
				}

				Ok(Value::Nil)
			},
			ref t => {
				let span: Span;
				let value = match return_data.value {
					Some(ref e) => {
						span = e.span.clone();
						try!(self.value_from_expression(context, e))
					},
					None => return Err(Error { kind: ErrorKind::ExpectedExprReturn, span: return_data.span.clone()}),
				};

				let value_type = try!(Self::type_from_value(&value, span.clone()));
				if value_type != *t {
					return Err(Error { kind: ErrorKind::MismatchedTypes(value_type, (*t).clone()), span: span})
				}

				Ok(value)
			},
		}
	}

	fn execute_func_call(&'a self, context: *mut InterpreterContext<'a>, func: &Expression, args: &[Box<Expression>], span: Span) -> Result<Value> {
		fn is_builtin_func(func: &Expression, name: &str) -> bool {
			match func.expr {
				Expression_::Variable(ref p) => {
					if p.parts.len() == 1 {
						p.parts.get(0).unwrap().ident == name
					} else {
						false
					}
				},
				_ => false
			}
		}

		if is_builtin_func(func, "print") {
			self.builtin_print(context, args, span)
		} else if is_builtin_func(func, "readln") {
			self.builtin_readln(args, span)
		} else {
			let (path, func_decl) = unsafe {
				match *try!(self.value_p_from_expression(context, &func)) {
					Value::Func(ref p, ref fd) => ((*p).clone(), fd),
					_ => return Err(Error { kind: ErrorKind::CannotCallNonFunction, span: func.span.clone()})
				}
			};

			let mut local_context = InterpreterContext {
				vars: std::collections::HashMap::new(),
				current_path: path,
			};

            if args.len() > func_decl.parameters.len() {
                return Err(Error { kind: ErrorKind::InvalidArgCount, span: func.span.clone() })
            }

			for (param_count, param) in func_decl.parameters.iter().enumerate() {
				let variable = Variable {
					name: param.name.clone(),
					var_type: param.param_type.clone(),
					value: {
						if param_count < args.len() {
							let value = try!(self.value_from_expression(context, &args.get(param_count).unwrap()));

							let value_type = try!(Self::type_from_value(&value, args.get(param_count).unwrap().span.clone()));

							if value_type != param.param_type {
								return Err(Error { kind: ErrorKind::MismatchedTypes(value_type, param.param_type.clone()), span: args.get(param_count).unwrap().span.clone()})
							} else {
								value
							}
						} else if let Some(ref e) = param.default_value {
							let value = try!(self.value_from_expression(context, e));

							let value_type = try!(Self::type_from_value(&value, e.span.clone()));

							if value_type != param.param_type {
								return Err(Error { kind: ErrorKind::MismatchedTypes(value_type, param.param_type.clone()), span: e.span.clone()})
							} else {
								value
							}
						} else {
							return Err(Error { kind: ErrorKind::ExpectedArgument(param.name.clone()), span: span})
						}
					}
				};

				local_context.vars.insert(
					param.name.clone(),
					variable
				);
			}

			let mut return_value: Value = Value::Nil;
			let mut local_vars: std::vec::Vec<String> = vec![];
			for statement in &func_decl.statements {
				if let BlockStatement::VarDecl(ref vd) = *statement {
					local_vars.push(vd.name.clone());
				};

				match try!(self.execute_block_statement(&mut local_context, statement)) {
					Value::Nil => (),
					v => {
						return_value = v;
						break
					},
				}
			}

			Ok(return_value)
		}
	}

	fn execute_var_decl(&'a self, context: *mut InterpreterContext<'a>, var_decl_data: &'a VarDeclData) -> Result<()> {
		let span: Span;
		let value = match var_decl_data.value {
			Some(ref v) => {
				span = v.span.clone();
				try!(self.value_from_expression(context, v))
			},
			None => {
				span = var_decl_data.span.clone();
				try!(self.default_value(var_decl_data.var_type.clone(), var_decl_data.span.clone()))
			},
		};

		unsafe {
			(*context).vars.insert(
				var_decl_data.name.clone(),
				Variable {
					name: var_decl_data.name.clone(),
					var_type: var_decl_data.var_type.clone(),
					value: {
						let value_type = try!(Self::type_from_value(&value, span.clone()));

						if value_type != var_decl_data.var_type {
							return Err(Error { kind: ErrorKind::MismatchedTypes(value_type, var_decl_data.var_type.clone()), span: span})
						} else {
							value
						}
					},
				}
			);
		};

		Ok(())
	}

	fn execute_var_assignment(&'a self, context: *mut InterpreterContext<'a>, lhs: &Expression, rhs: &Expression) -> Result<()> {
		let lhs_value_ref = try!(self.value_mut_p_from_expression(context, &lhs));

		let rhs_value = try!(self.value_from_expression(context, &rhs));

		let value_type = try!(Self::type_from_value(&rhs_value, rhs.span.clone()));
		let current_type = try!(Self::type_from_value(lhs_value_ref, lhs.span.clone()));

		if value_type != current_type {
			return Err(Error { kind: ErrorKind::MismatchedTypes(value_type, current_type), span: rhs.span.clone()})
		} else {
			unsafe {
				*lhs_value_ref = rhs_value;
			}
		};

		Ok(())
	}

	fn execute_if(&'a self, context: *mut InterpreterContext<'a>, if_data: &'a IfData) -> Result<Value> {
		match try!(self.value_from_expression(context, &if_data.condition)) {
			Value::Bool(b) => {
				if b {
					for statement in &if_data.if_statements {
						let v = try!(self.execute_block_statement(context, statement));
						if v != Value::Nil {
							return Ok(v)
						}
					}
				} else if let Some(ref else_statements) = if_data.else_statements {
					for statement in else_statements {
						let v = try!(self.execute_block_statement(context, statement));
						if v != Value::Nil {
							return Ok(v)
						}
					}
				}
				Ok(Value::Nil)
			},
			other => Err(Error { kind: ErrorKind::MismatchedTypes(Type::Bool, try!(Self::type_from_value(&other, if_data.condition.span.clone()))), span: if_data.condition.span.clone()})
		}
	}

	fn execute_while(&'a self, context: *mut InterpreterContext<'a>, while_data: &'a WhileData) -> Result<Value> {
		loop {
			match try!(self.value_from_expression(context, &while_data.condition)) {
				Value::Bool(b) => {
					if b {
						for statement in &while_data.statements {
							let v = try!(self.execute_block_statement(context, statement));
							if v != Value::Nil {
								return Ok(v)
							}
						}
					} else {
						return Ok(Value::Nil)
					}
				},
				other => return Err(Error { kind: ErrorKind::MismatchedTypes(Type::Bool, try!(Self::type_from_value(&other, while_data.condition.span.clone()))), span: while_data.condition.span.clone()})
			}
		}
	}

	fn value_mut_p_from_expression(&'a self, context: *mut InterpreterContext<'a>, expression: &Expression) -> Result<*mut Value> {
		match expression.expr {
			Expression_::Variable(ref p) => {
				if p.parts.len() == 1 {
					unsafe { if let Some(ref mut var) = (*context).vars.get_mut(AsRef::<str>::as_ref(&p.parts.get(0).unwrap().ident[..])) {
						 return Ok(&mut var.value);
					} }
				};

				unsafe {
					if p.parts.len() == 1 && self.funcs.get(&Path::concat((*context).current_path.clone(), p.clone())).is_some() {
						 return Err(Error { kind: ErrorKind::CannotMutablyRefFunction, span: p.span.clone()});
					}

					let mut root_path = (*context).current_path.parts.clone();
					root_path.pop();

					if self.funcs.get(&Path::concat(Path { span: (*context).current_path.span.clone(), parts: root_path }, p.clone())).is_some() {
						 return Err(Error { kind: ErrorKind::CannotMutablyRefFunction, span: p.span.clone()});
					};
				}

				Err(Error { kind: ErrorKind::UnknownVariable(p.parts.clone()), span: p.span.clone()})
			},

			Expression_::Index(ref indexed, ref index) => {
				match *index {
					Some(ref e) => {
						let indexed_value_p = try!(self.value_mut_p_from_expression(context, indexed));

						let index_value = try!(self.value_from_expression(context, e));

						unsafe {
							match *indexed_value_p {
								Value::Array(_, ref mut a) => {
									match index_value {
										Value::Integer(i) => {
											match a.get_mut(i as usize) {
												Some(v) => Ok(v),
												None => Err(Error { kind: ErrorKind::IndexOutOfBounds, span: e.span.clone()}),
											}
										},
										other => Err(Error { kind: ErrorKind::MismatchedTypes(Type::Int, try!(Self::type_from_value(&other, e.span.clone()))), span: e.span.clone()})
									}
								},
								Value::Map(ref t, _, ref mut m) => {
									let index_type = try!(Self::type_from_value(&index_value, e.span.clone()));

									if index_type != *t {
									 	Err(Error { kind: ErrorKind::MismatchedTypes((*t).clone(), index_type), span: e.span.clone()})
									} else {
										match m.map.get_mut(&index_value) {
											Some(v) => Ok(v),
											None => Err(Error { kind: ErrorKind::UnknownIndex(index_value), span: e.span.clone()}),
										}
									}
								},
								_ => Err(Error { kind: ErrorKind::CannotIndexNonIndexable, span: expression.span.clone()})
							}
						}
					},

					None => {
						let indexed_value_ref = try!(self.value_mut_p_from_expression(context, indexed));

						unsafe {
							match *indexed_value_ref {
								Value::Array(ref t, ref mut a) => {
									a.push(
										match *t {
											Type::None => return Err(Error { kind: ErrorKind::CannotPushToUntypedArray, span: indexed.span.clone()}),
											ref array_type => try!(self.default_value((*array_type).clone(), indexed.span.clone())),
										}
									);

									let last_index = a.len() - 1;
									Ok(a.get_mut(last_index).unwrap())
								},
								_ => Err(Error { kind: ErrorKind::CannotPushToNonArray, span: indexed.span.clone()})
							}
						}
					},
				}
			},

			Expression_::Field(ref struct_expr, ref field) => {
				let struct_value_ref = try!(self.value_mut_p_from_expression(context, struct_expr));
				unsafe {
					match *struct_value_ref {
						Value::Struct(_, ref mut fields) => {
							match fields.map.get_mut(&field.ident) {
								Some(f) => Ok(f.borrow_mut()),
				                None => Err(Error { kind: ErrorKind::UnknownStructField(field.ident.clone()), span: field.span.clone()}),
							}
						},
						_ => Err(Error { kind: ErrorKind::CannotAccessFieldOnNonStruct, span: expression.span.clone()})
					}
				}
			},

			Expression_::UnaryOp(ref unop, ref e) => {
				match *unop {
					UnaryOp::Dereference => {
						match try!(self.value_from_expression(context, e)) {
							Value::MutReference(v) => Ok(v),
							Value::Reference(_) => Err(Error { kind: ErrorKind::CannotDerefConstRefInMutContext, span: expression.span.clone()}),
							_ => Err(Error { kind: ErrorKind::CannotDerefNonRef, span: expression.span.clone()})
						}
					},
					_ => Err(Error { kind: ErrorKind::CannotGetMutRef(expression.expr.clone()), span: expression.span.clone()})
				}
			},

			_ => Err(Error { kind: ErrorKind::CannotGetMutRef(expression.expr.clone()), span: expression.span.clone()})
		}
	}

	fn value_p_from_expression(&'a self, context: *mut InterpreterContext<'a>, expression: &Expression) -> Result<*const Value> {
		match expression.expr {
			Expression_::Variable(ref p) => {
				if p.parts.len() == 1 {
					unsafe { if let Some(ref var) = (*context).vars.get(AsRef::<str>::as_ref(&p.parts.get(0).unwrap().ident[..])) {
						 return Ok(&var.value);
					} }
				};

				unsafe {
					if p.parts.len() == 1 {
						if let Some(f) = self.funcs.get(&Path::concat((*context).current_path.clone(), p.clone())) {
							 return Ok(f);
						};
					} else {
						let mut root_path = (*context).current_path.parts.clone();
						root_path.pop();

						if let Some(f) = self.funcs.get(&Path::concat(Path { span: (*context).current_path.span.clone(), parts: root_path }, p.clone())) {
							 return Ok(f);
						};
					}
				}

				Err(Error { kind: ErrorKind::UnknownVariable(p.parts.clone()), span: p.span.clone()})
			},
			Expression_::Index(ref indexed, ref index) => {
				match *index {
					Some(ref e) => {
						let indexed_value_ref = try!(self.value_p_from_expression(context, indexed));

						let index_value = try!(self.value_from_expression(context, e));

						unsafe {
							match *indexed_value_ref {
								Value::Array(_, ref a) => {
									match index_value {
										Value::Integer(i) => {
											match a.get(i as usize) {
												Some(v) => Ok(v),
												None => Err(Error { kind: ErrorKind::IndexOutOfBounds, span: e.span.clone()}),
											}
										},
										other => Err(Error { kind: ErrorKind::MismatchedTypes(Type::Int, try!(Self::type_from_value(&other, e.span.clone()))), span: e.span.clone()})
									}
								},
								Value::Map(ref t, _, ref m) => {
									let index_type = try!(Self::type_from_value(&index_value, e.span.clone()));

									if index_type != *t {
									 	Err(Error { kind: ErrorKind::MismatchedTypes((*t).clone(), index_type), span: e.span.clone()})
									} else {
										match m.map.get(&index_value) {
											Some(v) => Ok(v),
											None => Err(Error { kind: ErrorKind::UnknownIndex(index_value), span: e.span.clone()}),
										}
									}
								},
								_ => Err(Error { kind: ErrorKind::CannotIndexNonIndexable, span: expression.span.clone()})
							}
						}
					},

					None => {
						let indexed_value_ref = try!(self.value_mut_p_from_expression(context, indexed));

						unsafe {
							match *indexed_value_ref {
								Value::Array(ref t, ref mut a) => {
									a.push(
										match *t {
											Type::None => return Err(Error { kind: ErrorKind::CannotPushToUntypedArray, span: indexed.span.clone()}),
											ref array_type => try!(self.default_value((*array_type).clone(), indexed.span.clone())),
										}
									);

									let last_index = a.len() - 1;
									Ok(a.get(last_index).unwrap())
								},
								_ => Err(Error { kind: ErrorKind::CannotPushToNonArray, span: indexed.span.clone()})
							}
						}
					},
				}
			},

			Expression_::Field(ref struct_expr, ref field) => {
				let struct_value_ref = try!(self.value_p_from_expression(context, struct_expr));

				unsafe {
					match *struct_value_ref {
						Value::Struct(_, ref fields) => {
							match fields.map.get(&field.ident) {
								Some(f) => Ok(f.borrow()),
				                None => Err(Error { kind: ErrorKind::UnknownStructField(field.ident.clone()), span: field.span.clone()}),
							}
						},
						_ => Err(Error { kind: ErrorKind::CannotAccessFieldOnNonStruct, span: expression.span.clone()})
					}
				}
			},

			Expression_::UnaryOp(ref unop, ref e) => {
				match *unop {
					UnaryOp::Dereference => {
						match try!(self.value_from_expression(context, e)) {
							Value::Reference(v) => Ok(v),
							Value::MutReference(v) => Ok(v),
							_ => Err(Error { kind: ErrorKind::CannotDerefNonRef, span: expression.span.clone()})
						}
					},
					_ => Err(Error { kind: ErrorKind::CannotGetRef(expression.expr.clone()), span: expression.span.clone()})
				}
			},

			_ => Err(Error { kind: ErrorKind::CannotGetRef(expression.expr.clone()), span: expression.span.clone()})
		}
	}

	fn value_from_expression(&'a self, context: *mut InterpreterContext<'a>, expression: &Expression) -> Result<Value> {
		match expression.expr {
			Expression_::StringLiteral(ref sl) => Ok(Value::String(sl.clone())),
			Expression_::IntegerLiteral(il) => Ok(Value::Integer(il)),
			Expression_::BoolLiteral(bl) => Ok(Value::Bool(bl)),
			Expression_::CharLiteral(cl) => Ok(Value::Char(cl)),
			Expression_::Variable(_) => {
				unsafe {
					Ok((*try!(self.value_p_from_expression(context, expression))).clone())
				}
			},

			Expression_::Array(ref a) => {
				let mut values: std::vec::Vec<Value> = vec![];
				let mut array_type: Type = Type::None;
				for item in a {
					let value = try!(self.value_from_expression(context, item));
					let value_type = try!(Self::type_from_value(&value, item.span.clone()));
					match array_type {
						Type::None => array_type = value_type,
						ref t => if value_type != *t {
							return Err(Error { kind: ErrorKind::HeterogeneousTypesInArray, span: expression.span.clone()})
						},
					};

					values.push(value)
				}

				Ok(Value::Array(array_type, values))
			},

			Expression_::Map(ref m) => { 
				let mut values = MapValue {
					map: std::collections::HashMap::new(),
				};
				let mut map_type1: Type = Type::None;
				let mut map_type2: Type = Type::None;
				for (key, value) in &m.map {
					let key_value = try!(self.value_from_expression(context, key));
					let key_type = try!(Self::type_from_value(&key_value, key.span.clone()));
					let value_value = try!(self.value_from_expression(context, value));
					let value_type = try!(Self::type_from_value(&value_value, value.span.clone()));
					match (map_type1.clone(), map_type2.clone()) {
						(Type::None, Type::None) => {
							map_type1 = key_type.clone();
							map_type2 = value_type.clone();
						},
						(ref t1, ref t2) => if key_type != *t1 || value_type != *t2 {
							return Err(Error { kind: ErrorKind::HeterogeneousTypesInMap, span: expression.span.clone()})
						},
					};

					values.map.insert(key_value, value_value);
				}

				Ok(Value::Map(map_type1, map_type2, values))
			},

			Expression_::StructInit(ref p, ref fields) => {
				let struct_decl = match self.structs.get(p) {
					Some(s) => s,
					None => return Err(Error { kind: ErrorKind::UnknownStruct(p.parts.clone()), span: p.span.clone()})
				};

				let mut new_content: std::collections::HashMap<String, Value> = std::collections::HashMap::new();

				for new_field in fields {
					let mut found_field = false;

					for field in &struct_decl.fields {
						if field.name == new_field.name.ident {
							found_field = true;
						}
					}

					if !found_field {
						return Err(Error { kind: ErrorKind::UnknownStructField(new_field.name.ident.clone()), span: new_field.span.clone()})
					}
				}

				for field in &struct_decl.fields {
					let mut found_field = false;

					for new_field in fields {
						if field.name == new_field.name.ident {
							let value = try!(self.value_from_expression(context, &new_field.value));
							let value_type = try!(Self::type_from_value(&value, new_field.span.clone()));

							if field.field_type != value_type {
								return Err(Error { kind: ErrorKind::MismatchedTypes(value_type, field.field_type.clone()), span: new_field.span.clone()})
							} else {
								new_content.insert(field.name.clone(), value);
							};

							found_field = true;
						}
					}

					if !found_field {
						return Err(Error { kind: ErrorKind::MissingStructField(field.name.clone()), span: expression.span.clone()})
					}
				}

				Ok(Value::Struct(
					p.clone(),
					StructValue { map: new_content },
				))
			},

			Expression_::FuncCall(ref func, ref args) => {
				self.execute_func_call(context, func, args, expression.span.clone())
			},

			Expression_::Field(ref struct_expr, ref field) => {
				match try!(self.value_from_expression(context, struct_expr)) {
					Value::Struct(_, ref fields) => {
						match fields.map.get(&field.ident) {
							Some(f) => Ok((*f).clone()),
							None => Err(Error { kind: ErrorKind::UnknownStructField(field.ident.clone()), span: field.span.clone()}),
						}
					},
					_ => Err(Error { kind: ErrorKind::CannotAccessFieldOnNonStruct, span: expression.span.clone()})
				}
			},

			Expression_::Index(ref indexed, ref index) => {
				match *index {
					Some(ref e) => {
						let indexed_value_ref = try!(self.value_p_from_expression(context, indexed));

						let index_value = try!(self.value_from_expression(context, e));

						unsafe {
							match *indexed_value_ref {
								Value::Array(_, ref a) => {
									match index_value {
										Value::Integer(i) => {
											match a.get(i as usize) {
												Some(v) => Ok((*v).clone()),
												None => Err(Error { kind: ErrorKind::IndexOutOfBounds, span: e.span.clone()}),
											}
										},
										other => Err(Error { kind: ErrorKind::MismatchedTypes(Type::Int, try!(Self::type_from_value(&other, e.span.clone()))), span: e.span.clone()})
									}
								},
								Value::Map(ref t, _, ref m) => {
									let index_type = try!(Self::type_from_value(&index_value, e.span.clone()));

									if index_type != *t {
										Err(Error { kind: ErrorKind::MismatchedTypes((*t).clone(), index_type), span: e.span.clone()})
									} else {
										match m.map.get(&index_value) {
											Some(v) => Ok((*v).clone()),
											None => Err(Error { kind: ErrorKind::UnknownIndex(index_value), span: e.span.clone()}),
										}
									}
								},
								_ => Err(Error { kind: ErrorKind::CannotIndexNonIndexable, span: expression.span.clone()})
							}
						}
					},

					None => {
						let indexed_value_ref = try!(self.value_mut_p_from_expression(context, indexed));

						unsafe {
							match *indexed_value_ref {
								Value::Array(ref t, ref mut a) => {
									a.push(
										match *t {
											Type::None => return Err(Error { kind: ErrorKind::CannotPushToUntypedArray, span: indexed.span.clone()}),
											ref array_type => try!(self.default_value((*array_type).clone(), indexed.span.clone())),
										}
									);

									let last_index = a.len() - 1;
									Ok(a.get(last_index).unwrap().clone())
								},
								_ => Err(Error { kind: ErrorKind::CannotPushToNonArray, span: indexed.span.clone()})
							}
						}
					},
				}
			},

			Expression_::UnaryOp(ref unop, ref e) => {
				match *unop {
					UnaryOp::Reference => {
						Ok(Value::Reference(try!(self.value_p_from_expression(context, e))))
					},
					UnaryOp::MutReference => {
						Ok(Value::MutReference(try!(self.value_mut_p_from_expression(context, e))))
					},
					UnaryOp::Count => {
						match try!(self.value_from_expression(context, e)) {
							Value::Array(_, ref a) => Ok(Value::Integer(a.len() as i64)),
							Value::String(ref s) => Ok(Value::Integer(s.len() as i64)),
							_ => Err(Error { kind: ErrorKind::CannotCountNonCountable, span: expression.span.clone()})
						}
					},
					UnaryOp::Dereference => {
						match try!(self.value_from_expression(context, e)) {
							Value::Reference(v) => unsafe { Ok((*v).clone()) },
							Value::MutReference(v) => unsafe { Ok((*v).clone()) },
							_ => Err(Error { kind: ErrorKind::CannotDerefNonRef, span: expression.span.clone()})
						}
					}
				}
			},

			Expression_::BinaryOp(ref binop, ref e1, ref e2) => {
				match *binop {
					BinaryOp::Addition => {
						let integer1 = match try!(self.value_from_expression(context, e1)) {
							Value::Integer(i) => i,
							other => return Err(Error { kind: ErrorKind::MismatchedTypes(Type::Int, try!(Self::type_from_value(&other, e1.span.clone()))), span: e1.span.clone()})
						};

						let integer2 = match try!(self.value_from_expression(context, e2)) {
							Value::Integer(i) => i,
							other => return Err(Error { kind: ErrorKind::MismatchedTypes(Type::Int, try!(Self::type_from_value(&other, e2.span.clone()))), span: e2.span.clone()})
						};

						Ok(Value::Integer(integer1 + integer2))
					},
					BinaryOp::Substraction => {
                        let integer1 = match try!(self.value_from_expression(context, e1)) {
							Value::Integer(i) => i,
							other => return Err(Error { kind: ErrorKind::MismatchedTypes(Type::Int, try!(Self::type_from_value(&other, e1.span.clone()))), span: e1.span.clone()})
						};

						let integer2 = match try!(self.value_from_expression(context, e2)) {
							Value::Integer(i) => i,
							other => return Err(Error { kind: ErrorKind::MismatchedTypes(Type::Int, try!(Self::type_from_value(&other, e2.span.clone()))), span: e2.span.clone()})
						};

						Ok(Value::Integer(integer1 - integer2))
					},
					BinaryOp::Multiplication => {
                        let integer1 = match try!(self.value_from_expression(context, e1)) {
							Value::Integer(i) => i,
							other => return Err(Error { kind: ErrorKind::MismatchedTypes(Type::Int, try!(Self::type_from_value(&other, e1.span.clone()))), span: e1.span.clone()})
						};

						let integer2 = match try!(self.value_from_expression(context, e2)) {
							Value::Integer(i) => i,
							other => return Err(Error { kind: ErrorKind::MismatchedTypes(Type::Int, try!(Self::type_from_value(&other, e2.span.clone()))), span: e2.span.clone()})
						};

						Ok(Value::Integer(integer1 * integer2))
					},
					BinaryOp::Division => {
                        let integer1 = match try!(self.value_from_expression(context, e1)) {
							Value::Integer(i) => i,
							other => return Err(Error { kind: ErrorKind::MismatchedTypes(Type::Int, try!(Self::type_from_value(&other, e1.span.clone()))), span: e1.span.clone()})
						};

						let integer2 = match try!(self.value_from_expression(context, e2)) {
							Value::Integer(i) => i,
							other => return Err(Error { kind: ErrorKind::MismatchedTypes(Type::Int, try!(Self::type_from_value(&other, e2.span.clone()))), span: e2.span.clone()})
						};

						Ok(Value::Integer(integer1 / integer2))
					},
					BinaryOp::Modulo => {
                        let integer1 = match try!(self.value_from_expression(context, e1)) {
							Value::Integer(i) => i,
							other => return Err(Error { kind: ErrorKind::MismatchedTypes(Type::Int, try!(Self::type_from_value(&other, e1.span.clone()))), span: e1.span.clone()})
						};

						let integer2 = match try!(self.value_from_expression(context, e2)) {
							Value::Integer(i) => i,
							other => return Err(Error { kind: ErrorKind::MismatchedTypes(Type::Int, try!(Self::type_from_value(&other, e2.span.clone()))), span: e2.span.clone()})
						};

						Ok(Value::Integer(integer1 % integer2))
					},

					BinaryOp::Concatenation => {
						let string1 = match try!(self.value_from_expression(context, e1)) {
							Value::String(s) => s,
							other => return Err(Error { kind: ErrorKind::MismatchedTypes(Type::String, try!(Self::type_from_value(&other, e1.span.clone()))), span: e1.span.clone()})
						};

						let string2 = match try!(self.value_from_expression(context, e2)) {
							Value::String(s) => s,
							other => return Err(Error { kind: ErrorKind::MismatchedTypes(Type::String, try!(Self::type_from_value(&other, e2.span.clone()))), span: e2.span.clone()})
						};

						let mut new_string = String::new();
						new_string.push_str(string1.as_ref());
						new_string.push_str(string2.as_ref());
						Ok(Value::String(new_string))
					}

					BinaryOp::Equality => {
						let value1 = try!(self.value_from_expression(context, e1));
						let value2 = try!(self.value_from_expression(context, e2));

						Ok(Value::Bool(value1 == value2))
					},
					BinaryOp::Inequality => {
						let value1 = try!(self.value_from_expression(context, e1));
						let value2 = try!(self.value_from_expression(context, e2));

						Ok(Value::Bool(value1 != value2))
					},
				}
			},
		}
	}



	fn default_value(&self, var_type: Type, span: Span) -> Result<Value> {
		match var_type {
			Type::String => Ok(Value::String("".to_string())),
			Type::Int => Ok(Value::Integer(0)),
			Type::Bool => Ok(Value::Bool(false)),
			Type::Char => Ok(Value::Char('\0')),
			Type::Array(t) => Ok(Value::Array(*t, vec![])),
			Type::Map(t1, t2) => Ok(Value::Map(*t1, *t2, MapValue { map: std::collections::HashMap::new() })),
			Type::Struct(ref p) => {
				let struct_decl = match self.structs.get(p) {
					Some(s) => s,
					None => return Err(Error { kind: ErrorKind::UnknownStruct(p.parts.clone()), span: span})
				};

				let mut fields: std::collections::HashMap<String, Value> = std::collections::HashMap::new();

				for field in &struct_decl.fields {
					fields.insert(field.name.clone(), try!(self.default_value(field.field_type.clone(), field.span.clone())));
				}

				Ok(Value::Struct(p.clone(), StructValue { map: fields }))
			},
			other => Err(Error { kind: ErrorKind::NoDefaultValue(other), span: span}),
		}
	}
}
