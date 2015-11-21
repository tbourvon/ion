use parser::ast::*;
use std;
use lexer;
use lexer::Span;
use parser;
use std::fs::File;
use std::io::prelude::*;
use std::io;
use std::borrow::Borrow;
use std::borrow::BorrowMut;

pub struct Interpreter<'a> {
	ast: &'a Ast,
	funcs: std::collections::HashMap<Path, Value<'a>>,
	structs: std::collections::HashMap<Path, StructDeclData>,
}

#[derive(Debug)]
pub struct InterpreterContext<'a> {
	vars: std::collections::HashMap<String, Variable<'a>>, // TODO: Bloody get rid of Strings for indexing... preferably Path indexing would be best
}

#[derive(Debug)]
pub struct Variable<'a> {
	name: String,
	var_type: Type,
	value: Value<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value<'a> {
	Nil,
	String(String),
	Integer(i64),
	Bool(bool),
	Char(char),
	Struct(Path, std::collections::HashMap<String, Box<Value<'a>>>),
	Array(Type, std::vec::Vec<Value<'a>>),
	Reference(*const Value<'a>),
	MutReference(*mut Value<'a>),
	Func(FuncDeclData),
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
					FuncDeclData {
						span: Span::nil_span(),
						name: name,
						return_type: Type::NoType,
						parameters: vec![],
						statements: vec![],
					}
				),
			)
		};

		inject_func("println".to_string());
		inject_func("readln".to_string());
	}

	pub fn execute(&'a mut self) -> Result<(), String> {
		self.inject_builtin_funcs();

		let initial_path = Path {
			span: Span::nil_span(),
			parts: vec![],
		};

		for statement in &self.ast.statements {
			try!(self.execute_statement(statement, initial_path.clone()));
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
		};

		try!(self.value_from_expression(&mut context, &main_func_call_expr));

		Ok(())
	}

	fn execute_statement(&mut self, statement: &Statement, current_path: Path) -> Result<(), String> {
		match *statement {
			Statement::FuncDecl(ref fd) => {
				let mut new_path = current_path.clone();
				new_path.parts.push(SpannedString {
					span: fd.span.clone(),
					ident: fd.name.clone(),
				});

				self.funcs.insert(new_path, Value::Func(*fd.clone()));
			},
			Statement::Import(ref i) => {
				try!(self.execute_import(i, current_path));
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

	fn type_from_value(value: *const Value, span: Span) -> Result<Type, String> {
		unsafe {
			match *value {
				Value::Array(ref t, _) => match *t {
					Type::NoType => Err(format!("Interpreter error: cannot deduce type for empty array, {:?}", span)),
					ref array_type => Ok(Type::ArrayType(
						Box::new((*array_type).clone())
					)),
				},
				Value::Bool(_) => Ok(Type::BoolType),
				Value::Char(_) => Ok(Type::CharType),
				Value::String(_) => Ok(Type::StringType),
				Value::Integer(_) => Ok(Type::IntType),
				Value::Struct(ref t, _) => Ok(Type::StructType((*t).clone())),
				Value::Reference(ref r) => Ok(Type::ReferenceType(Box::new(try!(Self::type_from_value(*r, span))))),
				Value::MutReference(ref r) => Ok(Type::MutReferenceType(Box::new(try!(Self::type_from_value(*r, span))))),
				Value::Func(ref fd) => {
					let mut param_types: std::vec::Vec<Box<Type>> = vec![];
					for parameter in &fd.parameters {
						param_types.push(Box::new(parameter.param_type.clone()));
					}
					Ok(Type::FuncType(Box::new(fd.return_type.clone()), param_types))
				},
				Value::Nil => Ok(Type::NoType),
			}
		}
	}

	fn execute_import(&mut self, import_data: &ImportData, current_path: Path) -> Result<(), String> { // TODO: rework that for more safety and non-naive handling
		let path_string = import_data.path.clone() + ".ion";
	    let path = std::path::Path::new(AsRef::<str>::as_ref(&path_string[..]));
	    let mut file = match File::open(&path) {
	        Ok(file) => file,
	        Err(err) => return Err(format!("{}", err)),
	    };

	    let mut s = String::new();
		let res = file.read_to_string(&mut s);
	    if let Some(err) = res.err() {
	        return Err(format!("{}", err))
	    }
	    let mut reader = lexer::Reader::new(s.as_ref());

	    let mut parser = parser::Parser::new(&mut reader);
	    let ast = try!(parser.parse());

		let mut new_path = current_path.clone();
		new_path.parts.push(SpannedString {
			span: import_data.span.clone(),
			ident: import_data.path.clone(),
		});

		for statement in &ast.statements {
			try!(self.execute_statement(statement, new_path.clone()));
		}

		Ok(())
	}

	fn execute_block_statement(&'a self, context: *mut InterpreterContext<'a>, block_statement: &'a BlockStatement) -> Result<Value, String> {
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

	fn execute_forin(&'a self, context: *mut InterpreterContext<'a>, forin_data: &'a ForInData) -> Result<Value, String> {
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
								var_type: Type::CharType,
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
			other => return Err(format!("Cannot iterate over {:?}, {:?}", other, forin_data.collection.span)),
		};

		Ok(Value::Nil)
	}

	fn execute_return(&'a self, context: *mut InterpreterContext<'a>, return_data: &ReturnData) -> Result<Value, String> { // TODO: detect ref to local in return expr
		match return_data.expected_type {
			Type::NoType => {
				if let Some(_) = return_data.value {
					return Err(format!("Interpreter error: unexpected expression in return statement, {:?}", return_data.span))
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
					None => return Err(format!("Interpreter error: expected expression for return statement, {:?}", return_data.span)),
				};

				let value_type = try!(Self::type_from_value(&value, span.clone()));
				if value_type != *t {
					return Err(format!("Interpreter error: mismatched types in return statement (got {:?} expected {:?}), {:?}", value_type, *t, span))
				}

				Ok(value)
			},
		}
	}

	fn execute_func_call(&'a self, context: *mut InterpreterContext<'a>, func: &Expression, args: &std::vec::Vec<Box<Expression>>, span: Span) -> Result<Value, String> {
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

		if is_builtin_func(&func, "println") {
			self.builtin_println(context, args, span)
		} else if is_builtin_func(&func, "readln") {
			self.builtin_readln(args, span)
		} else {
			let func_decl = unsafe {
				match *try!(self.value_p_from_expression(context, &func)) {
					Value::Func(ref fd) => fd,
					_ => return Err(format!("Interpereter error: cannot call non-function, {:?}", func.span))
				}
			};

			let mut local_context = InterpreterContext {
				vars: std::collections::HashMap::new(),
			};

			let mut param_count = 0;
			for param in &func_decl.parameters {
				let variable = Variable {
					name: param.name.clone(),
					var_type: param.param_type.clone(),
					value: {
						if param_count < args.len() {
							let value = try!(self.value_from_expression(context, &args.get(param_count).unwrap()));

							let value_type = try!(Self::type_from_value(&value, args.get(param_count).unwrap().span.clone()));

							if value_type != param.param_type {
								return Err(format!("Interpreter error: mismatched types in function call (got {:?} expected {:?}), {:?}", value_type, param.param_type, &args.get(param_count).unwrap().span))
							} else {
								value
							}
						} else {
							if let Some(ref e) = param.default_value {
								let value = try!(self.value_from_expression(context, e));

								let value_type = try!(Self::type_from_value(&value, e.span.clone()));

								if value_type != param.param_type {
									return Err(format!("Interpreter error: mismatched types in function declaration default value (got {:?} expected {:?}), {:?}", value_type, param.param_type, e.span))
								} else {
									value
								}
							} else {
								return Err(format!("Interpreter error: expected argument for {:?}, {:?}", param.name, span))
							}
						}
					}
				};

				local_context.vars.insert(
					param.name.clone(),
					variable
				);

				param_count += 1;
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

	fn execute_var_decl(&'a self, context: *mut InterpreterContext<'a>, var_decl_data: &'a VarDeclData) -> Result<(), String> {
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
							return Err(format!("Interpreter error: mismatched types in var initialization (got {:?} expected {:?}), {:?}", value_type, var_decl_data.var_type, span))
						} else {
							value
						}
					},
				}
			);
		};

		Ok(())
	}

	fn execute_var_assignment(&'a self, context: *mut InterpreterContext<'a>, lhs: &Expression, rhs: &Expression) -> Result<(), String> {
		let lhs_value_ref = try!(self.value_mut_p_from_expression(context, &lhs));

		let rhs_value = try!(self.value_from_expression(context, &rhs));

		let value_type = Self::type_from_value(&rhs_value, rhs.span.clone());
		let current_type = Self::type_from_value(lhs_value_ref, lhs.span.clone());

		if value_type != current_type {
			return Err(format!("Interpreter error: mismatched types in var assignment (got {:?} expected {:?}), {:?}", value_type, current_type, rhs.span))
		} else {
			unsafe {
				*lhs_value_ref = rhs_value;
			}
		};

		Ok(())
	}

	fn execute_if(&'a self, context: *mut InterpreterContext<'a>, if_data: &'a IfData) -> Result<Value, String> {
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
			_ => Err(format!("Interpreter error: expected bool expression in if statement, {:?}", if_data.condition.span))
		}
	}

	fn execute_while(&'a self, context: *mut InterpreterContext<'a>, while_data: &'a WhileData) -> Result<Value, String> {
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
				_ => return Err(format!("Interpreter error: expected bool expression in while statement, {:?}", while_data.condition.span))
			}
		}
	}

	fn value_mut_p_from_expression(&'a self, context: *mut InterpreterContext<'a>, expression: &Expression) -> Result<*mut Value, String> {
		match expression.expr {
			Expression_::Variable(ref p) => {
				if p.parts.len() == 1 {
					unsafe { if let Some(ref mut var) = (*context).vars.get_mut(AsRef::<str>::as_ref(&p.parts.get(0).unwrap().ident[..])) {
						 return Ok(&mut var.value);
					} }
				};

				if self.funcs.get(p).is_some() {
					 return Err(format!("Interpreter error: cannot mutably reference function, {:?}", p.span));
				};

				return Err(format!("Interpreter error: unknown variable {:?}, {:?}", p.parts, p.span));
			},

			Expression_::Index(ref indexed, ref index) => {
				match *index {
					Some(ref e) => {
						let indexed_value_p = try!(self.value_mut_p_from_expression(context, indexed));

						let index_value = try!(self.value_from_expression(context, e));

						match index_value {
							Value::Integer(i) => {
								unsafe {
									match *indexed_value_p {
										Value::Array(_, ref mut a) => match a.get_mut(i as usize) {
											Some(v) => Ok(v),
											None => return Err(format!("Interpreter error: index out of bounds, {:?}", e.span)),
										},
										_ => return Err(format!("Interpreter error: can't access index on non-array, {:?}", expression.span))
									}
								}
							},
							_ => return Err(format!("Interpreter error: invalid index type for array, {:?}", e.span))
						}
					},

					None => {
						let indexed_value_ref = try!(self.value_mut_p_from_expression(context, indexed));

						unsafe {
							match *indexed_value_ref {
								Value::Array(ref t, ref mut a) => {
									a.push(
										match *t {
											Type::NoType => return Err(format!("Interpreter error: cannot push to untyped array, {:?}", indexed.span)),
											ref array_type => try!(self.default_value((*array_type).clone(), indexed.span.clone())),
										}
									);

									let last_index = a.len() - 1;
									Ok(a.get_mut(last_index).unwrap())
								},
								_ => return Err(format!("Interpreter error: can't push to non-array, {:?}", indexed.span))
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
							match fields.get_mut(&field.ident) {
								Some(f) => Ok(f.borrow_mut()),
				                None => return Err(format!("Interpreter error: unknown struct field, {:?}", field.span)),
							}
						},
						_ => return Err(format!("Interpreter error: cannot access field on non-struct, {:?}", expression.span))
					}
				}
			},

			Expression_::UnOp(ref unop, ref e) => {
				match *unop {
					UnOp::Dereference => {
						match try!(self.value_from_expression(context, e)) {
							Value::MutReference(v) => Ok(v),
							Value::Reference(_) => return Err(format!("Interpreter error: cannot dereference const reference in mut context, {:?}", expression.span)),
							_ => return Err(format!("Interpreter error: cannot dereference non-reference, {:?}", expression.span))
						}
					},
					_ => return Err(format!("Interpreter error: cannot get mutable reference for {:?}, {:?}", expression.expr, expression.span))
				}
			},

			_ => return Err(format!("Interpreter error: cannot get mutable reference for {:?}, {:?}", expression.expr, expression.span))
		}
	}

	fn value_p_from_expression(&'a self, context: *mut InterpreterContext<'a>, expression: &Expression) -> Result<*const Value, String> {
		match expression.expr {
			Expression_::Variable(ref p) => {
				if p.parts.len() == 1 {
					unsafe { if let Some(ref var) = (*context).vars.get(AsRef::<str>::as_ref(&p.parts.get(0).unwrap().ident[..])) {
						 return Ok(&var.value);
					} }
				};

				if let Some(f) = self.funcs.get(p) {
					 return Ok(f);
				};

				return Err(format!("Interpreter error: unknown variable {:?}, {:?}", p.parts, p.span));
			},
			Expression_::Index(ref indexed, ref index) => {
				match *index {
					Some(ref e) => {
						let indexed_value_ref = try!(self.value_p_from_expression(context, indexed));

						let index_value = try!(self.value_from_expression(context, e));

						match index_value {
							Value::Integer(i) => {
								unsafe {
									match *indexed_value_ref {
										Value::Array(_, ref a) => match a.get(i as usize) {
											Some(v) => Ok(v),
											None => return Err(format!("Interpreter error: index out of bounds, {:?}", e.span)),
										},
										_ => return Err(format!("Interpreter error: can't access index on non-array, {:?}", expression.span))
									}
								}
							},
							_ => return Err(format!("Interpreter error: invalid index type for array, {:?}", e.span))
						}
					},

					None => {
						let indexed_value_ref = try!(self.value_mut_p_from_expression(context, indexed));

						unsafe {
							match *indexed_value_ref {
								Value::Array(ref t, ref mut a) => {
									a.push(
										match *t {
											Type::NoType => return Err(format!("Interpreter error: cannot push to untyped array, {:?}", indexed.span)),
											ref array_type => try!(self.default_value((*array_type).clone(), indexed.span.clone())),
										}
									);

									let last_index = a.len() - 1;
									Ok(a.get(last_index).unwrap())
								},
								_ => return Err(format!("Interpreter error: can't push to non-array, {:?}", indexed.span))
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
							match fields.get(&field.ident) {
								Some(f) => Ok(f.borrow()),
				                None => return Err(format!("Interpreter error: unknown struct field, {:?}", field.span)),
							}
						},
						_ => return Err(format!("Interpreter error: cannot access field on non-struct, {:?}", expression.span))
					}
				}
			},

			Expression_::UnOp(ref unop, ref e) => {
				match *unop {
					UnOp::Dereference => {
						match try!(self.value_from_expression(context, e)) {
							Value::Reference(v) => Ok(v),
							Value::MutReference(v) => Ok(v),
							_ => return Err(format!("Interpreter error: cannot dereference non-reference, {:?}", expression.span))
						}
					},
					_ => return Err(format!("Interpreter error: cannot get reference for {:?}, {:?}", expression.expr, expression.span))
				}
			},

			_ => return Err(format!("Interpreter error: cannot get reference for {:?}, {:?}", expression.expr, expression.span))
		}
	}

	fn value_from_expression(&'a self, context: *mut InterpreterContext<'a>, expression: &Expression) -> Result<Value, String> {
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
				let mut array_type: Type = Type::NoType;
				for item in a {
					let value = try!(self.value_from_expression(context, item));
					let value_type = try!(Self::type_from_value(&value, item.span.clone()));
					match array_type {
						Type::NoType => array_type = value_type,
						ref t => if value_type != *t {
							return Err(format!("Interpreter error: heterogeneous types in array literal, {:?}", expression.span))
						},
					};

					values.push(value)
				}

				Ok(Value::Array(array_type, values))
			},

			Expression_::StructInit(ref p, ref fields) => {
				let struct_decl = match self.structs.get(p) {
					Some(s) => s,
					None => return Err(format!("Interpreter error: unknown struct, {:?}", p.span))
				};

				let mut new_content: std::collections::HashMap<String, Box<Value>> = std::collections::HashMap::new();

				for new_field in fields {
					let mut found_field = false;

					for field in &struct_decl.fields {
						if field.name == new_field.name.ident {
							found_field = true;
						}
					}

					if !found_field {
						return Err(format!("Interpreter error: unknown field {:?} in struct init, {:?}", new_field.name, new_field.span))
					}
				}

				for field in &struct_decl.fields {
					let mut found_field = false;

					for new_field in fields {
						if field.name == new_field.name.ident {
							let value = try!(self.value_from_expression(context, &new_field.value));
							let value_type = try!(Self::type_from_value(&value, new_field.span.clone()));

							if field.field_type != value_type {
								return Err(format!("Interpreter error: mismatched types in struct init (got {:?} expected {:?}), {:?}", value_type, field.field_type, new_field.span))
							} else {
								new_content.insert(field.name.clone(), Box::new(value));
							};

							found_field = true;
						}
					}

					if !found_field {
						return Err(format!("Interpreter error: missing field {:?} in struct init, {:?}", field.name, expression.span))
					}
				}

				Ok(Value::Struct(
					p.clone(),
					new_content,
				))
			},

			Expression_::FuncCall(ref func, ref args) => {
				self.execute_func_call(context, func, args, expression.span.clone())
			},

			Expression_::Field(ref struct_expr, ref field) => {
				match try!(self.value_from_expression(context, struct_expr)) {
					Value::Struct(_, ref fields) => {
						match fields.get(&field.ident) {
							Some(f) => Ok(*f.clone()),
							None => return Err(format!("Interpreter error: unknown struct field, {:?}", field.span)),
						}
					},
					_ => return Err(format!("Interpreter error: cannot access field on non-struct, {:?}", expression.span))
				}
			},

			Expression_::Index(ref indexed, ref index) => {
				match *index {
					Some(ref e) => {
						let indexed_value_ref = try!(self.value_p_from_expression(context, indexed));

						let index_value = try!(self.value_from_expression(context, e));

						match index_value {
							Value::Integer(i) => {
								unsafe {
									match *indexed_value_ref {
										Value::Array(_, ref a) => match a.get(i as usize) {
											Some(v) => Ok((*v).clone()),
											None => return Err(format!("Interpreter error: index out of bounds, {:?}", e.span)),
										},
										_ => return Err(format!("Interpreter error: can't access index on non-array, {:?}", expression.span))
									}
								}
							},
							_ => return Err(format!("Interpreter error: invalid index type for array, {:?}", e.span))
						}
					},

					None => {
						let indexed_value_ref = try!(self.value_mut_p_from_expression(context, indexed));

						unsafe {
							match *indexed_value_ref {
								Value::Array(ref t, ref mut a) => {
									a.push(
										match *t {
											Type::NoType => return Err(format!("Interpreter error: cannot push to untyped array, {:?}", indexed.span)),
											ref array_type => try!(self.default_value((*array_type).clone(), indexed.span.clone())),
										}
									);

									let last_index = a.len() - 1;
									Ok(a.get(last_index).unwrap().clone())
								},
								_ => return Err(format!("Interpreter error: can't push to non-array, {:?}", indexed.span))
							}
						}
					},
				}
			},

			Expression_::UnOp(ref unop, ref e) => {
				match *unop {
					UnOp::Reference => {
						Ok(Value::Reference(try!(self.value_p_from_expression(context, e))))
					},
					UnOp::MutReference => {
						Ok(Value::MutReference(try!(self.value_mut_p_from_expression(context, e))))
					},
					UnOp::Count => {
						match try!(self.value_from_expression(context, e)) {
							Value::Array(_, ref a) => Ok(Value::Integer(a.len() as i64)),
							Value::String(ref s) => Ok(Value::Integer(s.len() as i64)),
							_ => return Err(format!("Interpreter error: can't get count on non-(array/string), {:?}", expression.span))
						}
					},
					UnOp::Dereference => {
						match try!(self.value_from_expression(context, e)) {
							Value::Reference(v) => unsafe { Ok((*v).clone()) },
							Value::MutReference(v) => unsafe { Ok((*v).clone()) },
							_ => return Err(format!("Interpreter error: cannot dereference non-reference, {:?}", expression.span))
						}
					}
				}
			},

			Expression_::BinOp(ref binop, ref e1, ref e2) => {
				match *binop {
					BinOp::Addition => {
						let integer1 = match try!(self.value_from_expression(context, e1)) {
							Value::Integer(i) => i,
							other => return Err(format!("Interpreter error: incorrect expression for addition: {:?}, {:?}", other, e1.span))
						};

						let integer2 = match try!(self.value_from_expression(context, e2)) {
							Value::Integer(i) => i,
							other => return Err(format!("Interpreter error: incorrect expression for addition: {:?}, {:?}", other, e2.span))
						};

						Ok(Value::Integer(integer1 + integer2))
					},
					BinOp::Substraction => {
						let integer1 = match try!(self.value_from_expression(context, e1)) {
							Value::Integer(i) => i,
							other => return Err(format!("Interpreter error: incorrect expression for substraction: {:?}, {:?}", other, e1.span))
						};

						let integer2 = match try!(self.value_from_expression(context, e2)) {
							Value::Integer(i) => i,
							other => return Err(format!("Interpreter error: incorrect expression for substraction: {:?}, {:?}", other, e2.span))
						};

						Ok(Value::Integer(integer1 - integer2))
					},
					BinOp::Multiplication => {
						let integer1 = match try!(self.value_from_expression(context, e1)) {
							Value::Integer(i) => i,
							other => return Err(format!("Interpreter error: incorrect expression for multiplication: {:?}, {:?}", other, e1.span))
						};

						let integer2 = match try!(self.value_from_expression(context, e2)) {
							Value::Integer(i) => i,
							other => return Err(format!("Interpreter error: incorrect expression for multiplication: {:?}, {:?}", other, e2.span))
						};

						Ok(Value::Integer(integer1 * integer2))
					},
					BinOp::Division => {
						let integer1 = match try!(self.value_from_expression(context, e1)) {
							Value::Integer(i) => i,
							other => return Err(format!("Interpreter error: incorrect expression for division: {:?}, {:?}", other, e1.span))
						};

						let integer2 = match try!(self.value_from_expression(context, e2)) {
							Value::Integer(i) => i,
							other => return Err(format!("Interpreter error: incorrect expression for division: {:?}, {:?}", other, e2.span))
						};

						Ok(Value::Integer(integer1 / integer2))
					},
					BinOp::Modulo => {
						let integer1 = match try!(self.value_from_expression(context, e1)) {
							Value::Integer(i) => i,
							other => return Err(format!("Interpreter error: incorrect expression for modulo: {:?}, {:?}", other, e1.span))
						};

						let integer2 = match try!(self.value_from_expression(context, e2)) {
							Value::Integer(i) => i,
							other => return Err(format!("Interpreter error: incorrect expression for modulo: {:?}, {:?}", other, e2.span))
						};

						Ok(Value::Integer(integer1 % integer2))
					},

					BinOp::Concatenation => {
						let string1 = match try!(self.value_from_expression(context, e1)) {
							Value::String(s) => s,
							other => return Err(format!("Interpreter error: incorrect expression for concatenation: {:?}, {:?}", other, e1.span))
						};

						let string2 = match try!(self.value_from_expression(context, e2)) {
							Value::String(s) => s,
							other => return Err(format!("Interpreter error: incorrect expression for concatenation: {:?}, {:?}", other, e2.span))
						};

						let mut new_string = String::new();
						new_string.push_str(string1.as_ref());
						new_string.push_str(string2.as_ref());
						Ok(Value::String(new_string))
					}

					BinOp::Equality => {
						let value1 = self.value_from_expression(context, e1);
						let value2 = self.value_from_expression(context, e2);

						Ok(Value::Bool(value1 == value2))
					},
					BinOp::Inequality => {
						let value1 = self.value_from_expression(context, e1);
						let value2 = self.value_from_expression(context, e2);

						Ok(Value::Bool(value1 != value2))
					},
				}
			},
		}
	}

	fn builtin_println(&'a self, context: *mut InterpreterContext<'a>, args: &std::vec::Vec<Box<Expression>>, span: Span) -> Result<Value, String> {
		if args.len() != 1 {
			return Err(format!("Interpreter error: invalid argument count for println, {:?}", span))
		};

		match try!(self.value_from_expression(context, args.get(0).unwrap())) {
			Value::String(s) => println!("{}", s),
			Value::Integer(i) => println!("{}", i),
			Value::Bool(b) => println!("{}", b),
			Value::Char(c) => println!("{}", c),
			Value::Struct(_, s) => println!("{:?}", s),
			Value::Array(_, a) => println!("{:?}", a),
			Value::Reference(v) => println!("ref {:?}", v),
			Value::MutReference(v) => println!("mutref {:?}", v),
			Value::Func(f) => println!("{:?}", f),
			Value::Nil => println!("nil"),
		};

		Ok(Value::Nil)
	}

	fn builtin_readln(&self, args: &std::vec::Vec<Box<Expression>>, span: Span) -> Result<Value, String> {
		if args.len() != 0 {
			return Err(format!("Interpreter error: invalid argument count for readln, {:?}", span))
		};

		let mut line = String::new();
	    let stdin = io::stdin();
	    stdin.lock().read_line(&mut line).unwrap();
    	Ok(Value::String(
			line
		))
	}

	fn default_value(&self, var_type: Type, span: Span) -> Result<Value, String> {
		match var_type {
			Type::StringType => Ok(Value::String("".to_string())),
			Type::IntType => Ok(Value::Integer(0)),
			Type::BoolType => Ok(Value::Bool(false)),
			Type::CharType => Ok(Value::Char('\0')),
			Type::ArrayType(t) => Ok(Value::Array(*t, vec![])),
			Type::StructType(ref p) => {
				let struct_decl = match self.structs.get(p) {
					Some(s) => s,
					None => return Err(format!("Interpreter error: unknown struct, {:?}", span))
				};

				let mut fields: std::collections::HashMap<String, Box<Value>> = std::collections::HashMap::new();

				for field in &struct_decl.fields {
					fields.insert(field.name.clone(), Box::new(try!(self.default_value(field.field_type.clone(), field.span.clone()))));
				}

				Ok(Value::Struct(p.clone(), fields))
			},
			Type::ReferenceType(_) => Err(format!("Interpreter error: no default value for references, {:?}", span)),
			Type::MutReferenceType(_) => Err(format!("Interpreter error: no default value for mut references, {:?}", span)),
			Type::FuncType(..) => Err(format!("Interpreter error: no default value for functions, {:?}", span)),
			Type::NoType => Err(format!("Interpreter error: no default value for nil type, {:?}", span))
		}
	}
}
