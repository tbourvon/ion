use parser::ast::*;
use std;
use lexer;
use lexer::Span;
use parser;
use std::fs::File;
use std::io::prelude::*;
use std::io;

pub struct Interpreter<'a> {
	ast: &'a Ast,
	funcs: std::collections::HashMap<String, FuncDeclData>, // TODO: Bloody get rid of Strings for indexing... preferably Path indexing would be best
	structs: std::collections::HashMap<String, StructDeclData>, // TODO: Bloody get rid of Strings for indexing... preferably Path indexing would be best
	imports: std::collections::HashMap<String, Box<Ast>>, // TODO: Bloody get rid of Strings for indexing... preferably Path indexing would be best
}

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
	String(String),
	Integer(i64),
	Bool(bool),
	Char(char),
	Struct(Type, std::collections::HashMap<String, Box<Value<'a>>>),
	Array(Option<Type>, std::vec::Vec<Value<'a>>),
	Reference(Path, &'a Value<'a>),
}

impl<'a> Interpreter<'a> {
	pub fn new(ast: &'a Ast) -> Self {
		Interpreter {
			ast: ast,
			funcs: std::collections::HashMap::new(),
			structs: std::collections::HashMap::new(),
			imports: std::collections::HashMap::new(),
		}
	}

	pub fn execute(&mut self) -> Result<(), String> {
		let mut iter = self.ast.statements.iter();
		loop { let statement = iter.next();
			match statement {
				Some(s) => {
					match *s {
						Statement::FuncDecl(ref fd) => {
							self.funcs.insert(fd.name.clone(), *fd.clone());
						},
						Statement::Import(ref i) => {
							try!(self.execute_import(i));
						}
						Statement::StructDecl(ref sd) => {
							self.structs.insert(sd.name.clone(), *sd.clone());
						},
						_ => ()
					};
				},
				None => break
			}
		};

		let main_func_call = FuncCallData {
			span: Span {
				scol: 0,
				srow: 0,
				ecol: 0,
				erow: 0,
			},
			path: vec![
				Box::new(
					PathPart::IdentifierPathPart(
						Box::new(
							IdentifierPathPartData {
								span: Span {
									scol: 0,
									srow: 0,
									ecol: 0,
									erow: 0,
								},
								identifier: "main".to_string() }
						)
					)
				)
			],
			arguments: vec![],
		};

		let mut context = InterpreterContext {
			vars: std::collections::HashMap::new(),
		};
		try!(self.execute_func_call(&mut context, &main_func_call));

		Ok(())
	}

	fn type_from_value(value: &Value, span: Span) -> Result<Type, String> {
		match *value {
			Value::Array(ref t, _) => match *t {
				Some(ref array_type) => Ok(Type::ArrayType(
					Box::new((*array_type).clone())
				)),
				None => Err(format!("Interpreter error: cannot deduce type for empty array, {:?}", span))
			},
			Value::Bool(_) => Ok(Type::BoolType),
			Value::Char(_) => Ok(Type::CharType),
			Value::String(_) => Ok(Type::StringType),
			Value::Integer(_) => Ok(Type::IntType),
			Value::Struct(ref t, _) => Ok((*t).clone()),
			Value::Reference(_, ref r) => Ok(Type::ReferenceType(Box::new(try!(Self::type_from_value(r, span))))),
		}
	}

	fn execute_import(&mut self, import_data: &ImportData) -> Result<(), String> { // TODO: rework that for more safety and non-naive handling
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

		self.imports.insert(import_data.path.clone(), Box::new((*ast).clone()));

		Ok(())
	}

	fn execute_block_statement(&'a self, context: *mut InterpreterContext<'a>, block_statement: &'a BlockStatement) -> Result<Option<Value>, String> {
		match *block_statement {
			BlockStatement::FuncCall(ref fc) => { try!(self.execute_func_call(context, fc)); Ok(None) },
			BlockStatement::VarDecl(ref vd) => { try!(self.execute_var_decl(context, vd)); Ok(None) },
			BlockStatement::VarAssignment(ref va) => { try!(self.execute_var_assignment(context, va)); Ok(None) },
			BlockStatement::If(ref i) => self.execute_if(context, i),
			BlockStatement::While(ref w) => self.execute_while(context, w),
			BlockStatement::ForIn(ref fi) => self.execute_forin(context, fi),
			BlockStatement::Return(ref r) => self.execute_return(context, r),
		}
	}

	fn execute_forin(&'a self, context: *mut InterpreterContext<'a>, forin_data: &'a ForInData) -> Result<Option<Value>, String> {
		let coll_value = try!(self.value_from_expression(context, &forin_data.collection));
		match coll_value {
			Value::Array(t, a) => {
				for elem in a {
					unsafe {
						(*context).vars.insert(
							forin_data.element_name.clone(),
							Variable {
								name: forin_data.element_name.clone(),
								var_type: t.clone().unwrap(),
								value: elem,
							}
						);
					}

					for statement in &forin_data.statements {
						if let Some(v) = try!(self.execute_block_statement(context, statement)) {
							unsafe {
								(*context).vars.remove(AsRef::<str>::as_ref(&forin_data.element_name[..]));
							}

							return Ok(Some(v))
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
						if let Some(v) = try!(self.execute_block_statement(context, statement)) {
							unsafe {
								(*context).vars.remove(AsRef::<str>::as_ref(&forin_data.element_name[..]));
							}

							return Ok(Some(v))
						}
					}

					unsafe {
						(*context).vars.remove(AsRef::<str>::as_ref(&forin_data.element_name[..]));
					}
				}
			},
			other => return Err(format!("Cannot iterate over {:?}, {:?}", other, Span::from_expression(&forin_data.collection))),
		};

		Ok(None)
	}

	fn execute_return(&'a self, context: *mut InterpreterContext<'a>, return_data: &ReturnData) -> Result<Option<Value>, String> {
		fn contains_local_ref(context: *mut InterpreterContext, e: &Expression) -> bool {
			match *e {
				Expression::StringLiteral(..) |
				Expression::IntegerLiteral(..) |
				Expression::BoolLiteral(..) |
				Expression::CharLiteral(..) |
				Expression::FuncCall(..) |
				Expression::Addition(..) |
				Expression::Substraction(..) |
				Expression::Multiplication(..) |
				Expression::Division(..) |
				Expression::Modulo(..) |
				Expression::Equality(..) |
				Expression::Inequality(..) |
				Expression::Concatenation(..) |
				Expression::Count(..) => false,

				Expression::Variable(ref v) => {
					match **v.path.iter().next().unwrap() {
						PathPart::IdentifierPathPart(ref ipp) => unsafe {
							match (*context).vars.get(AsRef::<str>::as_ref(&ipp.identifier[..])) {
								Some(ref var) => match var.value {
									Value::Reference(ref p, _) => {
										let fake_e = Expression::Variable(
											Box::new(
												VariableData {
													span: ipp.span.clone(),
													path: (*p).clone(),
												}
											)
										);
										contains_local_ref(context, &fake_e)
									},
									_ => false
								},
								None => false
							}
						},
						_ => false
					}
				},

				Expression::Reference(ref e, _) => {
					match **e {
						Expression::Variable(ref v) => {
							match **v.path.iter().next().unwrap() {
								PathPart::IdentifierPathPart(ref ipp) => unsafe {
									(*context).vars.get(AsRef::<str>::as_ref(&ipp.identifier[..])).is_some()
								},
								_ => false
							}
						},
						_ => false, // should never happen
					}
				},

				Expression::Dereference(ref e, _) => {
					match **e {
						ref v @ Expression::Variable(_) => contains_local_ref(context, v),
						Expression::FuncCall(_) => false,
						_ => false, // should never happen
					}
				}

				Expression::Array(ref a) => {
					for item in &a.items {
						if contains_local_ref(context, item) {
							return true
						}
					};

					false
				},

				Expression::StructInit(ref si) => {
					for field in &si.fields {
						if contains_local_ref(context, &field.value) {
							return true
						}
					};

					false
				},
			}
		}

		match return_data.expected_type {
			Some(ref t) => {
				let span: Span;
				let value = match return_data.value {
					Some(ref e) => {
						span = Span::from_expression(e);
						if contains_local_ref(context, e) {
							return Err(format!("Interpreter error: returning reference to local variable, {:?}", span))
						};
						try!(self.value_from_expression(context, e))
					},
					None => return Err(format!("Interpreter error: expected expression for return statement, {:?}", return_data.span)),
				};

				let value_type = try!(Self::type_from_value(&value, span.clone()));
				if value_type != *t {
					return Err(format!("Interpreter error: mismatched types in return statement (got {:?} expected {:?}), {:?}", value_type, *t, span))
				}

				Ok(Some(value))
			},
			None => {
				if let Some(_) = return_data.value {
					return Err(format!("Interpreter error: unexpected expression in return statement, {:?}", return_data.span))
				}

				Ok(None)
			}
		}
	}

	fn execute_func_call<'b>(&'b self, context: *mut InterpreterContext<'b>, func_call_data: &FuncCallData) -> Result<Option<Value>, String> {
		fn is_builtin_func(path: &Path, name: &str) -> bool {
			if path.len() == 1 {
				match **path.get(0).unwrap() {
					PathPart::IdentifierPathPart(ref ipp) => ipp.identifier == name,
					_ => false,
				}
			} else {
				false
			}
		}

		if is_builtin_func(&func_call_data.path, "println") {
			self.builtin_println(context, func_call_data)
		} else if is_builtin_func(&func_call_data.path, "readln") {
			self.builtin_readln(func_call_data)
		} else {
			let mut module_string = String::new();
			let mut last_id = String::new();
			for path_part in &func_call_data.path {
				match **path_part {
					PathPart::IdentifierPathPart(ref ipp) => {
						if last_id != "" {
							module_string.push_str(last_id.as_ref());
						};
						last_id = ipp.identifier.clone();
					},
					PathPart::ModulePathPart => {
						module_string.push_str("::");
					},
					_ => return Err(format!("Interpreter error: invalid function path {:?}", func_call_data.span))
				}
			};

			let func_decl = if module_string == "" {
				match self.funcs.get(&last_id) {
					Some(f) => f,
					None => return Err(format!("Interpreter error: unknown func, {:?}", func_call_data.span))
				}
			} else {
				let mut import_func_decl: Option<&FuncDeclData> = None;
				let import_statements = match self.imports.get(&module_string) {
					Some(ref m) => &m.statements,
					None => return Err(format!("Interpreter error: unknown module {:?}, {:?}", module_string, func_call_data.span)),
				};
				for statement in import_statements {
					match *statement {
						Statement::FuncDecl(ref fd) => {
							import_func_decl = Some(fd);
							break
						},
						_ => (),
					}
				}

				match import_func_decl {
					Some(f) => f,
					None => return Err(format!("Interpreter error: unknown func {:?}", func_call_data.span))
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
						if param_count < func_call_data.arguments.len() {
							let value = try!(self.value_from_expression(context, &func_call_data.arguments.get(param_count).unwrap().value));

							let value_type = try!(Self::type_from_value(&value, Span::from_expression(&func_call_data.arguments.get(param_count).unwrap().value)));

							if value_type != param.param_type {
								return Err(format!("Interpreter error: mismatched types in function call (got {:?} expected {:?}), {:?}", value_type, param.param_type, Span::from_expression(&func_call_data.arguments.get(param_count).unwrap().value)))
							} else {
								value
							}
						} else {
							if let Some(ref e) = param.default_value {
								let value = try!(self.value_from_expression(context, e));

								let value_type = try!(Self::type_from_value(&value, Span::from_expression(e)));

								if value_type != param.param_type {
									return Err(format!("Interpreter error: mismatched types in function declaration default value (got {:?} expected {:?}), {:?}", value_type, param.param_type, Span::from_expression(e)))
								} else {
									value
								}
							} else {
								return Err(format!("Interpreter error: expected argument for {:?}, {:?}", param.name, func_call_data.span))
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

			let mut return_value: Option<Value> = None;
			let mut local_vars: std::vec::Vec<String> = vec![];
			for statement in &func_decl.statements {
				if let BlockStatement::VarDecl(ref vd) = *statement {
					local_vars.push(vd.name.clone());
				};

				match try!(self.execute_block_statement(&mut local_context, statement)) {
					Some(v) => {
						return_value = Some(v);
						break
					},
					None => ()
				}
			}

			Ok(return_value)
		}
	}

	fn execute_var_decl(&'a self, context: *mut InterpreterContext<'a>, var_decl_data: &'a VarDeclData) -> Result<(), String> {
		let span: Span;
		let value = match var_decl_data.value {
			Some(ref v) => {
				span = Span::from_expression(v);
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

	fn execute_var_assignment(&'a self, context: *mut InterpreterContext<'a>, var_assignment_data: &VarAssignmentData) -> Result<(), String> {
		let value = try!(self.value_from_expression(context, &var_assignment_data.value));

		let mut path = var_assignment_data.path.iter();

		let mut current_ref = match **path.next().unwrap() {
			PathPart::IdentifierPathPart(ref ipp) => unsafe { match (*context).vars.get_mut(AsRef::<str>::as_ref(&ipp.identifier[..])) {
				Some(cr) => &mut cr.value,
				None => return Err(format!("Interpreter error: unknown variable, {:?}", ipp.span)),
			} },
			_ => return Err(format!("Interpreter error: incorrect var path, {:?}", var_assignment_data.span))
		};

		let mut is_field = false;
		for path_part in path {
			match **path_part {
				PathPart::FieldPathPart if !is_field => is_field = true,
				PathPart::IdentifierPathPart(ref ipp) if is_field => {
					let current_map = match *{current_ref} {
						Value::Struct(_, ref mut map) => map,
						_ => return Err(format!("Interpreter error: cannot access field on non-struct, {:?}", ipp.span))
					};

					current_ref = match current_map.get_mut(&ipp.identifier) {
						Some(cr) => cr,
						None => return Err(format!("Interpreter error: unknown struct field, {:?}", ipp.span)),
					};

					is_field = false;
				},
				PathPart::IndexPathPart(ref ipp) => {
					match ipp.index {
						Some(ref expr) => {
							let index_value = try!(self.value_from_expression(context, expr));
							match index_value {
								Value::Integer(i) => {
									let cell = match *{current_ref} {
										Value::Array(_, ref mut a) => match a.get_mut(i as usize) {
											Some(v) => v,
											None => return Err(format!("Interpreter error: index out of bounds, {:?}", Span::from_expression(expr))),
										},
										_ => return Err(format!("Interpreter error: can't access index on non-array, {:?}", ipp.span))
									};

									current_ref = cell;
								},
								_ => return Err(format!("Interpreter error: invalid index type for array, {:?}", Span::from_expression(expr)))
							}
						},
						None => {
							let new_cell = match *{current_ref} {
								Value::Array(ref t, ref mut a) => {
									a.push(
										match *t {
											Some(ref array_type) => try!(self.default_value((*array_type).clone(), ipp.span.clone())),
											None => return Err(format!("Interpreter error: cannot push to untyped array, {:?}", ipp.span)),
										}
									);

									let last_index = a.len() - 1;
									a.get_mut(last_index).unwrap()
								},
								_ => return Err(format!("Interpreter error: can't push to non-array, {:?}", ipp.span))
							};

							current_ref = new_cell;
						},
					}

					is_field = false;
				},
				_ => return Err(format!("Interpreter error: invalid path for var assignment, {:?}", var_assignment_data.span)),
			}
		};

		let value_type = Self::type_from_value(&value, Span::from_expression(&var_assignment_data.value));
		let current_type = Self::type_from_value(current_ref, var_assignment_data.span.clone());

		if value_type != current_type {
			return Err(format!("Interpreter error: mismatched types in var assignment (got {:?} expected {:?}), {:?}", value_type, current_type, Span::from_expression(&var_assignment_data.value)))
		} else {
			*current_ref = value;
		};

		Ok(())
	}

	fn execute_if(&'a self, context: *mut InterpreterContext<'a>, if_data: &'a IfData) -> Result<Option<Value>, String> {
		match try!(self.value_from_expression(context, &if_data.condition)) {
			Value::Bool(b) => {
				if b {
					for statement in &if_data.if_statements {
						if let Some(v) = try!(self.execute_block_statement(context, statement)) {
							return Ok(Some(v))
						}
					}
				} else if let Some(ref else_statements) = if_data.else_statements {
					for statement in else_statements {
						if let Some(v) = try!(self.execute_block_statement(context, statement)) {
							return Ok(Some(v))
						}
					}
				}
				Ok(None)
			},
			_ => Err(format!("Interpreter error: expected bool expression in if statement, {:?}", Span::from_expression(&if_data.condition)))
		}
	}

	fn execute_while(&'a self, context: *mut InterpreterContext<'a>, while_data: &'a WhileData) -> Result<Option<Value>, String> {
		loop {
			match try!(self.value_from_expression(context, &while_data.condition)) {
				Value::Bool(b) => {
					if b {
						for statement in &while_data.statements {
							if let Some(v) = try!(self.execute_block_statement(context, statement)) {
								return Ok(Some(v))
							}
						}
					} else {
						return Ok(None)
					}
				},
				_ => return Err(format!("Interpreter error: expected bool expression in while statement, {:?}", Span::from_expression(&while_data.condition)))
			}
		}
	}

	fn value_from_expression<'b>(&'b self, context: *mut InterpreterContext<'b>, expression: &Expression) -> Result<Value, String> {
		match *expression {
			Expression::StringLiteral(ref sl) => Ok(Value::String(sl.value.clone())),
			Expression::IntegerLiteral(ref il) => Ok(Value::Integer(il.value)),
			Expression::BoolLiteral(ref bl) => Ok(Value::Bool(bl.value)),
			Expression::CharLiteral(ref cl) => Ok(Value::Char(cl.value)),
			Expression::Variable(ref v) => {
				let mut path = v.path.iter();

				let mut current_ref = match **path.next().unwrap() {
					PathPart::IdentifierPathPart(ref ipp) =>
					unsafe { match (*context).vars.get(AsRef::<str>::as_ref(&ipp.identifier[..])) {
						Some(ref cr) => &cr.value,
						None => return Err(format!("Interpreter error: unknown variable, {:?}", ipp.span)),
					} },
					_ => return Err(format!("Interpreter error: incorrect var path, {:?}", v.span))
				};

				let mut is_field = false;
				for path_part in path {
					match **path_part {
						PathPart::FieldPathPart if !is_field => is_field = true,
						PathPart::IdentifierPathPart(ref ipp) if is_field => {
							let current_map = match *{current_ref} {
								Value::Struct(_, ref map) => map,
								_ => return Err(format!("Interpreter error: cannot access field on non-struct, {:?}", ipp.span))
							};

							current_ref = match current_map.get(&ipp.identifier) {
								Some(cr) => cr,
								None => return Err(format!("Interpreter error: unknown struct field, {:?}", ipp.span)),
							};

							is_field = false;
						},
						PathPart::IndexPathPart(ref ipp) => {
							match ipp.index {
								Some(ref expr) => {
									let index_value = try!(self.value_from_expression(context, expr));
									match index_value {
										Value::Integer(i) => {
											let cell = match *{current_ref} {
												Value::Array(_, ref a) => match a.get(i as usize) {
													Some(v) => v,
													None => return Err(format!("Interpreter error: index out of bounds, {:?}", Span::from_expression(expr))),
												},
												_ => return Err(format!("Interpreter error: can't access index on non-array, {:?}", ipp.span))
											};

											current_ref = cell;
										},
										_ => return Err(format!("Interpreter error: invalid index type for array, {:?}", Span::from_expression(expr)))
									}
								},
								None => return Err(format!("Interpreter error: array expression requires an index, {:?}", ipp.span))
							}

							is_field = false;
						},
						_ => return Err(format!("Interpreter error: invalid path for var assignment, {:?}", v.span)),
					}
				};

				Ok(current_ref.clone())
			},
			Expression::Array(ref a) => {
				let mut values: std::vec::Vec<Value> = vec![];
				let mut array_type: Option<Type> = None;
				for item in &a.items {
					let value = try!(self.value_from_expression(context, item));
					let value_type = try!(Self::type_from_value(&value, Span::from_expression(item)));
					match array_type {
						Some(ref t) => if value_type != *t {
							return Err(format!("Interpreter error: heterogeneous types in array literal, {:?}", a.span))
						},
						None => array_type = Some(value_type),
					};

					values.push(value)
				}

				Ok(Value::Array(array_type, values))
			},
			Expression::StructInit(ref si) => {
				let mut module_string = String::new();
				let mut last_id = String::new();
				for path_part in &si.path {
					match **path_part {
						PathPart::IdentifierPathPart(ref ipp) => {
							if last_id != "" {
								module_string.push_str(last_id.as_ref());
							};
							last_id = ipp.identifier.clone();
						},
						PathPart::ModulePathPart => {
							module_string.push_str("::");
						},
						_ => return Err(format!("Interpreter error: invalid function path, {:?}", si.span))
					}
				};

				let struct_decl = if module_string == "" {
					match self.structs.get(&last_id) {
						Some(s) => s,
						None => return Err(format!("Interpreter error: unknown struct, {:?}", si.span))
					}
				} else {
					let mut import_struct_decl: Option<&StructDeclData> = None;
					let import_statements = match self.imports.get(&module_string) {
						Some(ref m) => &m.statements,
						None => return Err(format!("Interpreter error: unknown module, {:?}", si.span)),
					};
					for statement in import_statements {
						match *statement {
							Statement::StructDecl(ref sd) => {
								import_struct_decl = Some(sd);
								break
							},
							_ => (),
						}
					}

					match import_struct_decl {
						Some(s) => s,
						None => return Err(format!("Interpreter error: unknown struct, {:?}", si.span))
					}
				};

				let mut new_content: std::collections::HashMap<String, Box<Value>> = std::collections::HashMap::new();

				for new_field in &si.fields {
					let mut found_field = false;

					for field in &struct_decl.fields {
						if field.name == new_field.name {
							found_field = true;
						}
					}

					if !found_field {
						return Err(format!("Interpreter error: unknown field {:?} in struct init, {:?}", new_field.name, new_field.span))
					}
				}

				for field in &struct_decl.fields {
					let mut found_field = false;

					for new_field in &si.fields {
						if field.name == new_field.name {
							let value = try!(self.value_from_expression(context, &new_field.value));
							let value_type = try!(Self::type_from_value(&value, Span::from_expression(&new_field.value)));

							if field.field_type != value_type {
								return Err(format!("Interpreter error: mismatched types in struct init (got {:?} expected {:?}), {:?}", value_type, field.field_type, Span::from_expression(&new_field.value)))
							} else {
								new_content.insert(field.name.clone(), Box::new(value));
							};

							found_field = true;
						}
					}

					if !found_field {
						return Err(format!("Interpreter error: missing field {:?} in struct init, {:?}", field.name, si.span))
					}
				}

				Ok(Value::Struct(
					Type::StructType(
						Box::new(
							StructTypeData { path: si.path.clone() }
						)
					),
					new_content,
				))
			}
			Expression::FuncCall(ref fc) => {
				match try!(self.execute_func_call(context, fc)) {
					Some(v) => Ok(v),
					None => return Err(format!("Interpreter error: func call in expression must return something, {:?}", fc.span)),
				}
			},
			Expression::Count(ref e, ref sp) => {
				match try!(self.value_from_expression(context, e)) {
					Value::Array(_, ref a) => Ok(Value::Integer(a.len() as i64)),
					Value::String(ref s) => Ok(Value::Integer(s.len() as i64)),
					_ => return Err(format!("Interpreter error: can't get count on non-(array/string), {:?}", *sp))
				}
			},
			Expression::Reference(ref e, ref s) => {
				match **e {
					Expression::Variable(ref v) => {
						let mut path = v.path.iter();

						let mut current_ref = match **path.next().unwrap() {
							PathPart::IdentifierPathPart(ref ipp) =>
							unsafe { match (*context).vars.get(AsRef::<str>::as_ref(&ipp.identifier[..])) {
								Some(ref cr) => &cr.value,
								None => return Err(format!("Interpreter error: unknown variable, {:?}", ipp.span)),
							} },
							_ => return Err(format!("Interpreter error: incorrect var path, {:?}", v.span))
						};

						let mut is_field = false;
						for path_part in path {
							match **path_part {
								PathPart::FieldPathPart if !is_field => is_field = true,
								PathPart::IdentifierPathPart(ref ipp) if is_field => {
									let current_map = match *{current_ref} {
										Value::Struct(_, ref map) => map,
										_ => return Err(format!("Interpreter error: cannot access field on non-struct, {:?}", ipp.span))
									};

									current_ref = match current_map.get(&ipp.identifier) {
										Some(cr) => cr,
										None => return Err(format!("Interpreter error: unknown struct field, {:?}", ipp.span)),
									};

									is_field = false;
								},
								PathPart::IndexPathPart(ref ipp) => {
									match ipp.index {
										Some(ref expr) => {
											let index_value = try!(self.value_from_expression(context, expr));
											match index_value {
												Value::Integer(i) => {
													let cell = match *{current_ref} {
														Value::Array(_, ref a) => match a.get(i as usize) {
															Some(v) => v,
															None => return Err(format!("Interpreter error: index out of bounds, {:?}", Span::from_expression(expr))),
														},
														_ => return Err(format!("Interpreter error: can't access index on non-array, {:?}", ipp.span))
													};

													current_ref = cell;
												},
												_ => return Err(format!("Interpreter error: invalid index type for array, {:?}", Span::from_expression(expr)))
											}
										},
										None => return Err(format!("Interpreter error: array expression requires an index, {:?}", ipp.span))
									}

									is_field = false;
								},
								_ => return Err(format!("Interpreter error: invalid path for reference, {:?}", v.span)),
							}
						};

						Ok(Value::Reference(v.path.clone(), current_ref))
					},
					_ => return Err(format!("Interpreter error: can't reference non(variable/field), {:?}", *s))
				}
			},
			Expression::Dereference(ref e, ref s) => {
				let deref_value = match try!(self.value_from_expression(context, e)) {
					Value::Reference(_, v) => (*v).clone(),
					_ => return Err(format!("Interpreter error: cannot dereference non-reference, {:?}", (*s).clone()))
				};

				Ok(deref_value)
			},
			Expression::Addition(ref e1, ref e2) => {
				let integer1 = match try!(self.value_from_expression(context, e1)) {
					Value::Integer(i) => i,
					other => return Err(format!("Interpreter error: incorrect expression for addition: {:?}, {:?}", other, Span::from_expression(e1)))
				};

				let integer2 = match try!(self.value_from_expression(context, e2)) {
					Value::Integer(i) => i,
					other => return Err(format!("Interpreter error: incorrect expression for addition: {:?}, {:?}", other, Span::from_expression(e2)))
				};

				Ok(Value::Integer(integer1 + integer2))
			},
			Expression::Substraction(ref e1, ref e2) => {
				let integer1 = match try!(self.value_from_expression(context, e1)) {
					Value::Integer(i) => i,
					other => return Err(format!("Interpreter error: incorrect expression for substraction: {:?}, {:?}", other, Span::from_expression(e1)))
				};

				let integer2 = match try!(self.value_from_expression(context, e2)) {
					Value::Integer(i) => i,
					other => return Err(format!("Interpreter error: incorrect expression for substraction: {:?}, {:?}", other, Span::from_expression(e2)))
				};

				Ok(Value::Integer(integer1 - integer2))
			},
			Expression::Multiplication(ref e1, ref e2) => {
				let integer1 = match try!(self.value_from_expression(context, e1)) {
					Value::Integer(i) => i,
					other => return Err(format!("Interpreter error: incorrect expression for multiplication: {:?}, {:?}", other, Span::from_expression(e1)))
				};

				let integer2 = match try!(self.value_from_expression(context, e2)) {
					Value::Integer(i) => i,
					other => return Err(format!("Interpreter error: incorrect expression for multiplication: {:?}, {:?}", other, Span::from_expression(e2)))
				};

				Ok(Value::Integer(integer1 * integer2))
			},
			Expression::Division(ref e1, ref e2) => {
				let integer1 = match try!(self.value_from_expression(context, e1)) {
					Value::Integer(i) => i,
					other => return Err(format!("Interpreter error: incorrect expression for division: {:?}, {:?}", other, Span::from_expression(e1)))
				};

				let integer2 = match try!(self.value_from_expression(context, e2)) {
					Value::Integer(i) => i,
					other => return Err(format!("Interpreter error: incorrect expression for division: {:?}, {:?}", other, Span::from_expression(e2)))
				};

				Ok(Value::Integer(integer1 / integer2))
			},
			Expression::Modulo(ref e1, ref e2) => {
				let integer1 = match try!(self.value_from_expression(context, e1)) {
					Value::Integer(i) => i,
					other => return Err(format!("Interpreter error: incorrect expression for modulo: {:?}, {:?}", other, Span::from_expression(e1)))
				};

				let integer2 = match try!(self.value_from_expression(context, e2)) {
					Value::Integer(i) => i,
					other => return Err(format!("Interpreter error: incorrect expression for modulo: {:?}, {:?}", other, Span::from_expression(e2)))
				};

				Ok(Value::Integer(integer1 % integer2))
			},
			Expression::Equality(ref e1, ref e2) => {
				let value1 = self.value_from_expression(context, e1);
				let value2 = self.value_from_expression(context, e2);

				Ok(Value::Bool(value1 == value2))
			},
			Expression::Inequality(ref e1, ref e2) => {
				let value1 = self.value_from_expression(context, e1);
				let value2 = self.value_from_expression(context, e2);

				Ok(Value::Bool(value1 != value2))
			},
			Expression::Concatenation(ref e1, ref e2) => {
				let string1 = match try!(self.value_from_expression(context, e1)) {
					Value::String(s) => s,
					other => return Err(format!("Interpreter error: incorrect expression for concatenation: {:?}, {:?}", other, Span::from_expression(e1)))
				};

				let string2 = match try!(self.value_from_expression(context, e2)) {
					Value::String(s) => s,
					other => return Err(format!("Interpreter error: incorrect expression for concatenation: {:?}, {:?}", other, Span::from_expression(e2)))
				};

				let mut new_string = String::new();
				new_string.push_str(string1.as_ref());
				new_string.push_str(string2.as_ref());
				Ok(Value::String(new_string))
			},
		}
	}

	fn builtin_println(&'a self, context: *mut InterpreterContext<'a>, func_call_data: &FuncCallData) -> Result<Option<Value>, String> {
		if func_call_data.arguments.len() != 1 {
			return Err(format!("Interpreter error: invalid argument count for println, {:?}", func_call_data.span))
		};

		match try!(self.value_from_expression(context, &func_call_data.arguments[0].value)) {
			Value::String(s) => println!("{}", s),
			Value::Integer(i) => println!("{}", i),
			Value::Bool(b) => println!("{}", b),
			Value::Char(c) => println!("{}", c),
			Value::Struct(_, s) => println!("{:?}", s),
			Value::Array(_, a) => println!("{:?}", a),
			Value::Reference(p, _) => println!("{:?}", p),
		};

		Ok(None)
	}

	fn builtin_readln(&self, func_call_data: &FuncCallData) -> Result<Option<Value>, String> {
		if func_call_data.arguments.len() != 0 {
			return Err(format!("Interpreter error: invalid argument count for readln, {:?}", func_call_data.span))
		};

		let mut line = String::new();
	    let stdin = io::stdin();
	    stdin.lock().read_line(&mut line).unwrap();
    	Ok(Some(
			Value::String(
				line
			)
		))
	}

	fn default_value(&self, var_type: Type, span: Span) -> Result<Value, String> {
		match var_type {
			Type::StringType => Ok(Value::String("".to_string())),
			Type::IntType => Ok(Value::Integer(0)),
			Type::BoolType => Ok(Value::Bool(false)),
			Type::CharType => Ok(Value::Char('\0')),
			Type::ArrayType(t) => Ok(Value::Array(Some(*t), vec![])),
			Type::StructType(ref s) => {
				let mut module_string = String::new();
				let mut last_id = String::new();
				for path_part in &s.path {
					match **path_part {
						PathPart::IdentifierPathPart(ref ipp) => {
							if last_id != "" {
								module_string.push_str(last_id.as_ref());
							};
							last_id = ipp.identifier.clone();
						},
						PathPart::ModulePathPart => {
							module_string.push_str("::");
						},
						_ => return Err(format!("Interpreter error: invalid function path, {:?}", span))
					}
				};

				let struct_decl = if module_string == "" {
					match self.structs.get(&last_id) {
						Some(s) => s,
						None => return Err(format!("Interpreter error: unknown struct, {:?}", span))
					}
				} else {
					let mut import_struct_decl: Option<&StructDeclData> = None;
					let import_statements = match self.imports.get(&module_string) {
						Some(ref m) => &m.statements,
						None => return Err(format!("Interpreter error: unknown module, {:?}", span)),
					};
					for statement in import_statements {
						match *statement {
							Statement::StructDecl(ref sd) => {
								import_struct_decl = Some(sd);
								break
							},
							_ => (),
						}
					}

					match import_struct_decl {
						Some(s) => s,
						None => return Err(format!("Interpreter error: unknown struct, {:?}", span))
					}
				};

				let mut fields: std::collections::HashMap<String, Box<Value>> = std::collections::HashMap::new();

				for field in &struct_decl.fields {
					fields.insert(field.name.clone(), Box::new(try!(self.default_value(field.field_type.clone(), field.span.clone()))));
				}

				Ok(Value::Struct(var_type.clone(), fields))
			},
			Type::ReferenceType(_) => Err(format!("Interpreter error: no default value for references, {:?}", span))
		}
	}
}
