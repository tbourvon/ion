use parser::ast::*;
use std;
use lexer;
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

type InterpreterVars = std::collections::HashMap<String, Variable>; // TODO: Bloody get rid of Strings for indexing... preferably Path indexing would be best

#[derive(Debug)]
pub struct Variable {
	name: String,
	var_type: Type,
	value: Value,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
	String(String),
	Integer(i64),
	Bool(bool),
	Char(char),
	Struct(Type, std::collections::HashMap<String, Box<Value>>),
	Array(Option<Type>, std::vec::Vec<Value>),
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
			path: vec![
				Box::new(
					PathPart::IdentifierPathPart(
						Box::new(
							IdentifierPathPartData { identifier: "main".to_string() }
						)
					)
				)
			],
			arguments: vec![],
		};

		let mut vars = std::collections::HashMap::new();
		try!(self.execute_func_call(&mut vars, &main_func_call));

		Ok(())
	}

	fn type_from_value(value: Value) -> Result<Type, String> {
		match value {
			Value::Array(t, _) => match t {
				Some(array_type) => Ok(Type::ArrayType(
					Box::new(array_type)
				)),
				None => Err("Interpreter error: cannot deduce type for empty array".to_string())
			},
			Value::Bool(_) => Ok(Type::BoolType),
			Value::Char(_) => Ok(Type::CharType),
			Value::String(_) => Ok(Type::StringType),
			Value::Integer(_) => Ok(Type::IntType),
			Value::Struct(t, _) => Ok(t),
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

	fn execute_block_statement(&self, vars: *mut InterpreterVars, block_statement: &'a BlockStatement) -> Result<Option<Value>, String> {
		match *block_statement {
			BlockStatement::FuncCall(ref fc) => { try!(self.execute_func_call(vars, fc)); Ok(None) },
			BlockStatement::VarDecl(ref vd) => { try!(self.execute_var_decl(vars, vd)); Ok(None) },
			BlockStatement::VarAssignment(ref va) => { try!(self.execute_var_assignment(vars, va)); Ok(None) },
			BlockStatement::If(ref i) => self.execute_if(vars, i),
			BlockStatement::While(ref w) => self.execute_while(vars, w),
			BlockStatement::ForIn(ref fi) => self.execute_forin(vars, fi),
			BlockStatement::Return(ref r) => self.execute_return(vars, r),
		}
	}

	fn execute_forin(&self, vars: *mut InterpreterVars, forin_data: &ForInData) -> Result<Option<Value>, String> {
		let coll_value = try!(self.value_from_expression(vars, &forin_data.collection));
		match coll_value {
			Value::Array(t, a) => {
				for elem in a {
					unsafe {
						(*vars).insert(
							forin_data.element_name.clone(),
							Variable {
								name: forin_data.element_name.clone(),
								var_type: t.clone().unwrap(),
								value: elem,
							}
						);
					}

					for statement in &forin_data.statements {
						if let Some(v) = try!(self.execute_block_statement(vars, statement)) {
							unsafe {
								(*vars).remove(AsRef::<str>::as_ref(&forin_data.element_name[..]));
							}

							return Ok(Some(v))
						}
					}

					unsafe {
						(*vars).remove(AsRef::<str>::as_ref(&forin_data.element_name[..]));
					}
				}
			},
			Value::String(s) => {
				for c in s.chars() {
					unsafe {
						(*vars).insert(
							forin_data.element_name.clone(),
							Variable {
								name: forin_data.element_name.clone(),
								var_type: Type::CharType,
								value: Value::Char(c),
							}
						);
					}

					for statement in &forin_data.statements {
						if let Some(v) = try!(self.execute_block_statement(vars, statement)) {
							unsafe {
								(*vars).remove(AsRef::<str>::as_ref(&forin_data.element_name[..]));
							}

							return Ok(Some(v))
						}
					}

					unsafe {
						(*vars).remove(AsRef::<str>::as_ref(&forin_data.element_name[..]));
					}
				}
			},
			other => return Err(format!("Cannot iterate over {:?}", other)),
		};

		Ok(None)
	}

	fn execute_return(&self, vars: *mut InterpreterVars, return_data: &ReturnData) -> Result<Option<Value>, String> {
		match return_data.expected_type {
			Some(ref t) => {
				let value = match return_data.value {
					Some(ref e) => try!(self.value_from_expression(vars, e)),
					None => return Err("Interpreter error: expected expression for return statement".to_string()),
				};

				let value_type = try!(Self::type_from_value(value.clone()));
				if value_type != *t {
					return Err(format!("Interpreter error: mismatched types in return statement (got {:?} expected {:?})", value_type, *t))
				}

				Ok(Some(value))
			},
			None => {
				if let Some(_) = return_data.value {
					return Err("Interpreter error: unexpected expression in return statement".to_string())
				}

				Ok(None)
			}
		}
	}

	fn execute_func_call(&self, vars: *mut InterpreterVars, func_call_data: &FuncCallData) -> Result<Option<Value>, String> {
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
			self.builtin_println(vars, func_call_data)
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
					_ => return Err("Interpreter error: invalid function path".to_string())
				}
			};

			let func_decl = if module_string == "" {
				match self.funcs.get(&last_id) {
					Some(f) => f,
					None => return Err("Interpreter error: unknown func".to_string())
				}
			} else {
				let mut import_func_decl: Option<&FuncDeclData> = None;
				let import_statements = match self.imports.get(&module_string) {
					Some(ref m) => &m.statements,
					None => return Err("Interpreter error: unknown module".to_string()),
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
					None => return Err("Interpreter error: unknown func".to_string())
				}
			};

			let mut param_count = 0;
			for param in &func_decl.parameters {
				let variable = Variable {
					name: param.name.clone(),
					var_type: param.param_type.clone(),
					value: {
						if param_count < func_call_data.arguments.len() {
							let value = try!(self.value_from_expression(vars, &func_call_data.arguments.get(param_count).unwrap().value));

							let value_type = try!(Self::type_from_value(value.clone()));

							if value_type != param.param_type {
								return Err(format!("Interpreter error: mismatched types in function call (got {:?} expected {:?})", value_type, param.param_type))
							} else {
								value
							}
						} else {
							if let Some(ref e) = param.default_value {
								let value = try!(self.value_from_expression(vars, e));

								let value_type = try!(Self::type_from_value(value.clone()));

								if value_type != param.param_type {
									return Err(format!("Interpreter error: mismatched types in function declaration default value (got {:?} expected {:?})", value_type, param.param_type))
								} else {
									value
								}
							} else {
								return Err(format!("Interpreter error: expected argument for {:?}", param.name))
							}
						}
					}
				};

				unsafe {
					(*vars).insert(
						param.name.clone(),
						variable
					);
				}

				param_count += 1;
			}

			let mut return_value: Option<Value> = None;
			let mut local_vars: std::vec::Vec<String> = vec![];
			for statement in &func_decl.statements {
				if let BlockStatement::VarDecl(ref vd) = *statement {
					local_vars.push(vd.name.clone());
				};

				match try!(self.execute_block_statement(vars, statement)) {
					Some(v) => {
						return_value = Some(v);
						break
					},
					None => ()
				}
			}

			for local_var in &local_vars {
				unsafe {
					(*vars).remove(AsRef::<str>::as_ref(&local_var[..]));
				}
			}

			for param in &func_decl.parameters {
				unsafe {
					(*vars).remove(AsRef::<str>::as_ref(&param.name[..]));
				}
			}

			Ok(return_value)
		}
	}

	fn execute_var_decl(&self, vars: *mut InterpreterVars, var_decl_data: &'a VarDeclData) -> Result<(), String> {
		let value = match var_decl_data.value {
			Some(ref v) => try!(self.value_from_expression(vars, v)),
			None => try!(self.default_value(var_decl_data.var_type.clone())),
		};

		unsafe {
			(*vars).insert(
				var_decl_data.name.clone(),
				Variable {
					name: var_decl_data.name.clone(),
					var_type: var_decl_data.var_type.clone(),
					value: {
						let value_type = try!(Self::type_from_value(value.clone()));

						if value_type != var_decl_data.var_type {
							return Err(format!("Interpreter error: mismatched types in var initialization (got {:?} expected {:?})", value_type, var_decl_data.var_type))
						} else {
							value
						}
					},
				}
			);
		};

		Ok(())
	}

	fn execute_var_assignment(&self, vars: *mut InterpreterVars, var_assignment_data: &VarAssignmentData) -> Result<(), String> {
		let value = try!(self.value_from_expression(vars, &var_assignment_data.value));

		let mut path = var_assignment_data.path.iter();

		let mut current_ref = match **path.next().unwrap() {
			PathPart::IdentifierPathPart(ref ipp) => unsafe { match (*vars).get_mut(AsRef::<str>::as_ref(&ipp.identifier[..])) {
				Some(cr) => &mut cr.value,
				None => return Err("Interpreter error: unknown variable".to_string()),
			} },
			_ => return Err("Interpreter error: incorrect var path".to_string())
		};

		let mut is_field = false;
		for path_part in path {
			match **path_part {
				PathPart::FieldPathPart if !is_field => is_field = true,
				PathPart::IdentifierPathPart(ref ipp) if is_field => {
					let current_map = match *{current_ref} {
						Value::Struct(_, ref mut map) => map,
						_ => return Err("Interpreter error: cannot access field on non-struct".to_string())
					};

					current_ref = match current_map.get_mut(&ipp.identifier) {
						Some(cr) => cr,
						None => return Err("Interpreter error: unknown struct field".to_string()),
					};

					is_field = false;
				},
				PathPart::IndexPathPart(ref ipp) => {
					match ipp.index {
						Some(ref expr) => {
							let index_value = try!(self.value_from_expression(vars, expr));
							match index_value {
								Value::Integer(i) => {
									let cell = match *{current_ref} {
										Value::Array(_, ref mut a) => match a.get_mut(i as usize) {
											Some(v) => v,
											None => return Err("Interpreter error: index out of bounds".to_string()),
										},
										_ => return Err("Interpreter error: can't access index on non-array".to_string())
									};

									current_ref = cell;
								},
								_ => return Err("Interpreter error: invalid index type for array".to_string())
							}
						},
						None => {
							let new_cell = match *{current_ref} {
								Value::Array(ref t, ref mut a) => {
									a.push(
										match *t {
											Some(ref array_type) => try!(self.default_value((*array_type).clone())),
											None => return Err("Interpreter error: cannot push to untyped array".to_string()),
										}
									);

									let last_index = a.len() - 1;
									a.get_mut(last_index).unwrap()
								},
								_ => return Err("Interpreter error: can't push to non-array".to_string())
							};

							current_ref = new_cell;
						},
					}

					is_field = false;
				},
				_ => return Err("Interpreter error: invalid path for var assignment".to_string()),
			}
		};

		let value_type = Self::type_from_value(value.clone());
		let current_type = Self::type_from_value((*current_ref).clone());

		if value_type != current_type {
			return Err(format!("Interpreter error: mismatched types in var assignment (got {:?} expected {:?})", value_type, current_type))
		} else {
			*current_ref = value;
		};

		Ok(())
	}

	fn execute_if(&self, vars: *mut InterpreterVars, if_data: &'a IfData) -> Result<Option<Value>, String> {
		match try!(self.value_from_expression(vars, &if_data.condition)) {
			Value::Bool(b) => {
				if b {
					for statement in &if_data.if_statements {
						if let Some(v) = try!(self.execute_block_statement(vars, statement)) {
							return Ok(Some(v))
						}
					}
				} else if let Some(ref else_statements) = if_data.else_statements {
					for statement in else_statements {
						if let Some(v) = try!(self.execute_block_statement(vars, statement)) {
							return Ok(Some(v))
						}
					}
				}
				Ok(None)
			},
			_ => Err("Interpreter error: expected bool expression in if statement".to_string())
		}
	}

	fn execute_while(&self, vars: *mut InterpreterVars, while_data: &'a WhileData) -> Result<Option<Value>, String> {
		loop {
			match try!(self.value_from_expression(vars, &while_data.condition)) {
				Value::Bool(b) => {
					if b {
						for statement in &while_data.statements {
							if let Some(v) = try!(self.execute_block_statement(vars, statement)) {
								return Ok(Some(v))
							}
						}
					} else {
						return Ok(None)
					}
				},
				_ => return Err("Interpreter error: expected bool expression in while statement".to_string())
			}
		}
	}

	fn value_from_expression(&self, vars: *mut InterpreterVars, expression: &Expression) -> Result<Value, String> {
		match *expression {
			Expression::StringLiteral(ref sl) => Ok(Value::String(sl.value.clone())),
			Expression::IntegerLiteral(ref il) => Ok(Value::Integer(il.value)),
			Expression::BoolLiteral(ref bl) => Ok(Value::Bool(bl.value)),
			Expression::CharLiteral(ref cl) => Ok(Value::Char(cl.value)),
			Expression::Variable(ref v) => {
				let mut path = v.path.iter();

				let mut current_ref = match **path.next().unwrap() {
					PathPart::IdentifierPathPart(ref ipp) =>
					unsafe { match (*vars).get(AsRef::<str>::as_ref(&ipp.identifier[..])) {
						Some(ref cr) => &cr.value,
						None => return Err("Interpreter error: unknown variable".to_string()),
					} },
					_ => return Err("Interpreter error: incorrect var path".to_string())
				};

				let mut is_field = false;
				for path_part in path {
					match **path_part {
						PathPart::FieldPathPart if !is_field => is_field = true,
						PathPart::IdentifierPathPart(ref ipp) if is_field => {
							let current_map = match *{current_ref} {
								Value::Struct(_, ref map) => map,
								_ => return Err("Interpreter error: cannot access field on non-struct".to_string())
							};

							current_ref = match current_map.get(&ipp.identifier) {
								Some(cr) => cr,
								None => return Err("Interpreter error: unknown struct field".to_string()),
							};

							is_field = false;
						},
						PathPart::IndexPathPart(ref ipp) => {
							match ipp.index {
								Some(ref expr) => {
									let index_value = try!(self.value_from_expression(vars, expr));
									match index_value {
										Value::Integer(i) => {
											let cell = match *{current_ref} {
												Value::Array(_, ref a) => match a.get(i as usize) {
													Some(v) => v,
													None => return Err("Interpreter error: index out of bounds".to_string()),
												},
												_ => return Err("Interpreter error: can't access index on non-array".to_string())
											};

											current_ref = cell;
										},
										_ => return Err("Interpreter error: invalid index type for array".to_string())
									}
								},
								None => return Err("Interpreter error: arrray expression requires an index".to_string())
							}

							is_field = false;
						},
						_ => return Err("Interpreter error: invalid path for var assignment".to_string()),
					}
				};

				Ok(current_ref.clone())
			},
			Expression::Array(ref a) => {
				let mut values: std::vec::Vec<Value> = vec![];
				let mut array_type: Option<Type> = None;
				for item in &a.items {
					let value = try!(self.value_from_expression(vars, item));
					let value_type = try!(Self::type_from_value(value.clone()));
					match array_type {
						Some(ref t) => if value_type != *t {
							return Err("Interpreter error: heterogeneous types in array literal".to_string())
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
						_ => return Err("Interpreter error: invalid function path".to_string())
					}
				};

				let struct_decl = if module_string == "" {
					match self.structs.get(&last_id) {
						Some(s) => s,
						None => return Err("Interpreter error: unknown struct".to_string())
					}
				} else {
					let mut import_struct_decl: Option<&StructDeclData> = None;
					let import_statements = match self.imports.get(&module_string) {
						Some(ref m) => &m.statements,
						None => return Err("Interpreter error: unknown module".to_string()),
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
						None => return Err("Interpreter error: unknown struct".to_string())
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
						return Err(format!("Interpreter error: unknown field {:?} in struct init", new_field.name))
					}
				}

				for field in &struct_decl.fields {
					let mut found_field = false;

					for new_field in &si.fields {
						if field.name == new_field.name {
							let value = try!(self.value_from_expression(vars, &new_field.value));
							let value_type = try!(Self::type_from_value(value.clone()));

							if field.field_type != value_type {
								return Err(format!("Interpreter error: mismatched types in struct init (got {:?} expected {:?})", value_type, field.field_type))
							} else {
								new_content.insert(field.name.clone(), Box::new(value));
							};

							found_field = true;
						}
					}

					if !found_field {
						return Err(format!("Interpreter error: missing field {:?} in struct init", field.name))
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
				match try!(self.execute_func_call(vars, fc)) {
					Some(v) => Ok(v),
					None => return Err("Interpreter error: func call in expression must return something".to_string()),
				}
			},
			Expression::Count(ref e) => {
				match try!(self.value_from_expression(vars, e)) {
					Value::Array(_, ref a) => Ok(Value::Integer(a.len() as i64)),
					Value::String(ref s) => Ok(Value::Integer(s.len() as i64)),
					_ => return Err("Interpreter error: can't get count on non-(array/string)".to_string())
				}
			},
			Expression::Addition(ref e1, ref e2) => {
				let integer1 = match try!(self.value_from_expression(vars, e1)) {
					Value::Integer(i) => i,
					other => return Err(format!("Interpreter error: incorrect expression for addition: {:?}", other))
				};

				let integer2 = match try!(self.value_from_expression(vars, e2)) {
					Value::Integer(i) => i,
					other => return Err(format!("Interpreter error: incorrect expression for addition: {:?}", other))
				};

				Ok(Value::Integer(integer1 + integer2))
			},
			Expression::Substraction(ref e1, ref e2) => {
				let integer1 = match try!(self.value_from_expression(vars, e1)) {
					Value::Integer(i) => i,
					other => return Err(format!("Interpreter error: incorrect expression for substraction: {:?}", other))
				};

				let integer2 = match try!(self.value_from_expression(vars, e2)) {
					Value::Integer(i) => i,
					other => return Err(format!("Interpreter error: incorrect expression for substraction: {:?}", other))
				};

				Ok(Value::Integer(integer1 - integer2))
			},
			Expression::Multiplication(ref e1, ref e2) => {
				let integer1 = match try!(self.value_from_expression(vars, e1)) {
					Value::Integer(i) => i,
					other => return Err(format!("Interpreter error: incorrect expression for multiplication: {:?}", other))
				};

				let integer2 = match try!(self.value_from_expression(vars, e2)) {
					Value::Integer(i) => i,
					other => return Err(format!("Interpreter error: incorrect expression for multiplication: {:?}", other))
				};

				Ok(Value::Integer(integer1 * integer2))
			},
			Expression::Division(ref e1, ref e2) => {
				let integer1 = match try!(self.value_from_expression(vars, e1)) {
					Value::Integer(i) => i,
					other => return Err(format!("Interpreter error: incorrect expression for division: {:?}", other))
				};

				let integer2 = match try!(self.value_from_expression(vars, e2)) {
					Value::Integer(i) => i,
					other => return Err(format!("Interpreter error: incorrect expression for division: {:?}", other))
				};

				Ok(Value::Integer(integer1 / integer2))
			},
			Expression::Modulo(ref e1, ref e2) => {
				let integer1 = match try!(self.value_from_expression(vars, e1)) {
					Value::Integer(i) => i,
					other => return Err(format!("Interpreter error: incorrect expression for modulo: {:?}", other))
				};

				let integer2 = match try!(self.value_from_expression(vars, e2)) {
					Value::Integer(i) => i,
					other => return Err(format!("Interpreter error: incorrect expression for modulo: {:?}", other))
				};

				Ok(Value::Integer(integer1 % integer2))
			},
			Expression::Equality(ref e1, ref e2) => {
				let value1 = self.value_from_expression(vars, e1);
				let value2 = self.value_from_expression(vars, e2);

				Ok(Value::Bool(value1 == value2))
			},
			Expression::Inequality(ref e1, ref e2) => {
				let value1 = self.value_from_expression(vars, e1);
				let value2 = self.value_from_expression(vars, e2);

				Ok(Value::Bool(value1 != value2))
			},
			Expression::Concatenation(ref e1, ref e2) => {
				let string1 = match try!(self.value_from_expression(vars, e1)) {
					Value::String(s) => s,
					other => return Err(format!("Interpreter error: incorrect expression for concatenation: {:?}", other))
				};

				let string2 = match try!(self.value_from_expression(vars, e2)) {
					Value::String(s) => s,
					other => return Err(format!("Interpreter error: incorrect expression for concatenation: {:?}", other))
				};

				let mut new_string = String::new();
				new_string.push_str(string1.as_ref());
				new_string.push_str(string2.as_ref());
				Ok(Value::String(new_string))
			},
		}
	}

	fn builtin_println(&self, vars: *mut InterpreterVars, func_call_data: &FuncCallData) -> Result<Option<Value>, String> {
		if func_call_data.arguments.len() != 1 {
			return Err("Interpreter error: invalid argument count for println".to_string())
		};

		match try!(self.value_from_expression(vars, &func_call_data.arguments[0].value)) {
			Value::String(s) => println!("{}", s),
			Value::Integer(i) => println!("{}", i),
			Value::Bool(b) => println!("{}", b),
			Value::Char(c) => println!("{}", c),
			Value::Struct(_, s) => println!("{:?}", s),
			Value::Array(_, a) => println!("{:?}", a),
		};

		Ok(None)
	}

	fn builtin_readln(&self, func_call_data: &FuncCallData) -> Result<Option<Value>, String> {
		if func_call_data.arguments.len() != 0 {
			return Err("Interpreter error: invalid argument count for readln".to_string())
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

	fn default_value(&self, var_type: Type) -> Result<Value, String> {
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
						_ => return Err("Interpreter error: invalid function path".to_string())
					}
				};

				let struct_decl = if module_string == "" {
					match self.structs.get(&last_id) {
						Some(s) => s,
						None => return Err("Interpreter error: unknown struct".to_string())
					}
				} else {
					let mut import_struct_decl: Option<&StructDeclData> = None;
					let import_statements = match self.imports.get(&module_string) {
						Some(ref m) => &m.statements,
						None => return Err("Interpreter error: unknown module".to_string()),
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
						None => return Err("Interpreter error: unknown struct".to_string())
					}
				};

				let mut fields: std::collections::HashMap<String, Box<Value>> = std::collections::HashMap::new();

				for field in &struct_decl.fields {
					fields.insert(field.name.clone(), Box::new(try!(self.default_value(field.field_type.clone()))));
				}

				Ok(Value::Struct(var_type.clone(), fields))
			},
		}
	}
}
