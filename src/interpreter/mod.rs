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

	pub fn execute(&mut self) {
		let mut iter = self.ast.statements.iter();
		loop { let statement = iter.next();
			match statement {
				Some(s) => {
					match *s {
						Statement::FuncDecl(ref fd) => {
							self.funcs.insert(fd.name.clone(), *fd.clone());
						},
						Statement::Import(ref i) => {
							self.execute_import(i);
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
		self.execute_func_call(&mut vars, &main_func_call);
	}

	fn type_from_value(value: Value) -> Type {
		match value {
			Value::Array(t, _) => match t {
				Some(array_type) => Type::ArrayType(
					Box::new(array_type)
				),
				None => panic!("Interpreter error: cannot deduce type for empty array")
			},
			Value::Bool(_) => Type::BoolType,
			Value::Char(_) => Type::CharType,
			Value::String(_) => Type::StringType,
			Value::Integer(_) => Type::IntType,
			Value::Struct(t, _) => t,
		}
	}

	fn execute_import(&mut self, import_data: &ImportData) { // TODO: rework that for more safety and non-naive handling
		let path_string = import_data.path.clone() + ".ion";
	    let path = std::path::Path::new(AsRef::<str>::as_ref(&path_string[..]));
	    let mut file = match File::open(&path) {
	        Ok(file) => file,
	        Err(_) => panic!(),
	    };

	    let mut s = String::new();
	    file.read_to_string(&mut s);
	    let mut reader = lexer::Reader::new(s.as_ref());

	    let mut parser = parser::Parser::new(&mut reader);
	    let ast = parser.parse();

		self.imports.insert(import_data.path.clone(), Box::new((*ast).clone()));
	}

	fn execute_block_statement(&self, vars: *mut InterpreterVars, block_statement: &'a BlockStatement) -> Option<Value> {
		match *block_statement {
			BlockStatement::FuncCall(ref fc) => { self.execute_func_call(vars, fc); None },
			BlockStatement::VarDecl(ref vd) => { self.execute_var_decl(vars, vd); None },
			BlockStatement::VarAssignment(ref va) => { self.execute_var_assignment(vars, va); None },
			BlockStatement::If(ref i) => { self.execute_if(vars, i); None },
			BlockStatement::While(ref w) => { self.execute_while(vars, w); None },
			BlockStatement::Return(ref r) => self.execute_return(vars, r),
		}
	}

	fn execute_return(&self, vars: *mut InterpreterVars, return_data: &ReturnData) -> Option<Value> {
		match return_data.expected_type {
			Some(ref t) => {
				let value = match return_data.value {
					Some(ref e) => self.value_from_expression(vars, e),
					None => panic!("Interpreter error: expected expression for return statement"),
				};

				let value_type = Self::type_from_value(value.clone());
				if value_type != *t {
					panic!("Interpreter error: mismatched types in return statement (got {:?} expected {:?})", value_type, *t)
				}

				Some(value)
			},
			None => {
				if let Some(_) = return_data.value {
					panic!("Interpreter error: unexpected expression in return statement")
				}

				None
			}
		}
	}

	fn execute_func_call(&self, vars: *mut InterpreterVars, func_call_data: &FuncCallData) -> Option<Value> {
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
			self.builtin_readln(vars, func_call_data)
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
					_ => panic!("Interpreter error: invalid function path")
				}
			};

			let func_decl = if module_string == "" {
				self.funcs.get(&last_id).unwrap()
			} else {
				let mut import_func_decl: Option<&FuncDeclData> = None;
				for statement in &self.imports.get(&module_string).unwrap().statements {
					match *statement {
						Statement::FuncDecl(ref fd) => {
							import_func_decl = Some(fd);
							break
						},
						_ => (),
					}
				}

				import_func_decl.unwrap()
			};

			let mut param_count = 0;
			for param in &func_decl.parameters {
				let variable = Variable {
					name: param.name.clone(),
					var_type: param.param_type.clone(),
					value: {
						if param_count < func_call_data.arguments.len() {
							let value = self.value_from_expression(vars, &func_call_data.arguments.get(param_count).unwrap().value);

							let value_type = Self::type_from_value(value.clone());

							if value_type != param.param_type {
								panic!("Interpreter error: mismatched types in function call (got {:?} expected {:?})", value_type, param.param_type)
							} else {
								value
							}
						} else {
							if let Some(ref e) = param.default_value {
								let value = self.value_from_expression(vars, e);

								let value_type = Self::type_from_value(value.clone());

								if value_type != param.param_type {
									panic!("Interpreter error: mismatched types in function declaration default value (got {:?} expected {:?})", value_type, param.param_type)
								} else {
									value
								}
							} else {
								panic!("Interpreter error: expected argument for {:?}", param.name)
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
			for statement in &func_decl.statements {
				match self.execute_block_statement(vars, statement) {
					Some(v) => {
						return_value = Some(v);
						break
					},
					None => ()
				}
			}

			for param in &func_decl.parameters {
				unsafe {
					(*vars).remove(AsRef::<str>::as_ref(&param.name[..]));
				}
			}

			return_value
		}
	}

	fn execute_var_decl(&self, vars: *mut InterpreterVars, var_decl_data: &'a VarDeclData) {
		let value = match var_decl_data.value {
			Some(ref v) => self.value_from_expression(vars, v),
			None => self.default_value(var_decl_data.var_type.clone()),
		};

		unsafe {
			(*vars).insert(
				var_decl_data.name.clone(),
				Variable {
					name: var_decl_data.name.clone(),
					var_type: var_decl_data.var_type.clone(),
					value: {
						let value_type = Self::type_from_value(value.clone());

						if value_type != var_decl_data.var_type {
							panic!("Interpreter error: mismatched types in var initialization (got {:?} expected {:?})", value_type, var_decl_data.var_type)
						} else {
							value
						}
					},
				}
			);
		}
	}

	fn execute_var_assignment(&self, vars: *mut InterpreterVars, var_assignment_data: &VarAssignmentData) {
		let value = self.value_from_expression(vars, &var_assignment_data.value);

		let mut path = var_assignment_data.path.iter();

		let mut current_ref = match **path.next().unwrap() {
			PathPart::IdentifierPathPart(ref ipp) => unsafe {
			&mut (*vars).get_mut(AsRef::<str>::as_ref(&ipp.identifier[..])).unwrap().value },
			_ => panic!("Interpreter error: incorrect var path")
		};

		let mut is_field = false;
		for path_part in path {
			match **path_part {
				PathPart::FieldPathPart if !is_field => is_field = true,
				PathPart::IdentifierPathPart(ref ipp) if is_field => {
					let current_map = match *{current_ref} {
						Value::Struct(_, ref mut map) => map,
						_ => panic!("Interpreter error: cannot access field on non-struct")
					};

					current_ref = current_map.get_mut(&ipp.identifier).unwrap();

					is_field = false;
				},
				PathPart::IndexPathPart(ref ipp) => {
					match ipp.index {
						Some(ref expr) => {
							let index_value = self.value_from_expression(vars, expr);
							match index_value {
								Value::Integer(i) => {
									let cell = match *{current_ref} {
										Value::Array(_, ref mut a) => {
											a.get_mut(i as usize).unwrap()
										},
										_ => panic!("Interpreter error: can't access index on non-array")
									};

									current_ref = cell;
								},
								_ => panic!("Interpreter error: invalid index type for array")
							}
						},
						None => {
							let new_cell = match *{current_ref} {
								Value::Array(ref t, ref mut a) => {
									a.push(
										match *t {
											Some(ref array_type) => self.default_value((*array_type).clone()),
											None => panic!("Interpreter error: cannot push to untyped array"),
										}
									);

									let last_index = a.len() - 1;
									a.get_mut(last_index).unwrap()
								},
								_ => panic!("Interpreter error: can't push to non-array")
							};

							current_ref = new_cell;
						},
					}

					is_field = false;
				},
				_ => panic!("Interpreter error: invalid path for var assignment"),
			}
		};

		let value_type = Self::type_from_value(value.clone());
		let current_type = Self::type_from_value((*current_ref).clone());

		if value_type != current_type {
			panic!("Interpreter error: mismatched types in var assignment (got {:?} expected {:?})", value_type, current_type)
		} else {
			*current_ref = value;
		};
	}

	fn execute_if(&self, vars: *mut InterpreterVars, if_data: &'a IfData) {
		match self.value_from_expression(vars, &if_data.condition) {
			Value::Bool(b) => {
				if b {
					for statement in &if_data.statements {
						self.execute_block_statement(vars, statement);
					}
				}
			},
			_ => panic!("Interpreter error: expected bool expression in if statement")
		}
	}

	fn execute_while(&self, vars: *mut InterpreterVars, while_data: &'a WhileData) {
		loop {
			match self.value_from_expression(vars, &while_data.condition) {
				Value::Bool(b) => {
					if b {
						for statement in &while_data.statements {
							self.execute_block_statement(vars, statement);
						}
					} else {
						return
					}
				},
				_ => panic!("Interpreter error: expected bool expression in while statement")
			}
		}
	}

	fn value_from_expression(&self, vars: *mut InterpreterVars, expression: &Expression) -> Value {
		match *expression {
			Expression::StringLiteral(ref sl) => Value::String(sl.value.clone()),
			Expression::IntegerLiteral(ref il) => Value::Integer(il.value),
			Expression::BoolLiteral(ref bl) => Value::Bool(bl.value),
			Expression::CharLiteral(ref cl) => Value::Char(cl.value),
			Expression::Variable(ref v) => {
				let mut path = v.path.iter();

				let mut current_ref = match **path.next().unwrap() {
					PathPart::IdentifierPathPart(ref ipp) =>
					unsafe { &(*vars).get(AsRef::<str>::as_ref(&ipp.identifier[..])).unwrap().value },
					_ => panic!("Interpreter error: incorrect var path")
				};

				let mut is_field = false;
				for path_part in path {
					match **path_part {
						PathPart::FieldPathPart if !is_field => is_field = true,
						PathPart::IdentifierPathPart(ref ipp) if is_field => {
							let current_map = match *{current_ref} {
								Value::Struct(_, ref map) => map,
								_ => panic!("Interpreter error: cannot access field on non-struct")
							};

							current_ref = current_map.get(&ipp.identifier).unwrap();

							is_field = false;
						},
						PathPart::IndexPathPart(ref ipp) => {
							match ipp.index {
								Some(ref expr) => {
									let index_value = self.value_from_expression(vars, expr);
									match index_value {
										Value::Integer(i) => {
											let cell = match *{current_ref} {
												Value::Array(_, ref a) => a.get(i as usize).unwrap(),
												Value::String(ref s) => return Value::Char(s.chars().nth(i as usize).unwrap()), // TODO: perform checks
												_ => panic!("Interpreter error: can't access index on non-array")
											};

											current_ref = cell;
										},
										_ => panic!("Interpreter error: invalid index type for array")
									}
								},
								None => panic!("Interpreter error: arrray expression requires an index")
							}

							is_field = false;
						},
						_ => panic!("Interpreter error: invalid path for var assignment"),
					}
				};

				current_ref.clone()
			},
			Expression::Array(ref a) => {
				let mut values: std::vec::Vec<Value> = vec![];
				let mut array_type: Option<Type> = None;
				for item in &a.items {
					let value = self.value_from_expression(vars, item);
					let value_type = Self::type_from_value(value.clone());
					match array_type {
						Some(ref t) => if value_type != *t {
							panic!("Interpreter error: heterogeneous types in array literal")
						},
						None => array_type = Some(value_type),
					};

					values.push(value)
				}

				Value::Array(array_type, values)
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
						_ => panic!("Interpreter error: invalid function path")
					}
				};

				let struct_decl = if module_string == "" {
					self.structs.get(&last_id).unwrap()
				} else {
					let mut import_struct_decl: Option<&StructDeclData> = None;
					for statement in &self.imports.get(&module_string).unwrap().statements {
						match *statement {
							Statement::StructDecl(ref sd) => {
								import_struct_decl = Some(sd);
								break
							},
							_ => (),
						}
					}

					import_struct_decl.unwrap()
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
						panic!("Interpreter error: unknown field {:?} in struct init", new_field.name)
					}
				}

				for field in &struct_decl.fields {
					let mut found_field = false;

					for new_field in &si.fields {
						if field.name == new_field.name {
							let value = self.value_from_expression(vars, &new_field.value);
							let value_type = Self::type_from_value(value.clone());

							if field.field_type != value_type {
								panic!("Interpreter error: mismatched types in struct init (got {:?} expected {:?})", value_type, field.field_type)
							} else {
								new_content.insert(field.name.clone(), Box::new(value));
							};

							found_field = true;
						}
					}

					if !found_field {
						panic!("Interpreter error: missing field {:?} in struct init", field.name)
					}
				}

				Value::Struct(
					Type::StructType(
						Box::new(
							StructTypeData { path: si.path.clone() }
						)
					),
					new_content,
				)
			}
			Expression::FuncCall(ref fc) => {
				self.execute_func_call(vars, fc).unwrap()
			},
			Expression::Count(ref e) => {
				match self.value_from_expression(vars, e) {
					Value::Array(_, ref a) => Value::Integer(a.len() as i64),
					Value::String(ref s) => Value::Integer(s.len() as i64),
					_ => panic!("Interpreter error: can't get count on non-(array/string)")
				}
			},
			Expression::Addition(ref e1, ref e2) => {
				let integer1 = match self.value_from_expression(vars, e1) {
					Value::Integer(i) => i,
					other => panic!("Interpreter error: incorrect expression for addition: {:?}", other)
				};

				let integer2 = match self.value_from_expression(vars, e2) {
					Value::Integer(i) => i,
					other => panic!("Interpreter error: incorrect expression for addition: {:?}", other)
				};

				Value::Integer(integer1 + integer2)
			},
			Expression::Substraction(ref e1, ref e2) => {
				let integer1 = match self.value_from_expression(vars, e1) {
					Value::Integer(i) => i,
					other => panic!("Interpreter error: incorrect expression for substraction: {:?}", other)
				};

				let integer2 = match self.value_from_expression(vars, e2) {
					Value::Integer(i) => i,
					other => panic!("Interpreter error: incorrect expression for substraction: {:?}", other)
				};

				Value::Integer(integer1 - integer2)
			},
			Expression::Multiplication(ref e1, ref e2) => {
				let integer1 = match self.value_from_expression(vars, e1) {
					Value::Integer(i) => i,
					other => panic!("Interpreter error: incorrect expression for multiplication: {:?}", other)
				};

				let integer2 = match self.value_from_expression(vars, e2) {
					Value::Integer(i) => i,
					other => panic!("Interpreter error: incorrect expression for multiplication: {:?}", other)
				};

				Value::Integer(integer1 * integer2)
			},
			Expression::Division(ref e1, ref e2) => {
				let integer1 = match self.value_from_expression(vars, e1) {
					Value::Integer(i) => i,
					other => panic!("Interpreter error: incorrect expression for division: {:?}", other)
				};

				let integer2 = match self.value_from_expression(vars, e2) {
					Value::Integer(i) => i,
					other => panic!("Interpreter error: incorrect expression for division: {:?}", other)
				};

				Value::Integer(integer1 / integer2)
			},
			Expression::Modulo(ref e1, ref e2) => {
				let integer1 = match self.value_from_expression(vars, e1) {
					Value::Integer(i) => i,
					other => panic!("Interpreter error: incorrect expression for modulo: {:?}", other)
				};

				let integer2 = match self.value_from_expression(vars, e2) {
					Value::Integer(i) => i,
					other => panic!("Interpreter error: incorrect expression for modulo: {:?}", other)
				};

				Value::Integer(integer1 % integer2)
			},
			Expression::Equality(ref e1, ref e2) => {
				let value1 = self.value_from_expression(vars, e1);
				let value2 = self.value_from_expression(vars, e2);

				Value::Bool(value1 == value2)
			},
			Expression::Inequality(ref e1, ref e2) => {
				let value1 = self.value_from_expression(vars, e1);
				let value2 = self.value_from_expression(vars, e2);

				Value::Bool(value1 != value2)
			},
			Expression::Concatenation(ref e1, ref e2) => {
				let string1 = match self.value_from_expression(vars, e1) {
					Value::String(s) => s,
					other => panic!("Interpreter error: incorrect expression for concatenation: {:?}", other)
				};

				let string2 = match self.value_from_expression(vars, e2) {
					Value::String(s) => s,
					other => panic!("Interpreter error: incorrect expression for concatenation: {:?}", other)
				};

				let mut new_string = String::new();
				new_string.push_str(string1.as_ref());
				new_string.push_str(string2.as_ref());
				Value::String(new_string)
			},
		}
	}

	fn builtin_println(&self, vars: *mut InterpreterVars, func_call_data: &FuncCallData) -> Option<Value> {
		if func_call_data.arguments.len() != 1 {
			panic!("Interpreter error: invalid argument count for println")
		};

		match self.value_from_expression(vars, &func_call_data.arguments[0].value) {
			Value::String(s) => println!("{}", s),
			Value::Integer(i) => println!("{}", i),
			Value::Bool(b) => println!("{}", b),
			Value::Char(c) => println!("{}", c),
			Value::Struct(_, s) => println!("{:?}", s),
			Value::Array(_, a) => println!("{:?}", a),
			/*other => panic!("Interpreter error: invalid argument type for println (got {:?})", other)*/
		};

		None
	}

	fn builtin_readln(&self, vars: *mut InterpreterVars, func_call_data: &FuncCallData) -> Option<Value> {
		if func_call_data.arguments.len() != 0 {
			panic!("Interpreter error: invalid argument count for readln")
		};

		let mut line = String::new();
	    let stdin = io::stdin();
	    stdin.lock().read_line(&mut line).unwrap();
    	Some(
			Value::String(
				line
			)
		)
	}

	fn default_value(&self, var_type: Type) -> Value {
		match var_type {
			Type::StringType => Value::String("".to_string()),
			Type::IntType => Value::Integer(0),
			Type::BoolType => Value::Bool(false),
			Type::CharType => Value::Char('\0'),
			Type::ArrayType(t) => Value::Array(Some(*t), vec![]),
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
						_ => panic!("Interpreter error: invalid function path")
					}
				};

				let struct_decl = if module_string == "" {
					self.structs.get(&last_id).unwrap()
				} else {
					let mut import_struct_decl: Option<&StructDeclData> = None;
					for statement in &self.imports.get(&module_string).unwrap().statements {
						match *statement {
							Statement::StructDecl(ref sd) => {
								import_struct_decl = Some(sd);
								break
							},
							_ => (),
						}
					}

					import_struct_decl.unwrap()
				};

				let mut fields: std::collections::HashMap<String, Box<Value>> = std::collections::HashMap::new();

				for field in &struct_decl.fields {
					fields.insert(field.name.clone(), Box::new(self.default_value(field.field_type.clone())));
				}

				Value::Struct(var_type.clone(), fields)
			},
		}
	}
}
