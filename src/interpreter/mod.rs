use parser::ast::*;
use std;
use lexer;
use parser;
use std::fs::File;
use std::io::prelude::*;

pub struct Interpreter<'a> {
	ast: &'a Ast,
	funcs: std::collections::HashMap<String, FuncDeclData>,
	structs: std::collections::HashMap<String, StructDeclData>,
	imports: std::collections::HashMap<String, Box<Ast>>,
}

type InterpreterVars = std::collections::HashMap<String, Variable>;

#[derive(Debug)]
pub struct Variable {
	name: String,
	var_type: Type,
	value: Value,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
	_Empty,
	String(String),
	Integer(i64),
	Bool(bool),
	Struct(std::collections::HashMap<String, Box<Value>>),
	Array(Type, std::vec::Vec<Value>),
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

	fn execute_import(&mut self, import_data: &ImportData) {
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
		Some(self.value_from_expression(vars, &return_data.value))
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
							self.value_from_expression(vars, &func_call_data.arguments.get(param_count).unwrap().value)
						} else {
							if let Some(ref e) = param.default_value {
								self.value_from_expression(vars, e)
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
					value: value,
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
						Value::Struct(ref mut map) => map,
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
										Value::Array(_, ref mut a) => a.get_mut(i as usize).unwrap(),
										_ => panic!("Interpreter error: can't access index on non-array")
									};

									current_ref = cell;
								},
								_ => panic!("Interpreter error: invalid index type for array")
							}
						},
						None => {
							let new_cell = match *{current_ref} {
								Value::Array(_, ref mut a) => {
									a.push(Value::_Empty);
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

		*current_ref = value;
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
								Value::Struct(ref map) => map,
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
				for item in &a.items {
					values.push(self.value_from_expression(vars, item))
				}

				Value::Array("".to_string(), values)
			},
			Expression::FuncCall(ref fc) => {
				self.execute_func_call(vars, fc).unwrap()
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
			Value::Struct(s) => println!("{:?}", s),
			Value::Array(_, a) => println!("{:?}", a),
			other => panic!("Interpreter error: invalid argument type for println (got {:?})", other)
		};

		None
	}

	fn default_value(&self, var_type: Type) -> Value {
		let mut chars = var_type.chars();
		while let Some(c1) = chars.next() {
			if c1 == '[' {
				if let Some(c2) = chars.next() {
					if c2.is_numeric() || c2 == ']' {
						return Value::Array(var_type.split("]").nth(1).unwrap().to_string(), vec![])
					} else {
						panic!("Interpreter error: incorrect type")
					}
				}
			} else {
				break
			}
		}

		match var_type.as_ref() {
			"string" => Value::String("".to_string()),
			"int" => Value::Integer(0),
			"bool" => Value::Bool(false),
			_ => {
				let mut fields: std::collections::HashMap<String, Box<Value>> = std::collections::HashMap::new();

				for field in &self.structs.get(&var_type).unwrap().fields {
					fields.insert(field.name.clone(), Box::new(self.default_value(field.field_type.clone())));
				}

				Value::Struct(fields)
			},
		}
	}
}
