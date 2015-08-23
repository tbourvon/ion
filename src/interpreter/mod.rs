use parser::ast::*;
use std::borrow::Borrow;
use std;
use lexer;
use parser;
use std::fs::File;
use std::path::Path;
use std::io::prelude::*;

pub struct Interpreter<'a> {
	ast: &'a Ast,
	funcs: std::collections::HashMap<String, FuncDeclData>,
	structs: std::collections::HashMap<String, StructDeclData>,
	imports: std::vec::Vec<Box<Ast>>,
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
	String(String),
	Integer(i64),
	Bool(bool),
	Struct(std::collections::HashMap<String, Box<Value>>),
}

impl<'a> Interpreter<'a> {
	pub fn new(ast: &'a Ast) -> Self {
		Interpreter {
			ast: ast,
			funcs: std::collections::HashMap::new(),
			structs: std::collections::HashMap::new(),
			imports: vec![],
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
			name: "main".to_string(),
			arguments: vec![],
		};

		let mut vars = std::collections::HashMap::new();
		self.execute_func_call(&mut vars, &main_func_call);
	}

	fn execute_import(&mut self, import_data: &ImportData) {
		let path_string = import_data.path.clone() + ".ion";
	    let path = Path::new(AsRef::<str>::as_ref(&path_string[..]));
	    let mut file = match File::open(&path) {
	        Ok(file) => file,
	        Err(_) => panic!(),
	    };

	    let mut s = String::new();
	    file.read_to_string(&mut s);
	    let mut reader = lexer::Reader::new(s.as_ref());

	    let mut parser = parser::Parser::new(&mut reader);
	    let ast = parser.parse();

		self.imports.push(Box::new((*ast).clone()));

		let mut iter = self.imports.last().unwrap().statements.iter();
		loop { let statement = iter.next();
			match statement {
				Some(s) => {
					match *s {
						Statement::FuncDecl(ref fd) => {
							let mut path = import_data.path.clone() + "::";
							path.push_str(fd.name.as_ref());
							self.funcs.insert(path, *fd.clone());
						},
						_ => ()
					}
				},
				None => break,
			}

		}
	}

	fn execute_block_statement(&self, vars: &mut InterpreterVars, block_statement: &'a BlockStatement) {
		match *block_statement {
			BlockStatement::FuncCall(ref fc) => self.execute_func_call(vars, fc),
			BlockStatement::VarDecl(ref vd) => self.execute_var_decl(vars, vd),
			BlockStatement::VarAssignment(ref va) => self.execute_var_assignment(vars, va),
			BlockStatement::If(ref i) => self.execute_if(vars, i),
			BlockStatement::While(ref w) => self.execute_while(vars, w),
		}
	}

	fn execute_func_call(&self, vars: &mut InterpreterVars, func_call_data: &FuncCallData) {
		match func_call_data.name.as_ref() {
			"println" => self.builtin_println(vars, func_call_data),
			n => {
				let mut param_count = 0;
				for param in &self.funcs.get(n).unwrap().parameters {
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

					vars.insert(
						param.name.clone(),
						variable
					);

					param_count += 1;
				}

				for statement in &self.funcs.get(n).unwrap().statements {
					self.execute_block_statement(vars, statement);
				}

				for param in &self.funcs.get(n).unwrap().parameters {
					vars.remove(AsRef::<str>::as_ref(&param.name[..]));
				}
			}
		}
	}

	fn execute_var_decl(&self, vars: &mut InterpreterVars, var_decl_data: &'a VarDeclData) {
		let value = match var_decl_data.value {
			Some(ref v) => self.value_from_expression(vars, v),
			None => self.default_value(var_decl_data.var_type.clone()),
		};

		vars.insert(
			var_decl_data.name.clone(),
			Variable {
				name: var_decl_data.name.clone(),
				var_type: var_decl_data.var_type.clone(),
				value: value,
			}
		);
	}

	fn execute_var_assignment(&self, vars: &mut InterpreterVars, var_assignment_data: &VarAssignmentData) {
		let value = self.value_from_expression(vars, &var_assignment_data.value);
		vars.get_mut(AsRef::<str>::as_ref(&var_assignment_data.name[..])).unwrap().value = value;
	}

	fn execute_if(&self, vars: &mut InterpreterVars, if_data: &'a IfData) {
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

	fn execute_while(&self, vars: &mut InterpreterVars, while_data: &'a WhileData) {
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

	fn value_from_expression(&self, vars: &mut InterpreterVars, expression: &Expression) -> Value {
		match *expression {
			Expression::StringLiteral(ref sl) => Value::String(sl.value.clone()),
			Expression::IntegerLiteral(ref il) => Value::Integer(il.value),
			Expression::BoolLiteral(ref bl) => Value::Bool(bl.value),
			Expression::Variable(ref v) => vars.get(Borrow::<str>::borrow(&v.name)).unwrap().value.clone(),
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

	fn builtin_println(&self, vars: &mut InterpreterVars, func_call_data: &FuncCallData) {
		if func_call_data.arguments.len() != 1 {
			panic!("Interpreter error: invalid argument count for println")
		}

		match self.value_from_expression(vars, &func_call_data.arguments[0].value) {
			Value::String(s) => println!("{}", s),
			Value::Integer(i) => println!("{}", i),
			Value::Bool(b) => println!("{}", b),
			Value::Struct(s) => println!("{:?}", s),
			/*other => panic!("Interpreter error: invalid argument type for println (got {:?})", other)*/
		}
	}

	fn default_value(&self, var_type: Type) -> Value {
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
