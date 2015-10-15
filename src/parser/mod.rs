pub mod ast;

use std;
use lexer::Token;
use lexer::Reader;
use lexer::Keyword;
use lexer::Symbol;
use self::ast::*;

pub struct Parser<'a> {
	reader: &'a mut Reader<'a>,
	ast: Box<Ast>,
	current_token: Token,
}

impl<'a> Parser<'a> {
	pub fn new<'b>(reader: &'b mut Reader<'b>) -> Parser<'b> {
		Parser {
			reader: reader,
			ast: Box::new(Ast::new()),
			current_token: Token::EOF,
		}
	}

	pub fn parse(&mut self) -> Result<&Ast, String> {
		try!(self.next_token());
		while self.current_token != Token::EOF {
			let statement = try!(self.parse_statement());
			self.ast.statements.push(statement);
		}

		Ok(&self.ast)
	}

	fn precedence_for_token(token: Token) -> u8 {
		match token {
			Token::Symbol(s) => {
				match s {
					Symbol::EqualEqual |
					Symbol::NotEqual => 1,
					Symbol::Plus |
					Symbol::Minus |
					Symbol::Concat => 2,
					Symbol::Times |
					Symbol::Over |
					Symbol::Modulo => 3,
					_ => 0
				}
			},
			_ => 0
		}
	}

	fn parse_statement(&mut self) -> Result<Statement, String> {
		if try!(self.accept(Token::Keyword(Keyword::Import))).is_some() {
			Ok(Statement::Import(try!(self.parse_import())))
		} else if try!(self.accept(Token::Keyword(Keyword::Package))).is_some() {
			Ok(Statement::Package(try!(self.parse_package())))
		} else if try!(self.accept(Token::Keyword(Keyword::Func))).is_some() {
			Ok(Statement::FuncDecl(try!(self.parse_func_decl())))
		} else if try!(self.accept(Token::Keyword(Keyword::Struct))).is_some() {
			Ok(Statement::StructDecl(try!(self.parse_struct_decl())))
		} else {
			Err(format!("Parser error: unexpected token {:?}", self.current_token))
		}
	}

	fn parse_package(&mut self) -> Result<Box<PackageData>, String> {
		let name_token = try!(self.expect_any(Token::Identifier("".to_string())));
		match name_token {
			Token::Identifier(n) => {
				Ok(Box::new(
					PackageData { name: n }
				))
			},
			_ => Err("".to_string()), // Should never happen
		}
	}

	fn parse_import(&mut self) ->  Result<Box<ImportData>, String> {
		let path_token = try!(self.expect_any(Token::StringLiteral("".to_string())));
		match path_token {
			Token::StringLiteral(p) => {
				Ok(Box::new(
					ImportData { path: p }
				))
			},
			_ => Err("".to_string()), // Should never happen
		}
	}

	fn parse_func_decl(&mut self) -> Result<Box<FuncDeclData>, String> {
		let name_token = try!(self.expect_any(Token::Identifier("".to_string())));
		let name = match name_token {
			Token::Identifier(s) => s,
			_ => return Err("".to_string()), // Should never happen
		};

		try!(self.expect(Token::Symbol(Symbol::LeftParenthesis)));

		let mut params: std::vec::Vec<Box<FuncDeclParamData>> = vec![];
		while try!(self.accept(Token::Symbol(Symbol::RightParenthesis))).is_none() {
			let arg_name_token = try!(self.expect_any(Token::Identifier("".to_string())));
			let arg_name = match arg_name_token {
				Token::Identifier(s) => s,
				_ => return Err("".to_string()), // Should never happen
			};

			try!(self.expect(Token::Symbol(Symbol::Colon)));

			let arg_type = try!(self.parse_type());

			params.push(
				Box::new(
					FuncDeclParamData {
						name: arg_name,
						param_type: arg_type,
						default_value: None,
					}
				)
			);

			if self.current_token == Token::Symbol(Symbol::RightParenthesis) {
				try!(self.accept(Token::Symbol(Symbol::Comma)));
			} else {
				try!(self.expect(Token::Symbol(Symbol::Comma)));
			};
		};

		let mut return_type: Option<Type> = None;
		if try!(self.accept(Token::Symbol(Symbol::Return))).is_some() {
			return_type = Some(try!(self.parse_type()));
		}

		try!(self.expect(Token::Symbol(Symbol::LeftBrace)));

		let mut statements: std::vec::Vec<BlockStatement> = vec![];
		while try!(self.accept(Token::Symbol(Symbol::RightBrace))).is_none() {
			statements.push(try!(self.parse_block_statement(return_type.clone())));
		};

		Ok(Box::new(
			FuncDeclData {
				name: name,
				return_type: return_type,
				parameters: params,
				statements: statements,
			}
		))
	}

	fn parse_struct_decl(&mut self) -> Result<Box<StructDeclData>, String> {
		let name_token = try!(self.expect_any(Token::Identifier("".to_string())));
		let name = match name_token {
			Token::Identifier(s) => s,
			_ => return Err("".to_string()), // Should never happen
		};

		try!(self.expect(Token::Symbol(Symbol::LeftBrace)));

		let mut fields: std::vec::Vec<Box<StructFieldData>> = vec![];
		while try!(self.accept(Token::Symbol(Symbol::RightBrace))).is_none() {
			let field_name_token = try!(self.expect_any(Token::Identifier("".to_string())));
			let field_name = match field_name_token {
				Token::Identifier(s) => s,
				_ => return Err("".to_string()), // Should never happen
			};

			try!(self.expect(Token::Symbol(Symbol::Colon)));

			let field_type = try!(self.parse_type());

			fields.push(
				Box::new(
					StructFieldData {
						name: field_name,
						field_type: field_type,
						default_value: None,
					}
				)
			);

			if self.current_token == Token::Symbol(Symbol::RightBrace) {
				try!(self.accept(Token::Symbol(Symbol::Comma)));
			} else {
				try!(self.expect(Token::Symbol(Symbol::Comma)));
			};
		};

		Ok(Box::new(
			StructDeclData {
				name: name,
				fields: fields,
			}
		))
	}

	fn parse_block_statement(&mut self, return_type: Option<Type>) -> Result<BlockStatement, String> {
		if try!(self.accept(Token::Keyword(Keyword::Var))).is_some() {
			Ok(BlockStatement::VarDecl(try!(self.parse_var_decl())))
		} else if try!(self.accept(Token::Keyword(Keyword::If))).is_some() {
			Ok(BlockStatement::If(try!(self.parse_if())))
		} else if try!(self.accept(Token::Keyword(Keyword::While))).is_some() {
			Ok(BlockStatement::While(try!(self.parse_while())))
		} else if try!(self.accept(Token::Keyword(Keyword::Return))).is_some() {
			Ok(BlockStatement::Return(try!(self.parse_return(return_type))))
		} else if let Some(identifier) = try!(self.accept_any(Token::Identifier("".to_string()))) {
			match identifier {
				Token::Identifier(i) => {
					let mut path: Path = vec![];

					path.push(
						Box::new(
							PathPart::IdentifierPathPart(
								Box::new(
									IdentifierPathPartData { identifier: i }
								)
							)
						)
					);

					while try!(self.accept(Token::Symbol(Symbol::ColonColon))).is_some() {
						let next_path_token = try!(self.expect_any(Token::Identifier("".to_string())));
						let next_path = match next_path_token {
							Token::Identifier(id) => id,
							_ => return Err("".to_string()) // Should never happen
						};

						path.push(
							Box::new(PathPart::ModulePathPart)
						);

						path.push(
							Box::new(
								PathPart::IdentifierPathPart(
									Box::new(
										IdentifierPathPartData { identifier: next_path }
									)
								)
							)
						);
					}

					if try!(self.accept(Token::Symbol(Symbol::LeftParenthesis))).is_some() {
						Ok(BlockStatement::FuncCall(try!(self.parse_func_call(path))))
					} else {
						loop {
							if try!(self.accept(Token::Symbol(Symbol::Dot))).is_some() {
								let next_path_token = try!(self.expect_any(Token::Identifier("".to_string())));
								let next_path = match next_path_token {
									Token::Identifier(id) => id,
									_ => return Err("".to_string()) // Should never happen
								};

								path.push(
									Box::new(PathPart::FieldPathPart)
								);

								path.push(
									Box::new(
										PathPart::IdentifierPathPart(
											Box::new(
												IdentifierPathPartData { identifier: next_path }
											)
										)
									)
								);
							} else if try!(self.accept(Token::Symbol(Symbol::LeftBracket))).is_some() {
								let index = if try!(self.accept(Token::Symbol(Symbol::RightBracket))).is_some() {
									None
								} else {
									let expr = try!(self.parse_expression());
									try!(self.expect(Token::Symbol(Symbol::RightBracket)));

									Some(expr)
								};

								path.push(
									Box::new(
										PathPart::IndexPathPart(
											Box::new(
												IndexPathPartData { index: index }
											)
										)
									)
								);
							} else {
								break
							};
						}

						if try!(self.accept(Token::Symbol(Symbol::Equal))).is_some() {
							Ok(BlockStatement::VarAssignment(try!(self.parse_var_assignment(path))))
						} else {
							return Err(format!("Parser error: unexpected token {:?}", self.current_token))
						}
					}
				},
				_ => return Err("".to_string()), // Should never happen
			}
		} else {
			return Err(format!("Parser error: unexpected token {:?}", self.current_token))
		}
	}

	fn parse_return(&mut self, return_type: Option<Type>) -> Result<Box<ReturnData>, String> {
		Ok(Box::new(
			ReturnData {
				value: match return_type {
					Some(_) => Some(try!(self.parse_expression())),
					None => None,
				},
				expected_type: return_type,
			}
		))
	}

	fn parse_if(&mut self) -> Result<Box<IfData>, String> {
		let condition = try!(self.parse_expression());

		try!(self.expect(Token::Symbol(Symbol::LeftBrace)));

		let mut statements: std::vec::Vec<BlockStatement> = vec![];
		while try!(self.accept(Token::Symbol(Symbol::RightBrace))).is_none() {
			statements.push(try!(self.parse_block_statement(None)));
		};

		Ok(Box::new(
			IfData {
				condition: condition,
				statements: statements,
			}
		))
	}

	fn parse_while(&mut self) -> Result<Box<WhileData>, String> {
		let condition = try!(self.parse_expression());

		try!(self.expect(Token::Symbol(Symbol::LeftBrace)));

		let mut statements: std::vec::Vec<BlockStatement> = vec![];
		while try!(self.accept(Token::Symbol(Symbol::RightBrace))).is_none() {
			statements.push(try!(self.parse_block_statement(None)));
		};

		Ok(Box::new(
			WhileData {
				condition: condition,
				statements: statements,
			}
		))
	}

	fn parse_func_call(&mut self, path: Path) -> Result<Box<FuncCallData>, String> {
		let mut args: std::vec::Vec<Box<FuncCallArgData>> = vec![];
		while try!(self.accept(Token::Symbol(Symbol::RightParenthesis))).is_none() {
			args.push(
				Box::new(
					FuncCallArgData { value: try!(self.parse_expression()) }
				)
			);
			if self.current_token == Token::Symbol(Symbol::RightParenthesis) {
				try!(self.accept(Token::Symbol(Symbol::Comma)));
			} else {
				try!(self.expect(Token::Symbol(Symbol::Comma)));
			};
		};

		Ok(Box::new(
			FuncCallData {
				path: path,
				arguments: args,
			}
		))
	}

	fn parse_var_decl(&mut self) -> Result<Box<VarDeclData>, String> {
		let name_token = try!(self.expect_any(Token::Identifier("".to_string())));
		let name = match name_token {
			Token::Identifier(i) => i,
			_ => return Err("".to_string()), // Should never happen
		};

		try!(self.expect(Token::Symbol(Symbol::Colon)));

		let var_type = try!(self.parse_type());

		let value = if try!(self.accept(Token::Symbol(Symbol::Equal))).is_some() {
			Some(try!(self.parse_expression()))
		} else {
			None
		};

		Ok(Box::new(
			VarDeclData {
				name: name,
				var_type: var_type,
				value: value
			}
		))
	}

	fn parse_var_assignment(&mut self, path: Path) -> Result<Box<VarAssignmentData>, String> {
		let expression = try!(self.parse_expression());
		Ok(Box::new(
			VarAssignmentData {
				path: path,
				value: expression,
			}
		))
	}

	fn parse_expression(&mut self) -> Result<Expression, String> {
		self.parse_expression_rec(None, 0)
	}

	fn parse_expression_rec(&mut self, prev_expr: Option<Expression>, minimum_precedence: u8) -> Result<Expression, String> {
		let new_expr = match prev_expr {
			Some(expr) => {
				if try!(self.accept(Token::Symbol(Symbol::Plus))).is_some() {
					Expression::Addition(
						Box::new(
							expr
						),
						Box::new(
							try!(self.parse_expression_rec(None, Self::precedence_for_token(Token::Symbol(Symbol::Plus))))
						)
					)
				} else if try!(self.accept(Token::Symbol(Symbol::Minus))).is_some() {
					Expression::Substraction(
						Box::new(
							expr
						),
						Box::new(
							try!(self.parse_expression_rec(None, Self::precedence_for_token(Token::Symbol(Symbol::Minus))))
						)
					)
				} else if try!(self.accept(Token::Symbol(Symbol::Times))).is_some() {
					Expression::Multiplication(
						Box::new(
							expr
						),
						Box::new(
							try!(self.parse_expression_rec(None, Self::precedence_for_token(Token::Symbol(Symbol::Times))))
						)
					)
				} else if try!(self.accept(Token::Symbol(Symbol::Over))).is_some() {
					Expression::Division(
						Box::new(
							expr
						),
						Box::new(
							try!(self.parse_expression_rec(None, Self::precedence_for_token(Token::Symbol(Symbol::Over))))
						)
					)
				} else if try!(self.accept(Token::Symbol(Symbol::Modulo))).is_some() {
					Expression::Modulo(
						Box::new(
							expr
						),
						Box::new(
							try!(self.parse_expression_rec(None, Self::precedence_for_token(Token::Symbol(Symbol::Modulo))))
						)
					)
				} else if try!(self.accept(Token::Symbol(Symbol::EqualEqual))).is_some() {
					Expression::Equality(
						Box::new(
							expr
						),
						Box::new(
							try!(self.parse_expression_rec(None, Self::precedence_for_token(Token::Symbol(Symbol::EqualEqual))))
						)
					)
				} else if try!(self.accept(Token::Symbol(Symbol::NotEqual))).is_some() {
					Expression::Inequality(
						Box::new(
							expr
						),
						Box::new(
							try!(self.parse_expression_rec(None, Self::precedence_for_token(Token::Symbol(Symbol::NotEqual))))
						)
					)
				} else if try!(self.accept(Token::Symbol(Symbol::Concat))).is_some() {
					Expression::Concatenation(
						Box::new(
							expr
						),
						Box::new(
							try!(self.parse_expression_rec(None, Self::precedence_for_token(Token::Symbol(Symbol::Concat))))
						)
					)
				} else {
					return Ok(expr)
				}
			},
			None => {
				if try!(self.accept(Token::Symbol(Symbol::LeftParenthesis))).is_some() {
					let e = try!(self.parse_expression());
					try!(self.expect(Token::Symbol(Symbol::RightParenthesis)));

					e
				} else if try!(self.accept(Token::Symbol(Symbol::LeftBracket))).is_some() {
					let mut items: std::vec::Vec<Expression> = vec![];
					while try!(self.accept(Token::Symbol(Symbol::RightBracket))).is_none() {
						items.push(try!(self.parse_expression()));
						if self.current_token == Token::Symbol(Symbol::RightBracket) {
							try!(self.accept(Token::Symbol(Symbol::Comma)));
						} else {
							try!(self.expect(Token::Symbol(Symbol::Comma)));
						};
					};

					Expression::Array(
						Box::new(
							ArrayData { items: items }
						)
					)
				} else if try!(self.accept(Token::Symbol(Symbol::Hash))).is_some() {
					Expression::Count(
						Box::new(
							try!(self.parse_expression())
						)
					)
				} else if let Some(string_literal) = try!(self.accept_any(Token::StringLiteral("".to_string()))) {
					match string_literal {
						Token::StringLiteral(s) => {
							Expression::StringLiteral(
								Box::new(
									StringLiteralData { value: s }
								)
							)
						},
						_ => return Err("".to_string()) // Should never happen
					}
				} else if let Some(integer_literal) = try!(self.accept_any(Token::IntegerLiteral(0))) {
					match integer_literal {
						Token::IntegerLiteral(i) => {
							Expression::IntegerLiteral(
								Box::new(
									IntegerLiteralData { value: i }
								)
							)
						},
						_ => return Err("".to_string()) // Should never happen
					}
				} else if let Some(bool_literal) = try!(self.accept_any(Token::BoolLiteral(false))) {
					match bool_literal {
						Token::BoolLiteral(b) => {
							Expression::BoolLiteral(
								Box::new(
									BoolLiteralData { value: b }
								)
							)
						},
						_ => return Err("".to_string()) // Should never happen
					}
				} else if let Some(char_literal) = try!(self.accept_any(Token::CharLiteral('\0'))) {
					match char_literal {
						Token::CharLiteral(c) => {
							Expression::CharLiteral(
								Box::new(
									CharLiteralData { value: c }
								)
							)
						},
						_ => return Err("".to_string()) // Should never happen
					}
				} else if let Some(identifier) = try!(self.accept_any(Token::Identifier("".to_string()))) {
					let mut path: Path = vec![];

					let i = match identifier {
						Token::Identifier(i) => i,
						_ => return Err("".to_string()) // Should never happen
					};

					path.push(
						Box::new(
							PathPart::IdentifierPathPart(
								Box::new(
									IdentifierPathPartData { identifier: i }
								)
							)
						)
					);

					while try!(self.accept(Token::Symbol(Symbol::ColonColon))).is_some() {
						let next_path_token = try!(self.expect_any(Token::Identifier("".to_string())));
						let next_path = match next_path_token {
							Token::Identifier(id) => id,
							_ => return Err("".to_string()) // Should never happen
						};

						path.push(
							Box::new(PathPart::ModulePathPart)
						);

						path.push(
							Box::new(
								PathPart::IdentifierPathPart(
									Box::new(
										IdentifierPathPartData { identifier: next_path }
									)
								)
							)
						);
					}

					if try!(self.accept(Token::Symbol(Symbol::LeftParenthesis))).is_some() {
						Expression::FuncCall(try!(self.parse_func_call(path)))
					} else if try!(self.accept(Token::Symbol(Symbol::LeftBrace))).is_some() {
						Expression::StructInit(try!(self.parse_struct_init(path)))
					} else {
						loop {
							if try!(self.accept(Token::Symbol(Symbol::Dot))).is_some() {
								let next_path_token = try!(self.expect_any(Token::Identifier("".to_string())));
								let next_path = match next_path_token {
									Token::Identifier(id) => id,
									_ => return Err("".to_string()) // Should never happen
								};

								path.push(
									Box::new(PathPart::FieldPathPart)
								);

								path.push(
									Box::new(
										PathPart::IdentifierPathPart(
											Box::new(
												IdentifierPathPartData { identifier: next_path }
											)
										)
									)
								);
							} else if try!(self.accept(Token::Symbol(Symbol::LeftBracket))).is_some() {
								let expr = try!(self.parse_expression());
								try!(self.expect(Token::Symbol(Symbol::RightBracket)));

								path.push(
									Box::new(
										PathPart::IndexPathPart(
											Box::new(
												IndexPathPartData { index: Some(expr) }
											)
										)
									)
								);
							} else {
								break
							};
						}

						Expression::Variable(
							Box::new(
								VariableData { path: path }
							)
						)
					}
				} else {
					return Err(format!("Parser error: unexpected token {:?}, ast: {:#?}", self.current_token, self.ast))
				}
			},
		};

		if Self::precedence_for_token(self.current_token.clone()) > minimum_precedence {
			self.parse_expression_rec(Some(new_expr), minimum_precedence)
		} else {
			Ok(new_expr)
		}
	}

	fn parse_struct_init(&mut self, path: Path) -> Result<Box<StructInitData>, String> {
		let mut fields: std::vec::Vec<Box<StructInitFieldData>> = vec![];
		while try!(self.accept(Token::Symbol(Symbol::RightBrace))).is_none() {
			let field_name_token = try!(self.expect_any(Token::Identifier("".to_string())));
			let field_name = match field_name_token {
				Token::Identifier(s) => s,
				_ => return Err("".to_string()), // Should never happen
			};

			try!(self.expect(Token::Symbol(Symbol::Colon)));

			let field_value = try!(self.parse_expression());

			fields.push(
				Box::new(
					StructInitFieldData {
						name: field_name,
						value: field_value,
					}
				)
			);

			if self.current_token == Token::Symbol(Symbol::RightBrace) {
				try!(self.accept(Token::Symbol(Symbol::Comma)));
			} else {
				try!(self.expect(Token::Symbol(Symbol::Comma)));
			};
		};

		Ok(Box::new(
			StructInitData {
				path: path,
				fields: fields,
			}
		))
	}

	fn parse_type(&mut self) -> Result<Type, String> {
		fn get_builtin_type(path: &Path) -> Option<Type> {
			if path.len() != 1 {
				None
			} else {
				match **path.get(0).unwrap() {
					PathPart::IdentifierPathPart(ref ipp) => {
						match ipp.identifier.as_ref() {
							"int" => Some(Type::IntType),
							"bool" => Some(Type::BoolType),
							"char" => Some(Type::CharType),
							"string" => Some(Type::StringType),
							_ => None,
						}
					},
					_ => None,
				}
			}
		}

		if try!(self.accept(Token::Symbol(Symbol::LeftBracket))).is_some() {
			try!(self.expect(Token::Symbol(Symbol::RightBracket)));
			let inner_type = try!(self.parse_type());

			return Ok(Type::ArrayType(
				Box::new(inner_type)
			))
		};

		let mut path: Path = vec![];
		let type_token = try!(self.expect_any(Token::Identifier("".to_string())));
		match type_token {
			Token::Identifier(i) => path.push(
				Box::new(
					PathPart::IdentifierPathPart(
						Box::new(
							IdentifierPathPartData { identifier: i }
						)
					)
				)
			),
			_ => return Err("".to_string()) // Should never happen
		};

		while try!(self.accept(Token::Symbol(Symbol::ColonColon))).is_some() {
			let next_path_token = try!(self.expect_any(Token::Identifier("".to_string())));
			let next_path = match next_path_token {
				Token::Identifier(id) => id,
				_ => return Err("".to_string()) // Should never happen
			};

			path.push(
				Box::new(PathPart::ModulePathPart)
			);

			path.push(
				Box::new(
					PathPart::IdentifierPathPart(
						Box::new(
							IdentifierPathPartData { identifier: next_path }
						)
					)
				)
			);
		}

		match get_builtin_type(&path) {
			Some(t) => Ok(t),
			None => Ok(Type::StructType(
				Box::new(
					StructTypeData { path: path }
				)
			))
		}
	}

	fn skip_newlines(&mut self) -> Result<(), String> {
		while self.current_token == Token::Symbol(Symbol::NewLine) {
			try!(self.next_token());
		};

		Ok(())
	}

	fn accept(&mut self, token: Token) -> Result<Option<Token>, String> {
		// TODO: use intrinsics with determinants, but unstable for now. Could also use macros
		// around enums to be able to automatically expand them into a match, but would be too
		// invasive.
		let token = match (self.current_token.clone(), token) {
			(Token::EOF, Token::EOF) => Some(self.current_token.clone()),
			(Token::Identifier(ref a), Token::Identifier(ref b)) if (a == b) => Some(self.current_token.clone()),
			(Token::Keyword(ref a), Token::Keyword(ref b)) if (a == b) => Some(self.current_token.clone()),
			(Token::StringLiteral(ref a), Token::StringLiteral(ref b)) if (a == b) => Some(self.current_token.clone()),
			(Token::CharLiteral(ref a), Token::CharLiteral(ref b)) if (a == b) => Some(self.current_token.clone()),
			(Token::IntegerLiteral(ref a), Token::IntegerLiteral(ref b)) if (a == b) => Some(self.current_token.clone()),
			(Token::FloatLiteral(ref a), Token::FloatLiteral(ref b)) if (a == b) => Some(self.current_token.clone()),
			(Token::BoolLiteral(ref a), Token::BoolLiteral(ref b)) if (a == b) => Some(self.current_token.clone()),
			(Token::Symbol(ref a), Token::Symbol(ref b)) if (a == b) => Some(self.current_token.clone()),
			(_, _) => None,
		};

		if token.is_some() {
			try!(self.next_token());
			try!(self.skip_newlines());
		}

		Ok(token)
	}

	fn accept_any(&mut self, token: Token) -> Result<Option<Token>, String> {
		// TODO: same as accept()
		let token = match (self.current_token.clone(), token) {
			(Token::EOF, Token::EOF) => Some(self.current_token.clone()),
			(Token::Identifier(_), Token::Identifier(_)) => Some(self.current_token.clone()),
			(Token::Keyword(_), Token::Keyword(_)) => Some(self.current_token.clone()),
			(Token::StringLiteral(_), Token::StringLiteral(_))  => Some(self.current_token.clone()),
			(Token::CharLiteral(_), Token::CharLiteral(_)) => Some(self.current_token.clone()),
			(Token::IntegerLiteral(_), Token::IntegerLiteral(_))  => Some(self.current_token.clone()),
			(Token::FloatLiteral(_), Token::FloatLiteral(_)) => Some(self.current_token.clone()),
			(Token::BoolLiteral(_), Token::BoolLiteral(_)) => Some(self.current_token.clone()),
			(Token::Symbol(_), Token::Symbol(_)) => Some(self.current_token.clone()),
			(_, _) => None,
		};

		if token.is_some() {
			try!(self.next_token());
			try!(self.skip_newlines());
		}

		Ok(token)
	}

	fn expect(&mut self, token: Token) -> Result<Token, String> {
		match try!(self.accept(token.clone())) {
			Some(t) => Ok(t),
			None => return Err(format!("Parser error: expected {:?}", token)),
		}
	}

	fn expect_any(&mut self, token: Token) -> Result<Token, String> {
		match try!(self.accept_any(token.clone())) {
			Some(t) => Ok(t),
			None => return Err(format!("Parser error: expected {:?}", token)),
		}
	}

	fn next_token(&mut self) -> Result<Token, String> {
		match self.reader.next_token() {
			Ok(t) => {
				self.current_token = t;
				Ok(self.current_token.clone())
			},
			Err(s) => return Err(s)
		}
	}
}
