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

	pub fn parse(&mut self) -> &Ast {
		self.next_token();
		while self.current_token != Token::EOF {
			let statement = self.parse_statement();
			self.ast.statements.push(statement);
		}

		&self.ast
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

	fn parse_statement(&mut self) -> Statement {
		if self.accept(Token::Keyword(Keyword::Import)).is_some() {
			Statement::Import(self.parse_import())
		} else if self.accept(Token::Keyword(Keyword::Package)).is_some() {
			Statement::Package(self.parse_package())
		} else if self.accept(Token::Keyword(Keyword::Func)).is_some() {
			Statement::FuncDecl(self.parse_func_decl())
		} else if self.accept(Token::Keyword(Keyword::Struct)).is_some() {
			Statement::StructDecl(self.parse_struct_decl())
		} else {
			panic!("Parser error: unexpected token {:?}", self.current_token)
		}
	}

	fn parse_package(&mut self) -> Box<PackageData> {
		let name_token = self.expect_any(Token::Identifier("".to_string()));
		match name_token {
			Token::Identifier(n) => {
				Box::new(
					PackageData { name: n }
				)
			},
			_ => panic!(), // Should never happen
		}
	}

	fn parse_import(&mut self) -> Box<ImportData> {
		let path_token = self.expect_any(Token::StringLiteral("".to_string()));
		match path_token {
			Token::StringLiteral(p) => {
				Box::new(
					ImportData { path: p }
				)
			},
			_ => panic!(), // Should never happen
		}
	}

	fn parse_func_decl(&mut self) -> Box<FuncDeclData> {
		let name_token = self.expect_any(Token::Identifier("".to_string()));
		let name = match name_token {
			Token::Identifier(s) => s,
			_ => panic!(), // Should never happen
		};

		self.expect(Token::Symbol(Symbol::LeftParenthesis));

		let mut params: std::vec::Vec<Box<FuncDeclParamData>> = vec![];
		while self.accept(Token::Symbol(Symbol::RightParenthesis)).is_none() {
			let arg_name_token = self.expect_any(Token::Identifier("".to_string()));
			let arg_name = match arg_name_token {
				Token::Identifier(s) => s,
				_ => panic!(), // Should never happen
			};

			self.expect(Token::Symbol(Symbol::Colon));

			let arg_type = self.parse_type();

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
				self.accept(Token::Symbol(Symbol::Comma));
			} else {
				self.expect(Token::Symbol(Symbol::Comma));
			};
		};

		let mut return_type: Option<Type> = None;
		if self.accept(Token::Symbol(Symbol::Return)).is_some() {
			return_type = Some(self.parse_type());
		}

		self.expect(Token::Symbol(Symbol::LeftBrace));

		let mut statements: std::vec::Vec<BlockStatement> = vec![];
		while self.accept(Token::Symbol(Symbol::RightBrace)).is_none() {
			statements.push(self.parse_block_statement());
		};

		Box::new(
			FuncDeclData {
				name: name,
				return_type: return_type,
				parameters: params,
				statements: statements,
			}
		)
	}

	fn parse_struct_decl(&mut self) -> Box<StructDeclData> {
		let name_token = self.expect_any(Token::Identifier("".to_string()));
		let name = match name_token {
			Token::Identifier(s) => s,
			_ => panic!(), // Should never happen
		};

		self.expect(Token::Symbol(Symbol::LeftBrace));

		let mut fields: std::vec::Vec<Box<StructFieldData>> = vec![];
		while self.accept(Token::Symbol(Symbol::RightBrace)).is_none() {
			let field_name_token = self.expect_any(Token::Identifier("".to_string()));
			let field_name = match field_name_token {
				Token::Identifier(s) => s,
				_ => panic!(), // Should never happen
			};

			self.expect(Token::Symbol(Symbol::Colon));

			let field_type = self.parse_type();

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
				self.accept(Token::Symbol(Symbol::Comma));
			} else {
				self.expect(Token::Symbol(Symbol::Comma));
			};
		};

		Box::new(
			StructDeclData {
				name: name,
				fields: fields,
			}
		)
	}

	fn parse_block_statement(&mut self) -> BlockStatement {
		if self.accept(Token::Keyword(Keyword::Var)).is_some() {
			BlockStatement::VarDecl(self.parse_var_decl())
		} else if self.accept(Token::Keyword(Keyword::If)).is_some() {
			BlockStatement::If(self.parse_if())
		} else if self.accept(Token::Keyword(Keyword::While)).is_some() {
			BlockStatement::While(self.parse_while())
		} else if self.accept(Token::Keyword(Keyword::Return)).is_some() {
			BlockStatement::Return(self.parse_return())
		} else if let Some(identifier) = self.accept_any(Token::Identifier("".to_string())) {
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

					while self.accept(Token::Symbol(Symbol::ColonColon)).is_some() {
						let next_path_token = self.expect_any(Token::Identifier("".to_string()));
						let next_path = match next_path_token {
							Token::Identifier(id) => id,
							_ => panic!() // Should never happen
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

					if self.accept(Token::Symbol(Symbol::LeftParenthesis)).is_some() {
						BlockStatement::FuncCall(self.parse_func_call(path))
					} else {
						loop {
							if self.accept(Token::Symbol(Symbol::Dot)).is_some() {
								let next_path_token = self.expect_any(Token::Identifier("".to_string()));
								let next_path = match next_path_token {
									Token::Identifier(id) => id,
									_ => panic!() // Should never happen
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
							} else if self.accept(Token::Symbol(Symbol::LeftBracket)).is_some() {
								let index = if self.accept(Token::Symbol(Symbol::RightBracket)).is_some() {
									None
								} else {
									let expr = self.parse_expression();
									self.expect(Token::Symbol(Symbol::RightBracket));

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

						if self.accept(Token::Symbol(Symbol::Equal)).is_some() {
							BlockStatement::VarAssignment(self.parse_var_assignment(path))
						} else {
							panic!("Parser error: unexpected token {:?}", self.current_token)
						}
					}
				},
				_ => panic!(), // Should never happen
			}
		} else {
			panic!("Parser error: unexpected token {:?}", self.current_token)
		}
	}

	fn parse_return(&mut self) -> Box<ReturnData> {
		Box::new(
			ReturnData { value: self.parse_expression() }
		)
	}

	fn parse_if(&mut self) -> Box<IfData> {
		let condition = self.parse_expression();

		self.expect(Token::Symbol(Symbol::LeftBrace));

		let mut statements: std::vec::Vec<BlockStatement> = vec![];
		while self.accept(Token::Symbol(Symbol::RightBrace)).is_none() {
			statements.push(self.parse_block_statement());
		};

		Box::new(
			IfData {
				condition: condition,
				statements: statements,
			}
		)
	}

	fn parse_while(&mut self) -> Box<WhileData> {
		let condition = self.parse_expression();

		self.expect(Token::Symbol(Symbol::LeftBrace));

		let mut statements: std::vec::Vec<BlockStatement> = vec![];
		while self.accept(Token::Symbol(Symbol::RightBrace)).is_none() {
			statements.push(self.parse_block_statement());
		};

		Box::new(
			WhileData {
				condition: condition,
				statements: statements,
			}
		)
	}

	fn parse_func_call(&mut self, path: Path) -> Box<FuncCallData> {
		let mut args: std::vec::Vec<Box<FuncCallArgData>> = vec![];
		while self.accept(Token::Symbol(Symbol::RightParenthesis)).is_none() {
			args.push(
				Box::new(
					FuncCallArgData { value: self.parse_expression() }
				)
			);
			if self.current_token == Token::Symbol(Symbol::RightParenthesis) {
				self.accept(Token::Symbol(Symbol::Comma));
			} else {
				self.expect(Token::Symbol(Symbol::Comma));
			};
		};

		Box::new(
			FuncCallData {
				path: path,
				arguments: args,
			}
		)
	}

	fn parse_var_decl(&mut self) -> Box<VarDeclData> {
		let name_token = self.expect_any(Token::Identifier("".to_string()));
		let name = match name_token {
			Token::Identifier(i) => i,
			_ => panic!(), // Should never happen
		};

		self.expect(Token::Symbol(Symbol::Colon));

		let var_type = self.parse_type();

		let value = if self.accept(Token::Symbol(Symbol::Equal)).is_some() {
			Some(self.parse_expression())
		} else {
			None
		};

		Box::new(
			VarDeclData {
				name: name,
				var_type: var_type,
				value: value
			}
		)
	}

	fn parse_var_assignment(&mut self, path: Path) -> Box<VarAssignmentData> {
		let expression = self.parse_expression();
		Box::new(
			VarAssignmentData {
				path: path,
				value: expression,
			}
		)
	}

	fn parse_expression(&mut self) -> Expression {
		self.parse_expression_rec(None, 0)
	}

	fn parse_expression_rec(&mut self, prev_expr: Option<Expression>, minimum_precedence: u8) -> Expression {
		let new_expr = match prev_expr {
			Some(expr) => {
				if self.accept(Token::Symbol(Symbol::Plus)).is_some() {
					Expression::Addition(
						Box::new(
							expr
						),
						Box::new(
							self.parse_expression_rec(None, Self::precedence_for_token(Token::Symbol(Symbol::Plus)))
						)
					)
				} else if self.accept(Token::Symbol(Symbol::Minus)).is_some() {
					Expression::Substraction(
						Box::new(
							expr
						),
						Box::new(
							self.parse_expression_rec(None, Self::precedence_for_token(Token::Symbol(Symbol::Minus)))
						)
					)
				} else if self.accept(Token::Symbol(Symbol::Times)).is_some() {
					Expression::Multiplication(
						Box::new(
							expr
						),
						Box::new(
							self.parse_expression_rec(None, Self::precedence_for_token(Token::Symbol(Symbol::Times)))
						)
					)
				} else if self.accept(Token::Symbol(Symbol::Over)).is_some() {
					Expression::Division(
						Box::new(
							expr
						),
						Box::new(
							self.parse_expression_rec(None, Self::precedence_for_token(Token::Symbol(Symbol::Over)))
						)
					)
				} else if self.accept(Token::Symbol(Symbol::Modulo)).is_some() {
					Expression::Modulo(
						Box::new(
							expr
						),
						Box::new(
							self.parse_expression_rec(None, Self::precedence_for_token(Token::Symbol(Symbol::Modulo)))
						)
					)
				} else if self.accept(Token::Symbol(Symbol::EqualEqual)).is_some() {
					Expression::Equality(
						Box::new(
							expr
						),
						Box::new(
							self.parse_expression_rec(None, Self::precedence_for_token(Token::Symbol(Symbol::EqualEqual)))
						)
					)
				} else if self.accept(Token::Symbol(Symbol::NotEqual)).is_some() {
					Expression::Inequality(
						Box::new(
							expr
						),
						Box::new(
							self.parse_expression_rec(None, Self::precedence_for_token(Token::Symbol(Symbol::NotEqual)))
						)
					)
				} else if self.accept(Token::Symbol(Symbol::Concat)).is_some() {
					Expression::Concatenation(
						Box::new(
							expr
						),
						Box::new(
							self.parse_expression_rec(None, Self::precedence_for_token(Token::Symbol(Symbol::Concat)))
						)
					)
				} else {
					return expr
				}
			},
			None => {
				if self.accept(Token::Symbol(Symbol::LeftParenthesis)).is_some() {
					let e = self.parse_expression();
					self.expect(Token::Symbol(Symbol::RightParenthesis));

					e
				} else if self.accept(Token::Symbol(Symbol::LeftBracket)).is_some() {
					let mut items: std::vec::Vec<Expression> = vec![];
					while self.accept(Token::Symbol(Symbol::RightBracket)).is_none() {
						items.push(self.parse_expression());
						if self.current_token == Token::Symbol(Symbol::RightBracket) {
							self.accept(Token::Symbol(Symbol::Comma));
						} else {
							self.expect(Token::Symbol(Symbol::Comma));
						};
					};

					Expression::Array(
						Box::new(
							ArrayData { items: items }
						)
					)
				} else if self.accept(Token::Symbol(Symbol::Hash)).is_some() {
					Expression::Count(
						Box::new(
							self.parse_expression()
						)
					)
				} else if let Some(string_literal) = self.accept_any(Token::StringLiteral("".to_string())) {
					match string_literal {
						Token::StringLiteral(s) => {
							Expression::StringLiteral(
								Box::new(
									StringLiteralData { value: s }
								)
							)
						},
						_ => panic!() // Should never happen
					}
				} else if let Some(integer_literal) = self.accept_any(Token::IntegerLiteral(0)) {
					match integer_literal {
						Token::IntegerLiteral(i) => {
							Expression::IntegerLiteral(
								Box::new(
									IntegerLiteralData { value: i }
								)
							)
						},
						_ => panic!() // Should never happen
					}
				} else if let Some(bool_literal) = self.accept_any(Token::BoolLiteral(false)) {
					match bool_literal {
						Token::BoolLiteral(b) => {
							Expression::BoolLiteral(
								Box::new(
									BoolLiteralData { value: b }
								)
							)
						},
						_ => panic!() // Should never happen
					}
				} else if let Some(char_literal) = self.accept_any(Token::CharLiteral('\0')) {
					match char_literal {
						Token::CharLiteral(c) => {
							Expression::CharLiteral(
								Box::new(
									CharLiteralData { value: c }
								)
							)
						},
						_ => panic!() // Should never happen
					}
				} else if let Some(identifier) = self.accept_any(Token::Identifier("".to_string())) {
					let mut path: Path = vec![];

					let i = match identifier {
						Token::Identifier(i) => i,
						_ => panic!() // Should never happen
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

					while self.accept(Token::Symbol(Symbol::ColonColon)).is_some() {
						let next_path_token = self.expect_any(Token::Identifier("".to_string()));
						let next_path = match next_path_token {
							Token::Identifier(id) => id,
							_ => panic!() // Should never happen
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

					if self.accept(Token::Symbol(Symbol::LeftParenthesis)).is_some() {
						Expression::FuncCall(self.parse_func_call(path))
					} else {
						loop {
							if self.accept(Token::Symbol(Symbol::Dot)).is_some() {
								let next_path_token = self.expect_any(Token::Identifier("".to_string()));
								let next_path = match next_path_token {
									Token::Identifier(id) => id,
									_ => panic!() // Should never happen
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
							} else if self.accept(Token::Symbol(Symbol::LeftBracket)).is_some() {
								let expr = self.parse_expression();
								self.expect(Token::Symbol(Symbol::RightBracket));

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
					panic!("Parser error: unexpected token {:?}, ast: {:#?}", self.current_token, self.ast)
				}
			},
		};

		if Self::precedence_for_token(self.current_token.clone()) > minimum_precedence {
			self.parse_expression_rec(Some(new_expr), minimum_precedence)
		} else {
			new_expr
		}
	}

	fn parse_type(&mut self) -> Type {
		if self.accept(Token::Symbol(Symbol::LeftBracket)).is_some() {
			self.expect(Token::Symbol(Symbol::RightBracket));
			let inner_type = self.parse_type();

			return Type::ArrayType(
				Box::new(inner_type)
			)
		};

		let mut path: Path = vec![];
		let type_token = self.expect_any(Token::Identifier("".to_string()));
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
			_ => panic!() // Should never happen
		};

		while self.accept(Token::Symbol(Symbol::ColonColon)).is_some() {
			let next_path_token = self.expect_any(Token::Identifier("".to_string()));
			let next_path = match next_path_token {
				Token::Identifier(id) => id,
				_ => panic!() // Should never happen
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



		Type::StructType(
			Box::new(
				StructTypeData { path: path }
			)
		)
	}

	fn skip_newlines(&mut self) {
		while self.current_token == Token::Symbol(Symbol::NewLine) {
			self.next_token();
		};
	}

	fn accept(&mut self, token: Token) -> Option<Token> {
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
			self.next_token();
			self.skip_newlines();
		}

		token
	}

	fn accept_any(&mut self, token: Token) -> Option<Token> {
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
			self.next_token();
			self.skip_newlines();
		}

		token
	}

	fn expect(&mut self, token: Token) -> Token {
		match self.accept(token.clone()) {
			Some(t) => t,
			None => panic!("Parser error: expected {:?}", token),
		}
	}

	fn expect_any(&mut self, token: Token) -> Token {
		match self.accept_any(token.clone()) {
			Some(t) => t,
			None => panic!("Parser error: expected {:?}", token),
		}
	}

	fn next_token(&mut self) -> Token {
		match self.reader.next_token() {
			Ok(t) => {
				self.current_token = t;
				self.current_token.clone()
			},
			Err(s) => panic!(s)
		}
	}
}
