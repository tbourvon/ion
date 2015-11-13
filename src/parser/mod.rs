pub mod ast;

use std;
use lexer::SToken;
use lexer::Token;
use lexer::Span;
use lexer::Reader;
use lexer::Keyword;
use lexer::Symbol;
use self::ast::*;

pub struct Parser<'a> {
	reader: &'a mut Reader<'a>,
	ast: Box<Ast>,
	last_sp: Span,
	current_token: SToken,
}

impl<'a> Parser<'a> {
	pub fn new<'b>(reader: &'b mut Reader<'b>) -> Parser<'b> {
		Parser {
			reader: reader,
			ast: Box::new(Ast::new()),
			last_sp: Span {
				scol: 0,
				srow: 0,
				ecol: 0,
				erow: 0,
			},
			current_token: SToken {
				tok: Token::EOF,
				sp: Span {
					scol: 0,
					srow: 0,
					ecol: 0,
					erow: 0,
				}
			},
		}
	}

	pub fn parse(&mut self) -> Result<&Ast, String> {
		try!(self.next_token());
		while self.current_token.tok != Token::EOF {
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
					Symbol::Hash => 4,
					_ => 0
				}
			},
			_ => 0
		}
	}

	fn parse_statement(&mut self) -> Result<Statement, String> {
		if let Some(t) = try!(self.accept(Token::Keyword(Keyword::Import))) {
			Ok(Statement::Import(try!(self.parse_import(t.sp))))
		} else if let Some(t) = try!(self.accept(Token::Keyword(Keyword::Package))) {
			Ok(Statement::Package(try!(self.parse_package(t.sp))))
		} else if let Some(t) = try!(self.accept(Token::Keyword(Keyword::Func))) {
			Ok(Statement::FuncDecl(try!(self.parse_func_decl(t.sp))))
		} else if let Some(t) = try!(self.accept(Token::Keyword(Keyword::Struct))) {
			Ok(Statement::StructDecl(try!(self.parse_struct_decl(t.sp))))
		} else {
			Err(format!("Parser error: unexpected token {:?}, {:?}", self.current_token.tok, self.current_token.sp))
		}
	}

	fn parse_package(&mut self, start_sp: Span) -> Result<Box<PackageData>, String> {
		let name_token = try!(self.expect_any(Token::Identifier("".to_string())));
		match name_token.tok {
			Token::Identifier(n) => {
				Ok(Box::new(
					PackageData {
						span: Span::concat(start_sp, name_token.sp),
						name: n,
					}
				))
			},
			_ => Err("".to_string()), // Should never happen
		}
	}

	fn parse_import(&mut self, start_sp: Span) ->  Result<Box<ImportData>, String> {
		let path_token = try!(self.expect_any(Token::StringLiteral("".to_string())));
		match path_token.tok {
			Token::StringLiteral(p) => {
				Ok(Box::new(
					ImportData {
						span: Span::concat(start_sp, path_token.sp),
						path: p,
					}
				))
			},
			_ => Err("".to_string()), // Should never happen
		}
	}

	fn parse_func_decl(&mut self, start_sp: Span) -> Result<Box<FuncDeclData>, String> {
		let name_token = try!(self.expect_any(Token::Identifier("".to_string())));
		let name = match name_token.tok {
			Token::Identifier(s) => s,
			_ => return Err("".to_string()), // Should never happen
		};

		try!(self.expect(Token::Symbol(Symbol::LeftParenthesis)));

		let mut params: std::vec::Vec<Box<FuncDeclParamData>> = vec![];
		while try!(self.accept(Token::Symbol(Symbol::RightParenthesis))).is_none() {
			let arg_name_token = try!(self.expect_any(Token::Identifier("".to_string())));
			let arg_name = match arg_name_token.tok {
				Token::Identifier(s) => s,
				_ => return Err("".to_string()), // Should never happen
			};

			try!(self.expect(Token::Symbol(Symbol::Colon)));

			let arg_type = try!(self.parse_type());

			params.push(
				Box::new(
					FuncDeclParamData {
						span: Span::concat(arg_name_token.sp, self.last_sp.clone()),
						name: arg_name,
						param_type: arg_type,
						default_value: None,
					}
				)
			);

			if self.current_token.tok == Token::Symbol(Symbol::RightParenthesis) {
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
				span: Span::concat(start_sp, self.last_sp.clone()),
				name: name,
				return_type: return_type,
				parameters: params,
				statements: statements,
			}
		))
	}

	fn parse_struct_decl(&mut self, start_sp: Span) -> Result<Box<StructDeclData>, String> {
		let name_token = try!(self.expect_any(Token::Identifier("".to_string())));
		let name = match name_token.tok {
			Token::Identifier(s) => s,
			_ => return Err("".to_string()), // Should never happen
		};

		try!(self.expect(Token::Symbol(Symbol::LeftBrace)));

		let mut fields: std::vec::Vec<Box<StructFieldData>> = vec![];
		while try!(self.accept(Token::Symbol(Symbol::RightBrace))).is_none() {
			let field_name_token = try!(self.expect_any(Token::Identifier("".to_string())));
			let field_name = match field_name_token.tok {
				Token::Identifier(s) => s,
				_ => return Err("".to_string()), // Should never happen
			};

			try!(self.expect(Token::Symbol(Symbol::Colon)));

			let field_type = try!(self.parse_type());

			fields.push(
				Box::new(
					StructFieldData {
						span: Span::concat(field_name_token.sp, self.last_sp.clone()),
						name: field_name,
						field_type: field_type,
						default_value: None,
					}
				)
			);

			if self.current_token.tok == Token::Symbol(Symbol::RightBrace) {
				try!(self.accept(Token::Symbol(Symbol::Comma)));
			} else {
				try!(self.expect(Token::Symbol(Symbol::Comma)));
			};
		};

		Ok(Box::new(
			StructDeclData {
				span: Span::concat(start_sp, self.last_sp.clone()),
				name: name,
				fields: fields,
			}
		))
	}

	fn parse_block_statement(&mut self, return_type: Option<Type>) -> Result<BlockStatement, String> {
		if let Some(t) = try!(self.accept(Token::Keyword(Keyword::Var))) {
			Ok(BlockStatement::VarDecl(try!(self.parse_var_decl(t.sp))))
		} else if let Some(t) = try!(self.accept(Token::Keyword(Keyword::If))) {
			Ok(BlockStatement::If(try!(self.parse_if(return_type, t.sp))))
		} else if let Some(t) = try!(self.accept(Token::Keyword(Keyword::While))) {
			Ok(BlockStatement::While(try!(self.parse_while(return_type, t.sp))))
		} else if let Some(t) = try!(self.accept(Token::Keyword(Keyword::For))) {
			Ok(BlockStatement::ForIn(try!(self.parse_forin(return_type, t.sp))))
		} else if let Some(t) = try!(self.accept(Token::Keyword(Keyword::Return))) {
			Ok(BlockStatement::Return(try!(self.parse_return(return_type, t.sp))))
		} else if let Some(identifier) = try!(self.accept_any(Token::Identifier("".to_string()))) {
			match identifier.tok {
				Token::Identifier(i) => {
					let mut path: Path = vec![];

					path.push(
						Box::new(
							PathPart::IdentifierPathPart(
								Box::new(
									IdentifierPathPartData {
										span: identifier.sp.clone(),
										identifier: i
									}
								)
							)
						)
					);

					while try!(self.accept(Token::Symbol(Symbol::ColonColon))).is_some() {
						let next_path_token = try!(self.expect_any(Token::Identifier("".to_string())));
						let next_path = match next_path_token.tok {
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
										IdentifierPathPartData {
											span: next_path_token.sp,											identifier: next_path
										}
									)
								)
							)
						);
					}

					if try!(self.accept(Token::Symbol(Symbol::LeftParenthesis))).is_some() {
						Ok(BlockStatement::FuncCall(try!(self.parse_func_call(path, identifier.sp))))
					} else {
						loop {
							if try!(self.accept(Token::Symbol(Symbol::Dot))).is_some() {
								let next_path_token = try!(self.expect_any(Token::Identifier("".to_string())));
								let next_path = match next_path_token.tok {
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
												IdentifierPathPartData {
													span: next_path_token.sp,
													identifier: next_path
												}
											)
										)
									)
								);
							} else if try!(self.accept(Token::Symbol(Symbol::LeftBracket))).is_some() {
								let st_sp = self.last_sp.clone();

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
												IndexPathPartData {
													span: Span::concat(st_sp, self.last_sp.clone()),
													index: index
												}
											)
										)
									)
								);
							} else {
								break
							};
						}

						if try!(self.accept(Token::Symbol(Symbol::Equal))).is_some() {
							Ok(BlockStatement::VarAssignment(try!(self.parse_var_assignment(path, identifier.sp))))
						} else {
							return Err(format!("Parser error: unexpected token {:?}, {:?}", self.current_token.tok, self.current_token.sp))
						}
					}
				},
				_ => return Err("".to_string()), // Should never happen
			}
		} else {
			return Err(format!("Parser error: unexpected token {:?}, {:?}", self.current_token.tok, self.current_token.sp))
		}
	}

	fn parse_forin(&mut self, return_type: Option<Type>, start_sp: Span) -> Result<Box<ForInData>, String> {
		let element_token = try!(self.expect_any(Token::Identifier("".to_string())));
		let element_name = match element_token.tok {
			Token::Identifier(s) => s,
			_ => return Err("".to_string()), // Should never happen
		};

		try!(self.expect(Token::Keyword(Keyword::In)));

		let collection = try!(self.parse_expression());

		try!(self.expect(Token::Symbol(Symbol::LeftBrace)));

		let mut statements: std::vec::Vec<BlockStatement> = vec![];
		while try!(self.accept(Token::Symbol(Symbol::RightBrace))).is_none() {
			statements.push(try!(self.parse_block_statement(return_type.clone())));
		};

		Ok(Box::new(
			ForInData {
				span: Span::concat(start_sp, self.last_sp.clone()),
				element_name: element_name,
				collection: collection,
				statements: statements,
			}
		))
	}

	fn parse_return(&mut self, return_type: Option<Type>, start_sp: Span) -> Result<Box<ReturnData>, String> {
		Ok(Box::new(
			ReturnData {
				value: match return_type {
					Some(_) => Some(try!(self.parse_expression())),
					None => None,
				},
				span: Span::concat(start_sp, self.last_sp.clone()),
				expected_type: return_type,
			}
		))
	}

	fn parse_if(&mut self, return_type: Option<Type>, start_sp: Span) -> Result<Box<IfData>, String> {
		let condition = try!(self.parse_expression());

		try!(self.expect(Token::Symbol(Symbol::LeftBrace)));

		let mut if_statements: std::vec::Vec<BlockStatement> = vec![];
		while try!(self.accept(Token::Symbol(Symbol::RightBrace))).is_none() {
			if_statements.push(try!(self.parse_block_statement(return_type.clone())));
		};

		let mut else_statements_opt: Option<std::vec::Vec<BlockStatement>> = None;
		if try!(self.accept(Token::Keyword(Keyword::Else))).is_some() {
			let mut else_statements: std::vec::Vec<BlockStatement> = vec![];
			if let Some(t) = try!(self.accept(Token::Keyword(Keyword::If))) {
				else_statements.push(BlockStatement::If(try!(self.parse_if(return_type.clone(), t.sp))));
			} else {
				try!(self.expect(Token::Symbol(Symbol::LeftBrace)));
				while try!(self.accept(Token::Symbol(Symbol::RightBrace))).is_none() {
					else_statements.push(try!(self.parse_block_statement(return_type.clone())));
				};
			}

			else_statements_opt = Some(else_statements);
		}

		Ok(Box::new(
			IfData {
				span: Span::concat(start_sp, self.last_sp.clone()),
				condition: condition,
				if_statements: if_statements,
				else_statements: else_statements_opt,
			}
		))
	}

	fn parse_while(&mut self, return_type: Option<Type>, start_sp: Span) -> Result<Box<WhileData>, String> {
		let condition = try!(self.parse_expression());

		try!(self.expect(Token::Symbol(Symbol::LeftBrace)));

		let mut statements: std::vec::Vec<BlockStatement> = vec![];
		while try!(self.accept(Token::Symbol(Symbol::RightBrace))).is_none() {
			statements.push(try!(self.parse_block_statement(return_type.clone())));
		};

		Ok(Box::new(
			WhileData {
				span: Span::concat(start_sp, self.last_sp.clone()),
				condition: condition,
				statements: statements,
			}
		))
	}

	fn parse_func_call(&mut self, path: Path, start_sp: Span) -> Result<Box<FuncCallData>, String> {
		let mut args: std::vec::Vec<Box<FuncCallArgData>> = vec![];
		while try!(self.accept(Token::Symbol(Symbol::RightParenthesis))).is_none() {
			let sp = self.current_token.sp.clone();

			args.push(
				Box::new(
					FuncCallArgData {
						value: try!(self.parse_expression()),
						span: Span::concat(sp, self.last_sp.clone())
					}
				)
			);
			if self.current_token.tok == Token::Symbol(Symbol::RightParenthesis) {
				try!(self.accept(Token::Symbol(Symbol::Comma)));
			} else {
				try!(self.expect(Token::Symbol(Symbol::Comma)));
			};
		};

		Ok(Box::new(
			FuncCallData {
				span: Span::concat(start_sp, self.last_sp.clone()),
				path: path,
				arguments: args,
			}
		))
	}

	fn parse_var_decl(&mut self, start_sp: Span) -> Result<Box<VarDeclData>, String> {
		let name_token = try!(self.expect_any(Token::Identifier("".to_string())));
		let name = match name_token.tok {
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
				span: Span::concat(start_sp, self.last_sp.clone()),
				name: name,
				var_type: var_type,
				value: value
			}
		))
	}

	fn parse_var_assignment(&mut self, path: Path, start_sp: Span) -> Result<Box<VarAssignmentData>, String> {
		let expression = try!(self.parse_expression());
		Ok(Box::new(
			VarAssignmentData {
				span: Span::concat(start_sp, self.last_sp.clone()),
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
				} else if let Some(h) = try!(self.accept(Token::Symbol(Symbol::Hash))) {
					Expression::Count(
						Box::new(
							try!(self.parse_expression_rec(None, Self::precedence_for_token(Token::Symbol(Symbol::Hash))))
						),
						h.sp
					)
				} else if let Some(lb) = try!(self.accept(Token::Symbol(Symbol::LeftBracket))) {
					let mut items: std::vec::Vec<Expression> = vec![];
					while try!(self.accept(Token::Symbol(Symbol::RightBracket))).is_none() {
						items.push(try!(self.parse_expression()));
						if self.current_token.tok == Token::Symbol(Symbol::RightBracket) {
							try!(self.accept(Token::Symbol(Symbol::Comma)));
						} else {
							try!(self.expect(Token::Symbol(Symbol::Comma)));
						};
					};

					Expression::Array(
						Box::new(
							ArrayData {
								span: Span::concat(lb.sp, self.last_sp.clone()),
								items: items
							}
						)
					)
				} else if let Some(n) = try!(self.accept(Token::Keyword(Keyword::New))) {
					let identifier = try!(self.expect_any(Token::Identifier("".to_string())));
					let mut path: Path = vec![];

					let i = match identifier.tok {
						Token::Identifier(i) => i,
						_ => return Err("".to_string()) // Should never happen
					};

					path.push(
						Box::new(
							PathPart::IdentifierPathPart(
								Box::new(
									IdentifierPathPartData {
										span: identifier.sp,
										identifier: i
									}
								)
							)
						)
					);

					while try!(self.accept(Token::Symbol(Symbol::ColonColon))).is_some() {
						let next_path_token = try!(self.expect_any(Token::Identifier("".to_string())));
						let next_path = match next_path_token.tok {
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
										IdentifierPathPartData {
											span: next_path_token.sp,
											identifier: next_path
										}
									)
								)
							)
						);
					}

					try!(self.expect(Token::Symbol(Symbol::LeftBrace)));

					Expression::StructInit(try!(self.parse_struct_init(path, n.sp)))
				} else if let Some(string_literal) = try!(self.accept_any(Token::StringLiteral("".to_string()))) {
					match string_literal.tok {
						Token::StringLiteral(s) => {
							Expression::StringLiteral(
								Box::new(
									StringLiteralData {
										span: string_literal.sp,
										value: s
									}
								)
							)
						},
						_ => return Err("".to_string()) // Should never happen
					}
				} else if let Some(integer_literal) = try!(self.accept_any(Token::IntegerLiteral(0))) {
					match integer_literal.tok {
						Token::IntegerLiteral(i) => {
							Expression::IntegerLiteral(
								Box::new(
									IntegerLiteralData {
										span: integer_literal.sp,
										value: i
									}
								)
							)
						},
						_ => return Err("".to_string()) // Should never happen
					}
				} else if let Some(bool_literal) = try!(self.accept_any(Token::BoolLiteral(false))) {
					match bool_literal.tok {
						Token::BoolLiteral(b) => {
							Expression::BoolLiteral(
								Box::new(
									BoolLiteralData {
										span: bool_literal.sp,
										value: b
									}
								)
							)
						},
						_ => return Err("".to_string()) // Should never happen
					}
				} else if let Some(char_literal) = try!(self.accept_any(Token::CharLiteral('\0'))) {
					match char_literal.tok {
						Token::CharLiteral(c) => {
							Expression::CharLiteral(
								Box::new(
									CharLiteralData {
										span: char_literal.sp,
										value: c
									}
								)
							)
						},
						_ => return Err("".to_string()) // Should never happen
					}
				} else if let Some(identifier) = try!(self.accept_any(Token::Identifier("".to_string()))) {
					let mut path: Path = vec![];

					let i = match identifier.tok {
						Token::Identifier(i) => i,
						_ => return Err("".to_string()) // Should never happen
					};

					path.push(
						Box::new(
							PathPart::IdentifierPathPart(
								Box::new(
									IdentifierPathPartData {
										span: identifier.sp.clone(),
										identifier: i
									}
								)
							)
						)
					);

					while try!(self.accept(Token::Symbol(Symbol::ColonColon))).is_some() {
						let next_path_token = try!(self.expect_any(Token::Identifier("".to_string())));
						let next_path = match next_path_token.tok {
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
										IdentifierPathPartData {
											span: next_path_token.sp,
											identifier: next_path
										}
									)
								)
							)
						);
					}

					if try!(self.accept(Token::Symbol(Symbol::LeftParenthesis))).is_some() {
						Expression::FuncCall(try!(self.parse_func_call(path, identifier.sp)))
					} else {
						loop {
							if try!(self.accept(Token::Symbol(Symbol::Dot))).is_some() {
								let next_path_token = try!(self.expect_any(Token::Identifier("".to_string())));
								let next_path = match next_path_token.tok {
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
												IdentifierPathPartData {
													span: next_path_token.sp,
													identifier: next_path
												}
											)
										)
									)
								);
							} else if let Some(lb) =  try!(self.accept(Token::Symbol(Symbol::LeftBracket))) {
								let expr = try!(self.parse_expression());
								try!(self.expect(Token::Symbol(Symbol::RightBracket)));

								path.push(
									Box::new(
										PathPart::IndexPathPart(
											Box::new(
												IndexPathPartData {
													span: Span::concat(lb.sp, self.last_sp.clone()),
													index: Some(expr)
												}
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
								VariableData {
									span: Span::concat(identifier.sp, self.last_sp.clone()),
									path: path
								}
							)
						)
					}
				} else {
					return Err(format!("Parser error: unexpected token {:?}. {:?}", self.current_token.tok, self.current_token.sp))
				}
			},
		};

		if Self::precedence_for_token(self.current_token.tok.clone()) > minimum_precedence {
			self.parse_expression_rec(Some(new_expr), minimum_precedence)
		} else {
			Ok(new_expr)
		}
	}

	fn parse_struct_init(&mut self, path: Path, start_sp: Span) -> Result<Box<StructInitData>, String> {
		let mut fields: std::vec::Vec<Box<StructInitFieldData>> = vec![];
		while try!(self.accept(Token::Symbol(Symbol::RightBrace))).is_none() {
			let field_name_token = try!(self.expect_any(Token::Identifier("".to_string())));
			let field_name = match field_name_token.tok {
				Token::Identifier(s) => s,
				_ => return Err("".to_string()), // Should never happen
			};

			try!(self.expect(Token::Symbol(Symbol::Colon)));

			let field_value = try!(self.parse_expression());

			fields.push(
				Box::new(
					StructInitFieldData {
						span: Span::concat(field_name_token.sp, self.last_sp.clone()),
						name: field_name,
						value: field_value,
					}
				)
			);

			if self.current_token.tok == Token::Symbol(Symbol::RightBrace) {
				try!(self.accept(Token::Symbol(Symbol::Comma)));
			} else {
				try!(self.expect(Token::Symbol(Symbol::Comma)));
			};
		};

		Ok(Box::new(
			StructInitData {
				span: Span::concat(start_sp, self.last_sp.clone()),
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
		match type_token.tok {
			Token::Identifier(i) => path.push(
				Box::new(
					PathPart::IdentifierPathPart(
						Box::new(
							IdentifierPathPartData {
								span: type_token.sp,
								identifier: i
							}
						)
					)
				)
			),
			_ => return Err("".to_string()) // Should never happen
		};

		while try!(self.accept(Token::Symbol(Symbol::ColonColon))).is_some() {
			let next_path_token = try!(self.expect_any(Token::Identifier("".to_string())));
			let next_path = match next_path_token.tok {
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
							IdentifierPathPartData {
								span: next_path_token.sp,
								identifier: next_path
							}
						)
					)
				)
			);
		}

		match get_builtin_type(&path) {
			Some(t) => Ok(t),
			None => Ok(Type::StructType(
				Box::new(
					StructTypeData {
						path: path
					}
				)
			))
		}
	}

	fn skip_newlines(&mut self) -> Result<Span, String> {
		let mut sp = Span {
			scol: self.current_token.sp.scol,
			srow: self.current_token.sp.srow,
			ecol: self.current_token.sp.scol, // intended
			erow: self.current_token.sp.srow, // intended
		};

		while self.current_token.tok == Token::Symbol(Symbol::NewLine) {
			sp = Span::concat(sp, self.current_token.sp.clone());
			try!(self.next_token());
		};

		Ok(sp)
	}

	fn accept(&mut self, mtoken: Token) -> Result<Option<SToken>, String> {
		// TODO: use intrinsics with determinants, but unstable for now. Could also use macros
		// around enums to be able to automatically expand them into a match, but would be too
		// invasive.
		let token = match (self.current_token.tok.clone(), mtoken) {
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

	fn accept_any(&mut self, mtoken: Token) -> Result<Option<SToken>, String> {
		// TODO: same as accept()
		let token = match (self.current_token.tok.clone(), mtoken) {
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

	fn expect(&mut self, token: Token) -> Result<SToken, String> {
		match try!(self.accept(token.clone())) {
			Some(t) => Ok(t),
			None => Err(format!("Parser error: expected {:?}, {:?}", token, self.current_token.sp)),
		}
	}

	fn expect_any(&mut self, token: Token) -> Result<SToken, String> {
		match try!(self.accept_any(token.clone())) {
			Some(t) => Ok(t),
			None => Err(format!("Parser error: expected {:?}, {:?}", token, self.current_token.sp)),
		}
	}

	fn next_token(&mut self) -> Result<SToken, String> {
		match self.reader.next_token() {
			Ok(t) => {
				self.last_sp = self.current_token.sp.clone();
				self.current_token = t;
				Ok(self.current_token.clone())
			},
			Err(s) => return Err(s)
		}
	}
}
