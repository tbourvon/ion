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
	just_skept_newline: bool,
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
			just_skept_newline: false,
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

	fn binop_for_token(stoken: SToken) -> Option<BinOp> {
		match stoken.tok.clone() {
			Token::Symbol(s) => {
				let binop = match s {
					Symbol::Plus => BinOp::Addition,
					Symbol::Minus => BinOp::Substraction,
					Symbol::Star => BinOp::Multiplication,
					Symbol::Over => BinOp::Division,
					Symbol::Modulo => BinOp::Modulo,
					Symbol::Concat => BinOp::Concatenation,
					Symbol::EqualEqual => BinOp::Equality,
					Symbol::NotEqual => BinOp::Inequality,
					_ => return None
				};

				Some(binop)
			},
			_ => None
		}
	}

	fn precedence_for_op(op: Op) -> u8 {
		match op {
			Op::UnOp(_) => std::u8::MAX,
			Op::BinOp(binop) => {
				match binop {
					BinOp::Equality |
					BinOp::Inequality => 1,
					BinOp::Addition |
					BinOp::Substraction |
					BinOp::Concatenation => 2,
					BinOp::Multiplication |
					BinOp::Division |
					BinOp::Modulo => 3,
				}
			},
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

		let mut return_type: Type = Type::NoType;
		if try!(self.accept(Token::Symbol(Symbol::Return))).is_some() {
			return_type = try!(self.parse_type());
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

	fn parse_block_statement(&mut self, return_type: Type) -> Result<BlockStatement, String> {
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
		} else {
			let expr = try!(self.parse_expression());

			if try!(self.accept(Token::Symbol(Symbol::Equal))).is_some() {
				Ok(BlockStatement::VarAssignment(
					Box::new(expr),
					Box::new(try!(self.parse_expression())),
				))
			} else {
				Ok(BlockStatement::Expression(Box::new(expr)))
			}
		}
	}

	fn parse_forin(&mut self, return_type: Type, start_sp: Span) -> Result<Box<ForInData>, String> {
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

	fn parse_return(&mut self, return_type: Type, start_sp: Span) -> Result<Box<ReturnData>, String> {
		Ok(Box::new(
			ReturnData {
				value: match return_type {
					Type::NoType => None,
					_ => Some(try!(self.parse_expression())),
				},
				span: Span::concat(start_sp, self.last_sp.clone()),
				expected_type: return_type,
			}
		))
	}

	fn parse_if(&mut self, return_type: Type, start_sp: Span) -> Result<Box<IfData>, String> {
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

	fn parse_while(&mut self, return_type: Type, start_sp: Span) -> Result<Box<WhileData>, String> {
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

	fn parse_path(&mut self, first_part: Option<SToken>) -> Result<Path, String> {
		let mut parts: std::vec::Vec<SpannedString> = vec![];

		let ident_token = match first_part {
			Some(t) => t,
			None => try!(self.expect_any(Token::Identifier("".to_string())))
		};

		let ident = match ident_token.tok {
			Token::Identifier(i) => i,
			_ => return Err("".to_string()) // Should never happen
		};

		parts.push(SpannedString {
			span: ident_token.sp.clone(),
			ident: ident,
		});

		while try!(self.accept(Token::Symbol(Symbol::ColonColon))).is_some() {
			let next_path_token = try!(self.expect_any(Token::Identifier("".to_string())));
			let next_path = match next_path_token.tok {
				Token::Identifier(id) => id,
				_ => return Err("".to_string()) // Should never happen
			};

			parts.push(
				SpannedString {
					span: next_path_token.sp,
					ident: next_path,
				}
			);
		}

		Ok(Path {
			parts: parts,
			span: Span::concat(ident_token.sp, self.last_sp.clone()),
		})
	}

	fn parse_expression_binop(&mut self, start_sp: Span, binop: BinOp, lhs: Expression) -> Result<Expression, String> {
		Ok(Expression {
			expr: Expression_::BinOp(
				binop.clone(),
				Box::new(
					lhs
				),
				Box::new(
					try!(self.parse_expression_(None, Self::precedence_for_op(Op::BinOp(binop))))
				)
			),
			span: Span::concat(start_sp, self.last_sp.clone()),
		})
	}

	fn parse_expression_unop(&mut self, start_sp: Span, unop: UnOp) -> Result<Expression, String> {
		Ok(Expression {
			expr: Expression_::UnOp(
				unop.clone(),
				Box::new(
					try!(self.parse_expression_(None, Self::precedence_for_op(Op::UnOp(unop))))
				)
			),
			span: Span::concat(start_sp, self.last_sp.clone()),
		})
	}

	fn parse_expression_array(&mut self, start_sp: Span) -> Result<Expression, String> {
		let mut items: std::vec::Vec<Box<Expression>> = vec![];
		while try!(self.accept(Token::Symbol(Symbol::RightBracket))).is_none() {
			items.push(Box::new(try!(self.parse_expression())));
			if self.current_token.tok == Token::Symbol(Symbol::RightBracket) {
				try!(self.accept(Token::Symbol(Symbol::Comma)));
			} else {
				try!(self.expect(Token::Symbol(Symbol::Comma)));
			};
		};

		Ok(Expression {
			expr: Expression_::Array(items),
			span: Span::concat(start_sp, self.last_sp.clone()),
		})
	}

	fn parse_expression_struct_init(&mut self, start_sp: Span) -> Result<Expression, String> {
		let path = try!(self.parse_path(None));

		try!(self.expect(Token::Symbol(Symbol::LeftBrace)));

		let mut fields: std::vec::Vec<StructInitFieldData> = vec![];
		while try!(self.accept(Token::Symbol(Symbol::RightBrace))).is_none() {
			let field_name_token = try!(self.expect_any(Token::Identifier("".to_string())));
			let field_name = match field_name_token.tok {
				Token::Identifier(s) => s,
				_ => return Err("".to_string()), // Should never happen
			};

			try!(self.expect(Token::Symbol(Symbol::Colon)));

			let field_value = try!(self.parse_expression());

			fields.push(
				StructInitFieldData {
					span: Span::concat(field_name_token.sp.clone(), self.last_sp.clone()),
					name: SpannedString {
						span: field_name_token.sp,
						ident: field_name,
					},
					value: Box::new(field_value),
				}
			);

			if self.current_token.tok == Token::Symbol(Symbol::RightBrace) {
				try!(self.accept(Token::Symbol(Symbol::Comma)));
			} else {
				try!(self.expect(Token::Symbol(Symbol::Comma)));
			};
		};

		Ok(Expression {
			expr: Expression_::StructInit(
				path,
				fields,
			),
			span: Span::concat(start_sp, self.last_sp.clone()),
		})

	}

	fn parse_expression_literal(&mut self, stoken: SToken) -> Result<Expression, String> {
		let expr = match stoken.tok {
			Token::StringLiteral(s) => Expression_::StringLiteral(s),
			Token::IntegerLiteral(i) => Expression_::IntegerLiteral(i),
			Token::BoolLiteral(b) => Expression_::BoolLiteral(b),
			Token::CharLiteral(c) => Expression_::CharLiteral(c),
			_ => return Err("".to_string()) // Should never happen
		};

		Ok(Expression {
			expr: expr,
			span: stoken.sp,
		})
	}

	fn parse_expression_func_call(&mut self, start_sp: Span, func_expr: Expression) -> Result<Expression, String> {
		let mut args: std::vec::Vec<Box<Expression>> = vec![];
		while try!(self.accept(Token::Symbol(Symbol::RightParenthesis))).is_none() {
			args.push(Box::new(try!(self.parse_expression())));

			if self.current_token.tok == Token::Symbol(Symbol::RightParenthesis) {
				try!(self.accept(Token::Symbol(Symbol::Comma)));
			} else {
				try!(self.expect(Token::Symbol(Symbol::Comma)));
			};
		};

		Ok(Expression {
			span: Span::concat(start_sp, self.last_sp.clone()),
			expr: Expression_::FuncCall(Box::new(func_expr), args),
		})
	}

	fn parse_expression_field(&mut self, start_sp: Span, struct_expr: Expression) -> Result<Expression, String> {
		let field_token = try!(self.expect_any(Token::Identifier("".to_string())));
		let field = match field_token.tok {
			Token::Identifier(i) => i,
			_ => return Err("".to_string()) // Should never happen
		};

		Ok(Expression {
			span: Span::concat(start_sp, field_token.sp.clone()),
			expr: Expression_::Field(
				Box::new(struct_expr),
				SpannedString {
					span: field_token.sp,
					ident: field,
				},
			),
		})
	}

	fn parse_expression_index(&mut self, start_sp: Span, indexable_expr: Expression) -> Result<Expression, String> {
		let index = if try!(self.accept(Token::Symbol(Symbol::RightBracket))).is_some() {
			None
		} else {
			let expr = try!(self.parse_expression());
			try!(self.expect(Token::Symbol(Symbol::RightBracket)));

			Some(Box::new(expr))
		};

		Ok(Expression {
			expr: Expression_::Index(Box::new(indexable_expr), index),
			span: Span::concat(start_sp, self.last_sp.clone()),
		})
	}

	fn parse_expression_variable(&mut self, stoken: SToken) -> Result<Expression, String> {
		let path = try!(self.parse_path(Some(stoken)));

		Ok(Expression {
			span: path.span.clone(),
			expr: Expression_::Variable(path),
		})
	}

	fn parse_expression(&mut self) -> Result<Expression, String> {
		self.parse_expression_(None, 0)
	}

	fn parse_expression_(&mut self, prev_expr: Option<Expression>, minimum_precedence: u8) -> Result<Expression, String> {
		let new_expr = match prev_expr {
			Some(expr) => {
				if try!(self.accept(Token::Symbol(Symbol::Plus))).is_some() {
					try!(self.parse_expression_binop(expr.span.clone(), BinOp::Addition, expr))
				} else if try!(self.accept(Token::Symbol(Symbol::Minus))).is_some() {
					try!(self.parse_expression_binop(expr.span.clone(), BinOp::Substraction, expr))
				} else if try!(self.accept(Token::Symbol(Symbol::Star))).is_some() {
					try!(self.parse_expression_binop(expr.span.clone(), BinOp::Multiplication, expr))
				} else if try!(self.accept(Token::Symbol(Symbol::Over))).is_some() {
					try!(self.parse_expression_binop(expr.span.clone(), BinOp::Division, expr))
				} else if try!(self.accept(Token::Symbol(Symbol::Modulo))).is_some() {
					try!(self.parse_expression_binop(expr.span.clone(), BinOp::Modulo, expr))
				} else if try!(self.accept(Token::Symbol(Symbol::EqualEqual))).is_some() {
					try!(self.parse_expression_binop(expr.span.clone(), BinOp::Equality, expr))
				} else if try!(self.accept(Token::Symbol(Symbol::NotEqual))).is_some() {
					try!(self.parse_expression_binop(expr.span.clone(), BinOp::Inequality, expr))
				} else if try!(self.accept(Token::Symbol(Symbol::Concat))).is_some() {
					try!(self.parse_expression_binop(expr.span.clone(), BinOp::Concatenation, expr))
				} else if try!(self.accept(Token::Symbol(Symbol::LeftParenthesis))).is_some() {
					try!(self.parse_expression_func_call(expr.span.clone(), expr))
				} else if try!(self.accept(Token::Symbol(Symbol::Dot))).is_some() {
					try!(self.parse_expression_field(expr.span.clone(), expr))
				} else if try!(self.accept(Token::Symbol(Symbol::LeftBracket))).is_some() {
					try!(self.parse_expression_index(expr.span.clone(), expr))
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
					try!(self.parse_expression_unop(h.sp, UnOp::Count))
				} else if let Some(a) = try!(self.accept(Token::Symbol(Symbol::Amp))) {
					try!(self.parse_expression_unop(a.sp, UnOp::Reference))
				} else if let Some(a) = try!(self.accept(Token::Symbol(Symbol::At))) {
					try!(self.parse_expression_unop(a.sp, UnOp::MutReference))
				} else if let Some(s) = try!(self.accept(Token::Symbol(Symbol::Star))) {
					try!(self.parse_expression_unop(s.sp, UnOp::Dereference))
				} else if let Some(lb) = try!(self.accept(Token::Symbol(Symbol::LeftBracket))) {
					try!(self.parse_expression_array(lb.sp))
				} else if let Some(n) = try!(self.accept(Token::Keyword(Keyword::New))) {
					try!(self.parse_expression_struct_init(n.sp))
				} else if let Some(sl) = try!(self.accept_any(Token::StringLiteral("".to_string()))) {
					try!(self.parse_expression_literal(sl))
				} else if let Some(il) = try!(self.accept_any(Token::IntegerLiteral(0))) {
					try!(self.parse_expression_literal(il))
				} else if let Some(bl) = try!(self.accept_any(Token::BoolLiteral(false))) {
					try!(self.parse_expression_literal(bl))
				} else if let Some(cl) = try!(self.accept_any(Token::CharLiteral('\0'))) {
					try!(self.parse_expression_literal(cl))
				} else if let Some(ident_token) = try!(self.accept_any(Token::Identifier("".to_string()))) {
					try!(self.parse_expression_variable(ident_token))
				} else {
					return Err(format!("Parser error: unexpected token {:?}. {:?}", self.current_token.tok, self.current_token.sp))
				}
			},
		};

		if self.just_skept_newline {
			Ok(new_expr)
		} else {
			if let Some(binop) = Self::binop_for_token(self.current_token.clone()) {
				if Self::precedence_for_op(Op::BinOp(binop)) > minimum_precedence {
					self.parse_expression_(Some(new_expr), minimum_precedence)
				} else {
					Ok(new_expr)
				}
			} else {
				if self.current_token.tok == Token::Symbol(Symbol::LeftParenthesis) ||
				   self.current_token.tok == Token::Symbol(Symbol::LeftBracket) ||
				   self.current_token.tok == Token::Symbol(Symbol::Dot) { // TODO: find a prettier solution
					self.parse_expression_(Some(new_expr), std::u8::MAX)
				} else {
					Ok(new_expr)
				}
			}
		}
	}

	fn parse_type(&mut self) -> Result<Type, String> {
		fn get_builtin_type(path: &Path) -> Option<Type> {
			if path.parts.len() != 1 {
				None
			} else {
				match path.parts.get(0).unwrap().ident.as_ref() {
					"int" => Some(Type::IntType),
					"bool" => Some(Type::BoolType),
					"char" => Some(Type::CharType),
					"string" => Some(Type::StringType),
					_ => None,
				}
			}
		}

		if try!(self.accept(Token::Symbol(Symbol::Amp))).is_some() {
			return Ok(Type::ReferenceType(
				Box::new(try!(self.parse_type()))
			))
		} else if try!(self.accept(Token::Symbol(Symbol::At))).is_some() {
			return Ok(Type::MutReferenceType(
				Box::new(try!(self.parse_type()))
			))
		} else if try!(self.accept(Token::Symbol(Symbol::LeftBracket))).is_some() {
			try!(self.expect(Token::Symbol(Symbol::RightBracket)));
			let inner_type = try!(self.parse_type());

			return Ok(Type::ArrayType(
				Box::new(inner_type)
			))
		};

		let path = try!(self.parse_path(None));

		match get_builtin_type(&path) {
			Some(t) => Ok(t),
			None => Ok(Type::StructType(path)),
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
			self.just_skept_newline = true;
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
			self.just_skept_newline = false;
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
			self.just_skept_newline = false;
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
