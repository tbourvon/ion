use super::super::lexer;
use super::super::lexer::Span;
use parser;
use super::ast::*;
use std;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ErrorKind {
    IO(std::io::Error),
    Parser(parser::Error),
}

impl From<parser::Error> for Error {
    fn from(error: parser::Error) -> Error {
        Error {
            kind: ErrorKind::Parser(error.clone()),
            span: error.span,
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;

pub struct Importer<'a> {
	ast: &'a mut Ast,
}

impl<'a> Importer<'a> {
	pub fn new(ast: &'a mut Ast) -> Self {
		Importer {
			ast: ast,
		}
	}

	pub fn import(&mut self) -> Result<()> {
		self.get_decls(String::new())
	}

	fn get_decls(&mut self, current_dir: String) -> Result<()> {
		let mut imported_statements = vec![];
		for statement in &mut self.ast.statements {
			match *statement {
				Statement::FuncDecl(ref mut func_decl_data) if current_dir.is_empty() => { // Only for root decls
					let (span, new_parts): (Span, std::vec::Vec<String>)
					= if let Path::Unresolved(ref up) = func_decl_data.name {
						(up.span.clone(), up.parts.iter().map(|ss| ss.ident.clone()).collect())
					} else {
						continue
					};

					func_decl_data.name = Path::Resolved(ResolvedPath {
						span: span,
						parts: new_parts,
					});
				},
				Statement::StructDecl(ref mut struct_decl_data) if current_dir.is_empty() => { // Only for root decls
					let (span, new_parts): (Span, std::vec::Vec<String>)
					= if let Path::Unresolved(ref up) = struct_decl_data.name {
						(up.span.clone(), up.parts.iter().map(|ss| ss.ident.clone()).collect())
					} else {
						continue
					};

					struct_decl_data.name = Path::Resolved(ResolvedPath {
						span: span,
						parts: new_parts,
					});
				}
				Statement::Import(ref import_data) => {
					let path_string = current_dir.clone() + import_data.path.as_ref() + ".ion";
			        let path = std::path::Path::new(AsRef::<str>::as_ref(&path_string[..]));
			        let mut file = match File::open(&path) {
			            Ok(file) => file,
			            Err(err) => {
			                return Err(Error {
			                    kind: ErrorKind::IO(err),
			                    span: Span::nil_span(),
			                })
			            }
			        };

			        let mut s = String::new();
			        let res = file.read_to_string(&mut s);
			        if let Some(err) = res.err() {
			            return Err(Error {
			                kind: ErrorKind::IO(err),
			                span: Span::nil_span(),
			            });
			        }

			        let mut reader = lexer::Reader::new(s.as_ref(), path_string.clone());

			        let parser = parser::Parser::new(&mut reader);
			        let mut ast = try!(parser.parse());


                    let new_dir = match path_string.rsplitn(2, '/').nth(1) {
                        Some(s) => s.to_string() + "/",
                        None => String::new(),
                    };

					{
						let mut importer = Importer::new(&mut ast);
						try!(importer.get_decls(new_dir));
					}

					for statement in &mut ast.statements {
						match *statement {
							Statement::FuncDecl(ref mut func_decl_data) => {
								let (span, mut new_parts): (Span, std::vec::Vec<String>)
								= if let Path::Unresolved(ref up) = func_decl_data.name {
									(up.span.clone(), up.parts.iter().map(|ss| ss.ident.clone()).collect())
								} else {
									continue
								};

								let full_path = current_dir.clone() + import_data.path.as_ref();
								let added_parts = full_path.split('/');

								for (i, part) in added_parts.enumerate() {
									new_parts.insert(i, part.to_string());
								}
								
								func_decl_data.name = Path::Resolved(ResolvedPath {
									span: span,
									parts: new_parts,
								});
							},
							Statement::StructDecl(ref mut struct_decl_data) => {
								let (span, mut new_parts): (Span, std::vec::Vec<String>)
								= if let Path::Unresolved(ref up) = struct_decl_data.name {
									(up.span.clone(), up.parts.iter().map(|ss| ss.ident.clone()).collect())
								} else {
									continue
								};

								let full_path = current_dir.clone() + import_data.path.as_ref();
								let added_parts = full_path.split('/');

								for (i, part) in added_parts.enumerate() {
									new_parts.insert(i, part.to_string());
								}

								struct_decl_data.name = Path::Resolved(ResolvedPath {
									span: span,
									parts: new_parts,
								});
							},
							_ => (),
						}
					}

					imported_statements.extend(ast.statements.into_iter())
				},
                _ => (),
			}
		}

		self.ast.statements.extend(imported_statements);

		Ok(())
	}
}
