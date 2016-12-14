use std;
use parser;
use super::super::lexer::Span;
use super::ast::*;

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ErrorKind {
    Parser(parser::Error),
    UnresolvedContextPath(UnresolvedPath),
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

pub struct PathResolver<'a> {
	ast: &'a mut Ast,
}

impl<'a> PathResolver<'a> {
	pub fn new(ast: &'a mut Ast) -> Self {
		PathResolver {
			ast: ast,
		}
	}

    pub fn resolve(&mut self) -> Result<()> {
        for statement in &mut self.ast.statements {
            match *statement {
                Statement::FuncDecl(ref mut func_decl_data) => {
                    let path = func_decl_data.name.clone();
                    try!(PathResolver::resolve_type(&mut func_decl_data.return_type, path.clone()));
                    try!(PathResolver::resolve_block_statements(&mut func_decl_data.statements, path.clone()));
                    for param in &mut func_decl_data.parameters {
                        try!(PathResolver::resolve_type(&mut param.param_type, path.clone()));
                        if let Some(ref mut expr) = param.default_value {
                            try!(PathResolver::resolve_expression(expr, path.clone(), false));
                        }
                    }
                },
                Statement::StructDecl(ref mut struct_decl_data) => {
                    let path = struct_decl_data.name.clone();
                    for field in &mut struct_decl_data.fields {
                        try!(PathResolver::resolve_type(&mut field.field_type, path.clone()));
                        if let Some(ref mut expr) = field.default_value {
                            try!(PathResolver::resolve_expression(expr, path.clone(), false))
                        }
                    }
                }
                _ => ()
            }
        }

        Ok(())
    }

    fn resolve_block_statements(statements: &mut [BlockStatement], context: Path) -> Result<()> {
        for statement in statements {
            match *statement {
                BlockStatement::Expression(ref mut expr) => {
                    try!(PathResolver::resolve_expression(expr, context.clone(), false));
                },
                BlockStatement::While(ref mut while_data) => {
                    try!(PathResolver::resolve_expression(&mut while_data.condition, context.clone(), false));
                    try!(PathResolver::resolve_block_statements(&mut while_data.statements, context.clone()));
                },
                BlockStatement::If(ref mut if_data) => {
                    try!(PathResolver::resolve_expression(&mut if_data.condition, context.clone(), false));
                    try!(PathResolver::resolve_block_statements(&mut if_data.if_statements, context.clone()));
                    if let Some(ref mut statements) = if_data.else_statements {
                        try!(PathResolver::resolve_block_statements(statements, context.clone()));
                    }
                },
                BlockStatement::ForIn(ref mut forin_data) => {
                    try!(PathResolver::resolve_expression(&mut forin_data.collection, context.clone(), false));
                    try!(PathResolver::resolve_block_statements(&mut forin_data.statements, context.clone()));
                },
                BlockStatement::Return(ref mut return_data) => {
                    if let Some(ref mut expr) = return_data.value {
                        try!(PathResolver::resolve_expression(expr, context.clone(), false));
                    }
                },
                BlockStatement::VarAssignment(ref mut lhs, ref mut rhs) => {
                    try!(PathResolver::resolve_expression(lhs, context.clone(), false));
                    try!(PathResolver::resolve_expression(rhs, context.clone(), false));
                },
                BlockStatement::VarDecl(ref mut var_decl_data) => {
                    try!(PathResolver::resolve_type(&mut var_decl_data.var_type, context.clone()));
                    if let Some(ref mut expr) = var_decl_data.value {
                        try!(PathResolver::resolve_expression(expr, context.clone(), false));
                    }
                },
            }
        };

        Ok(())
    }

    fn resolve_parts(parts: &mut std::vec::Vec<String>, context: Path) -> Result<()> {
            match context {
                Path::Resolved(rp) => {
                    // local or external context
                    let offset = rp.parts.len() as isize - (if parts.len() == 1 { 1 } else { 2 });
                    let trunc = if offset < 0 {
                        0
                    } else {
                        offset
                    } as usize;

                    for (i, part) in rp.parts.iter().enumerate() {
                        if i >= trunc {
                            break;
                        }
                        parts.insert(i, part.clone());
                    };

                    Ok(())
                }
                Path::Unresolved(up) => {
                    Err(Error {
                        span: up.span.clone(),
                        kind: ErrorKind::UnresolvedContextPath(up),
                    })
                }
            }
        }
    
    fn resolve_type(ty: &mut Type, context: Path) -> Result<()> {
        match *ty {
            Type::Reference(ref mut t)
            | Type::MutReference(ref mut t)
            | Type::Array(ref mut t) => {
                try!(PathResolver::resolve_type(t, context))
            },
            Type::Map(ref mut t1, ref mut t2) => {
                try!(PathResolver::resolve_type(t1, context.clone()));
                try!(PathResolver::resolve_type(t2, context));
            },
            Type::Func(ref mut t, ref mut arg_ts) => {
                try!(PathResolver::resolve_type(t, context.clone()));
                for arg_t in arg_ts {
                    try!(PathResolver::resolve_type(arg_t, context.clone()));
                }
            },
            Type::Struct(ref mut p) => {
                let (span, mut new_parts): (Span, std::vec::Vec<String>)
                = if let Path::Unresolved(ref up) = *p {
                    (up.span.clone(), up.parts.iter().map(|ss| ss.ident.clone()).collect())
                } else {
                    return Ok(())
                };
                
                try!(PathResolver::resolve_parts(&mut new_parts, context.clone()));

                *p = Path::Resolved(ResolvedPath {
                    span: span,
                    parts: new_parts,
                });
            },
            _ => ()
        };

        Ok(())
    }

    fn resolve_expression(expr: &mut Expression, context: Path, func_call: bool) -> Result<()> {
        match expr.expr {
            Expression_::Array(ref mut vec) => {
                for arr_expr in vec {
                    try!(PathResolver::resolve_expression(arr_expr, context.clone(), false));
                }
            },
            Expression_::StructInit(ref mut p, ref mut fields) => {
                let (span, mut new_parts): (Span, std::vec::Vec<String>)
                = if let Path::Unresolved(ref up) = *p {
                    (up.span.clone(), up.parts.iter().map(|ss| ss.ident.clone()).collect())
                } else {
                    return Ok(())
                };
                
                try!(PathResolver::resolve_parts(&mut new_parts, context.clone()));

                *p = Path::Resolved(ResolvedPath {
                    span: span,
                    parts: new_parts,
                });

                for field in fields {
                    try!(PathResolver::resolve_expression(&mut field.value, context.clone(), false));
                }
            },
            Expression_::Map(ref mut map) => {
                let map_vec: std::vec::Vec<(Box<Expression>, Box<Expression>)>
                = map.map.iter().map(|(k, v)| (k.clone(), v.clone())).collect();

                map.map.clear();

                for (mut e1, mut e2) in map_vec {
                    try!(PathResolver::resolve_expression(&mut e1, context.clone(), false));
                    try!(PathResolver::resolve_expression(&mut e2, context.clone(), false));

                    map.map.insert(e1, e2);
                }
            },
            Expression_::Index(ref mut e1, ref mut oe2) => {
                try!(PathResolver::resolve_expression(e1, context.clone(), false));
                if let Some(ref mut e2) = *oe2 {
                    try!(PathResolver::resolve_expression(e2, context, false));
                }
            },
            Expression_::Field(ref mut e, _) 
            | Expression_::UnaryOp(_, ref mut e) => {
                try!(PathResolver::resolve_expression(e, context, false));
            },
            Expression_::BinaryOp(_, ref mut e1, ref mut e2) => {
                try!(PathResolver::resolve_expression(e1, context.clone(), false));
                try!(PathResolver::resolve_expression(e2, context, false));
            },
            Expression_::FuncCall(ref mut e, ref mut vec) => {
                try!(PathResolver::resolve_expression(e, context.clone(), true));
                for mut arr_expr in vec {
                    try!(PathResolver::resolve_expression(&mut arr_expr, context.clone(), false));
                }
            },
            Expression_::Variable(ref mut p) => {
                let (span, mut new_parts): (Span, std::vec::Vec<String>)
                = if let Path::Unresolved(ref up) = *p {
                    (up.span.clone(), up.parts.iter().map(|ss| ss.ident.clone()).collect())
                } else {
                    return Ok(())
                };

                if !PathResolver::is_builtin_func(&new_parts) && func_call {
                    try!(PathResolver::resolve_parts(&mut new_parts, context))
                }

                *p = Path::Resolved(ResolvedPath {
                    span: span,
                    parts: new_parts,
                });
            },
            _ => ()
        }

        Ok(())
    }

    fn is_builtin_func(parts: &[String]) -> bool {
        if parts.len() != 1 {
            return false;
        }

        match parts[0].as_ref() {
            "print"
            | "readln" => true,
            _ => false
        }
    }
}
