use std;
use lexer::Span;

#[derive(Debug, Clone)]
pub struct Ast {
	pub statements: std::vec::Vec<Statement>,
}

impl Ast {
	pub fn new() -> Self {
		Ast {
			statements: vec![],
		}
	}
}

#[derive(Debug, Clone)]
pub enum Statement {
	Import(Box<ImportData>),
	Package(Box<PackageData>),
	FuncDecl(Box<FuncDeclData>),
	StructDecl(Box<StructDeclData>),
}

#[derive(Debug, Clone)]
pub struct ImportData {
	pub span: Span,
	pub path: String,
}

#[derive(Debug, Clone)]
pub struct PackageData {
	pub span: Span,
	pub name: String,
}

#[derive(Debug, Clone)]
pub struct FuncDeclData {
	pub span: Span,
	pub name: String,
	pub return_type: Option<Type>,
	pub parameters: std::vec::Vec<Box<FuncDeclParamData>>,
	pub statements: std::vec::Vec<BlockStatement>,
}

#[derive(Debug, Clone)]
pub enum BlockStatement {
	FuncCall(Box<FuncCallData>),
	VarDecl(Box<VarDeclData>),
	VarAssignment(Box<VarAssignmentData>),
	If(Box<IfData>),
	While(Box<WhileData>),
	Return(Box<ReturnData>),
	ForIn(Box<ForInData>),
}

#[derive(Debug, Clone)]
pub struct ForInData {
	pub span: Span,
	pub element_name: String,
	pub collection: Expression,
	pub statements: std::vec::Vec<BlockStatement>,
}

#[derive(Debug, Clone)]
pub struct ReturnData {
	pub span: Span,
	pub value: Option<Expression>,
	pub expected_type: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct IfData {
	pub span: Span,
	pub condition: Expression,
	pub if_statements: std::vec::Vec<BlockStatement>,
	pub else_statements: Option<std::vec::Vec<BlockStatement>>,
}

#[derive(Debug, Clone)]
pub struct WhileData {
	pub span: Span,
	pub condition: Expression,
	pub statements: std::vec::Vec<BlockStatement>,
}

#[derive(Debug, Clone)]
pub struct VarAssignmentData {
	pub span: Span,
	pub path: Path,
	pub value: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncCallData {
	pub span: Span,
	pub path: Path,
	pub arguments: std::vec::Vec<Box<FuncCallArgData>>,
}

#[derive(Debug, Clone)]
pub struct VarDeclData {
	pub span: Span,
	pub name: String,
	pub var_type: Type,
	pub value: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncCallArgData {
	pub span: Span,
	pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct FuncDeclParamData {
	pub span: Span,
	pub name: String,
	pub param_type: Type,
	pub default_value: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
	ArrayType(Box<Type>),
	StructType(Box<StructTypeData>),
	StringType,
	IntType,
	BoolType,
	CharType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructTypeData {
	pub path: Path,
}

#[derive(Debug, Clone)]
pub struct StructDeclData {
	pub span: Span,
	pub name: String,
	pub fields: std::vec::Vec<Box<StructFieldData>>,
}

#[derive(Debug, Clone)]
pub struct StructFieldData {
	pub span: Span,
	pub name: String,
	pub field_type: Type,
	pub default_value: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
	StringLiteral(Box<StringLiteralData>),
	IntegerLiteral(Box<IntegerLiteralData>),
	BoolLiteral(Box<BoolLiteralData>),
	CharLiteral(Box<CharLiteralData>),
	Variable(Box<VariableData>),
	StructInit(Box<StructInitData>),
	Array(Box<ArrayData>),
	FuncCall(Box<FuncCallData>),
	Addition(Box<Expression>, Box<Expression>),
	Substraction(Box<Expression>, Box<Expression>),
	Multiplication(Box<Expression>, Box<Expression>),
	Division(Box<Expression>, Box<Expression>),
	Modulo(Box<Expression>, Box<Expression>),
	Equality(Box<Expression>, Box<Expression>),
	Inequality(Box<Expression>, Box<Expression>),
	Concatenation(Box<Expression>, Box<Expression>),
	Count(Box<Expression>, Span),
}

impl Span {
	pub fn from_expression(e: &Expression) -> Span {
		match *e {
			Expression::StringLiteral(ref sl) => sl.span.clone(),
			Expression::IntegerLiteral(ref il) => il.span.clone(),
			Expression::BoolLiteral(ref bl) => bl.span.clone(),
			Expression::CharLiteral(ref cl) => cl.span.clone(),
			Expression::Variable(ref v) => v.span.clone(),
			Expression::StructInit(ref si) => si.span.clone(),
			Expression::Array(ref a) => a.span.clone(),
			Expression::FuncCall(ref fc) => fc.span.clone(),
			Expression::Addition(ref e1, ref e2) => Span::concat(Span::from_expression(e1), Span::from_expression(e2)),
			Expression::Substraction(ref e1, ref e2) => Span::concat(Span::from_expression(e1), Span::from_expression(e2)),
			Expression::Multiplication(ref e1, ref e2) => Span::concat(Span::from_expression(e1), Span::from_expression(e2)),
			Expression::Division(ref e1, ref e2) => Span::concat(Span::from_expression(e1), Span::from_expression(e2)),
			Expression::Modulo(ref e1, ref e2) => Span::concat(Span::from_expression(e1), Span::from_expression(e2)),
			Expression::Equality(ref e1, ref e2) => Span::concat(Span::from_expression(e1), Span::from_expression(e2)),
			Expression::Inequality(ref e1, ref e2) => Span::concat(Span::from_expression(e1), Span::from_expression(e2)),
			Expression::Concatenation(ref e1, ref e2) => Span::concat(Span::from_expression(e1), Span::from_expression(e2)),
			Expression::Count(ref c, ref s) => Span::concat((*s).clone(), Span::from_expression(c)),
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructInitData {
	pub span: Span,
	pub path: Path,
	pub fields: std::vec::Vec<Box<StructInitFieldData>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructInitFieldData {
	pub span: Span,
	pub name: String,
	pub value: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayData {
	pub span: Span,
	pub items: std::vec::Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableData {
	pub span: Span,
	pub path: Path,
}

pub type Path = std::vec::Vec<Box<PathPart>>;

#[derive(Debug, Clone, PartialEq)]
pub enum PathPart { // TODO: rework with recursive data structures... The problem right now is that we read from left to right, but recursive data structures would imply right to left construction.
	ModulePathPart,
	FieldPathPart,
	IndexPathPart(Box<IndexPathPartData>),
	IdentifierPathPart(Box<IdentifierPathPartData>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IndexPathPartData {
	pub span: Span,
	pub index: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IdentifierPathPartData {
	pub span: Span,
	pub identifier: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StringLiteralData {
	pub span: Span,
	pub value: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IntegerLiteralData {
	pub span: Span,
	pub value: i64,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BoolLiteralData {
	pub span: Span,
	pub value: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CharLiteralData {
	pub span: Span,
	pub value: char,
}
