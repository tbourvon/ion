use std;

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
	pub path: String,
}

#[derive(Debug, Clone)]
pub struct PackageData {
	pub name: String,
}

#[derive(Debug, Clone)]
pub struct FuncDeclData {
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
}

#[derive(Debug, Clone)]
pub struct ReturnData {
	pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct IfData {
	pub condition: Expression,
	pub statements: std::vec::Vec<BlockStatement>,
}

#[derive(Debug, Clone)]
pub struct WhileData {
	pub condition: Expression,
	pub statements: std::vec::Vec<BlockStatement>,
}

#[derive(Debug, Clone)]
pub struct VarAssignmentData {
	pub path: Path,
	pub value: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncCallData {
	pub path: Path,
	pub arguments: std::vec::Vec<Box<FuncCallArgData>>,
}

#[derive(Debug, Clone)]
pub struct VarDeclData {
	pub name: String,
	pub var_type: Type,
	pub value: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncCallArgData {
	pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct FuncDeclParamData {
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
	pub name: String,
	pub fields: std::vec::Vec<Box<StructFieldData>>,
}

#[derive(Debug, Clone)]
pub struct StructFieldData {
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
	Count(Box<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayData {
	pub items: std::vec::Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableData {
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
	pub index: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IdentifierPathPartData {
	pub identifier: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StringLiteralData {
	pub value: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IntegerLiteralData {
	pub value: i64,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BoolLiteralData {
	pub value: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CharLiteralData {
	pub value: char,
}
