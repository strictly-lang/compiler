use crate::location::{Anchor, Location, Span};
use derive_new::new;
use ordered_float::OrderedFloat;

#[salsa::tracked]
pub struct Program {
    #[return_ref]
    statements: Vec<Statement>,
}

#[derive(Eq, PartialEq, Debug, Hash, new)]
pub struct Statement {
    pub span: Span,

    pub data: StatementData,
}

#[derive(Eq, PartialEq, Debug, Hash)]
pub enum StatementData {
    /// Defines `fn <name>(<args>) = <body>`
    Function(Function),
    /// Defines `print <expr>`
    Print(Expression),
}

#[derive(Eq, PartialEq, Debug, Hash, new)]
pub struct Expression {
    pub span: Span,

    pub data: ExpressionData,
}

#[derive(Eq, PartialEq, Debug, Hash)]
pub enum ExpressionData {
    Op(Box<Expression>, Op, Box<Expression>),
    Number(OrderedFloat<f64>),
    Variable(VariableId),
    Call(FunctionId, Vec<Expression>),
}

#[derive(Eq, PartialEq, Copy, Clone, Hash, Debug)]
pub enum Op {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[salsa::tracked]
pub struct Function {
    #[id]
    name: FunctionId,

    /// The absolute position of the start of this function.
    /// All spans within the function (including `name_span`)
    /// are stored relative to this location.
    anchor_location: Location,

    name_span: Span,

    #[return_ref]
    args: Vec<VariableId>,

    #[return_ref]
    body: Expression,
}

#[salsa::interned]
pub struct VariableId {
    #[return_ref]
    pub text: String,
}

#[salsa::interned]
pub struct FunctionId {
    #[return_ref]
    pub text: String,
}

impl Anchor for Program {
    fn anchor_location(&self, _db: &dyn crate::Db) -> Location {
        Location::start()
    }
}

impl Anchor for Function {
    fn anchor_location(&self, db: &dyn crate::Db) -> Location {
        Function::anchor_location(*self, db)
    }
}
