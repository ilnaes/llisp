#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Def<'a> {
    // first and second field Exprs will always be EId of string,
    // it should just be string, but this is a dirty hack for type convenience
    FuncDef(Expr<'a>, Vec<Expr<'a>>, Expr<'a>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Expr<'a> {
    ENum(i64),
    EBool(bool),
    EId(&'a str),
    EPrim2(Prim2, Box<Expr<'a>>, Box<Expr<'a>>),
    ELet(Vec<Binding<'a>>, Box<Expr<'a>>),
    EIf(Box<Expr<'a>>, Box<Expr<'a>>, Box<Expr<'a>>),
    EPrint(Box<Expr<'a>>),
    // EApp(Box<Expr<'a>>, Vec<Expr<'a>>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Binding<'a>(pub &'a str, pub Expr<'a>);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Prim2 {
    Add,
    Minus,
    Times,
    Less,
    Greater,
    Equal,
}
