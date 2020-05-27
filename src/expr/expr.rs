#[derive(Debug, Clone)]
pub enum Expr<'a> {
    ENum(i64),
    EBool(bool),
    EId(&'a str),
    EPrim2(Prim2, Box<Expr<'a>>, Box<Expr<'a>>),
    ELet(Vec<Binding<'a>>, Box<Expr<'a>>),
    EIf(Box<Expr<'a>>, Box<Expr<'a>>, Box<Expr<'a>>),
}

#[derive(Debug, Clone)]
pub struct Binding<'a>(pub &'a str, pub Expr<'a>);

#[derive(Debug, Clone)]
pub enum Prim2 {
    Add,
    Minus,
    Times,
    Less,
    Greater,
    Equal,
}
