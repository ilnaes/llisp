use super::prim2::Prim2;

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    ENum(i64),
    EBool(bool),
    EPrim2(Prim2, Box<Expr<'a>>, Box<Expr<'a>>),
    ELet(Vec<Binding<'a>>, Box<Expr<'a>>),
    EId(&'a str),
}

#[derive(Debug, Clone)]
pub struct Binding<'a>(pub &'a str, pub Expr<'a>);
