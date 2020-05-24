use super::prim2::Prim2;

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    ENum(i64),
    EPrim2(Prim2, Box<Expr<'a>>, Box<Expr<'a>>),
    EId(&'a str),
}
