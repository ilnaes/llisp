use super::prim2::Prim2;

#[derive(Debug, Clone)]
pub enum Expr {
    ENum(i64),
    EPrim2(Prim2, Box<Expr>, Box<Expr>),
}
