use super::prim2::*;

pub enum Expr {
    ENum(i64),
    EPrim2(Prim2, Box<Expr>, Box<Expr>),
}
