use crate::sexp::{Sexp, Sexp::*};
use expr::{Expr, Expr::*};
use prim2::Prim2::*;

pub mod expr;
pub mod prim2;

fn parse(sexp: &Sexp) -> Expr {
    match sexp {
        Atom(s) => {
            if let Ok(i) = s.parse::<i64>() {
                ENum(i)
            } else {
                panic!("Cannot parse {}", s)
            }
        }
        List(v) => match &v[..] {
            [Atom("+"), e1, e2] => EPrim2(Add, Box::new(parse(e1)), Box::new(parse(e2))),
            [Atom("-"), e1, e2] => EPrim2(Minus, Box::new(parse(e1)), Box::new(parse(e2))),
            [Atom("*"), e1, e2] => EPrim2(Times, Box::new(parse(e1)), Box::new(parse(e2))),
            _ => panic!("Not yet implemented"),
        },
    }
}

pub fn parse_ast(sexps: &[Sexp]) -> Vec<Expr> {
    sexps.into_iter().map(parse).collect()
}
