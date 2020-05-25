use crate::sexp::{Sexp, Sexp::*};
use expr::{Binding, Expr, Expr::*};
use prim2::Prim2::*;

pub mod expr;
pub mod prim2;

fn parse_binding<'a>(b: &Sexp<'a>) -> Binding<'a> {
    match b {
        List(l) => match &l[..] {
            [Atom(x), e] => Binding(x, parse(e)),
            _ => panic!("Binding parse error {:?}", b),
        },
        _ => panic!("Binding parse error {:?}", b),
    }
}

fn parse<'a>(sexp: &Sexp<'a>) -> Expr<'a> {
    match sexp {
        Atom(s) => {
            if let Ok(i) = s.parse::<i64>() {
                ENum(i)
            } else {
                EId(s)
            }
        }
        List(v) => match &v[..] {
            [Atom("+"), e1, e2] => EPrim2(Add, Box::new(parse(e1)), Box::new(parse(e2))),
            [Atom("-"), e1, e2] => EPrim2(Minus, Box::new(parse(e1)), Box::new(parse(e2))),
            [Atom("*"), e1, e2] => EPrim2(Times, Box::new(parse(e1)), Box::new(parse(e2))),
            [Atom("let"), List(l), e2] => ELet(
                l.into_iter().map(parse_binding).collect(),
                Box::new(parse(e2)),
            ),
            _ => panic!("Parse error {:?}", sexp),
        },
    }
}

pub fn parse_ast<'a>(sexps: &[Sexp<'a>]) -> Vec<Expr<'a>> {
    sexps.into_iter().map(parse).collect()
}
