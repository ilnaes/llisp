use crate::sexp::{Sexp, Sexp::*};
use expr::{Binding, Expr, Expr::*};
use prim2::Prim2::*;
use regex::Regex;

pub mod expr;
pub mod prim2;

const FORBIDDEN_ID_REGEX: &'static str = r"[^\w\-]+";
const RESERVED_NAMES: &'static [&'static str] = &["let", "if"];

fn parse_binding<'a>(b: &Sexp<'a>) -> Result<Binding<'a>, String> {
    match b {
        List(l) => match &l[..] {
            [Atom(x), e] => Ok(Binding(x, parse(e)?)),
            _ => return Err(format!("Parse error: binding {:?}", b)),
        },
        _ => return Err(format!("Parse error: binding {:?}", b)),
    }
}

fn parse<'a>(sexp: &Sexp<'a>) -> Result<Expr<'a>, String> {
    match sexp {
        Atom("true") => Ok(EBool(true)),
        Atom("false") => Ok(EBool(false)),
        Atom(s) => {
            if let Ok(i) = s.parse::<i64>() {
                Ok(ENum(i))
            } else {
                let re = Regex::new(FORBIDDEN_ID_REGEX).unwrap();
                if !re.is_match(s) && !RESERVED_NAMES.contains(s) {
                    Ok(EId(s))
                } else {
                    Err(format!("Parse error: Invalid identifier {}", s))
                }
            }
        }
        List(v) => match &v[..] {
            [Atom("+"), e1, e2] => Ok(EPrim2(Add, Box::new(parse(e1)?), Box::new(parse(e2)?))),
            [Atom("-"), e1, e2] => Ok(EPrim2(Minus, Box::new(parse(e1)?), Box::new(parse(e2)?))),
            [Atom("*"), e1, e2] => Ok(EPrim2(Times, Box::new(parse(e1)?), Box::new(parse(e2)?))),
            [Atom("<"), e1, e2] => Ok(EPrim2(Less, Box::new(parse(e1)?), Box::new(parse(e2)?))),
            [Atom(">"), e1, e2] => Ok(EPrim2(Greater, Box::new(parse(e1)?), Box::new(parse(e2)?))),
            [Atom("=="), e1, e2] => Ok(EPrim2(Equal, Box::new(parse(e1)?), Box::new(parse(e2)?))),
            [Atom("let"), List(l), e2] => Ok(ELet(
                l.into_iter()
                    .map(parse_binding)
                    .collect::<Result<Vec<Binding<'a>>, String>>()?,
                Box::new(parse(e2)?),
            )),
            [Atom("if"), e1, e2, e3] => Ok(EIf(
                Box::new(parse(e1)?),
                Box::new(parse(e2)?),
                Box::new(parse(e3)?),
            )),
            _ => return Err(format!("Parse error: {:?}", sexp)),
        },
    }
}

pub fn parse_ast<'a>(sexps: &[Sexp<'a>]) -> Result<Vec<Expr<'a>>, String> {
    sexps
        .into_iter()
        .map(parse)
        .collect::<Result<Vec<Expr<'a>>, String>>()
}
