use crate::sexp::{Sexp, Sexp::*};
use expr::{Expr::*, Prim2::*, *};
use regex::Regex;

pub mod expr;

const FORBIDDEN_ID_REGEX: &'static str = r"[^\w\-\?]+";
const RESERVED_NAMES: &'static [&'static str] =
    &["let", "if", "print", "true", "false", "func", "defn"];

// (defn f (args) (body))
fn parse_def<'a>(sexp: &Sexp<'a>) -> Result<Def<'a>, String> {
    match sexp {
        Atom(_) => Err(format!("Parse error: Invalid def {:?}", sexp)),
        List(v) => match &v[..] {
            [Atom("defn"), Atom(f), List(args), body] => {
                let mut args_vec: Vec<&'a str> = Vec::new();
                for a in args.into_iter() {
                    match a {
                        Atom(x) => args_vec.push(x),
                        _ => return Err(format!("Parse error: Invalid param {:?}", a)),
                    }
                }

                Ok(Def::FuncDef(
                    EId(f),
                    args_vec
                        .into_iter()
                        .map(|x| Expr::EId(x))
                        .collect::<Vec<Expr<'a>>>(),
                    parse_expr(body)?,
                ))
            }
            _ => Err(format!("Parse error: Not proper def {:?}", sexp)),
        },
    }
}

fn parse_expr<'a>(sexp: &Sexp<'a>) -> Result<Expr<'a>, String> {
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
            [Atom("print"), e] => Ok(EPrint(Box::new(parse_expr(e)?))),
            [Atom("+"), e1, e2] => Ok(EPrim2(
                Add,
                Box::new(parse_expr(e1)?),
                Box::new(parse_expr(e2)?),
            )),
            [Atom("-"), e1, e2] => Ok(EPrim2(
                Minus,
                Box::new(parse_expr(e1)?),
                Box::new(parse_expr(e2)?),
            )),
            [Atom("*"), e1, e2] => Ok(EPrim2(
                Times,
                Box::new(parse_expr(e1)?),
                Box::new(parse_expr(e2)?),
            )),
            [Atom("<"), e1, e2] => Ok(EPrim2(
                Less,
                Box::new(parse_expr(e1)?),
                Box::new(parse_expr(e2)?),
            )),
            [Atom(">"), e1, e2] => Ok(EPrim2(
                Greater,
                Box::new(parse_expr(e1)?),
                Box::new(parse_expr(e2)?),
            )),
            [Atom("=="), e1, e2] => Ok(EPrim2(
                Equal,
                Box::new(parse_expr(e1)?),
                Box::new(parse_expr(e2)?),
            )),
            [Atom("let"), List(l), e2] => Ok(ELet(
                l.into_iter()
                    .map(parse_binding)
                    .collect::<Result<Vec<Binding<'a>>, String>>()?,
                Box::new(parse_expr(e2)?),
            )),
            [Atom("if"), e1, e2, e3] => Ok(EIf(
                Box::new(parse_expr(e1)?),
                Box::new(parse_expr(e2)?),
                Box::new(parse_expr(e3)?),
            )),
            _ => return Err(format!("Parse error: {:?}", sexp)),
        },
    }
}

fn parse_binding<'a>(b: &Sexp<'a>) -> Result<Binding<'a>, String> {
    match b {
        List(l) => match &l[..] {
            [Atom(x), e] => Ok(Binding(x, parse_expr(e)?)),
            _ => return Err(format!("Parse error: binding {:?}", b)),
        },
        _ => return Err(format!("Parse error: binding {:?}", b)),
    }
}

pub fn parse_ast<'a>(sexps: &[Sexp<'a>]) -> Result<Vec<Def<'a>>, String> {
    sexps
        .into_iter()
        .map(parse_def)
        .collect::<Result<Vec<Def<'a>>, String>>()
}
