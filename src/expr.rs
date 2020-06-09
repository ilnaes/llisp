use crate::backend::llvm::scope::*;
use crate::sexp::{Sexp, Sexp::*};
use expr::{Expr::*, Prim2::*, *};
use regex::Regex;

pub mod expr;

const FORBIDDEN_ID_REGEX: &'static str = r"[^\w\-\?]+";

const RESERVED_NAMES: &'static [&'static str] = &[
    "let", "if", "print", "true", "false", "defn", "self", "lambda",
];

fn proper_name<'a>(x: &'a str) -> bool {
    let forbid = Regex::new(FORBIDDEN_ID_REGEX).unwrap();
    let alpha = Regex::new(r"[a-zA-Z]").unwrap();
    return !forbid.is_match(x) && alpha.is_match(x) && !RESERVED_NAMES.contains(&x);
}

fn parse_def<'a>(sexp: &Sexp<'a>, gen: &mut Generator) -> Result<Def<'a>, String> {
    match sexp {
        Atom(_) => Err(format!("Parse error: Invalid def {:?}", sexp)),
        List(v) => match &v[..] {
            [Atom("defn"), Atom(f), List(args), body] => {
                let mut args_vec: Vec<&'a str> = Vec::new();
                for a in args.into_iter() {
                    match a {
                        Atom(x) => {
                            if proper_name(x) {
                                args_vec.push(x)
                            } else {
                                return Err(format!("Parse error: Invalid parameter {}", x));
                            }
                        }
                        _ => return Err(format!("Parse error: Invalid param {:?}", a)),
                    }
                }

                if !proper_name(f) {
                    return Err(format!("Parse error: Invalid function name {}", f));
                }

                Ok(Def::FuncDef(
                    EId(f),
                    args_vec
                        .into_iter()
                        .map(|x| Expr::EId(x))
                        .collect::<Vec<Expr<'a>>>(),
                    parse_expr(body, gen)?,
                ))
            }
            _ => Err(format!("Parse error: Not proper def {:?}", sexp)),
        },
    }
}

fn parse_expr<'a>(sexp: &Sexp<'a>, gen: &mut Generator) -> Result<Expr<'a>, String> {
    match sexp {
        Atom("true") => Ok(EBool(true)),
        Atom("false") => Ok(EBool(false)),
        Atom(s) => {
            if let Ok(i) = s.parse::<i64>() {
                Ok(ENum(i))
            } else {
                if proper_name(s) {
                    Ok(EId(s))
                } else {
                    Err(format!("Parse error: Invalid identifier {}", s))
                }
            }
        }
        List(v) => match &v[..] {
            [Atom("print"), e] => Ok(EPrint(Box::new(parse_expr(e, gen)?))),
            [Atom("+"), e1, e2] => Ok(EPrim2(
                Add,
                Box::new(parse_expr(e1, gen)?),
                Box::new(parse_expr(e2, gen)?),
            )),
            [Atom("-"), e1, e2] => Ok(EPrim2(
                Minus,
                Box::new(parse_expr(e1, gen)?),
                Box::new(parse_expr(e2, gen)?),
            )),
            [Atom("*"), e1, e2] => Ok(EPrim2(
                Times,
                Box::new(parse_expr(e1, gen)?),
                Box::new(parse_expr(e2, gen)?),
            )),
            [Atom("<"), e1, e2] => Ok(EPrim2(
                Less,
                Box::new(parse_expr(e1, gen)?),
                Box::new(parse_expr(e2, gen)?),
            )),
            [Atom(">"), e1, e2] => Ok(EPrim2(
                Greater,
                Box::new(parse_expr(e1, gen)?),
                Box::new(parse_expr(e2, gen)?),
            )),
            [Atom("=="), e1, e2] => Ok(EPrim2(
                Equal,
                Box::new(parse_expr(e1, gen)?),
                Box::new(parse_expr(e2, gen)?),
            )),
            [Atom("let"), List(l), e2] => Ok(ELet(
                l.into_iter()
                    .map(|x| parse_binding(x, gen))
                    .collect::<Result<Vec<Binding<'a>>, String>>()?,
                Box::new(parse_expr(e2, gen)?),
            )),
            [Atom("if"), e1, e2, e3] => Ok(EIf(
                Box::new(parse_expr(e1, gen)?),
                Box::new(parse_expr(e2, gen)?),
                Box::new(parse_expr(e3, gen)?),
            )),
            [Atom("lambda"), List(args), body] => {
                let s = gen.sym();
                Ok(ELambda(
                    s,
                    args.iter()
                        .map(|x| {
                            if let Atom(s) = x {
                                Ok(EId(s))
                            } else {
                                Err(format!("Parse error: Invalid lambda argument {:?}", x))
                            }
                        })
                        .collect::<Result<Vec<Expr<'a>>, String>>()?,
                    Box::new(parse_expr(body, gen)?),
                ))
            }
            _ => {
                if v.len() == 0 {
                    return Err(format!("Parse error: {:?}", sexp));
                }

                let mut args = Vec::new();

                for i in 1..v.len() {
                    args.push(parse_expr(&v[i], gen)?)
                }

                Ok(EApp(Box::new(parse_expr(&v[0], gen)?), args))
            }
        },
    }
}

fn parse_binding<'a>(b: &Sexp<'a>, gen: &mut Generator) -> Result<Binding<'a>, String> {
    match b {
        List(l) => match &l[..] {
            [Atom(x), e] => {
                if proper_name(x) {
                    Ok(Binding(EId(x), parse_expr(e, gen)?))
                } else {
                    return Err(format!("Parse error: Invalid binding {}", x));
                }
            }
            _ => return Err(format!("Parse error: binding {:?}", b)),
        },
        _ => return Err(format!("Parse error: binding {:?}", b)),
    }
}

pub fn parse_ast<'a>(sexps: &[Sexp<'a>]) -> Result<(Vec<Def<'a>>, Generator), String> {
    let mut gen = Generator::new();
    Ok((
        sexps
            .into_iter()
            .map(|x| parse_def(x, &mut gen))
            .collect::<Result<Vec<Def<'a>>, String>>()?,
        gen,
    ))
}
