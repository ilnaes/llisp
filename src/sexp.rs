use std::str;

#[derive(Debug, Clone, PartialEq)]
pub enum Sexp<'a> {
    Atom(&'a str),
    List(Vec<Sexp<'a>>),
}

pub fn parse_sexps<'a>(prog: &'a str) -> Result<Vec<Sexp<'a>>, String> {
    let mut res: Vec<Sexp> = Vec::new();
    let mut i = 0;

    while i < prog.len() {
        let (curr, j) = parse_sexp(prog, i, 0)?;
        if curr != Sexp::Atom("") {
            res.push(curr);
        }
        i = j;
    }

    return Ok(res);
}

fn parse_sexp<'a>(
    prog: &'a str,
    mut start: usize,
    depth: i64,
) -> Result<(Sexp<'a>, usize), String> {
    let mut res: Vec<Sexp> = Vec::new();
    let mut list = false; // indicates if we are parsing list
    let mut i = start;
    let chars: Vec<char> = prog.chars().collect();

    while i < prog.len() {
        match chars[i] {
            '(' => {
                if i == start {
                    list = true;
                } else {
                    if !list {
                        // found ( in middle of parsing atom
                        let s = &prog[start..i];
                        return Ok((Sexp::Atom(s), i));
                    }

                    let (exp, j) = parse_sexp(prog, i, depth + 1)?;
                    res.push(exp);
                    i = j;
                    continue;
                }
            }
            ' ' | '\t' | '\n' => {
                if i == start {
                    // remove leading whitespace
                    start += 1;
                }
                if !list {
                    if i > start {
                        let s = &prog[start..i];
                        return Ok((Sexp::Atom(s), i));
                    }
                }
            }
            ')' => {
                if !list {
                    if depth == 0 {
                        return Err(format!(
                            "Lex error: Too many ) in {}",
                            prog.get(0..i).unwrap()
                        ));
                    }
                    let s = &prog[start..i];
                    return Ok((Sexp::Atom(s), i));
                } else {
                    return Ok((Sexp::List(res), i + 1));
                }
            }
            _ => {
                if list {
                    let (exp, j) = parse_sexp(prog, i, depth + 1)?;
                    res.push(exp);
                    i = j;
                    continue;
                }
            }
        }
        i += 1;
    }

    if depth > 0 || list {
        return Err("Lex error: Too many (".to_string());
    }

    if res.len() == 0 {
        let s = &prog[start..i];
        return Ok((Sexp::Atom(s), i));
    } else {
        return Ok((Sexp::List(res), i));
    }
}
