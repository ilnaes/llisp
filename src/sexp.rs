#[derive(Debug, Clone)]
pub enum Sexp {
    Atom(String),
    List(Vec<Sexp>),
}

pub fn parse(prog: &[u8]) -> Vec<Sexp> {
    let mut res: Vec<Sexp> = Vec::new();
    let mut i = 0;

    while i < prog.len() - 1 {
        let (curr, j) = parse_sexp(prog, i);
        res.push(curr);
        i = j;
    }

    return res;
}

fn parse_sexp(prog: &[u8], mut start: usize) -> (Sexp, usize) {
    let mut res: Vec<Sexp> = Vec::new();
    let mut curr = Vec::new();
    let mut list = false; // indicates if we are parsing list
    let mut i = start;

    while i < prog.len() {
        match char::from(prog[i]) {
            '(' => {
                if i == start {
                    list = true;
                } else {
                    if !list {
                        // found ( in middle of parsing atom
                        let s = String::from_utf8(curr).unwrap();
                        return (Sexp::Atom(s), i);
                    }

                    let (exp, j) = parse_sexp(prog, i);
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
                    if curr.len() > 0 {
                        let s = String::from_utf8(curr).unwrap();
                        return (Sexp::Atom(s), i);
                    }
                }
            }
            ')' => {
                if !list {
                    let s = String::from_utf8(curr).unwrap();
                    return (Sexp::Atom(s), i);
                } else {
                    return (Sexp::List(res), i + 1);
                }
            }
            _ => {
                if !list {
                    curr.push(prog[i]);
                } else {
                    let (exp, j) = parse_sexp(prog, i);
                    res.push(exp);
                    i = j;
                    continue;
                }
            }
        }
        i += 1;
    }

    if res.len() == 0 {
        let s = String::from_utf8(curr).unwrap();
        return (Sexp::Atom(s), i);
    } else {
        return (Sexp::List(res), i);
    }
}
