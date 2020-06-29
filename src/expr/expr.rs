#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Def<'a> {
    // second field Exprs will always be EId of string,
    // first field will either be EId (global func) or
    // ELambda for lifted lambda
    FuncDef(Expr<'a>, Vec<Expr<'a>>, Expr<'a>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Expr<'a> {
    ENum(i64),
    EBool(bool),
    EId(&'a str),
    ETup(Vec<Expr<'a>>),
    EPrim2(Prim2, Box<Expr<'a>>, Box<Expr<'a>>),
    ELet(Vec<Binding<'a>>, Box<Expr<'a>>),
    EIf(Box<Expr<'a>>, Box<Expr<'a>>, Box<Expr<'a>>),
    EPrint(Box<Expr<'a>>),
    EApp(Box<Expr<'a>>, Vec<Expr<'a>>),
    ELambda(String, Vec<Expr<'a>>, Box<Expr<'a>>), // string is name of function in LLVM
}

impl<'a> Expr<'a> {
    pub fn get_str(&self) -> Result<String, String> {
        match self {
            Expr::EId(s) => Ok(s.to_string()),
            _ => Err("Expr not EId".to_string()),
        }
    }
}

// first will always be EId(x)
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Binding<'a>(pub Expr<'a>, pub Expr<'a>);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Prim2 {
    Add,
    Minus,
    Times,
    Less,
    Greater,
    Equal,
}
