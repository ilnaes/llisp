use super::*;
use im::HashMap;

#[derive(Debug, Clone)]
pub struct Scope<'a> {
    map: HashMap<&'a str, Arg>,
    n: usize,
}

impl<'a> Scope<'a> {
    pub fn incr(&mut self, m: usize) {
        self.n += m
    }

    pub fn new(_: &'a str) -> Scope<'a> {
        Scope {
            map: HashMap::new(),
            n: 0,
        }
    }

    /// inserts a new variable
    pub fn register(&mut self, k: &'a str, v: Arg) {
        self.map.insert(k, v);
    }

    /// gets a new clean symbol
    pub fn sym(&mut self) -> Arg {
        self.n += 1;
        Arg::AVar(Var::Local(format!("sym{}", self.n)))
    }

    /// gets variable name associated to string
    pub fn get(&self, s: &'a str) -> Result<Arg, String> {
        match self.map.get(s).cloned() {
            Some(x) => Ok(x),
            None => Err(format!("Unbound identifier {}", s)),
        }
    }
}
