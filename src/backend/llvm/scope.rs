use super::*;
use im::HashMap;

#[derive(Debug, Clone)]
pub struct Scope {
    map: HashMap<String, Arg>,
    n: usize,
}

impl Scope {
    pub fn incr(&mut self, m: usize) {
        self.n += m
    }

    pub fn new() -> Scope {
        Scope {
            map: HashMap::new(),
            n: 0,
        }
    }

    /// inserts a new variable
    pub fn register(&mut self, k: String, v: Arg) {
        self.map.insert(k, v);
    }

    /// gets a new clean symbol
    pub fn sym(&mut self) -> Arg {
        self.n += 1;
        Arg::AVar(Var::Local(format!("sym{}", self.n)))
    }

    /// gets variable name associated to string
    pub fn get(&self, s: String) -> Arg {
        self.map.get(&s).cloned().expect("Invalid binding")
    }
}
