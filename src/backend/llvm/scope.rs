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
    pub fn register(&mut self, s: String) -> Arg {
        self.n += 1;
        let ok = if self.map.contains_key(&s) {
            self.map.insert(
                s.clone(),
                Arg::AVar(Var::Local(format!("{}{}", s.replace("-", "_"), self.n))),
            )
        } else {
            self.map.insert(
                s.clone(),
                Arg::AVar(Var::Local(String::from(s.replace("-", "_")))),
            )
        };

        if let Some(res) = ok {
            res
        } else {
            panic!("Bad insertion");
        }
    }

    /// gets a new clean symbol
    pub fn sym(&mut self) -> Arg {
        self.n += 1;
        Arg::AVar(Var::Local(format!("sym{}", self.n)))
    }

    /// gets variable name associated to string
    pub fn get(&self, s: String) -> Option<&Arg> {
        self.map.get(&s)
    }
}
