use super::*;
use im::HashMap;

pub struct Generator {
    n: usize,
}

impl Generator {
    pub fn new() -> Generator {
        Generator { n: 0 }
    }

    pub fn sym(&mut self, local: bool) -> Arg {
        self.n += 1;
        if local {
            Arg::AVar(Var::Local(format!("sym{}", self.n)))
        } else {
            Arg::AVar(Var::Global(format!("sym{}", self.n)))
        }
    }
}

#[derive(Clone)]
pub struct Scope<'a> {
    map: HashMap<&'a str, Arg>,
}

impl<'a> Scope<'a> {
    pub fn new(_: &'a str) -> Scope<'a> {
        Scope {
            map: HashMap::new(),
        }
    }

    /// inserts a new variable
    pub fn register(&mut self, k: &'a str, v: Arg) {
        self.map.insert(k, v);
    }

    /// gets variable name associated to string
    pub fn get(&self, s: &'a str) -> Result<Arg, String> {
        match self.map.get(s).cloned() {
            Some(x) => Ok(x),
            None => Err(format!("Unbound identifier {}", s)),
        }
    }
}
