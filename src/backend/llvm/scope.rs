use im::HashMap;

#[derive(Debug, Clone)]
pub struct Scope<'a> {
    map: HashMap<&'a str, String>,
    n: i64,
}

impl<'a> Scope<'a> {
    pub fn new(_: &[u8]) -> Scope {
        Scope {
            map: HashMap::new(),
            n: 0,
        }
    }

    /// inserts a new variable
    pub fn register(&mut self, s: &'a str) -> String {
        self.n += 1;
        let ok = if self.map.contains_key(s) {
            self.map
                .insert(s, format!("{}{}", s.replace("-", "_"), self.n))
        } else {
            self.map.insert(s, String::from(s.replace("-", "_")))
        };

        if let Some(res) = ok {
            res
        } else {
            panic!("Bad insertion");
        }
    }

    /// gets a new clean symbol
    pub fn sym(&self) -> String {
        format!("sym{}", self.n)
    }

    /// gets variable name associated to string
    pub fn get(&'a self, s: &'a str) -> Option<&String> {
        self.map.get(s)
    }
}
