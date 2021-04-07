use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Scope {
    Global,
    Local,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Symbol {
    scope: Scope,
    pub index: u16,
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    store: HashMap<String, Symbol>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            store: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: &str) -> &Symbol {
        let symbol = Symbol {
            scope: Scope::Global,
            index: self.store.len() as u16,
        };

        self.store.entry(name.to_string()).or_insert(symbol)
    }

    pub fn resolve(&self, name: &str) -> Option<&Symbol> {
        self.store.get(name)
    }
}

impl fmt::Display for SymbolTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.store)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn define_resolve() {
        let mut tbl = SymbolTable::new();
        let a0 = tbl.define("a").clone();
        let b0 = tbl.define("b").clone();

        let a1 = tbl.resolve("a");
        let b1 = tbl.resolve("b");

        let a = Symbol {
            scope: Scope::Global,
            index: 0,
        };
        let b = Symbol {
            scope: Scope::Global,
            index: 1,
        };

        assert!(a0 == a);
        assert!(b0 == b);
        assert!(a1.unwrap() == &a);
        assert!(b1.unwrap() == &b)
    }
}
