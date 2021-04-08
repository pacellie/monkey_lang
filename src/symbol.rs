use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Scope {
    Global,
    Local,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Symbol {
    pub scope: Scope,
    pub index: u16,
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    pub outer: Option<Box<SymbolTable>>,
    store: HashMap<String, Symbol>,
}

impl SymbolTable {
    pub fn empty() -> SymbolTable {
        SymbolTable {
            outer: None,
            store: HashMap::new(),
        }
    }

    pub fn new(outer: SymbolTable) -> SymbolTable {
        SymbolTable {
            outer: Some(Box::new(outer)),
            store: HashMap::new(),
        }
    }

    pub fn size(&self) -> usize {
        self.store.len()
    }

    pub fn define(&mut self, name: &str) -> &Symbol {
        let scope = if self.outer.is_none() {
            Scope::Global
        } else {
            Scope::Local
        };

        let symbol = Symbol {
            scope,
            index: self.store.len() as u16,
        };

        self.store.entry(name.to_string()).or_insert(symbol)
    }

    pub fn resolve(&self, name: &str) -> Option<&Symbol> {
        self.store
            .get(name)
            .or_else(|| self.outer.as_ref().and_then(|outer| outer.resolve(name)))
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
    fn define_resolve_0() {
        let mut tbl = SymbolTable::empty();
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

    #[test]
    fn define_resolve_1() {
        let mut tbl = SymbolTable::empty();
        let a0 = tbl.define("a").clone();
        let b0 = tbl.define("b").clone();

        let mut tbl = SymbolTable::new(tbl);
        let c0 = tbl.define("c").clone();
        let d0 = tbl.define("d").clone();

        let a1 = tbl.resolve("a");
        let b1 = tbl.resolve("b");
        let c1 = tbl.resolve("c");
        let d1 = tbl.resolve("d");

        let a = Symbol {
            scope: Scope::Global,
            index: 0,
        };
        let b = Symbol {
            scope: Scope::Global,
            index: 1,
        };
        let c = Symbol {
            scope: Scope::Local,
            index: 0,
        };
        let d = Symbol {
            scope: Scope::Local,
            index: 1,
        };

        assert!(a0 == a);
        assert!(b0 == b);
        assert!(a1.unwrap() == &a);
        assert!(b1.unwrap() == &b);
        assert!(c0 == c);
        assert!(d0 == d);
        assert!(c1.unwrap() == &c);
        assert!(d1.unwrap() == &d)
    }
}
