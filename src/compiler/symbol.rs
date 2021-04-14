use crate::builtin::Builtin;

use std::collections::HashMap;
use std::fmt;

use itertools::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Scope {
    Global,
    Local,
    Builtin,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Symbol {
    pub scope: Scope,
    pub index: u16,
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({:?}, {})", self.scope, self.index)
    }
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

    pub fn toplevel() -> SymbolTable {
        let mut store = HashMap::new();

        for (index, builtin) in Builtin::builtins().iter().enumerate() {
            let symbol = Symbol {
                scope: Scope::Builtin,
                index: index as u16,
            };

            store.insert(builtin.to_string(), symbol);
        }

        SymbolTable { outer: None, store }
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
        let symbol = if self.outer.is_none() {
            Symbol {
                scope: Scope::Global,
                index: self.store.len() as u16 - 6,
            }
        } else {
            Symbol {
                scope: Scope::Local,
                index: self.store.len() as u16,
            }
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
        write!(
            f,
            "{{{}}}",
            self.store
                .iter()
                .map(|(key, value)| format!("{} -> {}", key, value))
                .join(", ")
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn define_resolve_0() {
        let mut tbl = SymbolTable::toplevel();
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
        let mut tbl = SymbolTable::toplevel();
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
