use crate::builtin::Builtin;

use std::collections::HashMap;
use std::fmt;

use itertools::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Scope {
    Global,
    Local,
    Free,
    Builtin,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Symbol {
    pub name: String,
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
    pub free_symbols: Vec<Symbol>,
    pub cnt: u16,
}

impl SymbolTable {
    pub fn empty() -> SymbolTable {
        SymbolTable {
            outer: None,
            store: HashMap::new(),
            free_symbols: vec![],
            cnt: 0,
        }
    }

    pub fn toplevel() -> SymbolTable {
        let mut store = HashMap::new();

        for (index, builtin) in Builtin::builtins().iter().enumerate() {
            let symbol = Symbol {
                name: builtin.to_string(),
                scope: Scope::Builtin,
                index: index as u16,
            };

            store.insert(builtin.to_string(), symbol);
        }

        SymbolTable {
            outer: None,
            store,
            free_symbols: vec![],
            cnt: 0,
        }
    }

    pub fn enclose(outer: SymbolTable) -> SymbolTable {
        SymbolTable {
            outer: Some(Box::new(outer)),
            store: HashMap::new(),
            free_symbols: vec![],
            cnt: 0,
        }
    }

    pub fn define(&mut self, name: &str) -> &Symbol {
        let symbol = if self.outer.is_none() {
            Symbol {
                name: name.to_string(),
                scope: Scope::Global,
                index: self.cnt,
            }
        } else {
            Symbol {
                name: name.to_string(),
                scope: Scope::Local,
                index: self.cnt,
            }
        };

        self.cnt += 1;

        self.store.entry(name.to_string()).or_insert(symbol)
    }

    fn define_free(&mut self, symbol: Symbol) -> Symbol {
        let name = symbol.name.clone();

        self.free_symbols.push(symbol);

        let symbol = Symbol {
            name: name.clone(),
            scope: Scope::Free,
            index: (self.free_symbols.len() - 1) as u16,
        };

        self.store.entry(name).or_insert(symbol.clone());

        symbol
    }

    pub fn resolve(&mut self, name: &str) -> Option<Symbol> {
        self.store
            .get(name)
            .map(|symbol| symbol.clone())
            .or_else(|| {
                if let Some(outer) = self.outer.as_mut() {
                    outer.resolve(name).and_then(|symbol| {
                        if symbol.scope == Scope::Global || symbol.scope == Scope::Builtin {
                            Some(symbol)
                        } else {
                            Some(self.define_free(symbol))
                        }
                    })
                } else {
                    None
                }
            })
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
        let mut global = SymbolTable::toplevel();
        let _ = global.define("a");
        let _ = global.define("b");

        let a0 = Symbol {
            name: "a".to_string(),
            scope: Scope::Global,
            index: 0,
        };
        let b0 = Symbol {
            name: "b".to_string(),
            scope: Scope::Global,
            index: 1,
        };

        let a1 = global.resolve("a").unwrap();
        let b1 = global.resolve("b").unwrap();

        assert_eq!(a0, a1);
        assert_eq!(b0, b1)
    }

    #[test]
    fn define_resolve_1() {
        let mut tbl = SymbolTable::toplevel();
        let _ = tbl.define("a");
        let _ = tbl.define("b");

        let mut tbl = SymbolTable::enclose(tbl);
        let _ = tbl.define("c");
        let _ = tbl.define("d");

        let a0 = Symbol {
            name: "a".to_string(),
            scope: Scope::Global,
            index: 0,
        };
        let b0 = Symbol {
            name: "b".to_string(),
            scope: Scope::Global,
            index: 1,
        };
        let c0 = Symbol {
            name: "c".to_string(),
            scope: Scope::Local,
            index: 0,
        };
        let d0 = Symbol {
            name: "d".to_string(),
            scope: Scope::Local,
            index: 1,
        };

        let a1 = tbl.resolve("a").unwrap();
        let b1 = tbl.resolve("b").unwrap();
        let c1 = tbl.resolve("c").unwrap();
        let d1 = tbl.resolve("d").unwrap();

        assert_eq!(a0, a1);
        assert_eq!(b0, b1);
        assert_eq!(c0, c1);
        assert_eq!(d0, d1)
    }

    #[test]
    fn define_resolve_2() {
        let mut global = SymbolTable::toplevel();
        let _ = global.define("a");
        let _ = global.define("b");

        let mut fst_local = SymbolTable::enclose(global);
        let _ = fst_local.define("c");
        let _ = fst_local.define("d");

        let mut snd_local = SymbolTable::enclose(fst_local);
        let _ = snd_local.define("e");
        let _ = snd_local.define("f");

        let a0 = Symbol {
            name: "a".to_string(),
            scope: Scope::Global,
            index: 0,
        };
        let b0 = Symbol {
            name: "b".to_string(),
            scope: Scope::Global,
            index: 1,
        };
        let c0 = Symbol {
            name: "c".to_string(),
            scope: Scope::Free,
            index: 0,
        };
        let d0 = Symbol {
            name: "d".to_string(),
            scope: Scope::Free,
            index: 1,
        };
        let e0 = Symbol {
            name: "e".to_string(),
            scope: Scope::Local,
            index: 0,
        };
        let f0 = Symbol {
            name: "f".to_string(),
            scope: Scope::Local,
            index: 1,
        };

        let a1 = snd_local.resolve("a").unwrap();
        let b1 = snd_local.resolve("b").unwrap();
        let c1 = snd_local.resolve("c").unwrap();
        let d1 = snd_local.resolve("d").unwrap();
        let e1 = snd_local.resolve("e").unwrap();
        let f1 = snd_local.resolve("f").unwrap();

        assert_eq!(a0, a1);
        assert_eq!(b0, b1);
        assert_eq!(c0, c1);
        assert_eq!(d0, d1);
        assert_eq!(e0, e1);
        assert_eq!(f0, f1)
    }

    #[test]
    fn define_resolve_3() {
        let mut global = SymbolTable::toplevel();
        let _ = global.define("a");

        let mut fst_local = SymbolTable::enclose(global);
        let _ = fst_local.define("c");

        let mut snd_local = SymbolTable::enclose(fst_local);
        let _ = snd_local.define("e");
        let _ = snd_local.define("f");

        let a0 = Symbol {
            name: "a".to_string(),
            scope: Scope::Global,
            index: 0,
        };
        let c0 = Symbol {
            name: "c".to_string(),
            scope: Scope::Free,
            index: 0,
        };
        let e0 = Symbol {
            name: "e".to_string(),
            scope: Scope::Local,
            index: 0,
        };
        let f0 = Symbol {
            name: "f".to_string(),
            scope: Scope::Local,
            index: 1,
        };

        let a1 = snd_local.resolve("a").unwrap();
        let c1 = snd_local.resolve("c").unwrap();
        let e1 = snd_local.resolve("e").unwrap();
        let f1 = snd_local.resolve("f").unwrap();

        let b = snd_local.resolve("b");
        let d = snd_local.resolve("d");

        assert_eq!(a0, a1);
        assert_eq!(c0, c1);
        assert_eq!(e0, e1);
        assert_eq!(f0, f1);
        assert!(b.is_none());
        assert!(d.is_none());
    }
}
