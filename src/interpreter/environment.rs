use crate::interpreter::Object;

use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::rc::Rc;

use itertools::Itertools;

thread_local! {
    static ID: Cell<usize> = Cell::new(0);
}

fn next_id() -> usize {
    ID.with(|cell| {
        let i = cell.get();
        cell.set(i + 1);
        i
    })
}

#[derive(Debug, Clone)]
pub struct Environment {
    parent: Option<Rc<RefCell<Environment>>>,
    map: HashMap<String, Object>,
    id: usize,
}

impl PartialEq for Environment {
    fn eq(&self, other: &Self) -> bool {
        self.parent == other.parent && self.map == other.map
    }
}

impl Eq for Environment {}

impl fmt::Display for Environment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.format(&mut HashSet::new()))
    }
}

impl Environment {
    pub fn empty() -> Environment {
        Environment {
            parent: None,
            map: HashMap::new(),
            id: next_id(),
        }
    }

    pub fn new(parent: Rc<RefCell<Environment>>) -> Environment {
        Environment {
            parent: Some(parent),
            map: HashMap::new(),
            id: next_id(),
        }
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        self.map.get(name).map(|obj| obj.clone()).or_else(|| {
            if let Some(parent) = &self.parent {
                parent.borrow().get(name)
            } else {
                None
            }
        })
    }

    pub fn set(&mut self, name: String, obj: Object) {
        self.map.insert(name, obj);
    }

    pub fn clear(&mut self) {
        self.map.clear();
    }

    fn format(&self, done: &mut HashSet<usize>) -> String {
        if done.contains(&self.id) {
            return "".to_string();
        }

        done.insert(self.id);

        let mut output = self
            .parent
            .as_ref()
            .map_or("".to_string(), |parent| parent.borrow().format(done));

        let pretty = self
            .map
            .iter()
            .map(|(key, value)| {
                if let Object::Function { env, .. } = value {
                    output.push_str(&env.borrow().format(done));
                    format!("{} :- {} -> {}", env.borrow().id, key, value)
                } else {
                    format!("{} -> {}", key, value)
                }
            })
            .join(", ");

        if output.len() != 0 {
            output.push_str("\n");
        }
        output.push_str(&format!("{}: {}", self.id, pretty));

        output
    }
}
