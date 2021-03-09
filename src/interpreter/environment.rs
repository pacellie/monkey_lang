use crate::interpreter::Object;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use itertools::Itertools;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Environment {
    parent: Option<Rc<RefCell<Environment>>>,
    map: HashMap<String, Object>,
}

impl fmt::Display for Environment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let parent = self
            .parent
            .as_ref()
            .map_or("".to_string(), |parent| parent.borrow().to_string());

        let map = self
            .map
            .iter()
            .map(|(key, value)| format!("{} -> {}", key, value))
            .join(", ");

        write!(f, "{} <= {{ {} }}", parent, map)
    }
}

impl Environment {
    pub fn empty() -> Environment {
        Environment {
            parent: None,
            map: HashMap::new(),
        }
    }

    pub fn new(parent: Rc<RefCell<Environment>>) -> Environment {
        Environment {
            parent: Some(parent),
            map: HashMap::new(),
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
}
