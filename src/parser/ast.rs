#[derive(Debug, PartialEq, Eq)]
pub struct Let {
    name: Name,
    value: Expression,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    LetKind(Let),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Name {
    name: String,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    NameKind(Name),
    Dummy,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Program {
    stmts: Vec<Statement>,
}

impl Let {
    pub fn new<S: Into<String>>(name: S, value: Expression) -> Let {
        Let {
            name: Name::new(name),
            value,
        }
    }
}

impl Statement {
    pub fn let_stmt<S: Into<String>>(name: S, value: Expression) -> Statement {
        Statement::LetKind(Let::new(name, value))
    }
}

impl Name {
    pub fn new<S: Into<String>>(name: S) -> Name {
        Name { name: name.into() }
    }
}

impl Program {
    pub fn new(stmts: Vec<Statement>) -> Program {
        Program { stmts }
    }
}
