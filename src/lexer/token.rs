use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Eof,
    Illegal(String),

    // Identifiers and Literals
    Ident(String),
    Int(String),
    String(String),

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Lt,
    Gt,
    Eq,
    Neq,

    // Delimiters
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,

    // Keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        #[rustfmt::skip]
        let pretty = match self {
            Token::Illegal(string) => format!("ILLEGAL: {}", string),
            Token::Ident(string)   => string.clone(),
            Token::Int(string)     => string.clone(),
            Token::String(string)  => format!("\"{}\"", string),
            Token::Eof       => "EOF".to_string(),
            Token::Assign    => "=".to_string(),
            Token::Plus      => "+".to_string(),
            Token::Minus     => "-".to_string(),
            Token::Bang      => "!".to_string(),
            Token::Asterisk  => "*".to_string(),
            Token::Slash     => "/".to_string(),
            Token::Lt        => "<".to_string(),
            Token::Gt        => ">".to_string(),
            Token::Eq        => "==".to_string(),
            Token::Neq       => "!=".to_string(),
            Token::Comma     => ",".to_string(),
            Token::Semicolon => ";".to_string(),
            Token::LParen    => "(".to_string(),
            Token::RParen    => ")".to_string(),
            Token::LBracket  => "[".to_string(),
            Token::RBracket  => "]".to_string(),
            Token::LBrace    => "{".to_string(),
            Token::RBrace    => "}".to_string(),
            Token::Function  => "fn".to_string(),
            Token::Let       => "let".to_string(),
            Token::True      => "true".to_string(),
            Token::False     => "false".to_string(),
            Token::If        => "if".to_string(),
            Token::Else      => "else".to_string(),
            Token::Return    => "return".to_string(),
        };

        write!(f, "{}", pretty)
    }
}
