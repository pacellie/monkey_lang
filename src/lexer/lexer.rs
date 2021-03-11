use crate::lexer::token::Token;

use std::iter::Iterator;
use std::str;

#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'a [u8],
    current: usize,
    peek: usize,
    ch: u8,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a [u8]) -> Lexer<'a> {
        let mut lexer = Lexer {
            input,
            current: 0,
            peek: 0,
            ch: 0,
        };

        lexer.read_char();

        lexer
    }

    fn read_char(&mut self) {
        if self.peek >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input[self.peek];
        }
        self.current = self.peek;
        self.peek += 1;
    }

    fn peek_char(&self) -> u8 {
        if self.peek >= self.input.len() {
            0
        } else {
            self.input[self.peek]
        }
    }

    fn read_if_peek(&mut self, ch: u8, yes: Token, no: Token) -> Token {
        if self.peek_char() == ch {
            self.read_char();
            yes
        } else {
            no
        }
    }

    fn read_while(&mut self, predicate: fn(u8) -> bool) -> &str {
        let position = self.current;

        while predicate(self.ch) {
            self.read_char()
        }

        str::from_utf8(&self.input[position..self.current]).unwrap()
    }

    fn skip_whitespace(&mut self) {
        while (self.ch as char).is_ascii_whitespace() {
            self.read_char()
        }
    }
}

fn lookup_ident(ident: &str) -> Token {
    #[rustfmt::skip]
    let token = match ident {
        "fn"     => Token::Function,
        "let"    => Token::Let,
        "true"   => Token::True,
        "false"  => Token::False,
        "if"     => Token::If,
        "else"   => Token::Else,
        "return" => Token::Return,
        _        => Token::Ident(ident.to_string()),
    };

    token
}

fn is_letter(ch: u8) -> bool {
    (ch as char).is_ascii_alphabetic() || (ch as char) == '_'
}

fn is_digit(ch: u8) -> bool {
    (ch as char).is_ascii_digit()
}

fn is_string_letter(ch: u8) -> bool {
    ch != 0 && (ch as char) != '"'
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();

        let token = match self.ch {
            b'=' => self.read_if_peek(b'=', Token::Eq, Token::Assign),
            b'+' => Token::Plus,
            b'-' => Token::Minus,
            b'!' => self.read_if_peek(b'=', Token::Neq, Token::Bang),
            b'*' => Token::Asterisk,
            b'/' => Token::Slash,
            b'<' => Token::Lt,
            b'>' => Token::Gt,
            b',' => Token::Comma,
            b';' => Token::Semicolon,
            b'(' => Token::LParen,
            b')' => Token::RParen,
            b'{' => Token::LBrace,
            b'}' => Token::RBrace,
            b'"' => {
                self.read_char();
                Token::String(self.read_while(is_string_letter).to_string())
            }
            0 => Token::Eof,
            _ => {
                if is_letter(self.ch) {
                    return Some(lookup_ident(self.read_while(is_letter)));
                } else if is_digit(self.ch) {
                    return Some(Token::Int(self.read_while(is_digit).to_string()));
                } else {
                    Token::Illegal(self.ch.to_string())
                }
            }
        };

        self.read_char();

        Some(token)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn example1() {
        let input = b"=+(){},;";

        let expected = vec![
            Token::Assign,
            Token::Plus,
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::Comma,
            Token::Semicolon,
        ];

        let lexer = Lexer::new(input);
        let tokens: Vec<Token> = lexer
            .into_iter()
            .take_while(|token| token != &Token::Eof)
            .collect();

        assert_eq!(tokens, expected)
    }

    #[test]
    fn example2() {
        let input = b"let five = 5;\n\
            let ten = 10;\n\
            \n\
            let add = fn(x, y) {\n\
                x + y;\n\
            };\n\
            \n\
            let result = add(five, ten);\n\
            ";

        let expected = vec![
            Token::Let,
            Token::Ident("five".to_string()),
            Token::Assign,
            Token::Int("5".to_string()),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".to_string()),
            Token::Assign,
            Token::Int("10".to_string()),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::RParen,
            Token::LBrace,
            Token::Ident("x".to_string()),
            Token::Plus,
            Token::Ident("y".to_string()),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".to_string()),
            Token::Assign,
            Token::Ident("add".to_string()),
            Token::LParen,
            Token::Ident("five".to_string()),
            Token::Comma,
            Token::Ident("ten".to_string()),
            Token::RParen,
            Token::Semicolon,
        ];

        let lexer = Lexer::new(input);
        let tokens: Vec<Token> = lexer
            .into_iter()
            .take_while(|token| token != &Token::Eof)
            .collect();

        assert_eq!(tokens, expected)
    }

    #[test]
    fn example3() {
        let input = b"!-/*5;\n\
            5 < 10 > 5;\n\
            \n\
            if (5 < 10) {\n\
                return true;\n\
            } else {\n\
                return false;\n\
            }\n\
            \n\
            10 == 10;\n\
            10 != 9;\n\
            \"foobar\"\n\
            \"foo bar\"\n\
            ";

        let expected = vec![
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int("5".to_string()),
            Token::Semicolon,
            Token::Int("5".to_string()),
            Token::Lt,
            Token::Int("10".to_string()),
            Token::Gt,
            Token::Int("5".to_string()),
            Token::Semicolon,
            Token::If,
            Token::LParen,
            Token::Int("5".to_string()),
            Token::Lt,
            Token::Int("10".to_string()),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RBrace,
            Token::Int("10".to_string()),
            Token::Eq,
            Token::Int("10".to_string()),
            Token::Semicolon,
            Token::Int("10".to_string()),
            Token::Neq,
            Token::Int("9".to_string()),
            Token::Semicolon,
            Token::String("foobar".to_string()),
            Token::String("foo bar".to_string()),
        ];

        let lexer = Lexer::new(input);
        let tokens: Vec<Token> = lexer
            .into_iter()
            .take_while(|token| token != &Token::Eof)
            .collect();

        assert_eq!(tokens, expected)
    }
}
