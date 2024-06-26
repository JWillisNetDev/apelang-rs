//! This module lexes.
//!

use crate::ApeInteger;
use phf::phf_map;
use std::{
    iter::{Iterator, Peekable},
    str::Chars,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexToken {
    // Keywords
    Let,    // let
    If,     // if
    Else,   // else
    Fn,     // fn
    Return, // return
    True,   // true
    False,  // false

    // Operators
    Plus,              // +
    PlusAssign,        // +=
    Minus,             // -
    MinusAssign,       // -=
    Multiply,          // *
    MultiplyAssign,    // *=
    Power,             // **
    Divide,            // /
    DivideAssign,      // /=
    Assign,            // =
    Equals,            // ==
    Lambda,            // =>
    Bang,              // !
    NotEquals,         // !=
    GreaterThan,       // >
    GreaterThanEquals, // >=
    LessThan,          // <
    LessThanEquals,    // <=

    // Delimiters
    Semicolon, // ;
    Comma,     // ,
    LParen,    // (
    RParen,    // )
    LBrace,    // {
    RBrace,    // }
    LBracket,  // [
    RBracket,  // ]

    // Literals
    Integer(ApeInteger), // [0-9]+
    Identifier(String),  // [A-Za-z]+ (not another token)

    Error(String),
}

static KEYWORDS: phf::Map<&'static str, LexToken> = phf_map! {
    "let" => LexToken::Let,
    "if" => LexToken::If,
    "else" => LexToken::Else,
    "fn" => LexToken::Fn,
    "return" => LexToken::Return,
    "true" => LexToken::True,
    "false" => LexToken::False,
};

fn try_get_keyword(keyword: &str) -> Option<LexToken> {
    Some(KEYWORDS.get(keyword)?.clone())
}

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

fn is_delimiter(c: char) -> bool {
    c == ' '
        || c == ';'
        || c == ','
        || c == '('
        || c == ')'
        || c == '{'
        || c == '}'
        || c == '['
        || c == ']'
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer {
        Self {
            input: input.chars().peekable(),
        }
    }

    fn read_to_delimiter(&mut self) -> String {
        let mut buffer = String::new();
        while let Some(c) = self.input.peek() {
            if is_delimiter(*c) {
                break;
            }
            buffer.push(self.input.next().unwrap());
        }
        buffer
    }

    fn try_read_identifier_or_keyword(&mut self) -> Result<LexToken, String> {
        let ident = self.read_to_delimiter();
        if ident.is_empty() {
            return Err("Attempted to read, but found nothing.".into());
        }
        match try_get_keyword(ident.as_str()) {
            Some(token) => Ok(token),
            None => Ok(LexToken::Identifier(ident)),
        }
    }

    fn try_read_literal(&mut self) -> Result<LexToken, String> {
        let literal = self.read_to_delimiter();
        let literal = literal
            .parse::<ApeInteger>()
            .map_err(|_| format!("Could not convert `{literal}` to an integer!"))?;
        Ok(LexToken::Integer(literal))
    }

    /// Skips the buffer over all whitespace characters. The next character is guaranteed to not be a whitespace.
    fn eat_whitespace(&mut self) {
        while let Some(c) = self.input.peek() {
            if c.is_whitespace() {
                self.input.next();
            } else {
                break;
            }
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = LexToken;

    fn next(&mut self) -> Option<Self::Item> {
        self.eat_whitespace();

        match self.input.peek() {
            // Lex numeric literals
            Some(c) if c.is_numeric() => match self.try_read_literal() {
                Ok(token) => Some(token),
                Err(err) => Some(LexToken::Error(err)),
            },

            // Lex identifiers and keywords
            Some(c) if c.is_alphabetic() => match self.try_read_identifier_or_keyword() {
                Ok(identifier) => Some(identifier),
                Err(err) => Some(LexToken::Error(err)),
            },

            // Lex delimiters
            Some(&';') => {
                self.input.next();
                Some(LexToken::Semicolon)
            }
            Some(&',') => {
                self.input.next();
                Some(LexToken::Comma)
            }
            Some(&'(') => {
                self.input.next();
                Some(LexToken::LParen)
            }
            Some(&')') => {
                self.input.next();
                Some(LexToken::RParen)
            }
            Some(&'{') => {
                self.input.next();
                Some(LexToken::LBrace)
            }
            Some(&'}') => {
                self.input.next();
                Some(LexToken::RBrace)
            }
            Some(&'[') => {
                self.input.next();
                Some(LexToken::LBracket)
            }
            Some(&']') => {
                self.input.next();
                Some(LexToken::RBracket)
            }

            // Lex unary and binary operators
            Some(&'+') => {
                self.input.next();
                if self.input.peek() == Some(&'=') {
                    self.input.next();
                    Some(LexToken::PlusAssign)
                } else {
                    Some(LexToken::Plus)
                }
            }
            Some(&'-') => {
                self.input.next();
                if self.input.peek() == Some(&'=') {
                    self.input.next();
                    Some(LexToken::MinusAssign)
                } else {
                    Some(LexToken::Minus)
                }
            }
            Some(&'*') => {
                self.input.next();
                if self.input.peek() == Some(&'=') {
                    self.input.next();
                    Some(LexToken::MultiplyAssign)
                } else if self.input.peek() == Some(&'*') {
                    self.input.next();
                    Some(LexToken::Power)
                } else {
                    Some(LexToken::Multiply)
                }
            }
            Some(&'/') => {
                self.input.next();
                if self.input.peek() == Some(&'=') {
                    self.input.next();
                    Some(LexToken::DivideAssign)
                } else {
                    Some(LexToken::Divide)
                }
            }
            Some(&'=') => {
                self.input.next();
                if self.input.peek() == Some(&'=') {
                    self.input.next();
                    Some(LexToken::Equals)
                } else if self.input.peek() == Some(&'>') {
                    self.input.next();
                    Some(LexToken::Lambda)
                } else {
                    Some(LexToken::Assign)
                }
            }
            Some(&'>') => {
                self.input.next();
                if self.input.peek() == Some(&'=') {
                    self.input.next();
                    Some(LexToken::GreaterThanEquals)
                } else {
                    Some(LexToken::GreaterThan)
                }
            }
            Some(&'<') => {
                self.input.next();
                if self.input.peek() == Some(&'=') {
                    self.input.next();
                    Some(LexToken::LessThanEquals)
                } else {
                    Some(LexToken::LessThan)
                }
            }
            Some(&'!') => {
                self.input.next();
                if self.input.peek() == Some(&'=') {
                    self.input.next();
                    Some(LexToken::NotEquals)
                } else {
                    Some(LexToken::Bang)
                }
            }

            _ => None,
        }
    }
}

// TODO: Errors
#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn it_reads_words_and_eats_whitespace() {
        const INPUT: &'static str = "  this is  surrounded   by   spaces  a";

        let mut lexer = Lexer::new(INPUT);

        lexer.eat_whitespace();
        assert_eq!("this".to_string(), lexer.read_to_delimiter());

        lexer.eat_whitespace();
        assert_eq!("is".to_string(), lexer.read_to_delimiter());

        lexer.eat_whitespace();
        assert_eq!("surrounded".to_string(), lexer.read_to_delimiter());

        lexer.eat_whitespace();
        assert_eq!("by".to_string(), lexer.read_to_delimiter());

        lexer.eat_whitespace();
        assert_eq!("spaces".to_string(), lexer.read_to_delimiter());

        lexer.eat_whitespace();
        assert_eq!("a".to_string(), lexer.read_to_delimiter());

        lexer.eat_whitespace();
        assert_eq!("", lexer.read_to_delimiter());
    }

    #[test]
    fn it_lexes() {
        const INPUT: &'static str = "let var = 100;";

        let mut actual = Lexer::new(INPUT);

        assert_eq!(Some(LexToken::Let), actual.next());
        assert_eq!(Some(LexToken::Identifier("var".into())), actual.next());
        assert_eq!(Some(LexToken::Assign), actual.next());
        assert_eq!(Some(LexToken::Integer(ApeInteger(100))), actual.next());
        assert_eq!(Some(LexToken::Semicolon), actual.next());

        assert_eq!(None, actual.next());
    }

    #[test]
    fn it_lexes_function_calls() {
        const INPUT: &'static str = "let closure = fn(x, y) => x + y; closure(10, 20);";

        let mut actual = Lexer::new(INPUT);

        assert_eq!(Some(LexToken::Let), actual.next());
        assert_eq!(Some(LexToken::Identifier("closure".into())), actual.next());
        assert_eq!(Some(LexToken::Assign), actual.next());
        assert_eq!(Some(LexToken::Fn), actual.next());
        assert_eq!(Some(LexToken::LParen), actual.next());
        assert_eq!(Some(LexToken::Identifier("x".into())), actual.next());
        assert_eq!(Some(LexToken::Comma), actual.next());
        assert_eq!(Some(LexToken::Identifier("y".into())), actual.next());
        assert_eq!(Some(LexToken::RParen), actual.next());
        assert_eq!(Some(LexToken::Lambda), actual.next());
        assert_eq!(Some(LexToken::Identifier("x".into())), actual.next());
        assert_eq!(Some(LexToken::Plus), actual.next());
        assert_eq!(Some(LexToken::Identifier("y".into())), actual.next());
        assert_eq!(Some(LexToken::Semicolon), actual.next());
        assert_eq!(Some(LexToken::Identifier("closure".into())), actual.next());
        assert_eq!(Some(LexToken::LParen), actual.next());
        assert_eq!(Some(LexToken::Integer(ApeInteger(10))), actual.next());
        assert_eq!(Some(LexToken::Comma), actual.next());
        assert_eq!(Some(LexToken::Integer(ApeInteger(20))), actual.next());
        assert_eq!(Some(LexToken::RParen), actual.next());
        assert_eq!(Some(LexToken::Semicolon), actual.next());

        assert_eq!(None, actual.next());
    }

    #[test]
    fn it_lexes_keywords() {
        const INPUT: &'static str = "let if fn return true false";

        let mut actual = Lexer::new(INPUT);

        assert_eq!(Some(LexToken::Let), actual.next());
        assert_eq!(Some(LexToken::If), actual.next());
        assert_eq!(Some(LexToken::Fn), actual.next());
        assert_eq!(Some(LexToken::Return), actual.next());
        assert_eq!(Some(LexToken::True), actual.next());
        assert_eq!(Some(LexToken::False), actual.next());

        assert_eq!(None, actual.next());
    }

    #[test]
    fn it_lexes_operations() {
        const INPUT: &'static str = "+ += - -= * *= ** / /= = == ! != > >= < <=";

        let mut actual = Lexer::new(INPUT);

        assert_eq!(Some(LexToken::Plus), actual.next());
        assert_eq!(Some(LexToken::PlusAssign), actual.next());
        assert_eq!(Some(LexToken::Minus), actual.next());
        assert_eq!(Some(LexToken::MinusAssign), actual.next());
        assert_eq!(Some(LexToken::Multiply), actual.next());
        assert_eq!(Some(LexToken::MultiplyAssign), actual.next());
        assert_eq!(Some(LexToken::Power), actual.next());
        assert_eq!(Some(LexToken::Divide), actual.next());
        assert_eq!(Some(LexToken::DivideAssign), actual.next());
        assert_eq!(Some(LexToken::Assign), actual.next());
        assert_eq!(Some(LexToken::Equals), actual.next());
        assert_eq!(Some(LexToken::Bang), actual.next());
        assert_eq!(Some(LexToken::NotEquals), actual.next());
        assert_eq!(Some(LexToken::GreaterThan), actual.next());
        assert_eq!(Some(LexToken::GreaterThanEquals), actual.next());
        assert_eq!(Some(LexToken::LessThan), actual.next());
        assert_eq!(Some(LexToken::LessThanEquals), actual.next());

        assert_eq!(None, actual.next());
    }

    #[test]
    fn it_lexes_delimiters() {
        const INPUT: &'static str = "() [] {} , ;";

        let mut actual = Lexer::new(INPUT);

        assert_eq!(Some(LexToken::LParen), actual.next());
        assert_eq!(Some(LexToken::RParen), actual.next());
        assert_eq!(Some(LexToken::LBracket), actual.next());
        assert_eq!(Some(LexToken::RBracket), actual.next());
        assert_eq!(Some(LexToken::LBrace), actual.next());
        assert_eq!(Some(LexToken::RBrace), actual.next());
        assert_eq!(Some(LexToken::Comma), actual.next());
        assert_eq!(Some(LexToken::Semicolon), actual.next());

        assert_eq!(None, actual.next());
    }

    #[test]
    fn it_lexes_ifs() {
        const INPUT: &'static str = "if (x1 == x2) { return 100; }";

        let mut actual = Lexer::new(INPUT);

        assert_eq!(Some(LexToken::If), actual.next());
        assert_eq!(Some(LexToken::LParen), actual.next());
        assert_eq!(Some(LexToken::Identifier("x1".into())), actual.next());
        assert_eq!(Some(LexToken::Equals), actual.next());
        assert_eq!(Some(LexToken::Identifier("x2".into())), actual.next());
        assert_eq!(Some(LexToken::RParen), actual.next());
        assert_eq!(Some(LexToken::LBrace), actual.next());
        assert_eq!(Some(LexToken::Return), actual.next());
        assert_eq!(Some(LexToken::Integer(ApeInteger(100))), actual.next());
        assert_eq!(Some(LexToken::Semicolon), actual.next());
        assert_eq!(Some(LexToken::RBrace), actual.next());

        assert_eq!(None, actual.next());
    }
}
