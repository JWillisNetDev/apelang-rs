//! This module parses lex tokens into an AST
//!

use crate::{lexer::LexToken, ApeInteger, Identifier};
use std::iter::{Iterator, Peekable};

#[derive(Debug, Clone, PartialEq)]
pub enum Expression<'a> {
    BinaryOp(&'a Expression<'a>, &'a Expression<'a>),
    StringLiteral(String),
    IntegerLiteral(ApeInteger),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<'a> {
    Let {
        identifier: Identifier,
        value: &'a Expression<'a>,
    },
}

pub struct Parser<'a, T: Iterator<Item = &'a LexToken>> {
    tokens: Peekable<T>,
}

pub struct Program<'a> {
    statements: Vec<&'a Statement<'a>>,
}

impl<'a> Program<'a> {
    fn new(statements: Vec<&'a Statement>) -> Self {
        Program { statements }
    }

    fn from_empty() -> Self {
        Program { statements: vec![] }
    }
}

impl<'a, T> Parser<'a, T>
where
    T: Iterator<Item = &'a LexToken>,
{
    pub fn new(tokens: T) -> Self {
        Parser {
            tokens: tokens.peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<Program<'a>, String> {
        while let Some(token) = self.tokens.next() {
            match token {
                LexToken::Let => {
                    let identifier = self.expect_identifier()?;
                    self.expect_token_variant(&LexToken::Assignment)?;
                }
                _ => return Err(format!("Unexpected token encountered: {:?}", token)),
            }
        }

        Err("Some error".into())
    }

    fn expect_token_variant(&mut self, expected: &LexToken) -> Result<&LexToken, String> {
        if let Some(peek) = self.tokens.peek() {
            if std::mem::discriminant(*peek) == std::mem::discriminant(expected) {
                let token = self.tokens.next().unwrap();
                Ok(token)
            } else {
                Err(format!("Expected {:?}, found {:?}", expected, peek))
            }
        } else {
            Err(format!("Expected {:?}, found nothing", expected))
        }
    }

    fn expect_identifier(&mut self) -> Result<&str, String> {
        if let Some(peek) = self.tokens.peek() {
            match peek {
                LexToken::Identifier(ident) => {
                    self.tokens.next();
                    Ok(ident)
                }
                _ => Err(format!("Expected identifier, found {:?}", peek)),
            }
        } else {
            Err("Expected identifier, found none".into())
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn it_expects_identifiers() {
        let input = vec![
            LexToken::Identifier("SomeIdentifier".into()),
            LexToken::Identifier("SomeOtherIdentifier".into()),
            LexToken::Identifier("AnotherIdentifier".into()),
        ];

        let mut parser = Parser::new(input.iter().peekable());

        assert_eq!(parser.expect_identifier(), Ok("SomeIdentifier".into()));

        assert_eq!(parser.expect_identifier(), Ok("SomeOtherIdentifier".into()));

        assert_eq!(parser.expect_identifier(), Ok("AnotherIdentifier".into()));
    }

    #[test]
    fn it_expects_identifiers_and_errors() {
        let input = vec![LexToken::Let];

        let mut parser = Parser::new(input.iter().peekable());
        let actual = parser.expect_identifier();

        assert!(matches!(actual, Err(..)));
    }

    #[test]
    fn it_expects_token_variants() {
        let input = vec![LexToken::Let, LexToken::If, LexToken::Fn, LexToken::Return];

        let mut parser = Parser::new(input.iter().peekable());

        assert_eq!(
            Ok(&LexToken::Let),
            parser.expect_token_variant(&LexToken::Let)
        );

        assert_eq!(
            Ok(&LexToken::If),
            parser.expect_token_variant(&LexToken::If)
        );

        assert_eq!(
            Ok(&LexToken::Fn),
            parser.expect_token_variant(&LexToken::Fn)
        );

        assert_eq!(
            Ok(&LexToken::Return),
            parser.expect_token_variant(&LexToken::Return)
        );
    }

    #[test]
    fn it_expects_token_variants_and_errors() {
        let input = vec![LexToken::Let];

        let mut parser = Parser::new(input.iter().peekable());

        let actual = parser.expect_token_variant(&LexToken::Return);

        assert!(matches!(actual, Err(..)));
    }

    #[test]
    fn it_parses_let_statements() {
        // let x = 42;

        let input = vec![
            LexToken::Let,
            LexToken::Identifier("x".into()),
            LexToken::Assignment,
            LexToken::Integer(42),
            LexToken::Semicolon,
        ];

        let mut parser = Parser::new(input.iter().peekable());
        let actual = parser.parse().unwrap();

        assert_eq!(
            actual.statements[0],
            &Statement::Let {
                identifier: "x".into(),
                value: &Expression::IntegerLiteral(ApeInteger(42))
            }
        );
    }
}
