//! This module parses lex tokens into an AST
//!

use crate::{lexer::LexToken, ApeInteger, Identifier};
use std::{
    iter::{Iterator, Peekable},
    rc::Rc,
};

type ParseError = String;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    None,
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
}

fn get_precedence(token: &LexToken) -> Precedence {
    match token {
        LexToken::Equals => Precedence::Equals,
        LexToken::NotEquals => Precedence::Equals,
        LexToken::LessThan => Precedence::LessGreater,
        LexToken::GreaterThan => Precedence::LessGreater,
        LexToken::Plus => Precedence::Sum,
        LexToken::Minus => Precedence::Sum,
        LexToken::Multiply => Precedence::Product,
        LexToken::Divide => Precedence::Product,
        LexToken::LParen => Precedence::Call,
        LexToken::LBracket => Precedence::Index,
        _ => Precedence::None,
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    BinaryOp(Rc<Expression>, Rc<Expression>),
    Identifier(Identifier),
    
    StringLiteral(String),
    IntegerLiteral(ApeInteger),
    BooleanLiteral(bool),

    Prefix(LexToken, Rc<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let { ident: Identifier, expr: Expression },
    Expression(Expression),
}

pub struct Parser<'a, T: Iterator<Item = &'a LexToken>> {
    tokens: Peekable<T>,
}

pub struct Program {
    statements: Vec<Statement>,
}

impl Program {
    fn new(statements: Vec<Statement>) -> Self {
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

    pub fn parse(&mut self) -> Result<Program, String> {
        let mut program = Program::from_empty();

        while let Some(token) = self.tokens.next() {
            let statement = match token {
                LexToken::Let => {
                    let ident = expect_token_identifier(self.tokens.next())?;
                    expect_token(&LexToken::Assign, self.tokens.next())?;
                    let expr = self.try_parse_expression(Precedence::Lowest)?;
                    expect_token(&LexToken::Semicolon, self.tokens.next())?;

                    Statement::Let { ident, expr }
                }
                _ => return Err(format!("Unexpected token encountered: {:?}", token)),
            };

            program.statements.push(statement);
        }

        Ok(program)
    }

    fn try_parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParseError> {
        let prefix = match self.tokens.next() {
            Some(LexToken::Identifier(ident)) => Expression::Identifier(ident.clone()),
            Some(LexToken::Integer(i)) => Expression::IntegerLiteral(*i),
            Some(token) if is_boolean_literal(token) => {
                if token == &LexToken::True {
                    Expression::BooleanLiteral(true)
                }
                else {
                    Expression::BooleanLiteral(false)
                }
            }
            Some(token) if is_prefix(token) => {
                let right = self.try_parse_expression(Precedence::Prefix)?;
                Expression::Prefix(token.clone(), Rc::new(right))
            }
            Some(token) => {
                return Err(format!(
                    "Expected a prefix expression, but found {:?}",
                    token
                ))
            }
            None => return Err("Expected a prefix expression, but found nothing".into()),
        };

        while self
            .tokens
            .peek()
            .is_some_and(|peek| *peek != &LexToken::Semicolon && get_precedence(peek) > precedence)
        {

        }

        Ok(prefix)
    }
}

fn is_boolean_literal(token: &LexToken) -> bool {
    token == &LexToken::True || token == &LexToken::False
}

fn is_prefix(token: &LexToken) -> bool {
    token == &LexToken::Bang || token == &LexToken::Minus
}

fn expect_token_identifier(token: Option<&LexToken>) -> Result<Identifier, ParseError> {
    match token {
        Some(LexToken::Identifier(ident)) => Ok(ident.clone()),
        Some(_) => Err(format!("Expected identifier, found {:?}", token)),
        None => Err("Expected identifier, found none".into()),
    }
}

fn expect_token(expected: &LexToken, token: Option<&LexToken>) -> Result<(), ParseError> {
    match token {
        Some(token) if token == expected => Ok(()),
        Some(token) => Err(format!("Expected {:?}, found {:?}", expected, token)),
        None => Err(format!("Expected {:?}, found nothing", expected)),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn it_parses_let_statements() {
        // let x = 42;

        let input = vec![
            LexToken::Let,
            LexToken::Identifier("x".into()),
            LexToken::Assign,
            LexToken::Integer(ApeInteger(42)),
            LexToken::Semicolon,
        ];

        let mut parser = Parser::new(input.iter().peekable());
        let actual = parser.parse().unwrap();

        assert_eq!(
            actual.statements[0],
            Statement::Let {
                ident: "x".into(),
                expr: Expression::IntegerLiteral(ApeInteger(42))
            }
        );
    }
}
