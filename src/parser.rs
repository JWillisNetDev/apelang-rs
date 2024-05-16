//! This module parses lex tokens into an AST
//!

use crate::{lexer::LexToken, ApeInteger, Identifier};
use std::{
    iter::{Iterator, Peekable},
    rc::Rc,
};

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

fn get_precedence(token: LexToken) -> Precedence {
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
pub enum AstNode {
    StatementNode(Statement),
    ExpressionNode(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    BinaryOp(Rc<Expression>, Rc<Expression>),
    Identifier(Identifier),
    StringLiteral(String),
    IntegerLiteral(ApeInteger),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let {
        ident: Identifier,
        expr: Expression,
    },
    Expression(Expression),
}

pub struct Parser<'a, T: Iterator<Item = &'a LexToken>> {
    tokens: Peekable<T>,
}

pub struct Program<'a> {
    statements: Vec<&'a Statement>,
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
            let ast_node = match token {
                LexToken::Let => {
                    let ident = expect_token_identifier(self.tokens.next())?;
                    expect_token(&LexToken::Assign, self.tokens.next())?;
                    let expr = self.try_parse_expression(Precedence::Lowest)?;
                    expect_token(&LexToken::Semicolon, self.tokens.next())?;

                    Statement::Let {
                        ident: ident.into(),
                        expr,
                    }
                }
                _ => return Err(format!("Unexpected token encountered: {:?}", token)),
            };
        }

        Err("Some error".into())
    }

    fn try_parse_expression(&mut self, precedence: Precedence) -> Result<Expression, String> {
        while let Some(token) = self.tokens.next() {

        }

        Err("Expected an expression.".into())
    }
}

fn expect_token_identifier(token: Option<&LexToken>) -> Result<Identifier, String> {
    match token {
        Some(LexToken::Identifier(ident)) => Ok(ident.clone()),
        Some(_) => Err(format!("Expected identifier, found {:?}", token)),
        None => Err("Expected identifier, found none".into())
    }
}

fn expect_token<'a>(expected: &LexToken, token: Option<&LexToken>) -> Result<(), String> {
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
            &Statement::Let {
                ident: "x".into(),
                expr: Expression::IntegerLiteral(ApeInteger(42))
            }
        );
    }
}
