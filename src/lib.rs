pub mod lexer;
pub mod parser;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Copy, Clone)]
pub struct ApeInteger(i64);

#[derive(Debug, PartialEq, PartialOrd, Copy, Clone)]
pub struct ApeNumeric(f64);

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ApeString(String);

pub type Identifier = String;
