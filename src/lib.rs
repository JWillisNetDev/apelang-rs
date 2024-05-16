pub mod lexer;
pub mod parser;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Copy, Clone)]
pub struct ApeInteger(i64);

impl std::str::FromStr for ApeInteger {
    type Err = <i64 as std::str::FromStr>::Err;
    fn from_str(input: &str) -> Result<ApeInteger, Self::Err> {
        let parsed = input.parse::<i64>()?;
        Ok(ApeInteger(parsed))
    }
}

#[derive(Debug, PartialEq, PartialOrd, Copy, Clone)]
pub struct ApeNumeric(f64);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct ApeString(String);

pub type Identifier = String;
