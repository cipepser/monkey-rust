use std::fmt::Display;
use crate::lexer::Annot;
use crate::tokens::Token;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
trait Node: Display {
    fn token_literal() -> String;
}

type Ast = Annot<>;

enum ParseError {}

use std::iter::Peekable;

pub fn parse(tokens: Vec<Token>) -> Result<Ast, ParseError> {
    let mut tokens = tokens.into_iter().peekable();
//    tokens.next()
}