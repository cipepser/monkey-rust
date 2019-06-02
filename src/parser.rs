use crate::ast::Ast;
use crate::tokens::Token;

enum ParseError {}

use std::iter::Peekable;

pub fn parse(tokens: Vec<Token>) -> Result<Ast, ParseError> {
    let mut tokens = tokens.into_iter().peekable();
//    tokens.next()
}