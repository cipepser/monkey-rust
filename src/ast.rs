use std::fmt::Display;
use crate::lexer::Annot;
use crate::tokens::Token;

//#[derive(Debug, Clone, Eq, PartialEq, Hash)]
//pub trait Node: Display {
//    fn token_literal() -> String;
//}

//pub trait Statement: Node {}
//
//pub trait Expression: Node {}

#[derive(PartialEq, Debug, Clone)]
pub enum StatementKind {
    LetStatement { token: Token, name: String, value: Box<Expression> }
}

pub type Statement = Annot<StatementKind>;

#[derive(PartialEq, Debug, Clone)]
pub enum ExpressionKind {
    IdentifierExpression { token: Token, value: String }
}

pub type Expression = Annot<ExpressionKind>;

pub type Program = Vec<Statement>;
