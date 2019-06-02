use std::fmt::Display;
use crate::lexer::{Annot, Loc};
use crate::tokens::Token;
use crate::ast::StatementKind::LetStatement;
use crate::ast::ExpressionKind::IdentifierExpression;

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

impl Statement {
    pub fn token_literal(&self) -> String {
        match &self.value {
            StatementKind::LetStatement { token, .. } => {
                token.value.literal.clone()
            }
        }
    }

    // TODO: locをmergeする必要あり
    pub fn let_statement(token: Token, name: String, expr: Expression, loc: Loc) -> Self {
        Self {
            value: LetStatement {
                token,
                name,
                value: Box::new(expr),
            },
            loc,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum ExpressionKind {
    IdentifierExpression { token: Token, value: String }
}

pub type Expression = Annot<ExpressionKind>;

impl Expression {
    pub fn identifier_expression(token: Token, name: String, loc: Loc) -> Self {
        Self {
            value: IdentifierExpression {
                token,
                value: name,
            },
            loc,
        }
    }
}


pub type Program = Vec<Statement>;

impl Program {
    pub fn new() -> Self {
        let p = Vec::new();
        p
    }
}