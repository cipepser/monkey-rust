use crate::lexer::{Annot, Loc};
use crate::tokens::{Token, TokenKind};
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
    LetStatement { token_kind: TokenKind, name: String, value: Box<Expression> }
}

pub type Statement = Annot<StatementKind>;

impl Statement {
    pub fn token_literal(&self) -> String {
        match &self.value {
            StatementKind::LetStatement { token_kind, .. } => {
                match token_kind {
                    TokenKind::Ident(s) => s.clone(),
                    _ => "".to_string()
                }
            }
        }
    }

    pub fn let_statement(token_kind: TokenKind, name: String, expr: Expression, loc: Loc) -> Self {
        Self {
            value: LetStatement {
                token_kind,
                name,
                value: Box::new(expr),
            },
            loc,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum ExpressionKind {
    Num(u64),
    IdentifierExpression { token_kind: TokenKind, value: Box<Expression> },
}

pub type Expression = Annot<ExpressionKind>;

impl Expression {
    pub fn num(n: u64, loc: Loc) -> Self {
        Self::new(ExpressionKind::Num(n), loc)
    }

    pub fn identifier_expression(token_kind: TokenKind, value: Expression, loc: Loc) -> Self {
        Self::new(
            IdentifierExpression {
                token_kind,
                value: Box::new(value),
            }, loc,
        )
    }
}

pub type Program = Vec<Statement>;

//impl Program {
//    pub fn new() -> Self {
//        let p = Vec::new();
//        p
//    }
//}