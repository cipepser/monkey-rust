use crate::ast::{Program, Statement};
use crate::tokens::{Token, TokenKind};

pub enum ParseError {}

use std::iter::Peekable;
use crate::ast::StatementKind::LetStatement;
use crate::lexer::Loc;

pub fn parse(tokens: Vec<Token>) -> Result<Program, ParseError> {
    let mut tokens = tokens.into_iter().peekable();
//    tokens.next()
    let p = Program::new();
    Ok(p)
}

#[test]
fn test_let_statements() {
    let tokens = lex("let x = 5;");
    // This `tokens` has following Tokens.
    // Token::my_let(Loc(0, 3)),
    // Token::ident("x", Loc(4, 5)),
    // Token::assign(Loc(6, 7)),
    // Token::int(5, Loc(8, 9)),
    // Token::semicolon(Loc(9, 10)),

    let program = parse(tokens);
    assert!(program.is_ok());
    let mut program = program.unwrap();
    assert_eq!(program.len(), 1);

    let statement = program.pop();
    assert!(statement.is_some());
    let statement = statement.unwrap();
    assert_eq!(statement, Statement::let_statement(
        TokenKind::Ident("x".to_string()),
        "x",
        Box::new(5),
        Loc(0, 10),
    ));
}