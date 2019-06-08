use crate::ast::{Program, Statement, Expression};
use crate::tokens::{Token, TokenKind};

#[derive(PartialEq, Debug, Clone)]
pub enum ParseError {}

use crate::lexer::{Loc, lex};

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

    assert!(tokens.is_ok());
    let tokens = tokens.unwrap();

    let program = parse(tokens);
    assert!(program.is_ok());
    let mut program = program.unwrap();
    assert_eq!(program.len(), 1);

    let statement = program.pop();
    assert!(statement.is_some());
    let statement = statement.unwrap();
    assert_eq!(statement, Statement::let_statement(
        TokenKind::Ident("x".to_string()),
        "x".to_string(),
        Expression::num(5, Loc::new(8, 9)),
        Loc::new(0, 10),
    ));
}