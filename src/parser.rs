use crate::ast::{Program, Statement, Expression};
use crate::tokens::{Token, TokenKind};

#[derive(PartialEq, Debug, Clone)]
pub enum ParseError {
    Eof
}

use crate::lexer::{Loc, lex};
use std::iter::Peekable;
use std::collections::VecDeque;

#[derive(PartialEq, Debug, Clone)]
pub struct Parser {
    tokens: VecDeque<Token>,
    cur_token: Token,
    peek_token: Token,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Result<Self, ParseError> {
        let mut tokens = VecDeque::from(tokens);

        let cur_token = match tokens.pop_front() {
            Some(t) => t,
            None => { return Err(ParseError::Eof); }
        };
        let peek_token = match tokens.pop_front() {
            Some(t) => t,
            None => { return Err(ParseError::Eof); }
        };

        Ok(Self { tokens, cur_token, peek_token })
    }

    pub fn next_token(&mut self) -> Result<Self, ParseError> {
        let mut tokens = self.tokens.clone();
        let cur_token = self.peek_token.clone();
        let peek_token = match tokens.pop_front() {
            Some(t) => t,
            None => { return Err(ParseError::Eof); }
        };

        Ok(Self { tokens, cur_token, peek_token })
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<Program, ParseError> {
    let mut parser = Parser::new(tokens)?;
    println!("parser: {:?}", parser);
    let mut parser = parser.next_token()?;
    println!("parser: {:?}", parser);
    let mut program = Program::new();

    // TODO: parse_statementを実装する
    // ループをどうやって回す？
//    while parser.tokens.len() != 0 {
//
//
//    }


    Ok(program)
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