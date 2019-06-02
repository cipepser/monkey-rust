use crate::ast::{Program, Statement};
use crate::tokens::Token;

enum ParseError {}

use std::iter::Peekable;
use crate::ast::StatementKind::LetStatement;

pub fn parse(tokens: Vec<Token>) -> Result<Program, ParseError> {
    let mut tokens = tokens.into_iter().peekable();
//    tokens.next()
//    let p = Program;
//    Ok(p)
}

#[test]
fn test_let_statements() {
    let tokens = lex("let x = 5;");
    let program = parse(tokens);
    assert_eq!(
        p,
        Ok(Program::new().push(
            Statement::let_statement(
                    Token {
                        value: (),
                        loc: ()
                    },
            ),
            }
        ))
    )
}