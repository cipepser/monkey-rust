use std::fmt;
use std::iter::Peekable;
use std::collections::VecDeque;
use crate::ast::{Program, Statement, Expression};
use crate::tokens::{Token, TokenKind};
use crate::lexer::{Loc, lex};
use crate::parser::Precedence::Lowest;
use crate::parser::ParseError::NoInfixParseFn;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ParseError {
    Eof,
    UnexpectedToken(Token),
    NoPrefixParseFn(Token),
    NoInfixParseFn(Token),
}

//impl fmt::Display for ParseError {
//    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//        use self::ParseError::*;
//        match self {
//            Eof => write!(f, "End of file"),
//            UnexpectedToken(tok) => write!(f,
//                                           "{}: token '{}' is unexpected",
//                                           tok.loc, tok.value),
//        }
//    }
//}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub enum Precedence {
    Lowest,
    // ==
    Equals,
    // > or <
    LessGreater,
    // +
    Sum,
    // *
    Product,
    // -X or !X
    Prefix,
    // myFunction(X)
    Call,
    // array[index]
    Index,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Parser {
    tokens: VecDeque<Token>,
    cur_token: Token,
    peek_token: Token,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Result<Self, ParseError> {
        let mut tokens = VecDeque::from(tokens);

        let cur_token = tokens.pop_front()
            .ok_or(ParseError::Eof)
            .and_then(|token| Ok(token))?;

        // TODO: peekでNoneの場合はEofじゃない処理？
        let peek_token = tokens.front()
            .ok_or(ParseError::Eof)
            .and_then(|token| Ok(token.clone()))?;

        Ok(Self { tokens, cur_token, peek_token })
    }

    pub fn next_token(&mut self) -> Result<Self, ParseError> {
        let mut tokens = self.tokens.clone();
        let cur_token = tokens.pop_front()
            .ok_or(ParseError::Eof)
            .and_then(|token| Ok(token))?;

        // TODO: peekでNoneの場合はEofじゃない処理？
        let peek_token = tokens.front()
            .ok_or(ParseError::Eof)
            .and_then(|token| Ok(token.clone()))?;

        Ok(Self { tokens, cur_token, peek_token })
    }

    pub fn peek_precedence(&self) -> Precedence {
        match &self.peek_token.value.kind {
            TokenKind::Eq => Precedence::Equals,
            TokenKind::NotEq => Precedence::Equals,
            TokenKind::Lt => Precedence::LessGreater,
            TokenKind::Gt => Precedence::LessGreater,
            TokenKind::Plus => Precedence::Sum,
            TokenKind::Minus => Precedence::Sum,
            TokenKind::Slash => Precedence::Product,
            TokenKind::Asterisk => Precedence::Product,
            TokenKind::LParen => Precedence::Call,
            TokenKind::LBracket => Precedence::Index,
            _ => Lowest
        }
    }

    pub fn parse_statement(&mut self) -> Result<Statement, ParseError> {
//        println!("token in parse_statement: {:?}", self.cur_token.value.kind);
        match self.cur_token.value.kind {
            TokenKind::Let => self.parse_let_statement(),
            TokenKind::Return => {
                unimplemented!();
                // TODO: implement self.parse_return_statement()
            }
            _ => {
                unimplemented!();
                // TODO: implement self.parse_expression_statement()
            }
        }
    }

    pub fn parse_let_statement(&mut self) -> Result<Statement, ParseError> {
        let mut loc = self.cur_token.loc.clone();

        let token_kind = self.cur_token.value.kind.clone();

        let name = match &self.peek_token.value.kind {
            TokenKind::Ident(s) => s.clone(),
            _ => return Err(ParseError::UnexpectedToken(self.peek_token.clone()))
        };

        let mut parser = self.next_token()?;
        if parser.peek_token.value.kind != TokenKind::Assign {
            return Err(ParseError::UnexpectedToken(parser.peek_token.clone()));
        }
        parser = parser.next_token()?;
        parser = parser.next_token()?;

        let expr = parser.parse_expression(Precedence::Lowest)?;

        // TODO: ここにFunction Literalの処理が必要になる

        if parser.peek_token.value.kind != TokenKind::SemiColon {
            return Err(ParseError::UnexpectedToken(parser.peek_token.clone()));
        }
        println!("cur_token: {:?}", parser.cur_token);
        println!("peek_token: {:?}", parser.peek_token);
        parser = parser.next_token()?; // TODO: ここでEofなので終了してしまう

        loc.merge(&parser.cur_token.loc);

        Ok(Statement::let_statement(token_kind, name, expr, loc))
    }

    pub fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParseError> {
        // prefixParseFn[token.Type]
        // nilならの分岐⇛error処理なので、matchの_で拾う
        // leftExp := prefix()
//        println!("parser: {:?}", self);
        let mut left_expr = match self.cur_token.value.kind {
            TokenKind::Int(n) => Expression::num(n, self.cur_token.loc.clone()),
            // TODO: 他のTokenKindも生やす
            _ => return Err(ParseError::NoPrefixParseFn(self.cur_token.clone()))
        };

        let parser = self;
//        println!("parser: {:?}", parser);
//        println!("cur_token: {:?}", parser.cur_token);
//        println!("peek_token: {:?}", parser.peek_token);
        while parser.peek_token.value.kind != TokenKind::SemiColon && precedence < parser.peek_precedence() {
            match &parser.peek_token.value.kind {
                // TODO: 他のTokenKindも生やす
                // その時はここでleft_exprを返すようにする
                _ => return Err(ParseError::NoInfixParseFn(parser.peek_token.clone()))
            };
//            parser = parser.next_token()?;
        };
        // セミコロンまで& precedence < p.peekPrecedence()
        // // infixParseFn[token.Type]
        // // error処理
        // p.nextToken()
        // leftExp = infix(leftExp)
//        parser = parser.next_token()?;
        Ok(left_expr)
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<Program, ParseError> {
    let mut parser = Parser::new(tokens)?;
//    println!("parser: {:?}", parser);
    let mut program = Program::new();

    while parser.tokens.len() != 1 {
        let statement = parser.parse_statement()?;
        program.push(statement.clone());
        parser = parser.next_token()?;
    }

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
    println!("program: {:?}", program);

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

trait PrefixParseFn {
    fn prefix() -> Result<Expression, ParseError>;
}

trait InfixParseFn {
    fn infix(expr: Expression) -> Result<Expression, ParseError>;
}