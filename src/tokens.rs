use crate::lexer::{Annot, Loc};

use std::collections::HashMap;

lazy_static! {
    pub static ref KEYWORDS: HashMap<&'static str, TokenKind> = {
        let mut m = HashMap::new();
        m.insert("fn", TokenKind::Function);
        m.insert("let", TokenKind::Let);
        m.insert("true", TokenKind::True);
        m.insert("false", TokenKind::False);
        m.insert("if", TokenKind::If);
        m.insert("else", TokenKind::Else);
        m.insert("return", TokenKind::Return);
        m.insert("macro", TokenKind::Macro);
        m
    };
}

// need Deubg? Debug is not implemented for String.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum TokenKind {
    // identifier and literal
    // must have 'static in Ident?
    Ident(String),
    Int(u64),
    // TODO: implement String

    // operator
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

// TODO: implement following operators
//
//    Lt,
//    Gt,
//
//    EQ,
//    NOT_EQ,

    // delimiter
    Comma,
    SemiColon,
    Colon,

    // brackets
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    // keyword
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
    Macro,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct TokenStruct {
    pub kind: TokenKind,
    pub literal: String,
}

pub type Token = Annot<TokenStruct>;

impl Token {
    // identifier and literal
    pub fn ident(s: &str, loc: Loc) -> Self {
        Self::new(
            TokenStruct {
                kind: TokenKind::Ident(s.to_string()),
                literal: s.to_string(),
            },
            loc,
        )
    }

    pub fn int(n: u64, loc: Loc) -> Self {
        Self::new(
            TokenStruct {
                kind: TokenKind::Int(n),
                literal: n.to_string(),
            },
            loc,
        )
    }

    // operator
    pub fn assign(loc: Loc) -> Self {
        Self::new(
            TokenStruct {
                kind: TokenKind::Assign,
                literal: "=".to_string(),
            },
            loc,
        )
    }

    pub fn plus(loc: Loc) -> Self {
        Self::new(
            TokenStruct {
                kind: TokenKind::Plus,
                literal: "+".to_string(),
            },
            loc,
        )
    }

    pub fn minus(loc: Loc) -> Self {
        Self::new(
            TokenStruct {
                kind: TokenKind::Minus,
                literal: "-".to_string(),
            },
            loc,
        )
    }

    pub fn bang(loc: Loc) -> Self {
        Self::new(
            TokenStruct {
                kind: TokenKind::Bang,
                literal: "!".to_string(),
            },
            loc,
        )
    }

    pub fn asterisk(loc: Loc) -> Self {
        Self::new(
            TokenStruct {
                kind: TokenKind::Asterisk,
                literal: "*".to_string(),
            },
            loc,
        )
    }

    pub fn slash(loc: Loc) -> Self {
        Self::new(
            TokenStruct {
                kind: TokenKind::Slash,
                literal: "/".to_string(),
            },
            loc,
        )
    }

    // delimiter
    pub fn comma(loc: Loc) -> Self {
        Self::new(
            TokenStruct {
                kind: TokenKind::Comma,
                literal: ",".to_string(),
            },
            loc,
        )
    }

    pub fn semicolon(loc: Loc) -> Self {
        Self::new(
            TokenStruct {
                kind: TokenKind::SemiColon,
                literal: ";".to_string(),
            },
            loc,
        )
    }

    pub fn colon(loc: Loc) -> Self {
        Self::new(
            TokenStruct {
                kind: TokenKind::Colon,
                literal: ":".to_string(),
            },
            loc,
        )
    }

    // brackets
    pub fn lparen(loc: Loc) -> Self {
        Self::new(
            TokenStruct {
                kind: TokenKind::LParen,
                literal: "(".to_string(),
            },
            loc,
        )
    }

    pub fn rparen(loc: Loc) -> Self {
        Self::new(
            TokenStruct {
                kind: TokenKind::RParen,
                literal: ")".to_string(),
            },
            loc,
        )
    }

    pub fn lbrace(loc: Loc) -> Self {
        Self::new(
            TokenStruct {
                kind: TokenKind::LBrace,
                literal: "{".to_string(),
            },
            loc,
        )
    }

    pub fn rbrace(loc: Loc) -> Self {
        Self::new(
            TokenStruct {
                kind: TokenKind::RBrace,
                literal: "}".to_string(),
            },
            loc,
        )
    }

    pub fn lbracket(loc: Loc) -> Self {
        Self::new(
            TokenStruct {
                kind: TokenKind::LBracket,
                literal: "[".to_string(),
            },
            loc,
        )
    }

    pub fn rbracket(loc: Loc) -> Self {
        Self::new(
            TokenStruct {
                kind: TokenKind::RBracket,
                literal: "]".to_string(),
            },
            loc,
        )
    }

    // keyword
    pub fn function(loc: Loc) -> Self {
        Self::new(
            TokenStruct {
                kind: TokenKind::Function,
                literal: "fn".to_string(),
            },
            loc,
        )
    }

    pub fn my_let(loc: Loc) -> Self {
        Self::new(
            TokenStruct {
                kind: TokenKind::Let,
                literal: "let".to_string(),
            },
            loc,
        )
    }

    pub fn my_true(loc: Loc) -> Self {
        Self::new(
            TokenStruct {
                kind: TokenKind::True,
                literal: "true".to_string(),
            },
            loc,
        )
    }

    pub fn my_false(loc: Loc) -> Self {
        Self::new(
            TokenStruct {
                kind: TokenKind::False,
                literal: "false".to_string(),
            },
            loc,
        )
    }

    pub fn my_if(loc: Loc) -> Self {
        Self::new(
            TokenStruct {
                kind: TokenKind::If,
                literal: "if".to_string(),
            },
            loc,
        )
    }

    pub fn my_else(loc: Loc) -> Self {
        Self::new(
            TokenStruct {
                kind: TokenKind::Else,
                literal: "else".to_string(),
            },
            loc,
        )
    }

    pub fn my_return(loc: Loc) -> Self {
        Self::new(
            TokenStruct {
                kind: TokenKind::Return,
                literal: "return".to_string(),
            },
            loc,
        )
    }

    pub fn my_macro(loc: Loc) -> Self {
        Self::new(
            TokenStruct {
                kind: TokenKind::Macro,
                literal: "macro".to_string(),
            },
            loc,
        )
    }
}