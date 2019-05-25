use crate::lexer::{Annot, Loc};

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum TokenKind {
    // identifier and literal
    Int(u64),

    // operator
    Plus,
    Minus,
    Asterisk,
    Slash,

    // delimitor

    // brackets
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    // keyword
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct TokenStruct {
    pub kind: TokenKind,
    pub literal: String,
}

pub type Token = Annot<TokenStruct>;

impl Token {
    pub fn int(n: u64, loc: Loc) -> Self {
        Self::new(
            TokenStruct {
                kind: TokenKind::Int(n),
                literal: n.to_string(),
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
}