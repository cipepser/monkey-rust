use crate::tokens::{Token, TokenStruct, TokenKind};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Loc(usize, usize);

impl Loc {
    fn merge(&self, other: &Loc) -> Loc {
        use std::cmp::{max, min};
        Loc(min(self.0, other.0), max(self.1, other.1))
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Annot<T> {
    value: T,
    loc: Loc,
}

impl<T> Annot<T> {
    pub fn new(value: T, loc: Loc) -> Self {
        Self { value, loc }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum LexErrorKind {
    Invalid(char),
    Eof,
}

type LexError = Annot<LexErrorKind>;

impl LexError {
    fn invalid_char(c: char, loc: Loc) -> Self {
        LexError::new(LexErrorKind::Invalid(c), loc)
    }
    fn eof(loc: Loc) -> Self {
        LexError::new(LexErrorKind::Eof, loc)
    }
}

fn lex(input: &str) -> Result<Vec<Token>, LexError> {
    let mut tokens = Vec::new();
    let input = input.as_bytes();
    let mut pos = 0;
    macro_rules! lex_a_token {
        ($lexer:expr) => {{
            let (tok, p) = $lexer?;
            tokens.push(tok);
            pos = p;
        }};
    }

    while pos < input.len() {
        match input[pos] {
            b'0'...b'9' => lex_a_token!(lex_number(input, pos)),
            b'+' => lex_a_token!(lex_plus(input, pos)),
            b'-' => lex_a_token!(lex_minus(input, pos)),
            b'*' => lex_a_token!(lex_asterisk(input, pos)),
            b'/' => lex_a_token!(lex_slash(input, pos)),
            b'(' => lex_a_token!(lex_lparen(input, pos)),
            b')' => lex_a_token!(lex_rparen(input, pos)),
            b'{' => lex_a_token!(lex_lbrace(input, pos)),
            b'}' => lex_a_token!(lex_rbrace(input, pos)),
            b'[' => lex_a_token!(lex_lbracket(input, pos)),
            b']' => lex_a_token!(lex_rbracket(input, pos)),
            b' ' | b'\n' | b'\t' => {
                let ((), p) = skip_spaces(input, pos)?;
                pos = p;
            }
            b => return Err(LexError::invalid_char(b as char, Loc(pos, pos + 1))),
        }
    }
    Ok(tokens)
}

fn consume_byte(input: &[u8], pos: usize, b: u8) -> Result<(u8, usize), LexError> {
    if input.len() <= pos {
        return Err(LexError::eof(Loc(pos, pos)));
    }

    if input[pos] != b {
        return Err(LexError::invalid_char(
            input[pos] as char,
            Loc(pos, pos + 1),
        ));
    }

    Ok((b, pos + 1))
}

fn lex_plus(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b'+')
        .map(|(_, end)| (Token::plus(Loc(start, end)), end))
}

fn lex_minus(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b'-')
        .map(|(_, end)| (Token::minus(Loc(start, end)), end))
}

fn lex_asterisk(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b'*')
        .map(|(_, end)| (Token::asterisk(Loc(start, end)), end))
}

fn lex_slash(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b'/')
        .map(|(_, end)| (Token::slash(Loc(start, end)), end))
}

fn lex_lparen(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b'(')
        .map(|(_, end)| (Token::lparen(Loc(start, end)), end))
}

fn lex_rparen(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b')')
        .map(|(_, end)| (Token::rparen(Loc(start, end)), end))
}

fn lex_lbrace(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b'{')
        .map(|(_, end)| (Token::lbrace(Loc(start, end)), end))
}

fn lex_rbrace(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b'}')
        .map(|(_, end)| (Token::rbrace(Loc(start, end)), end))
}

fn lex_lbracket(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b'[')
        .map(|(_, end)| (Token::lbracket(Loc(start, end)), end))
}

fn lex_rbracket(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b']')
        .map(|(_, end)| (Token::rbracket(Loc(start, end)), end))
}

fn lex_number(input: &[u8], pos: usize) -> Result<(Token, usize), LexError> {
    use std::str::from_utf8;

    let start = pos;
    let end = recognize_many(input, start, |b| b"1234567890".contains(&b));

    let n = from_utf8(&input[start..end])
        .unwrap()
        .parse()
        .unwrap();
    Ok((Token::int(n, Loc(start, end)), end))
}

fn skip_spaces(input: &[u8], pos: usize) -> Result<((), usize), LexError> {
    let pos = recognize_many(input, pos, |b| b" \n\t".contains(&b));
    Ok(((), pos))
}

fn recognize_many(input: &[u8], mut pos: usize, mut f: impl FnMut(u8) -> bool) -> usize {
    while pos < input.len() && f(input[pos]) {
        pos += 1;
    }
    pos
}

#[test]
fn test_lexer() {
    assert_eq!(
        lex("1"),
        Ok(vec![
            Token {
                value: TokenStruct {
                    kind: TokenKind::Int(1),
                    literal: "1".to_string(),
                },
                loc: Loc(0, 1),
            }
        ])
    );

    assert_eq!(
        lex("10"),
        Ok(vec![
            Token {
                value: TokenStruct {
                    kind: TokenKind::Int(10),
                    literal: "10".to_string(),
                },
                loc: Loc(0, 2),
            }
        ])
    );

    assert_eq!(
        lex("+"),
        Ok(vec![
            Token {
                value: TokenStruct {
                    kind: TokenKind::Plus,
                    literal: "+".to_string(),
                },
                loc: Loc(0, 1),
            }
        ])
    );
    assert_eq!(
        lex("-"),
        Ok(vec![
            Token {
                value: TokenStruct {
                    kind: TokenKind::Minus,
                    literal: "-".to_string(),
                },
                loc: Loc(0, 1),
            }
        ])
    );
    assert_eq!(
        lex("*"),
        Ok(vec![
            Token {
                value: TokenStruct {
                    kind: TokenKind::Asterisk,
                    literal: "*".to_string(),
                },
                loc: Loc(0, 1),
            }
        ])
    );
    assert_eq!(
        lex("/"),
        Ok(vec![
            Token {
                value: TokenStruct {
                    kind: TokenKind::Slash,
                    literal: "/".to_string(),
                },
                loc: Loc(0, 1),
            }
        ])
    );
    assert_eq!(
        lex("("),
        Ok(vec![
            Token {
                value: TokenStruct {
                    kind: TokenKind::LParen,
                    literal: "(".to_string(),
                },
                loc: Loc(0, 1),
            }
        ])
    );
    assert_eq!(
        lex(")"),
        Ok(vec![
            Token {
                value: TokenStruct {
                    kind: TokenKind::RParen,
                    literal: ")".to_string(),
                },
                loc: Loc(0, 1),
            }
        ])
    );
    assert_eq!(
        lex("{"),
        Ok(vec![
            Token {
                value: TokenStruct {
                    kind: TokenKind::LBrace,
                    literal: "{".to_string(),
                },
                loc: Loc(0, 1),
            }
        ])
    );
    assert_eq!(
        lex("}"),
        Ok(vec![
            Token {
                value: TokenStruct {
                    kind: TokenKind::RBrace,
                    literal: "}".to_string(),
                },
                loc: Loc(0, 1),
            }
        ])
    );
    assert_eq!(
        lex("["),
        Ok(vec![
            Token {
                value: TokenStruct {
                    kind: TokenKind::LBracket,
                    literal: "[".to_string(),
                },
                loc: Loc(0, 1),
            }
        ])
    );
    assert_eq!(
        lex("]"),
        Ok(vec![
            Token {
                value: TokenStruct {
                    kind: TokenKind::RBracket,
                    literal: "]".to_string(),
                },
                loc: Loc(0, 1),
            }
        ])
    );

    assert_eq!(
        lex("1 + 2 * 3 - -10"),
        Ok(vec![
            Token::int(1, Loc(0, 1)),
            Token::plus(Loc(2, 3)),
            Token::int(2, Loc(4, 5)),
            Token::asterisk(Loc(6, 7)),
            Token::int(3, Loc(8, 9)),
            Token::minus(Loc(10, 11)),
            Token::minus(Loc(12, 13)),
            Token::int(10, Loc(13, 15)),
        ])
    );
}