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
            // identifier and literal
            b'a'...b'z' | b'A'...b'Z' | b'_' => lex_a_token!(lex_char(input, pos)),
            b'0'...b'9' => lex_a_token!(lex_number(input, pos)),
            // operator
            b'=' => {
                match peek_char(input, pos) {
                    Ok(b) => {
                        if b == b'=' {
                            lex_a_token!(lex_eq(pos))
                        } else {
                            lex_a_token!(lex_assign(input, pos))
                        }
                    }
                    Err(e) => match e.value {
                        LexErrorKind::Eof => lex_a_token!(lex_assign(input, pos)),
                        _ => return Err(e),
                    }
                }
            }
            b'+' => lex_a_token!(lex_plus(input, pos)),
            b'-' => lex_a_token!(lex_minus(input, pos)),
            b'!' => lex_a_token!(lex_bang(input, pos)),
            b'*' => lex_a_token!(lex_asterisk(input, pos)),
            b'/' => lex_a_token!(lex_slash(input, pos)),
            b'<' => lex_a_token!(lex_less_than(input, pos)),
            b'>' => lex_a_token!(lex_greater_than(input, pos)),
            // delimiter
            b',' => lex_a_token!(lex_comma(input, pos)),
            b';' => lex_a_token!(lex_semicolon(input, pos)),
            b':' => lex_a_token!(lex_colon(input, pos)),
            // brackets
            b'(' => lex_a_token!(lex_lparen(input, pos)),
            b')' => lex_a_token!(lex_rparen(input, pos)),
            b'{' => lex_a_token!(lex_lbrace(input, pos)),
            b'}' => lex_a_token!(lex_rbrace(input, pos)),
            b'[' => lex_a_token!(lex_lbracket(input, pos)),
            b']' => lex_a_token!(lex_rbracket(input, pos)),
            // others
            b' ' | b'\n' | b'\t' => {
                let ((), p) = skip_spaces(input, pos)?;
                pos = p;
            }
            b => return Err(LexError::invalid_char(b as char, Loc(pos, pos + 1))),
        }
    }
    Ok(tokens)
}

// identifier and literal
fn lex_char(input: &[u8], pos: usize) -> Result<(Token, usize), LexError> {
    use std::str::from_utf8;

    let start = pos;
    let end = recognize_many(input, start, |b| b"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_".contains(&b));

    let s = from_utf8(&input[start..end])
        .unwrap();

    match s {
        "fn" => Ok((Token::function(Loc(start, end)), end)),
        "let" => Ok((Token::my_let(Loc(start, end)), end)),
        "true" => Ok((Token::my_true(Loc(start, end)), end)),
        "false" => Ok((Token::my_false(Loc(start, end)), end)),
        "if" => Ok((Token::my_if(Loc(start, end)), end)),
        "else" => Ok((Token::my_else(Loc(start, end)), end)),
        "return" => Ok((Token::my_return(Loc(start, end)), end)),
        "macro" => Ok((Token::my_macro(Loc(start, end)), end)),
        _ => Ok((Token::ident(&s.to_string(), Loc(start, end)), end))
    }
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

// operator
fn lex_assign(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b'=')
        .map(|(_, end)| (Token::assign(Loc(start, end)), end))
}

fn lex_plus(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b'+')
        .map(|(_, end)| (Token::plus(Loc(start, end)), end))
}

fn lex_minus(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b'-')
        .map(|(_, end)| (Token::minus(Loc(start, end)), end))
}

fn lex_bang(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b'!')
        .map(|(_, end)| (Token::bang(Loc(start, end)), end))
}

fn lex_asterisk(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b'*')
        .map(|(_, end)| (Token::asterisk(Loc(start, end)), end))
}

fn lex_slash(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b'/')
        .map(|(_, end)| (Token::slash(Loc(start, end)), end))
}

fn lex_less_than(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b'<')
        .map(|(_, end)| (Token::less_than(Loc(start, end)), end))
}

fn lex_greater_than(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b'>')
        .map(|(_, end)| (Token::greater_than(Loc(start, end)), end))
}

fn lex_eq(pos: usize) -> Result<(Token, usize), LexError> {
    Ok((Token::equal(Loc(pos, pos + 2)), pos + 2))
}

// delimiter
fn lex_comma(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b',')
        .map(|(_, end)| (Token::comma(Loc(start, end)), end))
}

fn lex_semicolon(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b';')
        .map(|(_, end)| (Token::semicolon(Loc(start, end)), end))
}

fn lex_colon(input: &[u8], start: usize) -> Result<(Token, usize), LexError> {
    consume_byte(input, start, b':')
        .map(|(_, end)| (Token::colon(Loc(start, end)), end))
}

// brackets
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

// utilities
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

fn peek_char(input: &[u8], pos: usize) -> Result<u8, LexError> {
    if input.len() <= pos + 1 {
        return Err(LexError::eof(Loc(pos, pos)));
    }
    Ok(input[pos + 1].clone())
}

#[test]
fn test_lexer() {
    // identifier and literal
    assert_eq!(
        lex("x"),
        Ok(vec![
            Token {
                value: TokenStruct {
                    kind: TokenKind::Ident("x".to_string()),
                    literal: "x".to_string(),
                },
                loc: Loc(0, 1),
            }
        ])
    );

    assert_eq!(
        lex("two"),
        Ok(vec![
            Token {
                value: TokenStruct {
                    kind: TokenKind::Ident("two".to_string()),
                    literal: "two".to_string(),
                },
                loc: Loc(0, 3),
            }
        ])
    );

    assert_eq!(
        lex("one two three"),
        Ok(vec![
            Token {
                value: TokenStruct {
                    kind: TokenKind::Ident("one".to_string()),
                    literal: "one".to_string(),
                },
                loc: Loc(0, 3),
            },
            Token {
                value: TokenStruct {
                    kind: TokenKind::Ident("two".to_string()),
                    literal: "two".to_string(),
                },
                loc: Loc(4, 7),
            },
            Token {
                value: TokenStruct {
                    kind: TokenKind::Ident("three".to_string()),
                    literal: "three".to_string(),
                },
                loc: Loc(8, 13),
            },
        ])
    );

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

    // operator
    assert_eq!(
        lex("="),
        Ok(vec![
            Token {
                value: TokenStruct {
                    kind: TokenKind::Assign,
                    literal: "=".to_string(),
                },
                loc: Loc(0, 1),
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
        lex("!"),
        Ok(vec![
            Token {
                value: TokenStruct {
                    kind: TokenKind::Bang,
                    literal: "!".to_string(),
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
        lex("<"),
        Ok(vec![
            Token {
                value: TokenStruct {
                    kind: TokenKind::Lt,
                    literal: "<".to_string(),
                },
                loc: Loc(0, 1),
            }
        ])
    );
    assert_eq!(
        lex(">"),
        Ok(vec![
            Token {
                value: TokenStruct {
                    kind: TokenKind::Gt,
                    literal: ">".to_string(),
                },
                loc: Loc(0, 1),
            }
        ])
    );
    assert_eq!(
        lex("=="),
        Ok(vec![
            Token {
                value: TokenStruct {
                    kind: TokenKind::Eq,
                    literal: "==".to_string(),
                },
                loc: Loc(0, 2),
            }
        ])
    );

    // delimiter
    assert_eq!(
        lex(","),
        Ok(vec![
            Token {
                value: TokenStruct {
                    kind: TokenKind::Comma,
                    literal: ",".to_string(),
                },
                loc: Loc(0, 1),
            }
        ])
    );
    assert_eq!(
        lex(";"),
        Ok(vec![
            Token {
                value: TokenStruct {
                    kind: TokenKind::SemiColon,
                    literal: ";".to_string(),
                },
                loc: Loc(0, 1),
            }
        ])
    );
    assert_eq!(
        lex(":"),
        Ok(vec![
            Token {
                value: TokenStruct {
                    kind: TokenKind::Colon,
                    literal: ":".to_string(),
                },
                loc: Loc(0, 1),
            }
        ])
    );


    // brackets
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

    // keyword
    assert_eq!(
        lex("fn"),
        Ok(vec![
            Token {
                value: TokenStruct {
                    kind: TokenKind::Function,
                    literal: "fn".to_string(),
                },
                loc: Loc(0, 2),
            }
        ])
    );
    assert_eq!(
        lex("let"),
        Ok(vec![
            Token {
                value: TokenStruct {
                    kind: TokenKind::Let,
                    literal: "let".to_string(),
                },
                loc: Loc(0, 3),
            }
        ])
    );
    assert_eq!(
        lex("true"),
        Ok(vec![
            Token {
                value: TokenStruct {
                    kind: TokenKind::True,
                    literal: "true".to_string(),
                },
                loc: Loc(0, 4),
            }
        ])
    );
    assert_eq!(
        lex("false"),
        Ok(vec![
            Token {
                value: TokenStruct {
                    kind: TokenKind::False,
                    literal: "false".to_string(),
                },
                loc: Loc(0, 5),
            }
        ])
    );
    assert_eq!(
        lex("if"),
        Ok(vec![
            Token {
                value: TokenStruct {
                    kind: TokenKind::If,
                    literal: "if".to_string(),
                },
                loc: Loc(0, 2),
            }
        ])
    );
    assert_eq!(
        lex("else"),
        Ok(vec![
            Token {
                value: TokenStruct {
                    kind: TokenKind::Else,
                    literal: "else".to_string(),
                },
                loc: Loc(0, 4),
            }
        ])
    );
    assert_eq!(
        lex("return"),
        Ok(vec![
            Token {
                value: TokenStruct {
                    kind: TokenKind::Return,
                    literal: "return".to_string(),
                },
                loc: Loc(0, 6),
            }
        ])
    );
    assert_eq!(
        lex("macro"),
        Ok(vec![
            Token {
                value: TokenStruct {
                    kind: TokenKind::Macro,
                    literal: "macro".to_string(),
                },
                loc: Loc(0, 5),
            }
        ])
    );

    // others
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