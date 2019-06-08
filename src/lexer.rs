use crate::tokens::{Token, TokenStruct, TokenKind};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Loc(usize, usize);

impl Loc {
    pub fn merge(&self, other: &Loc) -> Loc {
        use std::cmp::{max, min};
        Loc(min(self.0, other.0), max(self.1, other.1))
    }

    pub fn new(start: usize, end: usize) -> Self {
        Self {
            0: start,
            1: end,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Annot<T> {
    pub value: T,
    pub loc: Loc,
}

impl<T> Annot<T> {
    pub fn new(value: T, loc: Loc) -> Self {
        Self { value, loc }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum LexErrorKind {
    Invalid(char),
    UnclosedOpenDoubleQuotation,
    Eof,
}

type LexError = Annot<LexErrorKind>;

impl LexError {
    fn invalid_char(c: char, loc: Loc) -> Self {
        LexError::new(LexErrorKind::Invalid(c), loc)
    }
    fn unclosed_open_double_quotation(loc: Loc) -> Self {
        LexError::new(LexErrorKind::UnclosedOpenDoubleQuotation, loc)
    }
    fn eof(loc: Loc) -> Self {
        LexError::new(LexErrorKind::Eof, loc)
    }
}

pub fn lex(input: &str) -> Result<Vec<Token>, LexError> {
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
            b'"' => lex_a_token!(lex_string(input, pos)),
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
            b'!' => {
                match peek_char(input, pos) {
                    Ok(b) => {
                        if b == b'=' {
                            lex_a_token!(lex_not_eq(pos))
                        } else {
                            lex_a_token!(lex_bang(input, pos))
                        }
                    }
                    Err(e) => match e.value {
                        LexErrorKind::Eof => lex_a_token!(lex_bang(input, pos)),
                        _ => return Err(e),
                    }
                }
            }
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

fn lex_string(input: &[u8], pos: usize) -> Result<(Token, usize), LexError> {
    use std::str::from_utf8;

    let start = pos;
    let end = recognize_many(input, start + 1, |b| !b"\"".contains(&b));

    println!("len: {:?}", input.len());
    println!("start: {:?}, end: {:?}", start, end);

    if end == start + 1 && input.len() <= end {
        return Err(LexError::unclosed_open_double_quotation(Loc(start, end)));
    }
    println!("input: {:?}", &input[start..end].to_ascii_lowercase());

    let s = from_utf8(&input[start + 1..end])
        .unwrap();

    Ok((Token::string(s, Loc(start, end + 1)), end + 1))
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

fn lex_not_eq(pos: usize) -> Result<(Token, usize), LexError> {
    Ok((Token::not_equal(Loc(pos, pos + 2)), pos + 2))
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
fn test_lexer_identifier() {
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
}

#[test]
fn test_lexer_string() {
    assert_eq!(
        lex("\"x\""),
        Ok(vec![
            Token {
                value: TokenStruct {
                    kind: TokenKind::Str("x".to_string()),
                    literal: "x".to_string(),
                },
                loc: Loc(0, 3),
            }
        ])
    );

    assert_eq!(
        lex("\"hello\""),
        Ok(vec![
            Token {
                value: TokenStruct {
                    kind: TokenKind::Str("hello".to_string()),
                    literal: "hello".to_string(),
                },
                loc: Loc(0, 7),
            }
        ])
    );
}

#[test]
fn test_lexer_string_blank() {
    assert_eq!(
        lex("\"\""),
        Ok(vec![
            Token {
                value: TokenStruct {
                    kind: TokenKind::Str("".to_string()),
                    literal: "".to_string(),
                },
                loc: Loc(0, 2),
            }
        ])
    );
}

#[test]
fn test_lexer_string_unclosed() {
    assert_eq!(
        lex("\""),
        Err(LexError {
            value: LexErrorKind::UnclosedOpenDoubleQuotation,
            loc: Loc(0, 1),
        })
    );
}

#[test]
fn test_lexer_number() {
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
}

#[test]
fn test_lexer_operator() {
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
    assert_eq!(
        lex("!="),
        Ok(vec![
            Token {
                value: TokenStruct {
                    kind: TokenKind::NotEq,
                    literal: "!=".to_string(),
                },
                loc: Loc(0, 2),
            }
        ])
    );
}

#[test]
fn test_lexer_delimiter() {
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
}

#[test]
fn test_lexer_backets() {
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
}

#[test]
fn test_lexer_keyword() {
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
}

#[test]
fn test_lexer() {
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

    assert_eq!(
        lex("let five = 5;"),
        Ok(vec![
            Token::my_let(Loc(0, 3)),
            Token::ident("five", Loc(4, 8)),
            Token::assign(Loc(9, 10)),
            Token::int(5, Loc(11, 12)),
            Token::semicolon(Loc(12, 13)),
        ])
    );

    assert_eq!(
        lex("if three == 3 { true } else { false };"),
        Ok(vec![
            Token::my_if(Loc(0, 2)),
            Token::ident("three", Loc(3, 8)),
            Token::equal(Loc(9, 11)),
            Token::int(3, Loc(12, 13)),
            Token::lbrace(Loc(14, 15)),
            Token::my_true(Loc(16, 20)),
            Token::rbrace(Loc(21, 22)),
            Token::my_else(Loc(23, 27)),
            Token::lbrace(Loc(28, 29)),
            Token::my_false(Loc(30, 35)),
            Token::rbrace(Loc(36, 37)),
            Token::semicolon(Loc(37, 38)),
        ])
    );
    assert_eq!(
        lex("if three != 3 { true } else { false };"),
        Ok(vec![
            Token::my_if(Loc(0, 2)),
            Token::ident("three", Loc(3, 8)),
            Token::not_equal(Loc(9, 11)),
            Token::int(3, Loc(12, 13)),
            Token::lbrace(Loc(14, 15)),
            Token::my_true(Loc(16, 20)),
            Token::rbrace(Loc(21, 22)),
            Token::my_else(Loc(23, 27)),
            Token::lbrace(Loc(28, 29)),
            Token::my_false(Loc(30, 35)),
            Token::rbrace(Loc(36, 37)),
            Token::semicolon(Loc(37, 38)),
        ])
    );
    assert_eq!(
        lex("let five = 5;
let ten = 10;
let add = fn(x, y) {
  x + y;
};
let result = add(five, ten);
!-/*5;
5 < 10 > 5;
if (5 < 10) {
	return true;
} else {
	return false;
}
10 == 10;
10 != 9;
\"foobar\"
\"foo bar\"
[1, 2];
{\"foo\": \"bar\"}
macro(x, y) { x + y; };"),
        Ok(vec![
            Token::my_let(Loc(0, 3)),
            Token::ident("five", Loc(4, 8)),
            Token::assign(Loc(9, 10)),
            Token::int(5, Loc(11, 12)),
            Token::semicolon(Loc(12, 13)),
            Token::my_let(Loc(14, 17)),
            Token::ident("ten", Loc(18, 21)),
            Token::assign(Loc(22, 23)),
            Token::int(10, Loc(24, 26)),
            Token::semicolon(Loc(26, 27)),
            Token::my_let(Loc(28, 31)),
            Token::ident("add", Loc(32, 35)),
            Token::assign(Loc(36, 37)),
            Token::function(Loc(38, 40)),
            Token::lparen(Loc(40, 41)),
            Token::ident("x", Loc(41, 42)),
            Token::comma(Loc(42, 43)),
            Token::ident("y", Loc(44, 45)),
            Token::rparen(Loc(45, 46)),
            Token::lbrace(Loc(47, 48)),
            Token::ident("x", Loc(51, 52)),
            Token::plus(Loc(53, 54)),
            Token::ident("y", Loc(55, 56)),
            Token::semicolon(Loc(56, 57)),
            Token::rbrace(Loc(58, 59)),
            Token::semicolon(Loc(59, 60)),
            Token::my_let(Loc(61, 64)),
            Token::ident("result", Loc(65, 71)),
            Token::assign(Loc(72, 73)),
            Token::ident("add", Loc(74, 77)),
            Token::lparen(Loc(77, 78)),
            Token::ident("five", Loc(78, 82)),
            Token::comma(Loc(82, 83)),
            Token::ident("ten", Loc(84, 87)),
            Token::rparen(Loc(87, 88)),
            Token::semicolon(Loc(88, 89)),
            Token::bang(Loc(90, 91)),
            Token::minus(Loc(91, 92)),
            Token::slash(Loc(92, 93)),
            Token::asterisk(Loc(93, 94)),
            Token::int(5, Loc(94, 95)),
            Token::semicolon(Loc(95, 96)),
            Token::int(5, Loc(97, 98)),
            Token::less_than(Loc(99, 100)),
            Token::int(10, Loc(101, 103)),
            Token::greater_than(Loc(104, 105)),
            Token::int(5, Loc(106, 107)),
            Token::semicolon(Loc(107, 108)),
            Token::my_if(Loc(109, 111)),
            Token::lparen(Loc(112, 113)),
            Token::int(5, Loc(113, 114)),
            Token::less_than(Loc(115, 116)),
            Token::int(10, Loc(117, 119)),
            Token::rparen(Loc(119, 120)),
            Token::lbrace(Loc(121, 122)),
            Token::my_return(Loc(124, 130)),
            Token::my_true(Loc(131, 135)),
            Token::semicolon(Loc(135, 136)),
            Token::rbrace(Loc(137, 138)),
            Token::my_else(Loc(139, 143)),
            Token::lbrace(Loc(144, 145)),
            Token::my_return(Loc(147, 153)),
            Token::my_false(Loc(154, 159)),
            Token::semicolon(Loc(159, 160)),
            Token::rbrace(Loc(161, 162)),
            Token::int(10, Loc(163, 165)),
            Token::equal(Loc(166, 168)),
            Token::int(10, Loc(169, 171)),
            Token::semicolon(Loc(171, 172)),
            Token::int(10, Loc(173, 175)),
            Token::not_equal(Loc(176, 178)),
            Token::int(9, Loc(179, 180)),
            Token::semicolon(Loc(180, 181)),
            Token::string("foobar", Loc(182, 190)),
            Token::string("foo bar", Loc(191, 200)),
            Token::lbracket(Loc(201, 202)),
            Token::int(1, Loc(202, 203)),
            Token::comma(Loc(203, 204)),
            Token::int(2, Loc(205, 206)),
            Token::rbracket(Loc(206, 207)),
            Token::semicolon(Loc(207, 208)),
            Token::lbrace(Loc(209, 210)),
            Token::string("foo", Loc(210, 215)),
            Token::colon(Loc(215, 216)),
            Token::string("bar", Loc(217, 222)),
            Token::rbrace(Loc(222, 223)),
            Token::my_macro(Loc(224, 229)),
            Token::lparen(Loc(229, 230)),
            Token::ident("x", Loc(230, 231)),
            Token::comma(Loc(231, 232)),
            Token::ident("y", Loc(233, 234)),
            Token::rparen(Loc(234, 235)),
            Token::lbrace(Loc(236, 237)),
            Token::ident("x", Loc(238, 239)),
            Token::plus(Loc(240, 241)),
            Token::ident("y", Loc(242, 243)),
            Token::semicolon(Loc(243, 244)),
            Token::rbrace(Loc(245, 246)),
            Token::semicolon(Loc(246, 247)),
        ])
    );
}