use crate::syntax_kind::SyntaxKind;
use crate::token::Token;
use std::str::Chars;
use xi_errors::Diagnostic;

use SyntaxKind::*;

pub struct LexerReturn(Token, Option<Diagnostic>);

pub struct Lexer<'src> {
    chars: Chars<'src>,
    input: &'src str,
    pos: usize,
}

impl<'src> Lexer<'src> {
    pub fn from_str(src: &'src str) -> Self {
        Lexer {
            chars: src.chars(),
            input: src,
            pos: 0,
        }
    }
}

impl<'src> Lexer<'src> {
    fn peek(&mut self) -> Option<char> {
        self.chars.clone().next()
    }

    fn bump(&mut self) -> Option<char> {
        let ch = self.chars.next();
        if let Some(ch) = ch {
            self.pos += ch.len_utf8()
        }
        ch
    }

    fn consume_ident_or_keyword(&mut self, start: usize) -> SyntaxKind {
        while let Some('a'..='z' | 'A'..='Z' | '_' | '\'') = self.peek() {
            self.next();
        }
        match &self.input[start..self.pos] {
            "if" => IF,
            "else" => ELSE,
            "while" => WHILE,
            "return" => RETURN,
            "length" => LENGTH,
            "use" => USE,
            "int" => INT,
            "bool" => BOOL,
            "true" => TRUE,
            "false" => FALSE,
            _ => IDENT,
        }
    }

    fn consume_escape_sequence(&mut self, quote: char) -> Option<Diagnostic> {
        if let Some(ch) = self.peek() {
            match ch {
                '\\' | 'n' => {
                    self.bump();
                    None
                }
                ch if ch == quote => Some(Diagnostic::error("empty escape sequence")),
                'x' => {
                    self.bump();
                    self.consume_unicode_escape_sequence()
                }
                _ => Some(Diagnostic::error("invalid escape sequence")),
            }
        } else {
            Some(Diagnostic::error("expected an escape sequence"))
        }
    }

    // Validate a `\x{...}` escape sequence. The lexer's position is expected to be the `x`.
    fn consume_unicode_escape_sequence(&mut self) -> Option<Diagnostic> {
        let invalid = Some(Diagnostic::error("invalid Unicode escape sequence"));

        if !matches!(self.peek(), Some('{')) {
            return invalid;
        }

        self.bump();

        // Read a hexadecimal number. After this completes, the lexer's position
        // is the last digit of the number.
        let start = self.pos;
        for _ in 0..6 {
            match self.peek() {
                Some(b) if !b.is_ascii_hexdigit() => {
                    return invalid;
                }
                None => return invalid,
                _ => {
                    self.bump();
                }
            }
        }
        let end = self.pos;

        // Expect to end with a '}'.
        if !matches!(self.peek(), Some('}')) {
            return invalid;
        }

        self.bump();

        // Invalid escape sequence if we didn't read any hex digits.
        if start == end {
            invalid
        } else {
            None
        }
    }

    fn consume_string(&mut self) -> Option<Diagnostic> {
        let mut diagnostic = None;

        while let Some(b) = self.peek() {
            match b {
                '"' => {
                    self.bump();
                    return diagnostic;
                }
                '\\' => {
                    let res = self.consume_escape_sequence('"');
                    if res.is_some() {
                        diagnostic = res;
                    }
                }
                _ => {}
            }
        }

        return Some(Diagnostic::error("unterminated string literal"));
    }

    fn consume_character(&mut self) -> Option<Diagnostic> {
        let mut diagnostic = None;
        match self.peek() {
            Some('\'') => {
                self.bump();
                diagnostic.get_or_insert(Diagnostic::error("empty character literal"));
            }
            Some('\\') => {
                let res = self.consume_escape_sequence('\'');
                if let Some(res) = res {
                    diagnostic.get_or_insert(res);
                }
            }
            None => return Some(Diagnostic::error("unterminated character literal")),
            _ => (),
        }
        diagnostic
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = (Token, Option<Diagnostic>);

    fn next(&mut self) -> Option<Self::Item> {
        macro_rules! peek_or {
            ($want:expr, $kind:expr, $alt:expr) => {
                match self.peek() {
                    Some($want) => {
                        self.bump();
                        $kind
                    }
                    _ => $alt,
                }
            };
        }
        let mut diagnostic = None;
        let start = self.pos;
        let kind = match self.bump()? {
            '+' => ADD,
            '-' => SUB,
            '*' => MUL,
            '/' => peek_or!('/', COMMENT, DIV),
            '%' => REM,
            '&' => AND,
            '|' => OR,
            '(' => L_PAREN,
            '[' => L_BRACK,
            '{' => L_BRACE,
            ')' => R_PAREN,
            ']' => R_BRACK,
            '}' => R_BRACE,
            ',' => COMMA,
            ':' => COLON,
            ';' => SEMICOLON,
            '_' => UNDERSCORE,
            '=' => peek_or!('=', ASSIGN, EQ),
            '!' => peek_or!('=', NOT, NEQ),
            '<' => peek_or!('=', LT, LE),
            '>' => peek_or!('=', GT, GE),
            'a'..='z' | 'A'..='Z' => self.consume_ident_or_keyword(start),
            '"' => {
                diagnostic = self.consume_string();
                STRING
            }
            '\'' => {
                diagnostic = self.consume_character();
                CHAR
            }
            '0'..='9' => todo!(),
            _ => todo!(),
        };
        Some((
            Token::new(kind, self.input[start..self.pos].to_owned()),
            diagnostic,
        ))
    }
}
