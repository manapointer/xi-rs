use crate::syntax_kind::SyntaxKind;
use crate::token::Token;
use std::str::Chars;
use xi_errors::Diagnostic;

use SyntaxKind::*;

#[derive(Debug)]
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

    // Consumes an escape sequence (e.g. '\\', '\n'). The lexer is expected to be positioned at the initial '\'.
    fn consume_escape_sequence(&mut self, quote: char) -> Option<Diagnostic> {
        self.bump();
        if let Some(ch) = self.peek() {
            match ch {
                '\\' | 'n' | 't' => {
                    self.bump();
                }
                ch if ch == quote => {
                    self.bump();
                }
                'x' => {
                    self.bump();
                    return self.consume_unicode_escape_sequence();
                }
                _ => {
                    return Some(Diagnostic::error("invalid escape sequence"));
                }
            }
        } else {
            return Some(Diagnostic::error("expected an escape sequence"));
        }
        None
    }

    // Validate a `\x{...}` escape sequence. The lexer is expected to be positioned at the escape sequence's 'x'.
    fn consume_unicode_escape_sequence(&mut self) -> Option<Diagnostic> {
        if !matches!(self.peek(), Some('{')) {
            return Some(Diagnostic::error(
                "expected Unicode escape sequence to start with '{'",
            ));
        }

        self.bump();

        // Read a hexadecimal number. After this completes, the lexer's position
        // is the last digit of the number.
        let start = self.pos;
        for _ in 0..6 {
            match self.peek() {
                Some('}') => break,
                Some(b) if !b.is_ascii_hexdigit() => {
                    return Some(Diagnostic::error(format!(
                        "invalid hexadecimal digit: {}",
                        b
                    )));
                }
                None => return Some(Diagnostic::error("untermined Unicode escape sequence")),
                _ => {
                    self.bump();
                }
            }
        }
        let end = self.pos;

        // Expect to end with a '}'.
        if !matches!(self.peek(), Some('}')) {
            return Some(Diagnostic::error(
                "expected Unicode escape sequence to end with '}'",
            ));
        }

        self.bump();

        // Invalid escape sequence if we didn't read any hex digits.
        if start == end {
            Some(Diagnostic::error("empty Unicode escape sequence"))
        } else {
            None
        }
    }

    fn consume_str_literal(&mut self) -> Option<Diagnostic> {
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
                _ => {
                    self.bump();
                }
            }
        }

        Some(Diagnostic::error("unterminated string literal"))
    }

    fn consume_char_literal(&mut self) -> Option<Diagnostic> {
        match self.peek() {
            Some('\'') => {
                self.bump();
                return Some(Diagnostic::error("empty character literal"));
            }
            Some('\\') => {
                let res = self.consume_escape_sequence('\'');
                if res.is_some() {
                    return res;
                }
            }
            None => return Some(Diagnostic::error("unterminated character literal")),
            _ => {
                self.bump();
            }
        }
        if let Some('\'') = self.peek() {
            self.bump();
            return None;
        }
        loop {
            match self.peek() {
                Some('\'') => {
                    self.bump();
                    break;
                }
                None => return Some(Diagnostic::error("unterminated character literal")),
                _ => {
                    self.bump();
                }
            }
        }
        Some(Diagnostic::error(
            "character literal must be one character long",
        ))
    }

    fn consume_int_literal(&mut self) {
        while let Some('0'..='9') = self.peek() {
            self.bump();
        }
    }

    fn consume_whitespace(&mut self) {
        while let Some(' ' | '\t' | '\n') = self.peek() {
            self.bump();
        }
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = LexerReturn;

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
                diagnostic = self.consume_str_literal();
                STRING
            }
            '\'' => {
                diagnostic = self.consume_char_literal();
                CHAR
            }
            '0'..='9' => {
                self.consume_int_literal();
                INTEGER
            }
            ' ' | '\t' | '\n' => {
                self.consume_whitespace();
                WHITESPACE
            }
            _ => {
                diagnostic = Some(Diagnostic::error("unexpected token"));
                ERROR
            }
        };
        Some(LexerReturn(
            Token::new(kind, self.input[start..self.pos].to_owned()),
            diagnostic,
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::Lexer;
    use expect_test::{expect, Expect};

    fn check_lexing(input: &str, expect: Expect) {
        let actual: String = Lexer::from_str(input)
            .map(|res| format!("{:?}\n", res))
            .collect();
        expect.assert_eq(&actual);
    }

    #[test]
    fn test_str_literal() {
        check_lexing(
            r#""foo" "bar""#,
            expect![[r#"
            LexerReturn(Token { kind: STRING, string: "\"foo\"" }, None)
            LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
            LexerReturn(Token { kind: STRING, string: "\"bar\"" }, None)
        "#]],
        )
    }

    #[test]
    fn test_int_literal() {
        check_lexing(
            r#"1234 012"#,
            expect![[r#"
            LexerReturn(Token { kind: INTEGER, string: "1234" }, None)
            LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
            LexerReturn(Token { kind: INTEGER, string: "012" }, None)
        "#]],
        )
    }

    #[test]
    fn test_char_literal() {
        check_lexing(
            r#"'a' '' '\n' '\'' '\x{ffffff}' 'asdf'"#,
            expect![[r#"
                LexerReturn(Token { kind: CHAR, string: "'a'" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: CHAR, string: "''" }, Some(Diagnostic { message: "empty character literal", severity: Error }))
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: CHAR, string: "'\\n'" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: CHAR, string: "'\\''" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: CHAR, string: "'\\x{ffffff}'" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: CHAR, string: "'asdf'" }, Some(Diagnostic { message: "character literal must be one character long", severity: Error }))
            "#]],
        )
    }

    #[test]
    fn test_ident_or_keyword() {
        check_lexing(
            "foo x if else while return length use int bool true false",
            expect![[r#"
                LexerReturn(Token { kind: IDENT, string: "foo" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: IDENT, string: "x" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: IF, string: "if" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: ELSE, string: "else" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: WHILE, string: "while" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: RETURN, string: "return" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: LENGTH, string: "length" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: USE, string: "use" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: INT, string: "int" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: BOOL, string: "bool" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: TRUE, string: "true" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: FALSE, string: "false" }, None)
            "#]],
        )
    }

    #[test]
    fn test_string_literal_escape_sequence() {
        check_lexing(
            r#"
"\n"
"\\"
"\""
"\a"
"\x{ff}"
"\x{}"
"\x{0a1b2c3}"
"#,
            expect![[r#"
                LexerReturn(Token { kind: WHITESPACE, string: "\n" }, None)
                LexerReturn(Token { kind: STRING, string: "\"\\n\"" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: "\n" }, None)
                LexerReturn(Token { kind: STRING, string: "\"\\\\\"" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: "\n" }, None)
                LexerReturn(Token { kind: STRING, string: "\"\\\"\"" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: "\n" }, None)
                LexerReturn(Token { kind: STRING, string: "\"\\a\"" }, Some(Diagnostic { message: "invalid escape sequence", severity: Error }))
                LexerReturn(Token { kind: WHITESPACE, string: "\n" }, None)
                LexerReturn(Token { kind: STRING, string: "\"\\x{ff}\"" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: "\n" }, None)
                LexerReturn(Token { kind: STRING, string: "\"\\x{}\"" }, Some(Diagnostic { message: "empty Unicode escape sequence", severity: Error }))
                LexerReturn(Token { kind: WHITESPACE, string: "\n" }, None)
                LexerReturn(Token { kind: STRING, string: "\"\\x{0a1b2c3}\"" }, Some(Diagnostic { message: "expected Unicode escape sequence to end with '}'", severity: Error }))
                LexerReturn(Token { kind: WHITESPACE, string: "\n" }, None)
            "#]],
        )
    }

    #[test]
    fn smoke_test() {
        let input = "
sort(a: int[]) {
    i:int = 0
    n:int = length(a)
    while (i < n) {
        j:int = i
        while (j > 0) {
            if (a[j-1] > a[j]) {
                swap:int = a[j]
                a[j] = a[j-1]
                a[j-1] = swap
            }
            j = j-1
        }
        i = i+1
    }
}        
";
        check_lexing(
            input,
            expect![[r#"
                LexerReturn(Token { kind: WHITESPACE, string: "\n" }, None)
                LexerReturn(Token { kind: IDENT, string: "sort" }, None)
                LexerReturn(Token { kind: L_PAREN, string: "(" }, None)
                LexerReturn(Token { kind: IDENT, string: "a" }, None)
                LexerReturn(Token { kind: COLON, string: ":" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: INT, string: "int" }, None)
                LexerReturn(Token { kind: L_BRACK, string: "[" }, None)
                LexerReturn(Token { kind: R_BRACK, string: "]" }, None)
                LexerReturn(Token { kind: R_PAREN, string: ")" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: L_BRACE, string: "{" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: "\n    " }, None)
                LexerReturn(Token { kind: IDENT, string: "i" }, None)
                LexerReturn(Token { kind: COLON, string: ":" }, None)
                LexerReturn(Token { kind: INT, string: "int" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: EQ, string: "=" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: INTEGER, string: "0" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: "\n    " }, None)
                LexerReturn(Token { kind: IDENT, string: "n" }, None)
                LexerReturn(Token { kind: COLON, string: ":" }, None)
                LexerReturn(Token { kind: INT, string: "int" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: EQ, string: "=" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: LENGTH, string: "length" }, None)
                LexerReturn(Token { kind: L_PAREN, string: "(" }, None)
                LexerReturn(Token { kind: IDENT, string: "a" }, None)
                LexerReturn(Token { kind: R_PAREN, string: ")" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: "\n    " }, None)
                LexerReturn(Token { kind: WHILE, string: "while" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: L_PAREN, string: "(" }, None)
                LexerReturn(Token { kind: IDENT, string: "i" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: LE, string: "<" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: IDENT, string: "n" }, None)
                LexerReturn(Token { kind: R_PAREN, string: ")" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: L_BRACE, string: "{" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: "\n        " }, None)
                LexerReturn(Token { kind: IDENT, string: "j" }, None)
                LexerReturn(Token { kind: COLON, string: ":" }, None)
                LexerReturn(Token { kind: INT, string: "int" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: EQ, string: "=" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: IDENT, string: "i" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: "\n        " }, None)
                LexerReturn(Token { kind: WHILE, string: "while" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: L_PAREN, string: "(" }, None)
                LexerReturn(Token { kind: IDENT, string: "j" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: GE, string: ">" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: INTEGER, string: "0" }, None)
                LexerReturn(Token { kind: R_PAREN, string: ")" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: L_BRACE, string: "{" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: "\n            " }, None)
                LexerReturn(Token { kind: IF, string: "if" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: L_PAREN, string: "(" }, None)
                LexerReturn(Token { kind: IDENT, string: "a" }, None)
                LexerReturn(Token { kind: L_BRACK, string: "[" }, None)
                LexerReturn(Token { kind: IDENT, string: "j" }, None)
                LexerReturn(Token { kind: SUB, string: "-" }, None)
                LexerReturn(Token { kind: INTEGER, string: "1" }, None)
                LexerReturn(Token { kind: R_BRACK, string: "]" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: GE, string: ">" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: IDENT, string: "a" }, None)
                LexerReturn(Token { kind: L_BRACK, string: "[" }, None)
                LexerReturn(Token { kind: IDENT, string: "j" }, None)
                LexerReturn(Token { kind: R_BRACK, string: "]" }, None)
                LexerReturn(Token { kind: R_PAREN, string: ")" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: L_BRACE, string: "{" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: "\n                " }, None)
                LexerReturn(Token { kind: IDENT, string: "swap" }, None)
                LexerReturn(Token { kind: COLON, string: ":" }, None)
                LexerReturn(Token { kind: INT, string: "int" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: EQ, string: "=" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: IDENT, string: "a" }, None)
                LexerReturn(Token { kind: L_BRACK, string: "[" }, None)
                LexerReturn(Token { kind: IDENT, string: "j" }, None)
                LexerReturn(Token { kind: R_BRACK, string: "]" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: "\n                " }, None)
                LexerReturn(Token { kind: IDENT, string: "a" }, None)
                LexerReturn(Token { kind: L_BRACK, string: "[" }, None)
                LexerReturn(Token { kind: IDENT, string: "j" }, None)
                LexerReturn(Token { kind: R_BRACK, string: "]" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: EQ, string: "=" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: IDENT, string: "a" }, None)
                LexerReturn(Token { kind: L_BRACK, string: "[" }, None)
                LexerReturn(Token { kind: IDENT, string: "j" }, None)
                LexerReturn(Token { kind: SUB, string: "-" }, None)
                LexerReturn(Token { kind: INTEGER, string: "1" }, None)
                LexerReturn(Token { kind: R_BRACK, string: "]" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: "\n                " }, None)
                LexerReturn(Token { kind: IDENT, string: "a" }, None)
                LexerReturn(Token { kind: L_BRACK, string: "[" }, None)
                LexerReturn(Token { kind: IDENT, string: "j" }, None)
                LexerReturn(Token { kind: SUB, string: "-" }, None)
                LexerReturn(Token { kind: INTEGER, string: "1" }, None)
                LexerReturn(Token { kind: R_BRACK, string: "]" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: EQ, string: "=" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: IDENT, string: "swap" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: "\n            " }, None)
                LexerReturn(Token { kind: R_BRACE, string: "}" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: "\n            " }, None)
                LexerReturn(Token { kind: IDENT, string: "j" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: EQ, string: "=" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: IDENT, string: "j" }, None)
                LexerReturn(Token { kind: SUB, string: "-" }, None)
                LexerReturn(Token { kind: INTEGER, string: "1" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: "\n        " }, None)
                LexerReturn(Token { kind: R_BRACE, string: "}" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: "\n        " }, None)
                LexerReturn(Token { kind: IDENT, string: "i" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: EQ, string: "=" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: " " }, None)
                LexerReturn(Token { kind: IDENT, string: "i" }, None)
                LexerReturn(Token { kind: ADD, string: "+" }, None)
                LexerReturn(Token { kind: INTEGER, string: "1" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: "\n    " }, None)
                LexerReturn(Token { kind: R_BRACE, string: "}" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: "\n" }, None)
                LexerReturn(Token { kind: R_BRACE, string: "}" }, None)
                LexerReturn(Token { kind: WHITESPACE, string: "        \n" }, None)
            "#]],
        );
    }
}
