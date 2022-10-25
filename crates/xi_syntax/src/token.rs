use crate::syntax_kind::SyntaxKind;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Token {
    pub kind: SyntaxKind,
    pub string: String,
}

impl Token {
    pub fn new(kind: SyntaxKind, string: String) -> Self {
        Self { kind, string }
    }
}
