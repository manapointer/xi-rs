#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(u16)]
pub enum SyntaxKind {
    If,
    Else,
    While,
    Return,
    Length,
    Use,
    Int,
    Bool,
    True,
    False,
    Underscore,

    Ident,
    Integer,
    Char,
    String,

    Add,
    Sub,
    Mul,
    Div,
    Rem,

    Assign,
    Not,
    Eq,
    Neq,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,

    LParen,
    LBrack,
    LBrace,
    RParen,
    RBrack,
    RBrace,

    Comma,
    Colon,
    Semicolon,

    BasicLit,
    ArrayLit,

    CallExpr,
    LengthExpr,
    SubscriptExpr,
    UnaryExpr,
    BinaryExpr,
}
