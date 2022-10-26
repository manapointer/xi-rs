#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[allow(non_camel_case_types)]
#[repr(u16)]
pub enum SyntaxKind {
    ERROR,
    IF,
    ELSE,
    WHILE,
    RETURN,
    LENGTH,
    USE,
    INT,
    BOOL,
    TRUE,
    FALSE,
    UNDERSCORE,
    WHITESPACE,

    IDENT,
    INTEGER,
    CHAR,
    STRING,

    ADD,
    SUB,
    MUL,
    DIV,
    REM,

    ASSIGN,
    NOT,
    EQ,
    NEQ,
    LT,
    LE,
    GT,
    GE,
    AND,
    OR,

    L_PAREN,
    L_BRACK,
    L_BRACE,
    R_PAREN,
    R_BRACK,
    R_BRACE,

    COMMA,
    COLON,
    SEMICOLON,
    COMMENT,

    SPEC,

    BASIC_LIT,
    ARRAY_LIT,

    CALL_EXPR,
    LENGTH_EXPR,
    SUBSCRIPT_EXPR,
    UNARY_EXPR,
    BINARY_EXPR,

    ASSIGN_STMT,
    IF_STMT,
    WHILE_STMT,
    RETURN_STMT,
    BLOCK_STMT,
    DECL_STMT,
}
