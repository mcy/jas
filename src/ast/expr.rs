use ast::*;
use reporting::*;
use source_file::*;

#[derive(Clone, Debug)]
pub struct SourceLine {
    pub label: Option<Ident>,
    pub instruction: Ident,
    pub args: Vec<Expr>,
    pub span: Span,
}

impl Spannable for SourceLine {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Clone, Debug)]
pub enum Expr {
    Ref(Ident),
    Int(IntLit),
    Float(FloatLit),
    Str(StrLit),
    Char(CharLit),
    Bracket(Box<Instruction>),
    BinOp(Box<Expr>, BinOpKind, Box<Expr>),
    //UnOp(UnOpKind, Box<Expr>),
}

impl Spannable for Expr {

    fn span(&self) -> Span {
        match *self {
            Expr::Ref(ref ident) => ident.span(),
            Expr::Int(ref int) => int.span(),
            Expr::Float(ref float) => float.span(),
            Expr::Str(ref str) => str.span(),
            Expr::Char(ref char) => char.span(),
            Expr::Bracket(ref line) => line.span(), // FIXME: include brackets?
            Expr::BinOp(ref first, _, ref second) => first.span().extend_to(&second.span()),
            //Expr::UnOp(_, ref expr) => expr.span, // FIXME: implement
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum BinOpKind {
    Add, Sub, Mul, Div,
    Rem, And, Or, Xor,
}

impl BinOpKind {

    pub fn from_str(val: &str) -> Option<BinOpKind> {
        use self::BinOpKind as K;
        match val {
            "+" => Some(K::Add),
            "-" => Some(K::Sub),
            "*" => Some(K::Mul),
            "/" => Some(K::Div),
            "%" => Some(K::Rem),
            "&" => Some(K::And),
            "|" => Some(K::Or),
            "^" => Some(K::Xor),
            _ => None,
        }
    }
}

use std::cmp::Ordering;
impl PartialOrd for BinOpKind {
    #[inline]
    fn partial_cmp(&self, other: &BinOpKind) -> Option<Ordering> {
        #[inline]
        fn precedence(val: BinOpKind) -> i8 {
            use self::BinOpKind as K;
            match val {
                K::And | K::Or | K::Xor => 0,
                K::Add | K::Sub => 1,
                K::Mul | K::Div | K::Rem => 2,
            }
        }

        if self == other {
            Some(Ordering::Equal)
        } else {
            match precedence(*self).cmp(&precedence(*other)) {
                Ordering::Equal => None,
                x => Some(x),
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum UnOpKind {
    Plus, Neg, Not
}

#[derive(Clone, Debug)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

impl Spannable for Ident {

    fn span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Clone, Debug)]
pub struct IntLit {
    pub value: i64,
    pub span: Span,
}

impl Spannable for IntLit {

    fn span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Clone, Debug)]
pub struct FloatLit {
    pub value: f64,
    pub span: Span,
}

impl Spannable for FloatLit {

    fn span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Clone, Debug)]
pub struct StrLit {
    pub value: String,
    pub span: Span,
}

impl Spannable for StrLit {

    fn span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Clone, Debug)]
pub struct CharLit {
    pub value: u16, // java chars, utf16!
    pub span: Span,
}

impl Spannable for CharLit {

    fn span(&self) -> Span {
        self.span.clone()
    }
}