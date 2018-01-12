use super::*;

use lexer::{Token, TokenType as TT};
use reporting::{Span, Result};

#[derive(Clone, Debug)]
pub struct SourceLine {
    label: Option<Ident>,
    instruction: Ident,
    args: Vec<Expr>,
    span: Span,
}

impl SourceLine {
    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Clone, Debug)]
pub enum Expr {
    Ref(Ident),
    Int(IntLit),
    Float(FloatLit),
    Str(StrLit),
    Char(CharLit),
    Bracket(Box<SourceLine>),
    BinOp(Box<Expr>, BinOpKind, Box<Expr>),
    UnOp(UnOpKind, Box<Expr>),
}

impl Expr {

    pub fn span(&self) -> Span {
        match *self {
            Expr::Ref(ref ident) => ident.span(),
            Expr::Int(ref int) => int.span(),
            Expr::Float(ref float) => float.span(),
            Expr::Str(ref str) => str.span(),
            Expr::Char(ref char) => char.span(),
            Expr::Bracket(ref line) => line.span(), // FIXME
            Expr::BinOp(ref first, _, ref second) => first.span().extend_to(second.span().end),
            Expr::UnOp(_, ref expr) => expr.span(), // FIXME
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
    name: String,
    span: Span,
}

impl Ident {

    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Clone, Debug)]
pub struct IntLit {
    value: i64,
    span: Span,
}

impl IntLit {

    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Clone, Debug)]
pub struct FloatLit {
    value: f64,
    span: Span,
}

impl FloatLit {

    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Clone, Debug)]
pub struct StrLit {
    value: String,
    span: Span,
}

impl StrLit {

    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Clone, Debug)]
pub struct CharLit {
    value: u16, // java chars, utf16!
    span: Span,
}

impl CharLit {

    pub fn span(&self) -> Span {
        self.span
    }
}

pub struct Parser {
    token_stack: Vec<Token>,
}

impl Parser {

    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            token_stack: tokens.into_iter().rev().collect(),
        }
    }

    fn pop_token(&mut self, what: &str) -> Result<Token> {
        if let Some(token) = self.token_stack.pop() {
            Ok(token)
        } else {
            Err(report_error!("expected {} but found eof", what))
        }
    }

    pub fn consume_ident(&mut self) -> Result<Ident> {
        loop {
            let token = self.pop_token("identifier")?;
            let span = token.span;
            match token.ty {
                TT::AlphaNum => {
                    break match parse_alphanum(token)? {
                        AlphaNum::Ident(ident) => Ok(ident),
                        _ => Err(report_error!("expected identifier but found literal"; span))?
                    };
                },
                TT::Str | TT::Char => {
                    Err(report_error!("expected identifier but found literal"; span))?
                },
                TT::Punct => {
                    Err(report_error!("expected identifier but found unexpected symbol"; span))?
                },
                TT::LineBreak => {}
            }
        }
    }

    pub fn consume_expr(&mut self, is_bracket: bool) -> Result<Expr> {
        // the shunting-yard algorithm
        // https://en.wikipedia.org/wiki/Shunting-yard_algorithm

        let mut output = Vec::new();
        let mut stack = Vec::<StackElement>::new();

        #[derive(Debug)]
        enum StackElement {
            Expr(Expr),
            BinOp(BinOpKind, Span),
            Delim,
        }

        while let Some(token) = { self.token_stack.pop() } {
            match token.ty {
                TT::AlphaNum => {
                    let expr = match parse_alphanum(token)? {
                        AlphaNum::Ident(ident) => Expr::Ref(ident),
                        AlphaNum::Int(int) => Expr::Int(int),
                        AlphaNum::Float(float) => Expr::Float(float),
                    };
                    output.push(StackElement::Expr(expr));
                },
                TT::Str => {
                    let Token { value, span, .. } = token;
                    let unquoted = value[1..value.len() - 1].into();
                    // FIXME: resolve escapes
                    let expr = Expr::Str(StrLit {
                        value: unquoted,
                        span,
                    });
                    output.push(StackElement::Expr(expr));
                },
                TT::Char => {
                    let Token { value, span, .. } = token;
                    // FIXME: resolve escapes
                    let unquoted = &value[1..value.len() - 1].encode_utf16().collect::<Vec<_>>();
                    if unquoted.len() != 1 {
                        Err(report_error!("character literals must contain exactly one utf16 character"; span))?;
                    }
                    let expr = Expr::Char(CharLit {
                        value: unquoted[0],
                        span,
                    });
                    output.push(StackElement::Expr(expr));
                },
                TT::LineBreak => {
                    if !is_bracket {
                        self.token_stack.push(token);
                        break;
                    }
                }
                TT::Punct => {
                    match token.value.as_str() {
                        // TODO: handle unary ops
                        "+" | "-" | "*" | "/" |
                        "%" | "&" | "|" | "^" => {
                            let op = BinOpKind::from_str(&token.value).unwrap();
                            while let Some(top) = { stack.pop() } {
                                match top {
                                    StackElement::BinOp(op2, _) if !(op2 < op) => {
                                        output.push(top);
                                    },
                                    _ => {
                                        stack.push(top);
                                        break
                                    },
                                }
                            }
                            stack.push(StackElement::BinOp(op, token.span));
                        },
                        "(" => {
                            stack.push(StackElement::Delim);
                        },
                        ")" => {
                            let mut parens_match = false;
                            while let Some(top) = { stack.pop() } {
                                match top {
                                    StackElement::Delim => {
                                        parens_match = true;
                                        break;
                                    },
                                    _ => output.push(top),
                                }
                            }
                            if !parens_match {
                                Err(report_error!("unmatched parens"))?;
                            }
                        },
                        "[" => {
                            let line = self.consume_source_line(true)?;
                            output.push(StackElement::Expr(Expr::Bracket(Box::new(line))));
                        },
                        "]" => {
                            if is_bracket {
                                self.token_stack.push(token);
                                break;
                            }
                        },
                        "," => {
                            self.token_stack.push(token);
                            break;
                        }
                        _ => Err(report_error!("unexpected symbol {}", token.value; token.span))?,
                    }
                }
            }
        }

        while let Some(top) = stack.pop() {
            output.push(top);
        }

        fn consume_output(stack: &mut Vec<StackElement>) -> Result<Expr> {
            match stack.pop() {
                Some(StackElement::Expr(expr)) => Ok(expr),
                Some(StackElement::BinOp(op, span)) => {
                    // backwards, since we're popping off the back
                    let second = consume_output(stack)?;
                    let first = consume_output(stack)?;
                    Ok(Expr::BinOp(Box::new(first), op, Box::new(second)))
                },
                // delimiters can't appear in the operand stack
                Some(_) => unreachable!(),
                // hmm... we ran out of
                None => Err(report_error!("")) // FIXME: this error
            }
        }

        println!("{:?}", output);

        let expr = consume_output(&mut output);

        if !output.is_empty() {
            Err(report_error!("orphaned expr"; match output.pop().unwrap() {
                StackElement::Expr(expr) => expr.span(),
                StackElement::BinOp(_, span) => span,
                _ => unreachable!(),
            }))?;
        }

        expr
    }

    pub fn consume_source_line(&mut self, is_bracket: bool) -> Result<SourceLine> {
        let (label, instruction, mut span) = {
            let maybe_label = self.consume_ident()?;
            let maybe_colon = self.pop_token("expression")?;
            if maybe_colon.value == ":" {
                let instruction = self.consume_ident()?;
                let span = maybe_label.span().extend_to(instruction.span().end);
                (Some(maybe_label), instruction, span)
            } else {
                self.token_stack.push(maybe_colon);
                let span = maybe_label.span();
                (None, maybe_label, span)
            }
        };

        println!("{:?}", instruction);

        let mut args = Vec::new();
        let mut scanning_for_comma = false;

        while let Some(token) = { self.token_stack.pop() } {
            match token.ty {
                TT::LineBreak => {
                    if !is_bracket {
                        break;
                    }
                },
                TT::Punct => {
                    match token.value.as_str() {
                        "," => {
                            if scanning_for_comma {
                                scanning_for_comma = false;
                                span.extend_to(token.span.end);
                            } else {
                                Err(report_error!("expected expression, found `,`"; token.span))?;
                            }
                        },
                        "]" => {
                            if is_bracket {
                                break;
                            }
                        }
                        _ => {
                            self.token_stack.push(token);
                            let expr = self.consume_expr(is_bracket)?;
                            span.extend_to_mut(expr.span().end);
                            args.push(expr);
                            scanning_for_comma = true;
                        },
                    }
                },
                other => {
                    self.token_stack.push(token);
                    let expr = self.consume_expr(is_bracket)?;
                    span.extend_to_mut(expr.span().end);
                    args.push(expr);
                    scanning_for_comma = true;
                },
            }
        }

        Ok(SourceLine{
            label, instruction, args, span,
        })
    }

    pub fn consume_all(&mut self) -> Result<Vec<SourceLine>> {
        let mut lines = Vec::new();
        while !self.token_stack.is_empty() {
            let line = self.consume_source_line(false)?;
            println!("{:?}", line);
            lines.push(line);
        }
        Ok(lines)
    }

}

enum AlphaNum {
    Ident(Ident),
    Int(IntLit),
    Float(FloatLit),
}

fn parse_alphanum(token: Token) -> Result<AlphaNum> {
    let Token { value, span, .. } = token;

    let mut iter = value.chars();
    let first = iter.next().unwrap();
    match first {
        '0' ... '9' => {
            if value.contains('.') ||
                value.contains('e') {
                use std::str::FromStr;
                let num = f64::from_str(&value[..]).map_err(|_| {
                    report_error!("invalid numeric literal"; span)
                })?;
                Ok(AlphaNum::Float(FloatLit{ value: num, span }))
            } else {
                let num =
                    if value.starts_with("0b") {
                        i64::from_str_radix(&value[2..], 2)
                    } else if value.starts_with("0o") {
                        i64::from_str_radix(&value[2..], 8)
                    } else if value.starts_with("0x") {
                        i64::from_str_radix(&value[2..], 16)
                    } else {
                        i64::from_str_radix(&value[..], 10)
                    };
                let num = num.map_err(|_| {
                    report_error!("invalid numeric literal"; span)
                })?;
                Ok(AlphaNum::Int(IntLit{ value: num, span }))
            }
        },
        _ => {
            // only two magic identifiers can
            // have angle brackets
            if (value.contains('<') ||
                value.contains('>')) &&
                !(value == "<init>" || value == "<clinit>"){
                Err(report_error!("only the identifiers `<init>` and `<clinit>` may contain `<` or `>`"; span))?;
            }
            Ok(AlphaNum::Ident(Ident { name: value.clone(), span }))
        },
    }
}
