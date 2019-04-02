
use crate::ast::*;
use crate::lexer::*;
use crate::phase::Phase;
use crate::reporting::*;
use crate::source_file::Span;

pub struct Parser {
    token_stack: Vec<Token>,
}

impl Parser {

    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            token_stack: tokens.into_iter().rev().collect(),
        }
    }

    fn pop_token(&mut self, what: &str) -> Reported<Token> {

        let mut reports = Reported::new();

        if let Some(token) = self.token_stack.pop() {
            reports.complete(token)
        } else {
            fatal_error!(reports; "expected {} but found eof", what)
        }
    }

    pub fn consume_ident(&mut self) -> Reported<Ident> {

        let mut reports = Reported::new();

        loop {
            let token = report_try!(reports; self.pop_token("identifier"));
            let span = token.span();
            match token.ty {
                TokenType::AlphaNum => {
                    break match report_try!(reports; AlphaNum::parse(token)) {
                        AlphaNum::Ident(ident) => reports.complete(ident),
                        _ => {
                            fatal_error!(reports; "expected identifier but found literal"; span)
                        }
                    };
                },
                TokenType::Str | TokenType::Char => {
                    fatal_error!(reports; "expected identifier but found literal"; span)
                },
                TokenType::Punct => {
                    fatal_error!(reports; "expected identifier but found unexpected symbol"; span)
                },
                TokenType::LineBreak => {}
            }
        }
    }

    pub fn consume_expr(&mut self, is_bracket: bool) -> Reported<Expr> {
        // the shunting-yard algorithm
        // https://en.wikipedia.org/wiki/Shunting-yard_algorithm

        let mut reports = Reported::new();

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
                TokenType::AlphaNum => {
                    let expr = match report_try!(reports; AlphaNum::parse(token)) {
                        AlphaNum::Ident(ident) => Expr::Ref(ident),
                        AlphaNum::Int(int) => Expr::Int(int),
                        AlphaNum::Float(float) => Expr::Float(float),
                    };
                    output.push(StackElement::Expr(expr));
                },
                TokenType::Str => {
                    let Token { value, span, .. } = token;
                    let unquoted = value[1..value.len() - 1].into();
                    // FIXME: resolve escapes
                    let expr = Expr::Str(StrLit {
                        value: unquoted,
                        span,
                    });
                    output.push(StackElement::Expr(expr));
                },
                TokenType::Char => {
                    let Token { value, span, .. } = token;
                    // FIXME: resolve escapes
                    let unquoted = &value[1..value.len() - 1].encode_utf16().collect::<Vec<_>>();
                    if unquoted.len() != 1 {
                        fatal_error!(reports; "character literals must contain exactly one utf16 character"; span);
                    }
                    let expr = Expr::Char(CharLit {
                        value: unquoted[0],
                        span,
                    });
                    output.push(StackElement::Expr(expr));
                },
                TokenType::LineBreak => {
                    if !is_bracket {
                        self.token_stack.push(token);
                        break;
                    }
                }
                TokenType::Punct => {
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
                                fatal_error!(reports; "unmatched parens")
                            }
                        },
                        "[" => {
                            let line = report_try!(reports; self.consume_source_line(true));
                            let instruction = report_try!(reports; Instruction::from_source(line));
                            output.push(StackElement::Expr(Expr::Bracket(Box::new(instruction))));
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
                        _ => {
                            fatal_error!(reports; "unexpected symbol {}", token.value; token.span)
                        },
                    }
                }
            }
        }

        while let Some(top) = stack.pop() {
            output.push(top);
        }

        fn consume_output(stack: &mut Vec<StackElement>) -> Reported<Expr> {
            let mut reports = Reported::new();
            match stack.pop() {
                Some(StackElement::Expr(expr)) => reports.complete(expr),
                Some(StackElement::BinOp(op, _span)) => {
                    // backwards, since we're popping off the back
                    let second = report_try!(reports; consume_output(stack));
                    let first = report_try!(reports; consume_output(stack));
                    reports.complete(Expr::BinOp(Box::new(first), op, Box::new(second)))
                },
                // delimiters can't appear in the operand stack
                Some(_) => unreachable!(),
                // hmm... we ran out of stuff to parse
                None => {
                    fatal_error!(reports; "")
                } // FIXME: this error
            }
        }

        let expr = report_try!(reports; consume_output(&mut output));

        if !output.is_empty() {
            fatal_error!(reports; "orphaned expr"; match output.pop().unwrap() {
                StackElement::Expr(expr) => expr.span(),
                StackElement::BinOp(_, span) => span,
                _ => unreachable!(),
            })
        }

        reports.complete(expr)
    }

    pub fn consume_source_line(&mut self, is_bracket: bool) -> Reported<SourceLine> {
        let mut reports = Reported::new();

        let (label, instruction, mut span) = {
            let maybe_label = report_try!(reports; self.consume_ident());
            let maybe_colon = self.token_stack.pop();
            if maybe_colon.is_some() && maybe_colon.as_ref().unwrap().value == ":" {
                let instruction = report_try!(reports; self.consume_ident());
                let _maybe_colon = maybe_colon.unwrap();
                let span = maybe_label.span.extend_to(&instruction);
                (Some(maybe_label), instruction, span)
            } else {
                if let Some(token) = maybe_colon {
                    self.token_stack.push(token);
                }
                let span = maybe_label.span();
                (None, maybe_label, span)
            }
        };

        let mut args = Vec::new();
        let mut scanning_for_comma = false;

        while let Some(token) = { self.token_stack.pop() } {
            match token.ty {
                TokenType::LineBreak => {
                    if !is_bracket {
                        break;
                    }
                },
                TokenType::Punct => {
                    match token.value.as_str() {
                        "," => {
                            if scanning_for_comma {
                                scanning_for_comma = false;
                                span.extend_to(&token);
                            } else {
                                fatal_error!(reports; "expected expression, found `,`"; token.span)
                            }
                        },
                        "]" => {
                            if is_bracket {
                                break;
                            }
                        }
                        _ => {
                            self.token_stack.push(token);
                            let expr = report_try!(reports; self.consume_expr(is_bracket));
                            span.extend_to_mut(&expr);
                            args.push(expr);
                            scanning_for_comma = true;
                        },
                    }
                },
                _ => {
                    self.token_stack.push(token);
                    let expr = report_try!(reports; self.consume_expr(is_bracket));
                    span.extend_to_mut(&expr);
                    args.push(expr);
                    scanning_for_comma = true;
                },
            }
        }

        reports.complete(SourceLine{
            label, instruction, args, span,
        })
    }

    pub fn consume_all(&mut self) -> Reported<Vec<Instruction>> {
        let mut reports = Reported::new();
        let mut lines = Vec::new();
        while !self.token_stack.is_empty() {
            let line = reports.merge(self.consume_source_line(false));
            if let Some(line) = line {
                let instruction = reports.merge(Instruction::from_source(line));
                if let Some(inst) = instruction {
                    lines.push(inst);
                }
            }
        }
        reports.complete(lines)
    }

}

impl Phase for Parser {

    type Input = Vec<Token>;
    type Output = Vec<Instruction>;

    fn run(input: Vec<Self::Input>) -> Reported<Vec<Self::Output>> {
        let mut reports = Reported::new();
        let mut out = Vec::new();
        for tokens in input {
            let mut parser = Parser::new(tokens);
            if let Some(lines) = reports.merge(parser.consume_all()) {
                out.push(lines);
            }
        }
        reports.complete(out)
    }
}

enum AlphaNum {
    Ident(Ident),
    Int(IntLit),
    Float(FloatLit),
}

impl AlphaNum {
    fn parse(token: Token) -> Reported<AlphaNum> {
        let mut reports = Reported::new();

        let Token { value, span, .. } = token;

        let mut iter = value.chars();
        let first = iter.next().unwrap();
        match first {
            '0' ..= '9' => {
                if value.contains('.') ||
                    (value.contains('e') && !value.contains('x')) {
                    use std::str::FromStr;
                    if let Ok(num) = f64::from_str(&value[..]) {
                        reports.complete(AlphaNum::Float(FloatLit{ value: num, span }))
                    } else {
                        fatal_error!(reports; "invalid numeric literal"; span)
                    }
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
                    if let Ok(num) = num {
                        reports.complete(AlphaNum::Int(IntLit{ value: num, span }))
                    } else {
                        fatal_error!(reports; "invalid numeric literal"; span)
                    }
                }
            },
            _ => {
                // only two magic identifiers can
                // have angle brackets
                if (value.contains('<') ||
                    value.contains('>')) &&
                    !(value == "<init>" || value == "<clinit>"){
                    fatal_error!(reports; "only the identifiers `<init>` and `<clinit>` may contain `<` or `>`"; span)
                }
                reports.complete(AlphaNum::Ident(Ident { name: value.clone(), span }))
            },
        }
    }
}