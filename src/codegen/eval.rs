use ast::*;
use codegen::Generator;
use codegen::labels::LabelKind;
use codegen::attrs;
use codegen::constants;
use reporting::*;
use sections::*;
use source_file::*;

use classfile::indexing::*;
use classfile::raw;

use std::collections::HashMap;
use std::{u16, i16, u32, i32};

pub const FN_NONE: Option<fn(&mut EvalContext, String) -> ConstantIndex> = None;

#[derive(Clone, Debug)]
pub enum Value {
    Int(i64, Span),
    Const(ConstantIndex, Span),
    // not a CodeIndex, since this
    // points to the nth *instruction*,
    // not the nth byte
    Code(u16, Span),
    Float(f64, Span),
    Str(String, Span),
}

impl Value {

    pub fn report_name(&self) -> &'static str {
        match *self {
            Value::Int(..) => "integer",
            Value::Const(..) => "constant pool index",
            Value::Code(..) => "code index",
            Value::Float(..) => "float",
            Value::Str(..) => "string"
        }
    }
}

impl Spannable for Value {
    fn span(&self) -> Span {
        match *self {
            Value::Int(_, ref span) => span,
            Value::Const(_, ref span) => span,
            Value::Code(_, ref span) => span,
            Value::Float(_, ref span) => span,
            Value::Str(_, ref span) => span,
        }.clone()
    }
}

pub struct EvalContext<'a> {
    pub gen: &'a mut Generator,
    pub local_labels: Option<&'a HashMap<String, LabelKind>>,
    pub opcode_indices: Option<&'a Vec<CodeIndex>>,
}

impl<'b> EvalContext<'b> {

    pub fn new(gen: &'b mut Generator) -> EvalContext {
        EvalContext {
            gen,
            local_labels: None,
            opcode_indices: None,
        }
    }

    pub fn with_locals(&mut self, locals: &'b HashMap<String, LabelKind>) -> &mut EvalContext<'b> {
        self.local_labels = Some(locals);
        self
    }

    pub fn with_opcodes(&mut self, opcodes: &'b Vec<CodeIndex>) -> &mut EvalContext<'b> {
        self.opcode_indices = Some(opcodes);
        self
    }

    fn reborrow<'a>(&mut self) -> &mut EvalContext<'a> where 'b: 'a {
        unsafe { ::std::mem::transmute(self) }
    }

    pub fn eval(&mut self, expr: Expr) -> Reported<Value> {

        let mut reports = Reported::new();

        let val = match expr {
            Expr::Ref(ident) => {
                macro_rules! expand_ref {
                    ($referent:expr) => {
                        match *$referent {
                            LabelKind::Constant(index) => Value::Const(ConstantIndex(index as u16), ident.span),
                            LabelKind::Item => fatal_error!(reports; "label points to item instruction; expected constant"; ident.span),
                            LabelKind::Bootstrap(index) => Value::Const(ConstantIndex(index as u16), ident.span),
                            LabelKind::Reserved => fatal_error!(reports; "label points to <reserved>; expected constant; this is probably a bug"; ident.span),
                            LabelKind::Code(index) => Value::Code(index as u16, ident.span),
                        }
                    }
                }

                if let Some(referent) = self.gen.labels.get(&ident.name) {
                    expand_ref!(referent)
                } else if let Some(Some(referent)) = self.local_labels.map(|m| m.get(&ident.name)) {
                    expand_ref!(referent)
                } else {
                    fatal_error!(reports; "could not find label `{}`", ident.name; ident.span)
                }
            },
            Expr::Int(int) => Value::Int(int.value, int.span),
            Expr::Float(float) => Value::Float(float.value, float.span),
            Expr::Str(str) => Value::Str(str.value , str.span),
            // at this point, we don't really care that char
            // had some value to begin with
            Expr::Char(char) => Value::Int(char.value as i64, char.span),
            Expr::Bracket(instruction) => {
                let Instruction { label, ident, body, span } = { *instruction };
                match body {
                    InstructionBody::Constant(c) => {
                        let constant = report_try!(reports; constants::expand_constant(&mut self.gen, ConstantSection {
                            label: None, ident, span: span.clone(), body: c
                        }));
                        Value::Const(self.gen.push_expanded_constant(constant), span)
                    },
                    InstructionBody::Meta(m) => {
                        // the only thing we actually support is a bootstrap method,
                        // which is *sorta* a kludge
                        let meta = report_try!(reports; attrs::expand_class_attr(&mut self.gen, MetaSection {
                            label: None, ident, span: span.clone(), body: m
                        }));
                        match meta {
                            attrs::ClassMeta::Bootstrap(m) => {
                                self.gen.bootstrap_table.push(m);
                                Value::Const(ConstantIndex(self.gen.bootstrap_table.len() as u16 - 1), span)
                            },
                            _ => {
                                fatal_error!(reports; "only `bootstrap_method` or constant instructions allowed here"; span);
                            },
                        }
                    },
                    _ => {
                        fatal_error!(reports; "only `bootstrap_method` or constant instructions allowed here"; span);
                    },
                }
            },
            Expr::BinOp(first, op, second) => {
                let span = first.span().extend_to(&second);
                let first = report_try!(reports; self.eval(*first));
                let second = report_try!(reports; self.eval(*second));

                match (first, second) {
                    (Value::Int(x, span1), Value::Int(y, span2)) => {
                        let val = match op {
                            BinOpKind::Add => x + y,
                            BinOpKind::Sub => x - y,
                            BinOpKind::Mul => x * y,
                            BinOpKind::Div => x / y,
                            BinOpKind::Rem => x % y,
                            BinOpKind::And => x & y,
                            BinOpKind::Or => x | y,
                            BinOpKind::Xor => x ^ y,
                        };
                        Value::Int(val, span)
                    },
                    (Value::Float(x, span1), Value::Float(y, span2)) => {
                        let val = match op {
                            BinOpKind::Add => x + y,
                            BinOpKind::Sub => x - y,
                            BinOpKind::Mul => x * y,
                            BinOpKind::Div => x / y,
                            BinOpKind::Rem => x % y,
                            _ => {
                                fatal_error!(reports; "operation between incompatible types"; span)
                            }
                        };
                        Value::Float(val, span)
                    }
                    _ => {
                        fatal_error!(reports; "operation between incompatible types")
                    }
                }
            }
        };

        reports.complete(val)
    }

    pub fn eval_into_const_index<'a, F>(&'a mut self, expr: Expr, str_to_const: Option<F>) -> Reported<ConstantIndex>
        where F: FnOnce(&'a mut Self, String) -> ConstantIndex {
        let mut reports = Reported::new();

        let val = match report_try!(reports; self.eval(expr)) {
            Value::Int(val, span) => {
                if val >= 0 && val <= (u16::MAX as i64) {
                    ConstantIndex(val as u16)
                } else {
                    fatal_error!(reports; "expected u16, found wider integer: {}", val as u64; span)
                }
            },
            Value::Const(index, _) => index,
            Value::Code(_, span) => {
                fatal_error!(reports; "expected constant pool index, found code index"; span)
            }
            Value::Float(_, span) => {
                fatal_error!(reports; "expected constant pool index, found float"; span)
            },
            Value::Str(str, span) => if let Some(f) = str_to_const {
                f(self.reborrow(), str)
            } else {
                fatal_error!(reports; "expected constant pool index, found string"; span)
            }
        };

        reports.complete(val)
    }

    pub fn eval_into_utf8<'a>(&'a mut self, expr: Expr) -> Reported<ConstantIndex> {
        EvalContext::eval_into_const_index(self, expr, Some(|cx: &mut EvalContext, str| cx.gen.push_string_constant(str)))
    }

    pub fn eval_into_class<'a>(&'a mut self, expr: Expr) -> Reported<ConstantIndex> {
        EvalContext::eval_into_const_index(self.reborrow(), expr, Some(|cx: &mut EvalContext, s| {
            let first = cx.gen.push_expanded_constant(raw::Constant::Utf8(s));
            cx.gen.push_expanded_constant(raw::Constant::Class(first))
        }))
    }

    pub fn eval_into_code_index(&mut self, expr: Expr) -> Reported<CodeIndex> {
        let mut reports = Reported::new();

        let val = match report_try!(reports; self.eval(expr)) {
            Value::Int(val, span) => {
                if val >= 0 && val <= (u16::MAX as i64) {
                    CodeIndex(val as u16)
                } else {
                    fatal_error!(reports; "expected u16, found wider integer: {}", val as u64; span)
                }
            },
            Value::Const(_, span) => {
                fatal_error!(reports; "expected code index, found constant pool index"; span)
            },
            Value::Code(index, span) => {
                if let Some(ref indices) = self.opcode_indices {
                    if (index as usize) < indices.len() {
                        indices[index as usize].clone()
                    } else {
                        fatal_error!(reports; "out of bounds code index: {}", index; span);
                    }
                } else {
                    fatal_error!(reports; "assembler error: could not find code index table"; span)
                }
            }
            Value::Float(_, span) => {
                fatal_error!(reports; "expected code index, found float"; span)
            },
            Value::Str(str, span) => {
                fatal_error!(reports; "expected code index, found string"; span)
            }
        };

        reports.complete(val)
    }

    pub fn eval_into_code_offset(&mut self, expr: Expr, relative_to: CodeIndex) -> Reported<CodeOffset> {
        let mut reports = Reported::new();

        let val = match report_try!(reports; self.eval(expr)) {
            Value::Int(val, span) => {
                if val >= 0 && val <= (i16::MAX as i64) {
                    CodeOffset(val as i16)
                } else {
                    fatal_error!(reports; "expected i16, found wider integer: {}", val as u64; span)
                }
            },
            Value::Const(_, span) => {
                fatal_error!(reports; "expected code index, found constant pool index"; span)
            },
            Value::Code(index, span) => {
                if let Some(ref indices) = self.opcode_indices {
                    if (index as usize) < indices.len() {
                        CodeOffset(indices[index as usize].clone().0 as i16 - relative_to.0 as i16)
                    } else {
                        fatal_error!(reports; "out of bounds code index: {}", index; span);
                    }
                } else {
                    fatal_error!(reports; "assembler error: could not find code index table"; span)
                }
            }
            Value::Float(_, span) => {
                fatal_error!(reports; "expected code index, found float"; span)
            },
            Value::Str(str, span) => {
                fatal_error!(reports; "expected code index, found string"; span)
            }
        };

        reports.complete(val)
    }

    pub fn eval_into_wide_code_offset(&mut self, expr: Expr, relative_to: CodeIndex) -> Reported<WideCodeOffset> {
        let mut reports = Reported::new();

        let val = match report_try!(reports; self.eval(expr)) {
            Value::Int(val, span) => {
                if val >= 0 && val <= (i32::MAX as i64) {
                    WideCodeOffset(val as i32)
                } else {
                    fatal_error!(reports; "expected i32, found wider integer: {}", val as u64; span)
                }
            },
            Value::Const(_, span) => {
                fatal_error!(reports; "expected code index, found constant pool index"; span)
            },
            Value::Code(index, span) => {
                if let Some(ref indices) = self.opcode_indices {
                    if (index as usize) < indices.len() {
                        WideCodeOffset(indices[index as usize].clone().0 as i32 - relative_to.0 as i32)
                    } else {
                        fatal_error!(reports; "out of bounds code index: {}", index; span);
                    }
                } else {
                    fatal_error!(reports; "assembler error: could not find code index table"; span)
                }
            }
            Value::Float(_, span) => {
                fatal_error!(reports; "expected code index, found float"; span)
            },
            Value::Str(str, span) => {
                fatal_error!(reports; "expected code index, found string"; span)
            }
        };

        reports.complete(val)
    }



    pub fn eval_into_bare_word(&mut self, expr: Expr) -> Reported<Ident> {
        let mut reports = Reported::new();

        let val = match expr {
            Expr::Ref(ident) => ident,
            other => fatal_error!(reports; "expected keyword"; other),
        };

        reports.complete(val)
    }

    pub fn eval_into_int<F, N>(&mut self, expr: Expr, expected: &str, convert: F) -> Reported<N>
        where F: FnOnce(i64) -> Option<N> {
        let mut reports = Reported::new();

        match report_try!(reports; self.eval(expr)) {
            Value::Int(val, span) => {
                if let Some(x) = convert(val) {
                    reports.complete(x)
                } else {
                    fatal_error!(reports; "expected {}, found wider integer", expected; span)
                }
            },
            other => fatal_error!(reports; "expected {}, found {}", expected, other.report_name(); other)
        }
    }

    pub fn eval_into_float<F, N>(&mut self, expr: Expr, expected: &str, convert: F) -> Reported<N>
        where F: FnOnce(f64) -> Option<N> {
        let mut reports = Reported::new();

        match report_try!(reports; self.eval(expr)) {
            Value::Float(val, span) => {
                if let Some(x) = convert(val) {
                    reports.complete(x)
                } else {
                    fatal_error!(reports; "expected {}, found wider float", expected; span)
                }
            },
            other => fatal_error!(reports; "expected {}, found {}", expected, other.report_name(); other)
        }
    }
}

