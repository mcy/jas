use ast::*;
use consts::special;
use reporting::*;

use classfile::raw;
use classfile::indexing::*;

use std::collections::HashMap;
use std::mem;
use std::{u8, u16, u32, u64, i32, f32};

#[derive(Debug)]
pub struct PartialClass {

    minor_version: Option<u16>,
    major_version: Option<u16>,

    labels: HashMap<String, u64>,
    declared_constants: Vec<raw::Constant>,
    declared_count: usize,
    expanded_constants: Vec<raw::Constant>,

    this_class: Option<ConstantIndex>,
    super_class: Option<ConstantIndex>,

}

impl PartialClass {

    pub fn new() -> PartialClass {
        PartialClass {
            minor_version: None,
            major_version: None,

            labels: HashMap::new(),
            declared_constants: Vec::new(),
            declared_count: 0,
            expanded_constants: Vec::new(),

            this_class: None,
            super_class: None,
        }
    }

    pub fn process_section(&mut self, section: ClassSection) -> Reported<()> {

        let mut reports = Reported::new();

        merge_reports!(reports, self.expand_labels(&section));

        merge_reports!(reports, self.expand_declared_constants(section.constants));

        reports.complete(())
    }

    fn push_declared_constant(&mut self, constant: raw::Constant) {
        let is_wide = match constant {
            raw::Constant::Long(..) |
            raw::Constant::Double(..) => true,
            _ => false,
        };

        self.declared_constants.push(constant);
        if is_wide { self.declared_constants.push(raw::Constant::WidePlaceholder) };
    }

    fn push_expanded_constant(&mut self, constant: raw::Constant) -> ConstantIndex {
        let is_wide = match constant {
            raw::Constant::Long(..) |
            raw::Constant::Double(..) => true,
            _ => false,
        };

        self.expanded_constants.push(constant);
        if is_wide { self.expanded_constants.push(raw::Constant::WidePlaceholder) };

        ConstantIndex((self.declared_count + self.expanded_constants.len()) as u16)
    }

    fn constant_string(&mut self, str: String) -> ConstantIndex {
        self.push_expanded_constant(raw::Constant::Utf8(str))
    }

    pub fn expand_labels(&mut self, section: &ClassSection) -> Reported<()> {

        let mut reports = Reported::new();

        let mut index = 1;
        for constant in section.constants.iter() {
            if let Some(ref label) = *constant.label() {
                if self.labels.contains_key(label.value()) {
                    fatal_error!(reports; "duplicate label"; label.span())
                } else {
                    self.labels.insert(label.value().clone(), index);
                }
                index += match *constant.instruction() {
                    InstructionType::Long(..) |
                    InstructionType::Double(..) => 2,
                    _ => 1,
                }
            }
        }

        self.declared_count = index as usize + 1;

        reports.complete(())
    }

    pub fn expand_declared_constants(&mut self, constants: Vec<Instruction>) -> Reported<()> {

        let mut reports = Reported::new();

        for constant in constants.into_iter() {
            let constant = merge_reports!(reports, self.expand_constant(constant));
            self.push_declared_constant(constant);
        }

        reports.complete(())
    }

    pub fn expand_constant(&mut self, constant: Instruction) -> Reported<raw::Constant> {

        let mut reports = Reported::new();

        fn handle_item_ref(this: &mut PartialClass, class: Expr, name: Expr, descriptor: Option<Expr>) -> Reported<(ConstantIndex, ConstantIndex)> {
            let mut reports = Reported::new();

            let no_function: Option<fn(&mut PartialClass, String) -> ConstantIndex> = None;

            let class_index = merge_reports!(reports, this.eval_expr_into_index(class, Some(|this: &mut PartialClass, s| {
                    let str = this.push_expanded_constant(raw::Constant::Utf8(s));
                    this.push_expanded_constant(raw::Constant::Class(str))
                })));
            let name_and_type = if let Some(descriptor) = descriptor {
                let name_index = merge_reports!(reports,
                        this.eval_expr_into_index(name, Some(PartialClass::constant_string)));
                let descriptor_index = merge_reports!(reports,
                        this.eval_expr_into_index(descriptor, Some(PartialClass::constant_string)));
                this.push_expanded_constant(raw::Constant::Signature {
                    name: name_index, descriptor: descriptor_index,
                })
            } else {
                merge_reports!(reports, this.eval_expr_into_index(name, no_function))
            };

            reports.complete((class_index, name_and_type))
        }

        let span = constant.span();
        let constant = match constant.into_instruction() {
            InstructionType::ClassRef(class) => {
                let index = merge_reports!(reports,
                    self.eval_expr_into_index(class, Some(PartialClass::constant_string)));
                raw::Constant::Class(index)
            },
            InstructionType::FieldRef { class, name, descriptor } => {
                let (class_index, name_and_type) = merge_reports!(reports, handle_item_ref(self, class, name, descriptor));
                raw::Constant::FieldRef { class: class_index, signature: name_and_type }
            },
            InstructionType::MethodRef { class, name, descriptor } => {
                let (class_index, name_and_type) = merge_reports!(reports, handle_item_ref(self, class, name, descriptor));
                raw::Constant::MethodRef { class: class_index, signature: name_and_type }
            },
            InstructionType::InterfaceMethodRef { class, name, descriptor } => {
                let (class_index, name_and_type) = merge_reports!(reports, handle_item_ref(self, class, name, descriptor));
                raw::Constant::InterfaceMethodRef { class: class_index, signature: name_and_type }
            },
            InstructionType::String(str) => {
                let index = merge_reports!(reports,
                    self.eval_expr_into_index(str, Some(PartialClass::constant_string)));
                raw::Constant::String(index)
            },
            InstructionType::Integer(expr) => {
                match merge_reports!(reports, self.eval_expr(expr)) {
                    Value::Int(val, span) => {
                        if val >= (i32::MIN as i64) && val <= (i32::MAX as i64) {
                            raw::Constant::Integer(val as i32)
                        } else {
                            fatal_error!(reports; "integer wider than four bytes: {}", val; span)
                        }
                    },
                    Value::Float(_, span) => {
                        fatal_error!(reports; "expected integer, found float"; span)
                    },
                    Value::Str(_, span) => {
                        fatal_error!(reports; "expected integer, found string"; span)
                    },
                }
            },
            InstructionType::Float(expr) => {
                match merge_reports!(reports, self.eval_expr(expr)) {
                    Value::Int(_, span) => {
                        fatal_error!(reports; "expected float, found string"; span)
                    },
                    Value::Float(val, _) => {
                        // FIXME: ensure this actually fits
                        raw::Constant::Float(val as f32)
                    },
                    Value::Str(_, span) => {
                        fatal_error!(reports; "expected float, found string"; span)
                    },
                }
            },
            InstructionType::Long(expr) => {
                match merge_reports!(reports, self.eval_expr(expr)) {
                    Value::Int(val, _) => {
                        raw::Constant::Long(val)
                    },
                    Value::Float(_, span) => {
                        fatal_error!(reports; "expected integer, found float"; span)
                    },
                    Value::Str(_, span) => {
                        fatal_error!(reports; "expected integer, found string"; span)
                    },
                }
            },
            InstructionType::Double(expr) => {
                match merge_reports!(reports, self.eval_expr(expr)) {
                    Value::Int(_, span) => {
                        fatal_error!(reports; "expected float, found string"; span)
                    },
                    Value::Float(val, _) => {
                        // FIXME: ensure this actually fits
                        raw::Constant::Double(val)
                    },
                    Value::Str(_, span) => {
                        fatal_error!(reports; "expected float, found string"; span)
                    },
                }
            },
            InstructionType::NameAndType(name, descriptor) => {
                let name_index = merge_reports!(reports,
                    self.eval_expr_into_index(name, Some(PartialClass::constant_string)));
                let descriptor_index = merge_reports!(reports,
                    self.eval_expr_into_index(descriptor, Some(PartialClass::constant_string)));
                raw::Constant::Signature {
                    name: name_index, descriptor: descriptor_index,
                }
            }
            InstructionType::Utf8(expr) => {
                match merge_reports!(reports, self.eval_expr(expr)) {
                    Value::Int(val, _) => {
                        fatal_error!(reports; "expected string, found integer"; span)
                    },
                    Value::Float(_, span) => {
                        fatal_error!(reports; "expected string, found float"; span)
                    },
                    Value::Str(str, _) => {
                        raw::Constant::Utf8(str)
                    },
                }
            }
            InstructionType::MethodHandle { kind, referent } => {
                let kind = match kind {
                    Expr::Ref(ident) => match ident.value().as_str() {
                        special::HANDLE_KIND_GET_FIELD => raw::MethodHandleKind::GetField,
                        special::HANDLE_KIND_GET_STATIC => raw::MethodHandleKind::GetStatic,
                        special::HANDLE_KIND_PUT_FIELD => raw::MethodHandleKind::PutStatic,
                        special::HANDLE_KIND_PUT_STATIC => raw::MethodHandleKind::PutStatic,

                        special::HANDLE_KIND_INVOKE_VIRTUAL => raw::MethodHandleKind::InvokeVirtual,
                        special::HANDLE_KIND_INVOKE_STATIC => raw::MethodHandleKind::InvokeStatic,
                        special::HANDLE_KIND_INVOKE_SPECIAL => raw::MethodHandleKind::InvokeSpecial,
                        special::HANDLE_KIND_NEW_INVOKE_SPECIAL => raw::MethodHandleKind::NewInvokeSpecial,
                        special::HANDLE_KIND_INVOKE_INTERFACE => raw::MethodHandleKind::InvokeInterface,
                        other => fatal_error!(reports; "`{}` is not a method handle kind", other; ident.span())
                    }
                    other => fatal_error!(reports; "expected ident"; other.span())
                };
                let no_function: Option<fn(&mut PartialClass, String) -> ConstantIndex> = None;
                let referent_index = merge_reports!(reports, self.eval_expr_into_index(referent, no_function));
                raw::Constant::MethodHandle {
                    kind, referent: referent_index,
                }
            },
            InstructionType::MethodType(expr) => {
                let index = merge_reports!(reports,
                    self.eval_expr_into_index(expr, Some(PartialClass::constant_string)));
                raw::Constant::MethodType(index)
            },
            InstructionType::DynamicTarget { .. } => {
                fatal_error!(reports; "dynamic_target is not currently supported"; span)
            },
            other => fatal_error!(reports; "expected constant instruction, found {} instruction", match other.category() {
                InstructionCategory::Code => "code",
                InstructionCategory::Item => "item",
                InstructionCategory::Meta => "meta",
                _ => "???"
            },; span)
        };

        reports.complete(constant)
    }

    fn eval_expr(&mut self, expr: Expr) -> Reported<Value> {

        let mut reports = Reported::new();

        let val = match expr {
            Expr::Ref(ident) => {
                if let Some(index) = self.labels.get(ident.value()) {
                    Value::Int(unsafe { mem::transmute(*index) }, ident.span())
                } else {
                    fatal_error!(reports; "could not find label `{}`", ident.value(); ident.span())
                }
            },
            Expr::Int(int) => Value::Int(int.value(), int.span()),
            Expr::Float(float) => Value::Float(float.value(), float.span()),
            Expr::Str(str) => Value::Str(str.value().clone() /* ugh */, str.span()),
            Expr::Char(char) => Value::Int(char.value() as i64, char.span()),
            Expr::Bracket(instruction) => {
                let span = instruction.span();
                let constant = merge_reports!(reports, self.expand_constant(*instruction));
                Value::Int(self.push_expanded_constant(constant).0 as i64, span)
            },
            Expr::BinOp(first, op, second) => {
                let span = first.span().extend_to(second.span().end);
                let first = merge_reports!(reports, self.eval_expr(*first));
                let second = merge_reports!(reports, self.eval_expr(*second));

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

    fn eval_expr_into_index<'a, F>(&'a mut self, expr: Expr, str_to_const: Option<F>) -> Reported<ConstantIndex>
        where F: Fn(&'a mut Self, String) -> ConstantIndex {
        let mut reports = Reported::new();

        #[inline]
        fn parse_constant_index(val: i64, span: Span) -> Reported<ConstantIndex> {
            let mut reports = Reported::new();
            if val >= 0 && val <= (u16::MAX as i64) {
                reports.complete(ConstantIndex(val as u16))
            } else {
                fatal_error!(reports; "integer wider than two bytes: {}", val; span)
            }
        }

        let val = match merge_reports!(reports, self.eval_expr(expr)) {
            Value::Int(val, span) => {
                merge_reports!(reports, parse_constant_index(val, span))
            },
            Value::Float(_, span) => {
                fatal_error!(reports; "expected index, found float"; span)
            },
            Value::Str(str, span) => if let Some(f) = str_to_const {
                f(self, str)
            } else {
                fatal_error!(reports; "expected index, found string"; span)
            }
        };

        reports.complete(val)
    }
}

pub struct PartialField {

}


// represents a pile of instructions
// contiguously making up a class
pub struct ClassSection {
    class: Instruction,
    top_level: Vec<Instruction>,
    constants: Vec<Instruction>,
    fields: Vec<FieldSection>,
    methods: Vec<MethodSection>,
}

impl ClassSection {

    pub fn from_instructions(ast: Vec<Instruction>) -> Reported<Vec<ClassSection>> {

        let mut stack = ast.into_iter().rev().collect::<Vec<_>>();

        let mut reports = Reported::new();
        let mut result = Vec::new();

        while !stack.is_empty() {
            result.push(merge_reports!(reports, ClassSection::consume_class(&mut stack)));
        }

        reports.complete(result)
    }

    fn consume_class(stack: &mut Vec<Instruction>) -> Reported<ClassSection> {

        use ast::InstructionCategory as IC;

        let mut reports = Reported::new();

        // first, get the class instruction, and error
        // at anything else
        let class = loop {
            let next = stack.pop().unwrap();
            if let InstructionType::Class(..) = *next.instruction() {
                break next;
            } else {
                reports.report(report_error!("found `{}` before `class` instruction", next.ident().value(); next.ident().span()));
            }
        };

        let mut top_level = Vec::new();

        let mut constants = Vec::new();

        // parse instructons until we hit an item
        loop {
            if let Some(op) = stack.pop() {
                match op.instruction().category() {
                    IC::Item => {
                        stack.push(op);
                        break;
                    },
                    IC::Meta => top_level.push(op),
                    IC::Constant => constants.push(op),
                    IC::Code => reports.report(report_error!(
                        "unexpected code instruction `{}` in class", op.ident().value(); op.ident().span()
                    ))
                }
            } else {
                break;
            }
        }

        let mut methods = Vec::new();
        let mut fields = Vec::new();

        while let Some(next) = stack.pop() {
            if next.instruction().category() == IC::Item {
                constants.push(next);
            } else {
                match *next.instruction() {
                    InstructionType::Field {..} => {
                        let field = next;
                        let mut meta = Vec::new();
                        while let Some(op) = stack.pop() {
                            match op.instruction().category() {
                                IC::Item => {
                                    stack.push(op);
                                    break
                                },
                                IC::Meta => meta.push(op),
                                IC::Constant => constants.push(op),
                                IC::Code => reports.report(report_error!(
                                    "unexpected code instruction `{}` in field", op.ident().value(); op.ident().span()
                                ))
                            }
                        }
                        fields.push(FieldSection { field, meta });
                    }
                    InstructionType::Method {..} => {
                        let method = next;
                        let mut meta = Vec::new();
                        let mut code = Vec::new();
                        while let Some(op) = stack.pop() {
                            match op.instruction().category() {
                                IC::Item => {
                                    stack.push(op);
                                    break
                                },
                                IC::Meta => meta.push(op),
                                IC::Constant => constants.push(op),
                                IC::Code => code.push(op),
                            }
                        }
                        methods.push(MethodSection { method, meta, code });
                    }
                    _ => {
                        stack.push(next);
                        break
                    }
                }
            }
        }

        reports.complete(ClassSection { class, top_level, constants, fields, methods })
    }
}

pub struct FieldSection {
    field: Instruction,
    meta: Vec<Instruction>,
}

pub struct MethodSection {
    method: Instruction,
    meta: Vec<Instruction>,
    code: Vec<Instruction>,
}

pub struct Transformer {

    instructions: Vec<Instruction>,

    classes: Vec<PartialClass>,
}

#[derive(Clone, Debug)]
pub enum Value {
    Int(i64, Span),
    Float(f64, Span),
    Str(String, Span),
}


