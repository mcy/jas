use ast::*;
use consts;
use consts::special;
use codegen::*;
use phase::Phase;
use reporting::*;
use source_file::*;

use classfile::raw;
use classfile::indexing::*;
use classfile::consts as flags;

use base64;

use std::collections::HashMap;
use std::mem;
use std::{u8, u16, u32, u64, i8, i16, i32, f32};
use std::vec;

#[derive(Debug)]
pub struct Generator {

    minor_major_version: Option<(u16, u16)>,

    labels: HashMap<String, LabelKind>,

    declared_constants: Vec<raw::Constant>,
    declared_count: usize,
    expanded_constants: Vec<raw::Constant>,

    flags: Option<flags::class::Flags>,

    this_class: Option<ConstantIndex>,
    super_class: Option<ConstantIndex>,
    interfaces: Vec<ConstantIndex>,

    fields: Vec<raw::Field>,
    methods: Vec<raw::Method>,

    attributes: Vec<raw::Attribute>,
}

impl Generator {

    pub fn new() -> Generator {
        Generator {
            minor_major_version: None,

            labels: HashMap::new(),

            declared_constants: Vec::new(),
            declared_count: 0,
            expanded_constants: Vec::new(),

            flags: None,

            this_class: None,
            super_class: None,
            interfaces: Vec::new(),

            fields: Vec::new(),
            methods: Vec::new(),

            attributes: Vec::new(),
        }
    }

    pub fn process_section(&mut self, section: ClassSection) -> Reported<()> {

        let mut reports = Reported::new();

        report_try!(reports; self.expand_labels(&section));

        report_try!(reports; self.expand_this_and_super(section.this_class, section.super_class));

        report_try!(reports; self.expand_declared_constants(section.constants));

        report_try!(reports; self.expand_top_level(section.top_level));

        for field in section.fields {
            let field = report_try!(reports; self.expand_field(field));
            self.fields.push(field);
        }

        for method in section.methods {
            let method = report_try!(reports; self.expand_method(method));
            self.methods.push(method);
        }
        reports.complete(())
    }

    fn get_constant(&self, index: ConstantIndex) -> Option<&raw::Constant> {
        let index = index.0 as usize;
        if index < self.declared_count {
            Some(&self.declared_constants[index - 1])
        } else if index - self.declared_count <= self.expanded_constants.len() {
            Some(&self.expanded_constants[index - self.declared_count])
        } else {
            None
        }
    }

    pub fn file_name(&mut self) -> Option<Vec<String>> {
        if let Some(&raw::Constant::Class(ref class)) = self.get_constant(self.this_class.as_ref().unwrap().clone()) {
            if let Some(&raw::Constant::Utf8(ref str)) = self.get_constant(class.clone()) {
                Some(format!("{}.class", str).split("/").map(Into::into).collect())
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn assemble_class(mut self) -> (Option<Vec<String>>, raw::Class) {
        (
            self.file_name(),
            raw::Class {
                minor_version: self.minor_major_version.unwrap().0,
                major_version: self.minor_major_version.unwrap().1,
                constant_pool: {
                    let mut constants = Vec::new();
                    constants.append(&mut self.declared_constants);
                    constants.append(&mut self.expanded_constants);
                    constants
                },
                flags: self.flags.unwrap(),
                this_class: self.this_class.unwrap(),
                super_class: self.super_class.unwrap(),
                interfaces: self.interfaces,
                fields: self.fields,
                methods: self.methods,
                attributes: self.attributes,
            }
        )
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

        ConstantIndex((self.declared_count + self.expanded_constants.len() - 1) as u16)
    }

    fn push_string_constant(&mut self, str: String) -> ConstantIndex {
        self.push_expanded_constant(raw::Constant::Utf8(str))
    }

    pub fn expand_labels(&mut self, section: &ClassSection) -> Reported<()> {

        let mut reports = Reported::new();

        // FIXME: better way to reserve labels
        self.labels.insert("this".into(), LabelKind::Item);
        self.labels.insert("super".into(), LabelKind::Item);

        if let Some(ref label) = section.label {
            if self.labels.contains_key(&label.name) {
                report_error!(reports; "duplicate label"; label.span);
            } else {
                self.labels.insert(label.name.clone(), LabelKind::Item);
            }
        }

        for meta in section.top_level.iter() {
            if let Some(ref label) = meta.label {
                if self.labels.contains_key(&label.name) {
                    report_error!(reports; "duplicate label"; label.span);
                } else {
                    self.labels.insert(label.name.clone(), LabelKind::Meta);
                }
            }
        }

        let mut index = 0;
        for constant in section.constants.iter() {
            if let Some(ref label) = constant.label {
                if self.labels.contains_key(&label.name) {
                    report_error!(reports; "duplicate label"; label.span);
                } else {
                    self.labels.insert(label.name.clone(), LabelKind::Constant(index + 1));
                }
            }
            index += match constant.body {
                ConstantInstruction::Long(..) |
                ConstantInstruction::Double(..) => 2,
                _ => 1,
            }
        }

        for field in section.fields.iter() {
            if let Some(ref label) = field.label {
                if self.labels.contains_key(&label.name) {
                    report_error!(reports; "duplicate label"; label.span);
                } else {
                    self.labels.insert(label.name.clone(), LabelKind::Item);
                }
            }
            for meta in field.meta.iter() {
                if let Some(ref label) = meta.label {
                    if self.labels.contains_key(&label.name) {
                        report_error!(reports; "duplicate label"; label.span);
                    } else {
                        self.labels.insert(label.name.clone(), LabelKind::Meta);
                    }
                }
            }
        }

        for method in section.methods.iter() {
            if let Some(ref label) = method.label {
                if self.labels.contains_key(&label.name) {
                    report_error!(reports; "duplicate label"; label.span);
                } else {
                    self.labels.insert(label.name.clone(), LabelKind::Item);
                }
            }
            for meta in method.meta.iter() {
                if let Some(ref label) = meta.label {
                    if self.labels.contains_key(&label.name) {
                        report_error!(reports; "duplicate label"; label.span);
                    } else {
                        self.labels.insert(label.name.clone(), LabelKind::Meta);
                    }
                }
            }
            let mut code_index = 0;
            for code in method.code.iter() {
                if let Some(ref label) = code.label {
                    if self.labels.contains_key(&label.name) {
                        report_error!(reports; "duplicate label"; label.span);
                    } else {
                        self.labels.insert(label.name.clone(), LabelKind::Code(code_index as u32)); // FIXME
                    }
                }
                code_index += code.body.len(code_index);
            }
        }

        self.declared_count = index as usize + 1;

        reports.complete(())
    }

    pub fn expand_this_and_super(&mut self, this_class: Expr, super_class: Expr) -> Reported<()> {

        let mut reports = Reported::new();

        let this_index = report_try!(reports; self.eval_expr_into_const_index(this_class.clone(), Some(|this: &mut Generator, s| {
            let str = this.push_expanded_constant(raw::Constant::Utf8(s));
            this.push_expanded_constant(raw::Constant::Class(str))
        })));
        let super_index = report_try!(reports; self.eval_expr_into_const_index(super_class.clone(), Some(|this: &mut Generator, s| {
            let str = this.push_expanded_constant(raw::Constant::Utf8(s));
            this.push_expanded_constant(raw::Constant::Class(str))
        })));
        self.labels.insert("this".into(), LabelKind::Constant(this_index.0 as u32));
        self.labels.insert("super".into(), LabelKind::Constant(super_index.0 as u32));
        self.this_class = Some(this_index);
        self.super_class = Some(super_index);

        reports.complete(())
    }

    pub fn expand_declared_constants(&mut self, constants: Vec<ConstantSection>) -> Reported<()> {

        let mut reports = Reported::new();

        for constant in constants.into_iter() {
            let constant = report_try!(reports; self.expand_constant(constant));
            self.push_declared_constant(constant);
        }

        reports.complete(())
    }

    pub fn expand_constant(&mut self, constant: ConstantSection) -> Reported<raw::Constant> {

        let mut reports = Reported::new();

        fn handle_item_ref(this: &mut Generator, class: Expr, name: Expr, descriptor: Option<Expr>) -> Reported<(ConstantIndex, ConstantIndex)> {
            let mut reports = Reported::new();

            let class_index = report_try!(reports; this.eval_expr_into_const_index(class, Some(|this: &mut Generator, s| {
                    let str = this.push_expanded_constant(raw::Constant::Utf8(s));
                    this.push_expanded_constant(raw::Constant::Class(str))
                })));
            let name_and_type = if let Some(descriptor) = descriptor {
                let name_index = report_try!(reports;
                        this.eval_expr_into_const_index(name, Some(Generator::push_string_constant)));
                let descriptor_index = report_try!(reports;
                        this.eval_expr_into_const_index(descriptor, Some(Generator::push_string_constant)));
                this.push_expanded_constant(raw::Constant::Signature {
                    name: name_index, descriptor: descriptor_index,
                })
            } else {
                report_try!(reports; this.eval_expr_into_const_index(name, self::NO_INDEX_FN))
            };

            reports.complete((class_index, name_and_type))
        }

        let ConstantSection { label, ident, span, body } = constant;

        let constant = match body {
            ConstantInstruction::ClassRef(class) => {
                let index = report_try!(reports;
                    self.eval_expr_into_const_index(class, Some(Generator::push_string_constant)));
                raw::Constant::Class(index)
            },
            ConstantInstruction::FieldRef { class, name, descriptor } => {
                let (class_index, name_and_type) = report_try!(reports; handle_item_ref(self, class, name, descriptor));
                raw::Constant::FieldRef { class: class_index, signature: name_and_type }
            },
            ConstantInstruction::MethodRef { class, name, descriptor } => {
                let (class_index, name_and_type) = report_try!(reports; handle_item_ref(self, class, name, descriptor));
                raw::Constant::MethodRef { class: class_index, signature: name_and_type }
            },
            ConstantInstruction::InterfaceMethodRef { class, name, descriptor } => {
                let (class_index, name_and_type) = report_try!(reports; handle_item_ref(self, class, name, descriptor));
                raw::Constant::InterfaceMethodRef { class: class_index, signature: name_and_type }
            },
            ConstantInstruction::String(str) => {
                let index = report_try!(reports;
                    self.eval_expr_into_const_index(str, Some(Generator::push_string_constant)));
                raw::Constant::String(index)
            },
            ConstantInstruction::Integer(expr) => {
                match report_try!(reports; self.eval_expr(expr)) {
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
            ConstantInstruction::Float(expr) => {
                match report_try!(reports; self.eval_expr(expr)) {
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
            ConstantInstruction::Long(expr) => {
                match report_try!(reports; self.eval_expr(expr)) {
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
            ConstantInstruction::Double(expr) => {
                match report_try!(reports; self.eval_expr(expr)) {
                    Value::Int(_, span) => {
                        fatal_error!(reports; "expected float, found string"; span)
                    },
                    Value::Float(val, _) => {
                        raw::Constant::Double(val)
                    },
                    Value::Str(_, span) => {
                        fatal_error!(reports; "expected float, found string"; span)
                    },
                }
            },
            ConstantInstruction::NameAndType(name, descriptor) => {
                let name_index = report_try!(reports;
                    self.eval_expr_into_const_index(name, Some(Generator::push_string_constant)));
                let descriptor_index = report_try!(reports;
                    self.eval_expr_into_const_index(descriptor, Some(Generator::push_string_constant)));
                raw::Constant::Signature {
                    name: name_index, descriptor: descriptor_index,
                }
            }
            ConstantInstruction::Utf8(expr) => {
                match report_try!(reports; self.eval_expr(expr)) {
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
            ConstantInstruction::MethodHandle { kind, referent } => {
                let kind = match kind {
                    Expr::Ref(ident) => match ident.name.as_str() {
                        special::HANDLE_KIND_GET_FIELD => raw::MethodHandleKind::GetField,
                        special::HANDLE_KIND_GET_STATIC => raw::MethodHandleKind::GetStatic,
                        special::HANDLE_KIND_PUT_FIELD => raw::MethodHandleKind::PutStatic,
                        special::HANDLE_KIND_PUT_STATIC => raw::MethodHandleKind::PutStatic,

                        special::HANDLE_KIND_INVOKE_VIRTUAL => raw::MethodHandleKind::InvokeVirtual,
                        special::HANDLE_KIND_INVOKE_STATIC => raw::MethodHandleKind::InvokeStatic,
                        special::HANDLE_KIND_INVOKE_SPECIAL => raw::MethodHandleKind::InvokeSpecial,
                        special::HANDLE_KIND_NEW_INVOKE_SPECIAL => raw::MethodHandleKind::NewInvokeSpecial,
                        special::HANDLE_KIND_INVOKE_INTERFACE => raw::MethodHandleKind::InvokeInterface,
                        other => fatal_error!(reports; "`{}` is not a method handle kind", other; ident.span)
                    }
                    other => fatal_error!(reports; "expected ident"; other.span())
                };
                let referent_index = report_try!(reports; self.eval_expr_into_const_index(referent, self::NO_INDEX_FN));
                raw::Constant::MethodHandle {
                    kind, referent: referent_index,
                }
            },
            ConstantInstruction::MethodType(expr) => {
                let index = report_try!(reports;
                    self.eval_expr_into_const_index(expr, Some(Generator::push_string_constant)));
                raw::Constant::MethodType(index)
            },
            ConstantInstruction::DynamicTarget { bootstrap_method, name, descriptor } => {

                let index = report_try!(reports; self.eval_expr_into_const_index(bootstrap_method, self::NO_INDEX_FN));
                let name_and_type = if let Some(descriptor) = descriptor {
                    let name_index = report_try!(reports; self.eval_expr_into_const_index(name, Some(Generator::push_string_constant)));
                    let desc_index = report_try!(reports; self.eval_expr_into_const_index(descriptor, Some(Generator::push_string_constant)));
                    self.push_expanded_constant(raw::Constant::Signature { name: name_index, descriptor: desc_index, })
                } else {
                    report_try!(reports; self.eval_expr_into_const_index(name, self::NO_INDEX_FN))
                };

                raw::Constant::InvokeDynamic { bootstrap_method: index, signature: name_and_type }
            },
        };

        reports.complete(constant)
    }

    pub fn expand_top_level(&mut self, metas: Vec<MetaSection>) -> Reported<()> {
        let mut reports = Reported::new();

        let mut bootstrap_methods = Vec::new();

        for meta in metas.into_iter() {
            let MetaSection { label, ident, span, body } = meta;
            match body {
                MetaInstruction::Impl(exprs) => for expr in exprs {
                    let index = report_try!(reports; self.eval_expr_into_const_index(expr, Some(|this: &mut Generator, s| {
                        let str = this.push_expanded_constant(raw::Constant::Utf8(s));
                        this.push_expanded_constant(raw::Constant::Class(str))
                    })));
                    self.interfaces.push(index);
                },
                MetaInstruction::Version { minor, major } => {
                    if self.minor_major_version.is_some() {
                        report_error!(reports; "duplicate `version` instruction"; span);
                    } else {
                        // FIXME: use something reasonable
                        let minor_val = report_try!(reports; self.eval_expr_into_const_index(minor, self::NO_INDEX_FN));
                        let major_val = report_try!(reports; self.eval_expr_into_const_index(major, self::NO_INDEX_FN));
                        self.minor_major_version = Some((minor_val.0, major_val.0));
                    }
                },
                MetaInstruction::Flags(exprs) => {
                    if self.flags.is_some() {
                        report_error!(reports; "duplicate `flags` instruction"; span);
                    } else {
                        let mut flag = flags::class::Flags::new();
                        for expr in exprs {
                            match expr {
                                Expr::Ref(ident) => match ident.name.as_str() {
                                    consts::flags::PUBLIC => flag.set_public(true),
                                    consts::flags::FINAL => flag.set_final(true),
                                    consts::flags::SUPER => flag.set_super(true),
                                    consts::flags::INTERFACE => flag.set_interface(true),
                                    consts::flags::ABSTRACT => flag.set_abstract(true),
                                    consts::flags::SYNTHETIC => flag.set_synthetic(true),
                                    consts::flags::ANNOTATION => flag.set_annotation(true),
                                    consts::flags::ENUM => flag.set_enum(true),
                                    other => {
                                        report_error!(reports; "`{}` is not a class flag", other; ident.span);
                                        &mut flag // FIXME: this is to make this typecheck
                                    },
                                },
                                other => {
                                    report_error!(reports; "expected ident"; other.span());
                                    &mut flag // FIXME: this is to make this typecheck
                                }
                            };
                        }
                        self.flags = Some(flag);
                    }
                },
                MetaInstruction::Stack(..) => {
                    report_error!(reports; "`stack` may only appear after a `method` instruction"; span);
                },
                MetaInstruction::Locals(..) => {
                    report_error!(reports; "`locals` may only appear after a `method` instruction"; span);
                },
                MetaInstruction::Catch {..} => {
                    report_error!(reports; "`catch` may only appear after a `method` instruction"; span);
                },
                MetaInstruction::ConstantValue(..) => {
                    report_error!(reports; "`const_val` may only appear after a `field` instruction"; span);
                },
                MetaInstruction::Source(expr) => {
                    let index = report_try!(reports; self.eval_expr_into_const_index(expr, Some(Generator::push_string_constant)));
                    let name = self.push_string_constant(flags::attribute::ATTR_SOURCE_FILE.into());
                    self.attributes.push(raw::Attribute { name, info: raw::AttributeInfo::SourceFile(index), })
                }
                MetaInstruction::Bootstrap(mut exprs) => {
                    let mut iter = exprs.into_iter();
                    let handle = report_try!(reports; self.eval_expr_into_const_index(iter.next().unwrap(), self::NO_INDEX_FN));
                    let mut args = Vec::new();
                    for expr in iter {
                        args.push(report_try!(reports; self.eval_expr_into_const_index(expr, self::NO_INDEX_FN)));
                    }
                    bootstrap_methods.push(raw::BootstrapMethod { method: handle, arguments: args });
                },
                MetaInstruction::StackMap(..) => {
                    report_error!(reports; "`stack_map` may only appear after a `method` instruction"; span);
                },
                MetaInstruction::Attr(name, data) => {
                    // fixme: care about what "index" points to
                    let name_index = report_try!(reports; self.eval_expr_into_const_index(name, Some(Generator::push_string_constant)));
                    match data {
                        Expr::Str(str) => {
                            let span = str.span;
                            if let Ok(data) = base64::decode(str.value.as_str()) {
                                let attribute = raw::Attribute {
                                    name: name_index,
                                    info: raw::AttributeInfo::Other(data),
                                };
                                self.attributes.push(attribute);
                            } else {
                                report_error!(reports; "expected base64 string"; span);
                            }
                        },
                        _ => report_error!(reports; "expected base64 string"; data.span()),
                    }
                },
            }
        }

        if !bootstrap_methods.is_empty() {
            let name = self.push_string_constant(flags::attribute::ATTR_BOOTSTRAP_METHODS.into());
            self.attributes.push(raw::Attribute { name, info: raw::AttributeInfo::BootstrapMethods(bootstrap_methods), });
        }

        reports.complete(())
    }

    pub fn expand_field(&mut self, field: FieldSection) -> Reported<raw::Field> {

        let mut reports = Reported::new();

        let name = {
            if let Some(label) = field.label {
                if let Some(name) = field.name {
                    fatal_error!(reports; "`field` cannot have both a label and a second argument"; name.span())
                } else {
                    self.push_string_constant(label.name.clone()) // FIXME: get rid of clone
                }
            } else {
                if let Some(name) = field.name {
                    report_try!(reports; self.eval_expr_into_const_index(name, Some(Generator::push_string_constant)))
                } else {
                    fatal_error!(reports; "`field` requires either a label or a second argument"; field.ident.span)
                }
            }
        };

        let descriptor = report_try!(reports; self.eval_expr_into_const_index(field.descriptor, Some(Generator::push_string_constant)));

        let mut flags = None;
        let mut attrs = Vec::new();

        for meta in field.meta.into_iter() {
            let MetaSection { label, ident, span, body } = meta;
            match body {
                MetaInstruction::Impl(..) => {
                    report_error!(reports; "`impl` may only appear after a `class` instruction"; span);
                },
                MetaInstruction::Version { .. } => {
                    report_error!(reports; "`version` may only appear after a `class` instruction"; span);
                },
                MetaInstruction::Flags(exprs) => {
                    if flags.is_some() {
                        report_error!(reports; "duplicate `flags` instruction"; span);
                    } else {
                        let mut flag = flags::field::Flags::new();
                        for expr in exprs {
                            match expr {
                                Expr::Ref(ident) => match ident.name.as_str() {
                                    consts::flags::PUBLIC => flag.set_public(true),
                                    consts::flags::PRIVATE => flag.set_private(true),
                                    consts::flags::PROTECTED => flag.set_protected(true),
                                    consts::flags::STATIC => flag.set_static(true),
                                    consts::flags::FINAL => flag.set_final(true),
                                    consts::flags::VOLATILE => flag.set_volatile(true),
                                    consts::flags::TRANSIENT => flag.set_transient(true),
                                    consts::flags::SYNTHETIC => flag.set_synthetic(true),
                                    consts::flags::ENUM => flag.set_enum(true),
                                    other => {
                                        report_error!(reports; "`{}` is not a field flag", other; ident.span);
                                        &mut flag // FIXME: this is to make this typecheck
                                    },
                                },
                                other => {
                                    report_error!(reports; "expected ident"; other.span());
                                    &mut flag // FIXME: this is to make this typecheck
                                }
                            };
                        }
                        flags = Some(flag);
                    }
                },
                MetaInstruction::Stack(..) => {
                    report_error!(reports; "`stack` may only appear after a `method` instruction"; span);
                },
                MetaInstruction::Locals(..) => {
                    report_error!(reports; "`locals` may only appear after a `method` instruction"; span);
                },
                MetaInstruction::Catch {..} => {
                    report_error!(reports; "`catch` may only appear after a `method` instruction"; span);
                },
                MetaInstruction::ConstantValue(expr) => {
                    let index = report_try!(reports; self.eval_expr_into_const_index(expr, self::NO_INDEX_FN));
                    let name = self.push_string_constant(flags::attribute::ATTR_CONSTANT_VALUE.into());
                    attrs.push(raw::Attribute { name, info: raw::AttributeInfo::ConstantValue(index)})
                },
                MetaInstruction::Source(..) => {
                    report_error!(reports; "`source` may only appear after a `class` instruction"; span);
                },
                MetaInstruction::Bootstrap(..) => {
                    report_error!(reports; "`bootstrap` may only appear after a `class` instruction"; span);
                },
                MetaInstruction::StackMap(..) => {
                    report_error!(reports; "`stack_map` may only appear after a `method` instruction"; span);
                },
                MetaInstruction::Attr(name, data) => {
                    // fixme: care about what "index" points to
                    let name_index = report_try!(reports; self.eval_expr_into_const_index(name, Some(Generator::push_string_constant)));
                    match data {
                        Expr::Str(str) => {
                            let span = str.span;
                            if let Ok(data) = base64::decode(str.value.as_str()) { // FIXME: clone
                                let attribute = raw::Attribute {
                                    name: name_index,
                                    info: raw::AttributeInfo::Other(data),
                                };
                                self.attributes.push(attribute);
                            } else {
                                report_error!(reports; "expected base64 string"; span);
                            }
                        },
                        _ => report_error!(reports; "expected base64 string"; data.span()),
                    }
                },
            }
        }

        let flags = if let Some(flags) = flags {
            flags
        } else {
            flags::field::Flags::new()
        };

        reports.complete(raw::Field {
            flags, name, descriptor, attributes: attrs,
        })
    }

    fn expand_method(&mut self, method: MethodSection) -> Reported<raw::Method> {

        let mut reports = Reported::new();

        let name = {
            if let Some(label) = method.label {
                if let Some(name) = method.name {
                    fatal_error!(reports; "`method` cannot have both a label and a second argument"; name.span())
                } else {
                    self.push_string_constant(label.name.clone()) // FIXME: get rid of clone
                }
            } else {
                if let Some(name) = method.name {
                    report_try!(reports; self.eval_expr_into_const_index(name, Some(Generator::push_string_constant)))
                } else {
                    fatal_error!(reports; "`method` requires either a label or a second argument"; method.ident.span)
                }
            }
        };

        let descriptor = report_try!(reports; self.eval_expr_into_const_index(method.descriptor, Some(Generator::push_string_constant)));

        let mut flags = None;
        let mut attrs = Vec::new();

        let mut max_stack = None;
        let mut max_locals = None;
        let mut exceptions = Vec::new();

        let mut stack_map_table = Vec::new();

        for meta in method.meta.into_iter() {
            let MetaSection { label, ident, span, body } = meta;
            match body {
                MetaInstruction::Impl(..) => {
                    report_error!(reports; "`impl` may only appear after a `class` instruction"; span);
                },
                MetaInstruction::Version { .. } => {
                    report_error!(reports; "`version` may only appear after a `class` instruction"; span);
                },
                MetaInstruction::Flags(exprs) => {
                    if flags.is_some() {
                        report_error!(reports; "duplicate `flags` instruction"; span);
                    } else {
                        let mut flag = flags::method::Flags::new();
                        for expr in exprs {
                            match expr {
                                Expr::Ref(ident) => match ident.name.as_str() {
                                    consts::flags::PUBLIC => flag.set_public(true),
                                    consts::flags::PRIVATE => flag.set_private(true),
                                    consts::flags::PROTECTED => flag.set_protected(true),
                                    consts::flags::STATIC => flag.set_static(true),
                                    consts::flags::FINAL => flag.set_final(true),
                                    consts::flags::SYNCHRONIZED => flag.set_synchronized(true),
                                    consts::flags::BRIDGE => flag.set_bridge(true),
                                    consts::flags::VARARGS => flag.set_varargs(true),
                                    consts::flags::NATIVE => flag.set_native(true),
                                    consts::flags::ABSTRACT => flag.set_abstract(true),
                                    consts::flags::STRICT => flag.set_strict(true),
                                    consts::flags::SYNTHETIC => flag.set_synthetic(true),
                                    other => {
                                        report_error!(reports; "`{}` is not a method flag", other; ident.span);
                                        &mut flag // FIXME: this is to make this typecheck
                                    },
                                },
                                other => {
                                    report_error!(reports; "expected ident"; other.span());
                                    &mut flag // FIXME: this is to make this typecheck
                                }
                            };
                        }
                        flags = Some(flag);
                    }
                },
                MetaInstruction::Stack(expr) => {
                    if max_stack.is_some() {
                        report_error!(reports; "duplicate `stack` instruction"; span);
                    } else {
                        let val = report_try!(reports; self.eval_expr_into_const_index(expr, self::NO_INDEX_FN)).0; // FIXME: less horrible
                        max_stack = Some(val);
                    }
                },
                MetaInstruction::Locals(expr) => {
                    if max_locals.is_some() {
                        report_error!(reports; "duplicate `locals` instruction"; span);
                    } else {
                        let val = report_try!(reports; self.eval_expr_into_const_index(expr, self::NO_INDEX_FN)).0; // FIXME: less horrible
                        max_locals = Some(val);
                    }
                },
                MetaInstruction::Catch { start, end, handler, ty } => {
                    // FIXME: omg this is all so horrible and sloppy
                    let start_index = report_try!(reports; self.eval_expr_into_const_index(start, self::NO_INDEX_FN)).0; // FIXME: less horrible
                    let end_index = report_try!(reports; self.eval_expr_into_const_index(end, self::NO_INDEX_FN)).0; // FIXME: less horrible
                    let handler_index = report_try!(reports; self.eval_expr_into_const_index(handler, self::NO_INDEX_FN)).0; // FIXME: less horrible

                    let ty_index = report_try!(reports; self.eval_expr_into_const_index(ty, Some(|this: &mut Generator, s| {
                        let str = this.push_expanded_constant(raw::Constant::Utf8(s));
                        this.push_expanded_constant(raw::Constant::Class(str))
                    })));

                    exceptions.push(raw::ExceptionTableEntry {
                        try_range: (CodeIndex(start_index), CodeIndex(end_index)),
                        handler_goto: CodeIndex(handler_index),
                        exception_type: ty_index,
                    });
                },
                MetaInstruction::ConstantValue(..) => {
                    report_error!(reports; "`const_val` may only appear after a `class` instruction"; span);
                },
                MetaInstruction::Source(..) => {
                    report_error!(reports; "`source` may only appear after a `class` instruction"; span);
                },
                MetaInstruction::Bootstrap(..) => {
                    report_error!(reports; "`bootstrap` may only appear after a `class` instruction"; span);
                },
                MetaInstruction::StackMap(exprs) => {
                    let mut iter = exprs.into_iter();
                    let frame_type = iter.next().unwrap();
                    let offset = CodeIndex(report_try!(reports; self.eval_expr_into_const_index(iter.next().unwrap(), self::NO_INDEX_FN)).0);
                    if let Expr::Ref(ident) = frame_type {
                        fn parse_vty<I: Iterator<Item = Expr>, T>(this: &mut Generator, iter: &mut I, reports: &mut Reported<T>, span: &Span) -> Option<raw::VerificationType> {
                            if let Some(Expr::Ref(ident)) = iter.next() {
                                match ident.name.as_str() {
                                    special::VTYPE_TOP => Some(raw::VerificationType::Top),
                                    special::VTYPE_INT => Some(raw::VerificationType::Int),
                                    special::VTYPE_FLOAT => Some(raw::VerificationType::Float),
                                    special::VTYPE_LONG => Some(raw::VerificationType::Long),
                                    special::VTYPE_DOUBLE => Some(raw::VerificationType::Double),
                                    special::VTYPE_NULL => Some(raw::VerificationType::Null),
                                    special::VTYPE_UNINIT_THIS => Some(raw::VerificationType::UninitializedThis),
                                    special::VTYPE_OBJ => {
                                        if let Some(next) = iter.next() {
                                            let ty = reports.merge(this.eval_expr_into_const_index(next, Some(|this: &mut Generator, s| {
                                                let str = this.push_expanded_constant(raw::Constant::Utf8(s));
                                                this.push_expanded_constant(raw::Constant::Class(str))
                                            })))?;
                                            Some(raw::VerificationType::Object(ty))
                                        } else {
                                            report_error!(reports; "expected class ref"; ident);
                                            None
                                        }
                                    }
                                    special::VTYPE_UNINIT => {
                                        if let Some(next) = iter.next() {
                                            let index = CodeIndex(reports.merge(this.eval_expr_into_const_index(next, self::NO_INDEX_FN))?.0);
                                            Some(raw::VerificationType::Uninitialized(index))
                                        } else {
                                            report_error!(reports; "expected class ref"; ident);
                                            None
                                        }
                                    }
                                    other => {
                                        report_error!(reports; "expected verification type, found `{}`", ident.name; ident);
                                        None
                                    }
                                }
                            } else {
                                report_error!(reports; "expected identifier"; span);
                                None
                            }
                        }
                        // FIXME: everywhere here, use something other than eval_exper_into_const_index
                        match ident.name.as_str() {
                            special::STACK_MAP_SAME => {
                                if offset.0 > flags::attribute::STACK_MAP_SAME_MAX as u16 {
                                    report_error!(reports; "expected integer at most {}, found {}",
                                        flags::attribute::STACK_MAP_SAME_MAX, offset.0 ; span)
                                }

                                stack_map_table.push(raw::StackMapFrame::Same(offset));

                                for extra in iter {
                                    report_error!(reports; "unexpected argument"; extra);
                                }
                            },
                            special::STACK_MAP_SAME_EXT => {
                                stack_map_table.push(raw::StackMapFrame::SameExt(offset));

                                for extra in iter {
                                    report_error!(reports; "unexpected argument"; extra);
                                }
                            },
                            special::STACK_MAP_SINGLE_STACK => {
                                if offset.0 > (flags::attribute::STACK_MAP_SAME_MAX - flags::attribute::STACK_MAP_SAME_MIN) as u16 {
                                    report_error!(reports; "expected integer at most {}, found {}",
                                        flags::attribute::STACK_MAP_SAME_MAX - flags::attribute::STACK_MAP_SAME_MIN, offset.0 ; span)
                                }
                                if let Some(ty) = parse_vty(self, &mut iter, &mut reports, &ident.span) {
                                    stack_map_table.push(raw::StackMapFrame::SingleStack(offset, ty));

                                    for extra in iter {
                                        report_error!(reports; "unexpected argument"; extra);
                                    }
                                }
                            },
                            special::STACK_MAP_SINGLE_STACK_EXT => {
                                if let Some(ty) = parse_vty(self, &mut iter, &mut reports, &ident.span) {
                                    stack_map_table.push(raw::StackMapFrame::SingleStackExt(offset, ty));

                                    for extra in iter {
                                        report_error!(reports; "unexpected argument"; extra);
                                    }
                                }
                            },
                            special::STACK_MAP_CHOP_1 => {
                                stack_map_table.push(raw::StackMapFrame::Chop1(offset));

                                for extra in iter {
                                    report_error!(reports; "unexpected argument"; extra);
                                }
                            },
                            special::STACK_MAP_CHOP_2 => {
                                stack_map_table.push(raw::StackMapFrame::Chop2(offset));

                                for extra in iter {
                                    report_error!(reports; "unexpected argument"; extra);
                                }
                            },
                            special::STACK_MAP_CHOP_3 => {
                                stack_map_table.push(raw::StackMapFrame::Chop3(offset));

                                for extra in iter {
                                    report_error!(reports; "unexpected argument"; extra);
                                }
                            },
                            special::STACK_MAP_APPEND_1 => {
                                let arg1 = parse_vty(self, &mut iter, &mut reports, &ident.span);
                                if arg1.is_some() {
                                    stack_map_table.push(raw::StackMapFrame::Append1(offset, arg1.unwrap()));

                                    for extra in iter {
                                        report_error!(reports; "unexpected argument"; extra);
                                    }
                                }
                            },
                            special::STACK_MAP_APPEND_2 => {
                                let arg1 = parse_vty(self, &mut iter, &mut reports, &ident.span);
                                let arg2 = parse_vty(self, &mut iter, &mut reports, &ident.span);
                                if arg1.is_some() && arg2.is_some() {
                                    stack_map_table.push(raw::StackMapFrame::Append2(offset, arg1.unwrap(), arg2.unwrap()));

                                    for extra in iter {
                                        report_error!(reports; "unexpected argument"; extra);
                                    }
                                }
                            },
                            special::STACK_MAP_APPEND_3 => {
                                let arg1 = parse_vty(self, &mut iter, &mut reports, &ident.span);
                                let arg2 = parse_vty(self, &mut iter, &mut reports, &ident.span);
                                let arg3 = parse_vty(self, &mut iter, &mut reports, &ident.span);
                                if arg1.is_some() && arg2.is_some() && arg3.is_some() {
                                    stack_map_table.push(raw::StackMapFrame::Append3(offset, arg1.unwrap(), arg2.unwrap(), arg3.unwrap()));

                                    for extra in iter {
                                        report_error!(reports; "unexpected argument"; extra);
                                    }
                                }
                            },
                            special::STACK_MAP_FULL => {
                                // everything about this is horrible.
                                // but this attribute is stupid anyways so I don't care
                                let mut stack = Vec::new();
                                let mut locals = Vec::new();
                                #[derive(PartialEq, Eq)]
                                enum Mode { Stack, Locals };
                                let mut mode = None;
                                let mut iter = iter.peekable();
                                while let Some(expr) = { iter.peek().cloned() } { // FIXME: get rid of this clone
                                    match mode {
                                        None => {
                                            match expr {
                                                Expr::Ref(ref ident) => match ident.name.as_str() {
                                                    // FIXME: other instructons maybe?
                                                    consts::instructions::STACK => mode = {
                                                        let _ = iter.next();
                                                        Some(Mode::Stack)
                                                    },
                                                    consts::instructions::LOCALS => mode = {
                                                        let _ = iter.next();
                                                        Some(Mode::Locals)
                                                    },
                                                    ref other => report_error!(reports; "expected `stack` or `locals`, found {}", other; ident),
                                                }
                                                ref other => report_error!(reports; "expected identifier"; ident),
                                            }
                                        },
                                        Some(Mode::Stack) => {
                                            match expr {
                                                Expr::Ref(ref ident) if ident.name == consts::instructions::STACK => {
                                                    let _ = iter.next();
                                                    mode = Some(Mode::Stack)
                                                },
                                                Expr::Ref(ref ident) if ident.name == consts::instructions::LOCALS => {
                                                    let _ = iter.next();
                                                    mode = Some(Mode::Stack)
                                                },
                                                ref other => {
                                                    if let Some(ty) = parse_vty(self, &mut iter, &mut reports, &ident.span) {
                                                        stack.push(ty);
                                                    }
                                                }
                                            }
                                        },
                                        Some(Mode::Locals) => {
                                            match expr {
                                                Expr::Ref(ref ident) if ident.name == consts::instructions::STACK => {
                                                    let _ = iter.next();
                                                    mode = Some(Mode::Stack)
                                                },
                                                Expr::Ref(ref ident) if ident.name == consts::instructions::LOCALS => {
                                                    let _ = iter.next();
                                                    mode = Some(Mode::Stack)
                                                },
                                                ref other => {
                                                    if let Some(ty) = parse_vty(self, &mut iter, &mut reports, &ident.span) {
                                                        locals.push(ty);
                                                    }
                                                }
                                            }
                                        },
                                    }
                                }
                                stack_map_table.push(raw::StackMapFrame::Full {
                                    offset, stack, locals,
                                })
                            }
                            other => report_error!(reports; "expected `stack_map_frame` type, found `{}`", other; ident)
                        }
                    } else {
                        report_error!(reports; "expected identifier"; frame_type);
                    }
                }
                MetaInstruction::Attr(name, data) => {
                    // fixme: care about what "index" points to
                    let name_index = report_try!(reports; self.eval_expr_into_const_index(name, Some(Generator::push_string_constant)));
                    match data {
                        Expr::Str(str) => {
                            let span = str.span;
                            if let Ok(data) = base64::decode(str.value.as_str()) { // FIXME: clone
                                let attribute = raw::Attribute {
                                    name: name_index,
                                    info: raw::AttributeInfo::Other(data),
                                };
                                self.attributes.push(attribute);
                            } else {
                                report_error!(reports; "expected base64 string"; span);
                            }
                        },
                        _ => report_error!(reports; "expected base64 string"; data.span()),
                    }
                },
            }
        }

        let mut code_len = 0;

        let mut code_array = Vec::new();

        // this part is the very worst. it's so horrible, I wrote a macro
        // to make it less painful.
        macro_rules! code_match {
            ($arg:expr;
                [no_args]
                    $($no_args:ident),* $(,)*
                [any_const]
                    $($any_const:ident),* $(,)*
                [class_const]
                    $($class_const:ident),* $(,)*
                [var]
                    $($var:ident),* $(,)*
                [jump]
                    $($jump:ident),* $(,)*
                [other]
                    $($tokens:tt)*
            ) => {
                match $arg {
                    $(
                        CodeInstruction::$no_args => raw::Instruction::$no_args,
                    )*
                    $(
                        CodeInstruction::$any_const(expr) => {
                            let index = report_try!(reports;
                                self.eval_expr_into_const_index(expr, self::NO_INDEX_FN));
                            raw::Instruction::$any_const(index)
                        },
                    )*
                    $(
                        CodeInstruction::$class_const(expr) => {
                            let class_index = report_try!(reports; self.eval_expr_into_const_index(expr, Some(|this: &mut Generator, s| {
                                let str = this.push_expanded_constant(raw::Constant::Utf8(s));
                                this.push_expanded_constant(raw::Constant::Class(str))
                            })));
                            raw::Instruction::$class_const(class_index)
                        },
                    )*
                    $(
                        CodeInstruction::$var(expr) => {
                            let index = VarIndex(report_try!(reports; self.eval_expr_into_int(expr, "u8", self::into_u8)));
                            raw::Instruction::$var(index)
                        },
                    )*
                    $(
                        CodeInstruction::$jump(expr) => {
                            let index = CodeOffset(report_try!(reports; self.eval_expr_into_int(expr, "u16", self::into_u16)) as i16 - code_len as i16);
                            raw::Instruction::$jump(index)
                        },
                    )*
                    $($tokens)*
                }
            };
        }

        for code in method.code {
            let CodeSection { label, ident, span, body, } = code;
            let code_index = body.len(code_len);
            code_array.push(code_match!(body;
                [no_args]

                    Nop,

                    ConstRefNull,
                    ConstIntM1,
                    ConstInt0,
                    ConstInt1,
                    ConstInt2,
                    ConstInt3,
                    ConstInt4,
                    ConstInt5,
                    ConstLong0,
                    ConstLong1,
                    ConstFloat0,
                    ConstFloat1,
                    ConstFloat2,
                    ConstDouble0,
                    ConstDouble1,

                    LoadInt0,
                    LoadInt1,
                    LoadInt2,
                    LoadInt3,

                    LoadLong0,
                    LoadLong1,
                    LoadLong2,
                    LoadLong3,

                    LoadFloat0,
                    LoadFloat1,
                    LoadFloat2,
                    LoadFloat3,

                    LoadDouble0,
                    LoadDouble1,
                    LoadDouble2,
                    LoadDouble3,

                    LoadRef0,
                    LoadRef1,
                    LoadRef2,
                    LoadRef3,

                    ArrayLoadInt,
                    ArrayLoadLong,
                    ArrayLoadFloat,
                    ArrayLoadDouble,
                    ArrayLoadRef,
                    ArrayLoadByte,
                    ArrayLoadChar,
                    ArrayLoadShort,

                    StoreInt0,
                    StoreInt1,
                    StoreInt2,
                    StoreInt3,

                    StoreLong0,
                    StoreLong1,
                    StoreLong2,
                    StoreLong3,

                    StoreFloat0,
                    StoreFloat1,
                    StoreFloat2,
                    StoreFloat3,

                    StoreDouble0,
                    StoreDouble1,
                    StoreDouble2,
                    StoreDouble3,

                    StoreRef0,
                    StoreRef1,
                    StoreRef2,
                    StoreRef3,

                    ArrayStoreInt,
                    ArrayStoreLong,
                    ArrayStoreFloat,
                    ArrayStoreDouble,
                    ArrayStoreRef,
                    ArrayStoreByte,
                    ArrayStoreChar,
                    ArrayStoreShort,


                    Pop,
                    DoublePop,

                    Dup,
                    DupDown,
                    DupDoubleDown,
                    DoubleDup,
                    DoubleDupDown,
                    DoubleDupDoubleDown,

                    Swap,


                    AddInt,
                    AddLong,
                    AddFloat,
                    AddDouble,

                    SubInt,
                    SubLong,
                    SubFloat,
                    SubDouble,

                    MulInt,
                    MulLong,
                    MulFloat,
                    MulDouble,

                    DivInt,
                    DivLong,
                    DivFloat,
                    DivDouble,

                    RemInt,
                    RemLong,
                    RemFloat,
                    RemDouble,

                    NegInt,
                    NegLong,
                    NegFloat,
                    NegDouble,

                    LeftShiftInt,
                    LeftShiftLong,
                    RightShiftInt,
                    RightShiftLong,
                    URightShiftInt,
                    URightShiftLong,

                    AndInt,
                    AndLong,

                    OrInt,
                    OrLong,

                    XorInt,
                    XorLong,

                    IntToLong,
                    IntToFloat,
                    IntToDouble,
                    LongToInt,
                    LongToFloat,
                    LongToDouble,
                    FloatToInt,
                    FloatToLong,
                    FloatToDouble,
                    DoubleToInt,
                    DoubleToLong,
                    DoubleToFloat,

                    IntToByte,
                    IntToChar,
                    IntToShort,


                    CompareLong,
                    CompareFloatL,
                    CompareFloatG,
                    CompareDoubleL,
                    CompareDoubleG,

                    ReturnInt,
                    ReturnLong,
                    ReturnFloat,
                    ReturnDouble,
                    ReturnRef,
                    ReturnVoid,

                    ArrayLen,

                    Throw,

                    EnterMonitor,
                    ExitMonitor,

                    Breakpoint,
                    ImplementationDefined1,
                    ImplementationDefined2,

                [any_const]

                    GetStaticField,
                    PutStaticField,
                    GetField,
                    PutField,

                    InvokeDynamic,
                    InvokeSpecial,
                    InvokeStatic,
                    InvokeVirtual,

                [class_const]

                    New,
                    NewRefArray,
                    CheckCast,
                    InstanceOf,

                [var]

                    LoadInt,
                    LoadLong,
                    LoadFloat,
                    LoadDouble,
                    LoadRef,

                    StoreInt,
                    StoreLong,
                    StoreFloat,
                    StoreDouble,
                    StoreRef,

                    RetSub,

                [jump]

                    IfIntEq0,
                    IfIntNe0,
                    IfIntLt0,
                    IfIntGe0,
                    IfIntGt0,
                    IfIntLe0,

                    IfIntEq,
                    IfIntNe,
                    IfIntLt,
                    IfIntGe,
                    IfIntGt,
                    IfIntLe,

                    IfRefEq,
                    IfRefNe,

                    Goto,
                    JumpSub,

                    IfRefNull,
                    IfRefNonNull,

                [other]
                    CodeInstruction::PushByte(expr) => {
                        let byte = report_try!(reports; self.eval_expr_into_int(expr, "i8", self::into_i8));
                        raw::Instruction::PushByte(byte)
                    },
                    CodeInstruction::PushShort(expr) => {
                        let short = report_try!(reports; self.eval_expr_into_int(expr, "i16", self::into_i16));
                        raw::Instruction::PushShort(short)
                    },

                    CodeInstruction::LoadConstant(expr) => {
                        let index = report_try!(reports; self.eval_expr_into_const_index(expr, Some(Generator::push_string_constant)));
                        let half = HalfConstantIndex(index.0 as u8); // FIXME
                        raw::Instruction::LoadConstant(half)
                    },
                    CodeInstruction::WideLoadConstant(expr) => {
                        let index = report_try!(reports; self.eval_expr_into_const_index(expr, Some(Generator::push_string_constant)));
                        raw::Instruction::WideLoadConstant(index)
                    },
                    CodeInstruction::WideLoadWideConstant(expr) => {
                        let index = report_try!(reports; self.eval_expr_into_const_index(expr, Some(Generator::push_string_constant)));
                        raw::Instruction::WideLoadWideConstant(index)
                    },

                    CodeInstruction::IncInt(var, val) => {
                        let var = VarIndex(report_try!(reports; self.eval_expr_into_int(var, "u8", self::into_u8)));
                        let val = report_try!(reports; self.eval_expr_into_int(val, "i8", self::into_i8));
                        raw::Instruction::IncInt(var, val)
                    },

                    CodeInstruction::LookupSwitch {
                        default_offset, match_table,
                    } => {
                        let default_index = WideCodeOffset(report_try!(reports; self.eval_expr_into_int(default_offset, "u32", self::into_u32)) as i32 - code_len as i32);
                        let mut match_table_indices = Vec::new();
                        for (mtch, offset) in match_table {
                            let match_index = report_try!(reports; self.eval_expr_into_int(mtch, "i32", self::into_i32));
                            let offset_index = WideCodeOffset(report_try!(reports; self.eval_expr_into_int(offset, "u32", self::into_u32)) as i32 - code_len as i32);
                            match_table_indices.push((match_index, offset_index));
                        }
                        raw::Instruction::LookupSwitch {
                            default_offset: default_index,
                            match_table: match_table_indices,
                        }
                    },

                    CodeInstruction::TableSwitch {
                        default_offset, match_range, offset_table,
                    } => {
                        let default_index = WideCodeOffset(report_try!(reports; self.eval_expr_into_int(default_offset, "u32", self::into_u32)) as i32 - code_len as i32);
                        let match_start = report_try!(reports; self.eval_expr_into_int(match_range.0, "i32", self::into_i32));
                        let match_end = report_try!(reports; self.eval_expr_into_int(match_range.1, "i32", self::into_i32));
                        let mut offset_indices = Vec::new();
                        for offset in offset_table {
                            let offset_index = WideCodeOffset(report_try!(reports; self.eval_expr_into_int(offset, "u32", self::into_u32)) as i32 - code_len as i32);
                            offset_indices.push(offset_index);
                        }
                        raw::Instruction::TableSwitch {
                            default_offset: default_index,
                            match_range: (match_start, match_end),
                            offset_table: offset_indices,
                        }
                    },

                    CodeInstruction::InvokeInterface(expr, count) => {
                        let index = report_try!(reports; self.eval_expr_into_const_index(expr, self::NO_INDEX_FN));
                        let val = report_try!(reports; self.eval_expr_into_int(count, "u8", self::into_u8));
                        raw::Instruction::InvokeInterface(index, val)
                    },

                    CodeInstruction::NewPrimitiveArray(expr) => {
                        let ty = match expr {
                            Expr::Ref(ident) => {
                                match ident.name.as_str() {
                                    special::ARR_BOOLEAN => raw::ArrayPrimitive::Boolean,
                                    special::ARR_CHAR => raw::ArrayPrimitive::Char,
                                    special::ARR_FLOAT => raw::ArrayPrimitive::Float,
                                    special::ARR_DOUBLE => raw::ArrayPrimitive::Double,
                                    special::ARR_BYTE => raw::ArrayPrimitive::Byte,
                                    special::ARR_SHORT => raw::ArrayPrimitive::Short,
                                    special::ARR_INT => raw::ArrayPrimitive::Int,
                                    special::ARR_LONG => raw::ArrayPrimitive::Long,
                                    other => {
                                        report_error!(reports; "expected primitive type, found `{}`", other; ident.span);
                                        continue
                                    }
                                }
                            }
                            _ => {
                                report_error!(reports; "expected identifier"; expr.span());
                                continue
                            }
                        };
                        raw::Instruction::NewPrimitiveArray(ty)
                    }

                    CodeInstruction::Wide(..) => unimplemented!(), // FIXME: deal with this...

                    CodeInstruction::NewRefMultiArray(class, dims) => {
                        let class_index = report_try!(reports; self.eval_expr_into_const_index(class, Some(|this: &mut Generator, s| {
                            let str = this.push_expanded_constant(raw::Constant::Utf8(s));
                            this.push_expanded_constant(raw::Constant::Class(str))
                        })));
                        let dims_val = report_try!(reports; self.eval_expr_into_int(dims, "u8", self::into_u8));
                        raw::Instruction::NewRefMultiArray(class_index, dims_val)
                    },

                    CodeInstruction::WideGoto(expr) => {
                        let index = WideCodeOffset(report_try!(reports; self.eval_expr_into_int(expr, "u32", self::into_u32)) as i32 - code_len as i32);
                        raw::Instruction::WideGoto(index)
                    },
                    CodeInstruction::WideJumpSub(expr) => {
                        let index = WideCodeOffset(report_try!(reports; self.eval_expr_into_int(expr, "u32", self::into_u32)) as i32 - code_len as i32);
                        raw::Instruction::WideJumpSub(index)
                    },


                    CodeInstruction::Breakpoint => raw::Instruction::Breakpoint,
                    CodeInstruction::ImplementationDefined1 => raw::Instruction::ImplementationDefined1,
                    CodeInstruction::ImplementationDefined2 => raw::Instruction::ImplementationDefined2,
            ));
            code_len += code_index;
        }

        if max_stack.is_some() ||
            max_locals.is_some() ||
            !exceptions.is_empty() ||
            !code_array.is_empty() {

            let name = self.push_string_constant(flags::attribute::ATTR_CODE.into());

            attrs.push(raw::Attribute {
                name, info: raw::AttributeInfo::Code {
                    max_stack: CodeIndex(max_stack.unwrap_or(0)),
                    max_locals: CodeIndex(max_locals.unwrap_or(0)),
                    code_len: code_len as u32, // FIXME: sketchy cast
                    code: code_array,
                    exception_table: exceptions,
                    attributes: Vec::new(),
                }
            })
        }

        let flags = if let Some(flags) = flags {
            flags
        } else {
            flags::method::Flags::new()
        };

        if !stack_map_table.is_empty() {
            let name_index = self.push_string_constant(flags::attribute::ATTR_STACK_MAP_TABLE.into());
            self.attributes.push(raw::Attribute { name: name_index, info: raw::AttributeInfo::StackMapTable(stack_map_table), })
        }

        reports.complete(raw::Method {
            flags, name, descriptor, attributes: attrs,
        })
    }

    fn eval_expr(&mut self, expr: Expr) -> Reported<Value> {

        let mut reports = Reported::new();

        let val = match expr {
            Expr::Ref(ident) => {
                if let Some(index) = self.labels.get(&ident.name) {
                    match *index {
                        LabelKind::Constant(index) => Value::Int(index as i64, ident.span),
                        LabelKind::Item => fatal_error!(reports; "label points to item instruction; expected constant"; ident.span),
                        LabelKind::Meta => fatal_error!(reports; "label points to meta instruction; expected constant"; ident.span),
                        LabelKind::Code(index) => Value::Int(index as i64, ident.span),
                    }

                } else {
                    fatal_error!(reports; "could not find label `{}`", ident.name; ident.span)
                }
            },
            Expr::Int(int) => Value::Int(int.value, int.span),
            Expr::Float(float) => Value::Float(float.value, float.span),
            Expr::Str(str) => Value::Str(str.value.clone() /* ugh */, str.span),
            Expr::Char(char) => Value::Int(char.value as i64, char.span),
            Expr::Bracket(instruction) => {
                let Instruction { label, ident, body, span } = { *instruction };
                if let InstructionBody::Constant(i) = body {
                    let constant = report_try!(reports; self.expand_constant(ConstantSection {
                        label: None, ident, span: span.clone(), body: i,
                    }));
                    Value::Int(self.push_expanded_constant(constant).0 as i64, span)
                } else {
                    fatal_error!(reports; "expected constant instruction"; span)
                }
            },
            Expr::BinOp(first, op, second) => {
                let span = first.span().extend_to(&second);
                let first = report_try!(reports; self.eval_expr(*first));
                let second = report_try!(reports; self.eval_expr(*second));

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

    fn eval_expr_into_const_index<'a, F>(&'a mut self, expr: Expr, str_to_const: Option<F>) -> Reported<ConstantIndex>
        where F: FnOnce(&'a mut Self, String) -> ConstantIndex {
        let mut reports = Reported::new();

        #[inline]
        fn parse_constant_index(val: i64, span: Span) -> Reported<ConstantIndex> {
            let mut reports = Reported::new();
            if val >= 0 && val <= (u16::MAX as i64) {
                reports.complete(ConstantIndex(val as u16))
            } else {
                fatal_error!(reports; "expected u16, found wider integer: {}", val; span)
            }
        }

        let val = match report_try!(reports; self.eval_expr(expr)) {
            Value::Int(val, span) => {
                report_try!(reports; parse_constant_index(val, span))
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

    fn eval_expr_into_int<'a, F, I>(&'a mut self, expr: Expr, expected: &str, convert: F) -> Reported<I>
        where F: FnOnce(i64) -> Option<I> {
        let mut reports = Reported::new();

        match report_try!(reports; self.eval_expr(expr)) {
            Value::Int(val, span) => {
                if let Some(x) = convert(val) {
                    reports.complete(x)
                } else {
                    fatal_error!(reports; "expected {}, found wider integer", expected; span)
                }
            },
            Value::Float(_, span) => {
                fatal_error!(reports; "expected {}, found float", expected; span)
            },
            Value::Str(_, span) => {
                fatal_error!(reports; "expected {}, found string", expected; span)
            }
        }
    }
}

impl Phase for Generator {

    type Input = ClassSection;
    type Output = (Option<Vec<String>>, raw::Class);

    fn run(input: Vec<Self::Input>) -> Reported<Vec<Self::Output>> {
        let mut reports = Reported::new();
        let mut out = Vec::new();

        for class in input {
            let mut gen = Generator::new();
            if let Some(..) = reports.merge(gen.process_section(class)) {
                out.push(gen.assemble_class());
            }
        }

        reports.complete(out)
    }
}

#[derive(Clone, Copy, Debug)]
enum LabelKind {
    Constant(u32),
    Item,
    Meta,
    Code(u32),
}

#[derive(Clone, Debug)]
pub enum Value {
    Int(i64, Span),
    Float(f64, Span),
    Str(String, Span),
}



