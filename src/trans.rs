use ast::*;
use consts;
use consts::special;
use reporting::*;

use classfile::raw;
use classfile::indexing::*;
use classfile::consts as flags;

use base64;

use std::collections::HashMap;
use std::mem;
use std::{u8, u16, u32, u64, i8, i16, i32, f32};

#[derive(Debug)]
pub struct PartialClass {

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

impl PartialClass {

    pub fn new() -> PartialClass {
        PartialClass {
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

        merge_reports!(reports, self.expand_labels(&section));

        merge_reports!(reports, self.expand_this_and_super(section.this_class, section.super_class));

        merge_reports!(reports, self.expand_declared_constants(section.constants));

        merge_reports!(reports, self.expand_top_level(section.top_level));

        for field in section.fields {
            let field = merge_reports!(reports, self.expand_field(field));
            self.fields.push(field);
        }

        for method in section.methods {
            let method = merge_reports!(reports, self.expand_method(method));
            self.methods.push(method);
        }
        reports.complete(())
    }

    pub fn assemble_class(mut self) -> raw::Class {
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
                reports.report(report_error!("duplicate label"; label.span));
            } else {
                self.labels.insert(label.name.clone(), LabelKind::Item);
            }
        }

        for meta in section.top_level.iter() {
            if let Some(ref label) = meta.label {
                if self.labels.contains_key(&label.name) {
                    reports.report(report_error!("duplicate label"; label.span));
                } else {
                    self.labels.insert(label.name.clone(), LabelKind::Meta);
                }
            }
        }

        let mut index = 0;
        for constant in section.constants.iter() {
            if let Some(ref label) = constant.label {
                if self.labels.contains_key(&label.name) {
                    reports.report(report_error!("duplicate label"; label.span));
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
                    reports.report(report_error!("duplicate label"; label.span));
                } else {
                    self.labels.insert(label.name.clone(), LabelKind::Item);
                }
            }
            for meta in field.meta.iter() {
                if let Some(ref label) = meta.label {
                    if self.labels.contains_key(&label.name) {
                        reports.report(report_error!("duplicate label"; label.span));
                    } else {
                        self.labels.insert(label.name.clone(), LabelKind::Meta);
                    }
                }
            }
        }

        for method in section.methods.iter() {
            if let Some(ref label) = method.label {
                if self.labels.contains_key(&label.name) {
                    reports.report(report_error!("duplicate label"; label.span));
                } else {
                    self.labels.insert(label.name.clone(), LabelKind::Item);
                }
            }
            for meta in method.meta.iter() {
                if let Some(ref label) = meta.label {
                    if self.labels.contains_key(&label.name) {
                        reports.report(report_error!("duplicate label"; label.span));
                    } else {
                        self.labels.insert(label.name.clone(), LabelKind::Meta);
                    }
                }
            }
            for code in method.code.iter() {
                if let Some(ref label) = code.label {
                    if self.labels.contains_key(&label.name) {
                        reports.report(report_error!("duplicate label"; label.span));
                    } else {
                        self.labels.insert(label.name.clone(), LabelKind::Code(0)); // FIXME
                    }
                }
            }
        }

        self.declared_count = index as usize + 1;

        reports.complete(())
    }

    pub fn expand_this_and_super(&mut self, this_class: Expr, super_class: Expr) -> Reported<()> {

        let mut reports = Reported::new();

        let this_index = merge_reports!(reports, self.eval_expr_into_index(this_class.clone(), Some(|this: &mut PartialClass, s| {
            let str = this.push_expanded_constant(raw::Constant::Utf8(s));
            this.push_expanded_constant(raw::Constant::Class(str))
        })));
        let super_index = merge_reports!(reports, self.eval_expr_into_index(super_class.clone(), Some(|this: &mut PartialClass, s| {
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
            let constant = merge_reports!(reports, self.expand_constant(constant));
            self.push_declared_constant(constant);
        }

        reports.complete(())
    }

    pub fn expand_constant(&mut self, constant: ConstantSection) -> Reported<raw::Constant> {

        let mut reports = Reported::new();

        fn handle_item_ref(this: &mut PartialClass, class: Expr, name: Expr, descriptor: Option<Expr>) -> Reported<(ConstantIndex, ConstantIndex)> {
            let mut reports = Reported::new();
            
            let class_index = merge_reports!(reports, this.eval_expr_into_index(class, Some(|this: &mut PartialClass, s| {
                    let str = this.push_expanded_constant(raw::Constant::Utf8(s));
                    this.push_expanded_constant(raw::Constant::Class(str))
                })));
            let name_and_type = if let Some(descriptor) = descriptor {
                let name_index = merge_reports!(reports,
                        this.eval_expr_into_index(name, Some(PartialClass::push_string_constant)));
                let descriptor_index = merge_reports!(reports,
                        this.eval_expr_into_index(descriptor, Some(PartialClass::push_string_constant)));
                this.push_expanded_constant(raw::Constant::Signature {
                    name: name_index, descriptor: descriptor_index,
                })
            } else {
                merge_reports!(reports, this.eval_expr_into_index(name, self::NO_INDEX_FN))
            };

            reports.complete((class_index, name_and_type))
        }

        let ConstantSection { label, ident, span, body } = constant;

        let constant = match body {
            ConstantInstruction::ClassRef(class) => {
                let index = merge_reports!(reports,
                    self.eval_expr_into_index(class, Some(PartialClass::push_string_constant)));
                raw::Constant::Class(index)
            },
            ConstantInstruction::FieldRef { class, name, descriptor } => {
                let (class_index, name_and_type) = merge_reports!(reports, handle_item_ref(self, class, name, descriptor));
                raw::Constant::FieldRef { class: class_index, signature: name_and_type }
            },
            ConstantInstruction::MethodRef { class, name, descriptor } => {
                let (class_index, name_and_type) = merge_reports!(reports, handle_item_ref(self, class, name, descriptor));
                raw::Constant::MethodRef { class: class_index, signature: name_and_type }
            },
            ConstantInstruction::InterfaceMethodRef { class, name, descriptor } => {
                let (class_index, name_and_type) = merge_reports!(reports, handle_item_ref(self, class, name, descriptor));
                raw::Constant::InterfaceMethodRef { class: class_index, signature: name_and_type }
            },
            ConstantInstruction::String(str) => {
                let index = merge_reports!(reports,
                    self.eval_expr_into_index(str, Some(PartialClass::push_string_constant)));
                raw::Constant::String(index)
            },
            ConstantInstruction::Integer(expr) => {
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
            ConstantInstruction::Float(expr) => {
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
            ConstantInstruction::Long(expr) => {
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
            ConstantInstruction::Double(expr) => {
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
            ConstantInstruction::NameAndType(name, descriptor) => {
                let name_index = merge_reports!(reports,
                    self.eval_expr_into_index(name, Some(PartialClass::push_string_constant)));
                let descriptor_index = merge_reports!(reports,
                    self.eval_expr_into_index(descriptor, Some(PartialClass::push_string_constant)));
                raw::Constant::Signature {
                    name: name_index, descriptor: descriptor_index,
                }
            }
            ConstantInstruction::Utf8(expr) => {
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
                let referent_index = merge_reports!(reports, self.eval_expr_into_index(referent, self::NO_INDEX_FN));
                raw::Constant::MethodHandle {
                    kind, referent: referent_index,
                }
            },
            ConstantInstruction::MethodType(expr) => {
                let index = merge_reports!(reports,
                    self.eval_expr_into_index(expr, Some(PartialClass::push_string_constant)));
                raw::Constant::MethodType(index)
            },
            ConstantInstruction::DynamicTarget { .. } => {
                fatal_error!(reports; "dynamic_target is not currently supported"; span)
            },
        };

        reports.complete(constant)
    }

    pub fn expand_top_level(&mut self, metas: Vec<MetaSection>) -> Reported<()> {
        let mut reports = Reported::new();

        for meta in metas.into_iter() {

            let MetaSection { label, ident, span, body } = meta;
            match body {
                MetaInstruction::Impl(exprs) => for expr in exprs {
                    let index = merge_reports!(reports, self.eval_expr_into_index(expr, Some(|this: &mut PartialClass, s| {
                        let str = this.push_expanded_constant(raw::Constant::Utf8(s));
                        this.push_expanded_constant(raw::Constant::Class(str))
                    })));
                    self.interfaces.push(index);
                },
                MetaInstruction::Version { minor, major } => {
                    if self.minor_major_version.is_some() {
                        reports.report(report_error!("duplicate `version` instruction"; span));
                    } else {
                        // FIXME: use something reasonable
                        let minor_val = merge_reports!(reports, self.eval_expr_into_index(minor, self::NO_INDEX_FN));
                        let major_val = merge_reports!(reports, self.eval_expr_into_index(major, self::NO_INDEX_FN));
                        self.minor_major_version = Some((minor_val.0, major_val.0));
                    }
                },
                MetaInstruction::Flags(exprs) => {
                    if self.flags.is_some() {
                        reports.report(report_error!("duplicate `flags` instruction"; span));
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
                                        reports.report(report_error!("`{}` is not a class flag", other; ident.span));
                                        &mut flag // FIXME: this is to make this typecheck
                                    },
                                },
                                other => {
                                    reports.report(report_error!("expected ident"; other.span()));
                                    &mut flag // FIXME: this is to make this typecheck
                                }
                            };
                        }
                        self.flags = Some(flag);
                    }
                },
                MetaInstruction::Stack(..) => {
                    reports.report(report_error!("`stack` may only appear after a `method` instruction"; span));
                },
                MetaInstruction::Locals(..) => {
                    reports.report(report_error!("`locals` may only appear after a `method` instruction"; span));
                },
                MetaInstruction::Catch {..} => {
                    reports.report(report_error!("`catch` may only appear after a `method` instruction"; span));
                },
                MetaInstruction::ConstantValue(..) => {
                    reports.report(report_error!("`const_val` may only appear after a `field` instruction"; span));
                },
                MetaInstruction::Attr(name, data) => {
                    // fixme: care about what "index" points to
                    let name_index = merge_reports!(reports, self.eval_expr_into_index(name, Some(PartialClass::push_string_constant)));
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
                                reports.report(report_error!("expected base64 string"; span));
                            }
                        },
                        _ => reports.report(report_error!("expected base64 string"; data.span())),
                    }
                },
            }
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
                    merge_reports!(reports, self.eval_expr_into_index(name, Some(PartialClass::push_string_constant)))
                } else {
                    fatal_error!(reports; "`field` requires either a label or a second argument"; field.ident.span)
                }
            }
        };

        let descriptor = merge_reports!(reports, self.eval_expr_into_index(field.descriptor, Some(PartialClass::push_string_constant)));

        let mut flags = None;
        let mut attrs = Vec::new();

        for meta in field.meta.into_iter() {
            let MetaSection { label, ident, span, body } = meta;
            match body {
                MetaInstruction::Impl(..) => {
                    reports.report(report_error!("`impl` may only appear after a `class` instruction"; span));
                },
                MetaInstruction::Version { .. } => {
                    reports.report(report_error!("`version` may only appear after a `class` instruction"; span));
                },
                MetaInstruction::Flags(exprs) => {
                    if flags.is_some() {
                        reports.report(report_error!("duplicate `flags` instruction"; span));
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
                                        reports.report(report_error!("`{}` is not a field flag", other; ident.span));
                                        &mut flag // FIXME: this is to make this typecheck
                                    },
                                },
                                other => {
                                    reports.report(report_error!("expected ident"; other.span()));
                                    &mut flag // FIXME: this is to make this typecheck
                                }
                            };
                        }
                        flags = Some(flag);
                    }
                },
                MetaInstruction::Stack(..) => {
                    reports.report(report_error!("`stack` may only appear after a `method` instruction"; span));
                },
                MetaInstruction::Locals(..) => {
                    reports.report(report_error!("`locals` may only appear after a `method` instruction"; span));
                },
                MetaInstruction::Catch {..} => {
                    reports.report(report_error!("`catch` may only appear after a `method` instruction"; span));
                },
                MetaInstruction::ConstantValue(expr) => {
                    let index = merge_reports!(reports, self.eval_expr_into_index(expr, self::NO_INDEX_FN));
                    let name = self.push_string_constant(flags::attribute::ATTR_CONSTANT_VALUE.into());
                    attrs.push(raw::Attribute { name, info: raw::AttributeInfo::ConstantValue(index)})
                },
                MetaInstruction::Attr(name, data) => {
                    // fixme: care about what "index" points to
                    let name_index = merge_reports!(reports, self.eval_expr_into_index(name, Some(PartialClass::push_string_constant)));
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
                                reports.report(report_error!("expected base64 string"; span));
                            }
                        },
                        _ => reports.report(report_error!("expected base64 string"; data.span())),
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
                    merge_reports!(reports, self.eval_expr_into_index(name, Some(PartialClass::push_string_constant)))
                } else {
                    fatal_error!(reports; "`method` requires either a label or a second argument"; method.ident.span)
                }
            }
        };

        let descriptor = merge_reports!(reports, self.eval_expr_into_index(method.descriptor, Some(PartialClass::push_string_constant)));

        let mut flags = None;
        let mut attrs = Vec::new();

        let mut max_stack = None;
        let mut max_locals = None;
        let mut exceptions = Vec::new();

        for meta in method.meta.into_iter() {
            let MetaSection { label, ident, span, body } = meta;
            match body {
                MetaInstruction::Impl(..) => {
                    reports.report(report_error!("`impl` may only appear after a `class` instruction"; span));
                },
                MetaInstruction::Version { .. } => {
                    reports.report(report_error!("`version` may only appear after a `class` instruction"; span));
                },
                MetaInstruction::Flags(exprs) => {
                    if flags.is_some() {
                        reports.report(report_error!("duplicate `flags` instruction"; span));
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
                                        reports.report(report_error!("`{}` is not a method flag", other; ident.span));
                                        &mut flag // FIXME: this is to make this typecheck
                                    },
                                },
                                other => {
                                    reports.report(report_error!("expected ident"; other.span()));
                                    &mut flag // FIXME: this is to make this typecheck
                                }
                            };
                        }
                        flags = Some(flag);
                    }
                },
                MetaInstruction::Stack(expr) => {
                    if max_stack.is_some() {
                        reports.report(report_error!("duplicate `stack` instruction"; span));
                    } else {
                        println!("{:?}", expr);
                        let val = merge_reports!(reports, self.eval_expr_into_index(expr, self::NO_INDEX_FN)).0; // FIXME: less horrible
                        max_stack = Some(val);
                    }
                },
                MetaInstruction::Locals(expr) => {
                    if max_locals.is_some() {
                        reports.report(report_error!("duplicate `locals` instruction"; span));
                    } else {
                        println!("{:?}", expr);
                        let val = merge_reports!(reports, self.eval_expr_into_index(expr, self::NO_INDEX_FN)).0; // FIXME: less horrible
                        max_locals = Some(val);
                    }
                },
                MetaInstruction::Catch { start, end, handler, ty } => {
                    // FIXME: omg this is all so horrible and sloppy
                    let start_index = merge_reports!(reports, self.eval_expr_into_index(start, self::NO_INDEX_FN)).0; // FIXME: less horrible
                    let end_index = merge_reports!(reports, self.eval_expr_into_index(end, self::NO_INDEX_FN)).0; // FIXME: less horrible
                    let handler_index = merge_reports!(reports, self.eval_expr_into_index(handler, self::NO_INDEX_FN)).0; // FIXME: less horrible

                    let ty_index = merge_reports!(reports, self.eval_expr_into_index(ty, Some(|this: &mut PartialClass, s| {
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
                    reports.report(report_error!("`const_val` may only appear after a `class` instruction"; span));
                },
                MetaInstruction::Attr(name, data) => {
                    // fixme: care about what "index" points to
                    let name_index = merge_reports!(reports, self.eval_expr_into_index(name, Some(PartialClass::push_string_constant)));
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
                                reports.report(report_error!("expected base64 string"; span));
                            }
                        },
                        _ => reports.report(report_error!("expected base64 string"; data.span())),
                    }
                },
            }
        }

        let mut code_len = 0;
        for code in method.code.iter() {
            code_len += code.body.len(code_len);
        }
        
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
                            let index = merge_reports!(reports,
                                self.eval_expr_into_index(expr, self::NO_INDEX_FN));
                            raw::Instruction::$any_const(index)
                        },
                    )*
                    $(
                        CodeInstruction::$class_const(expr) => {
                            let class_index = merge_reports!(reports, self.eval_expr_into_index(expr, Some(|this: &mut PartialClass, s| {
                                let str = this.push_expanded_constant(raw::Constant::Utf8(s));
                                this.push_expanded_constant(raw::Constant::Class(str))
                            })));
                            raw::Instruction::$class_const(class_index)
                        },
                    )*
                    $(
                        CodeInstruction::$var(expr) => {
                            let index = VarIndex(merge_reports!(reports, self.eval_expr_into_int(expr, "u8", self::into_u8)));
                            raw::Instruction::$var(index)
                        },
                    )*
                    $(
                        CodeInstruction::$jump(expr) => {
                            let index = CodeIndex(merge_reports!(reports, self.eval_expr_into_int(expr, "u16", self::into_u16)));
                            raw::Instruction::$jump(index)
                        },
                    )*
                    $($tokens)*
                }
            };
        }

        for code in method.code {
            let CodeSection { label, ident, span, body, } = code;
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
                        let byte = merge_reports!(reports, self.eval_expr_into_int(expr, "i8", self::into_i8));
                        raw::Instruction::PushByte(byte)
                    },
                    CodeInstruction::PushShort(expr) => {
                        let short = merge_reports!(reports, self.eval_expr_into_int(expr, "i16", self::into_i16));
                        raw::Instruction::PushShort(short)
                    },

                    CodeInstruction::LoadConstant(expr) => {
                        let index = merge_reports!(reports, self.eval_expr_into_index(expr, Some(PartialClass::push_string_constant)));
                        let half = HalfConstantIndex(index.0 as u8); // FIXME
                        raw::Instruction::LoadConstant(half)
                    },
                    CodeInstruction::WideLoadConstant(expr) => {
                        let index = merge_reports!(reports, self.eval_expr_into_index(expr, Some(PartialClass::push_string_constant)));
                        raw::Instruction::WideLoadConstant(index)
                    },
                    CodeInstruction::WideLoadWideConstant(expr) => {
                        let index = merge_reports!(reports, self.eval_expr_into_index(expr, Some(PartialClass::push_string_constant)));
                        raw::Instruction::WideLoadWideConstant(index)
                    },

                    CodeInstruction::IncInt(var, val) => {
                        let var = VarIndex(merge_reports!(reports, self.eval_expr_into_int(var, "u8", self::into_u8)));
                        let val = merge_reports!(reports, self.eval_expr_into_int(val, "i8", self::into_i8));
                        raw::Instruction::IncInt(var, val)
                    },

                    CodeInstruction::LookupSwitch {
                        default_offset, match_table,
                    } => unimplemented!(),

                    CodeInstruction::TableSwitch {
                        default_offset, match_range, offset_table,
                    } => unimplemented!(),

                    CodeInstruction::InvokeInterface(expr, count) => {
                        let index = merge_reports!(reports, self.eval_expr_into_index(expr, self::NO_INDEX_FN));
                        let val = merge_reports!(reports, self.eval_expr_into_int(count, "u8", self::into_u8));
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
                                        reports.report(report_error!("expected primitive type, found `{}`", other; ident.span));
                                        continue
                                    }
                                }
                            }
                            _ => {
                                reports.report(report_error!("expected identifier"; expr.span()));
                                continue
                            }
                        };
                        raw::Instruction::NewPrimitiveArray(ty)
                    }

                    CodeInstruction::Wide(..) => unimplemented!(), // FIXME: deal with this...

                    CodeInstruction::NewRefMultiArray(class, dims) => {
                        let class_index = merge_reports!(reports, self.eval_expr_into_index(class, Some(|this: &mut PartialClass, s| {
                            let str = this.push_expanded_constant(raw::Constant::Utf8(s));
                            this.push_expanded_constant(raw::Constant::Class(str))
                        })));
                        let dims_val = merge_reports!(reports, self.eval_expr_into_int(dims, "u8", self::into_u8));
                        raw::Instruction::NewRefMultiArray(class_index, dims_val)
                    },

                    CodeInstruction::WideGoto(expr) => {
                        let index = WideCodeIndex(merge_reports!(reports, self.eval_expr_into_int(expr, "u32", self::into_u32)));
                        raw::Instruction::WideGoto(index)
                    },
                    CodeInstruction::WideJumpSub(expr) => {
                        let index = WideCodeIndex(merge_reports!(reports, self.eval_expr_into_int(expr, "u32", self::into_u32)));
                        raw::Instruction::WideJumpSub(index)
                    },


                    CodeInstruction::Breakpoint => raw::Instruction::Breakpoint,
                    CodeInstruction::ImplementationDefined1 => raw::Instruction::ImplementationDefined1,
                    CodeInstruction::ImplementationDefined2 => raw::Instruction::ImplementationDefined2,
            ));
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
                    let constant = merge_reports!(reports, self.expand_constant(ConstantSection {
                        label: None, ident, span, body: i,
                    }));
                    Value::Int(self.push_expanded_constant(constant).0 as i64, span)
                } else {
                    fatal_error!(reports; "expected constant instruction"; span)
                }
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

    fn eval_expr_into_int<'a, F, I>(&'a mut self, expr: Expr, expected: &str, convert: F) -> Reported<I>
        where F: FnOnce(i64) -> Option<I> {
        let mut reports = Reported::new();

        match merge_reports!(reports, self.eval_expr(expr)) {
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

#[derive(Clone, Copy, Debug)]
enum LabelKind {
    Constant(u32),
    Item,
    Meta,
    Code(u32),
}

// represents a pile of instructions
// contiguously making up a class
pub struct ClassSection {
    label: Option<Ident>,
    ident: Ident,
    span: Span,
    this_class: Expr,
    super_class: Expr,
    top_level: Vec<MetaSection>,
    constants: Vec<ConstantSection>,
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

        let mut reports = Reported::new();

        // first, get the class instruction, and error
        // at anything else
        let (label, ident, span, this_class, super_class) = loop {
            let next = stack.pop().unwrap();
            let Instruction { label, ident, body, span } = next;
            if let InstructionBody::Item(ItemInstruction::Class { this_class, super_class }) = body {
                break (label, ident, span, this_class, super_class);
            } else {
                reports.report(report_error!("found `{}` before `class` instruction", ident.name; ident.span));
            }
        };

        let mut top_level = Vec::new();
        let mut constants = Vec::new();

        let mut methods = Vec::new();
        let mut fields = Vec::new();

        while let Some(next) = stack.pop() {
            let Instruction { label, ident, body, span } = next;
            match body.clone() { // FIXME: get rid of this clone
                InstructionBody::Item(item) => match item {
                    ItemInstruction::Field { descriptor, name } => {
                        let mut meta = Vec::new();
                        while let Some(op) = stack.pop() {
                            let Instruction { label, ident, body, span } = op;
                            match body {
                                InstructionBody::Item(..) => {
                                    stack.push(Instruction { label, ident, body, span });
                                    break
                                },
                                InstructionBody::Meta(i) => meta.push(MetaSection { label, ident, span, body: i }),
                                InstructionBody::Constant(i) => constants.push(ConstantSection { label, ident, span, body: i }),
                                InstructionBody::Code(..) => reports.report(report_error!(
                                    "unexpected code instruction `{}` in field", ident.name; ident.span
                                ))
                            }
                        }
                        fields.push(FieldSection {
                            label, ident, span,
                            name, descriptor,
                            meta,
                        });
                    }
                    ItemInstruction::Method { descriptor, name, } => {
                        let mut meta = Vec::new();
                        let mut code = Vec::new();
                        while let Some(op) = stack.pop() {
                            let Instruction { label, ident, body, span } = op;
                            match body {
                                InstructionBody::Item(..) => {
                                    stack.push(Instruction { label, ident, body, span });
                                    break
                                },
                                InstructionBody::Meta(i) => meta.push(MetaSection { label, ident, span, body: i }),
                                InstructionBody::Constant(i) => constants.push(ConstantSection { label, ident, span, body: i }),
                                InstructionBody::Code(i) => code.push(CodeSection { label, ident, span, body: i }),
                            }
                        }
                        methods.push(MethodSection {
                            label, ident, span,
                            name, descriptor,
                            meta, code,
                        });
                    }
                    _ => {
                        stack.push(Instruction { label, ident, body, span });
                        break
                    }
                }
                InstructionBody::Meta(i) => top_level.push(MetaSection { label, ident, span, body: i }),
                InstructionBody::Constant(i) => constants.push(ConstantSection { label, ident, span, body: i }),
                InstructionBody::Code(..) => reports.report(report_error!(
                    "unexpected code instruction `{}`", ident.name; ident.span
                ))
            }
        }

        reports.complete(ClassSection { label, ident, span, this_class, super_class, top_level, constants, fields, methods })
    }
}

pub struct MetaSection {
    label: Option<Ident>,
    ident: Ident,
    span: Span,
    body: MetaInstruction,
}

pub struct ConstantSection {
    label: Option<Ident>,
    ident: Ident,
    span: Span,
    body: ConstantInstruction,
}

pub struct FieldSection {
    label: Option<Ident>,
    ident: Ident,
    span: Span,
    name: Option<Expr>,
    descriptor: Expr,
    meta: Vec<MetaSection>,
}

pub struct MethodSection {
    label: Option<Ident>,
    ident: Ident,
    span: Span,
    name: Option<Expr>,
    descriptor: Expr,
    meta: Vec<MetaSection>,
    code: Vec<CodeSection>,
}

pub struct CodeSection {
    label: Option<Ident>,
    ident: Ident,
    span: Span,
    body: CodeInstruction,
}

#[derive(Clone, Debug)]
pub enum Value {
    Int(i64, Span),
    Float(f64, Span),
    Str(String, Span),
}

fn into_i8(val: i64) -> Option<i8> {
    if val >= (i8::MIN as i64) && val <= (i8::MAX as i64) {
        Some(val as i8)
    } else {
        None
    }
}

fn into_i16(val: i64) -> Option<i16> {
    if val >= (i16::MIN as i64) && val <= (i16::MAX as i64) {
        Some(val as i16)
    } else {
        None
    }
}

fn into_u8(val: i64) -> Option<u8> {
    if val >= (u8::MIN as i64) && val <= (u8::MAX as i64) {
        Some(val as u8)
    } else {
        None
    }
}

fn into_u16(val: i64) -> Option<u16> {
    if val >= (u16::MIN as i64) && val <= (u16::MAX as i64) {
        Some(val as u16)
    } else {
        None
    }
}

fn into_u32(val: i64) -> Option<u32> {
    if val >= (u32::MIN as i64) && val <= (u32::MAX as i64) {
        Some(val as u32)
    } else {
        None
    }
}

const NO_INDEX_FN: Option<fn(&mut PartialClass, String) -> ConstantIndex> = None;



