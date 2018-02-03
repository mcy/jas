pub mod labels;
pub mod constants;
pub mod attrs;
pub mod fields;
pub mod methods;
pub mod eval;

pub mod convert;

use codegen::labels::LabelKind;
use phase::Phase;
use sections::ClassSection;
use reporting::*;

use classfile::raw;
use classfile::indexing::*;
use classfile::consts as flags;

use std::collections::HashMap;
use std::mem;

#[derive(Debug)]
pub struct Generator {

    pub(in codegen) minor_major_version: Option<(u16, u16)>,

    pub(in codegen) labels: HashMap<String, LabelKind>,

    pub(in codegen) declared_constants: Vec<raw::Constant>,
    pub(in codegen) declared_count: usize,
    pub(in codegen) expanded_constants: Vec<raw::Constant>,

    pub(in codegen) flags: Option<flags::class::Flags>,

    pub(in codegen) this_class: Option<ConstantIndex>,
    pub(in codegen) super_class: Option<ConstantIndex>,
    pub(in codegen) interfaces: Vec<ConstantIndex>,

    pub(in codegen) fields: Vec<raw::Field>,
    pub(in codegen) methods: Vec<raw::Method>,

    pub(in codegen) attributes: Vec<raw::Attribute>,

    pub(in codegen) bootstrap_table: Vec<raw::BootstrapMethod>,
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

            bootstrap_table: Vec::new(),
        }
    }

    pub(in codegen) fn get_constant(&self, index: ConstantIndex) -> Option<&raw::Constant> {
        let index = index.0 as usize;
        if index < self.declared_count {
            Some(&self.declared_constants[index - 1])
        } else if index - self.declared_count <= self.expanded_constants.len() {
            Some(&self.expanded_constants[index - self.declared_count])
        } else {
            None
        }
    }

    pub(in codegen) fn push_declared_constant(&mut self, constant: raw::Constant) {
        let is_wide = match constant {
            raw::Constant::Long(..) |
            raw::Constant::Double(..) => true,
            _ => false,
        };

        self.declared_constants.push(constant);
        if is_wide { self.declared_constants.push(raw::Constant::WidePlaceholder) };
    }

    pub(in codegen) fn push_expanded_constant(&mut self, constant: raw::Constant) -> ConstantIndex {

        if let Some(index) = self.declared_constants.iter().position(|x| x == &constant) {
            ConstantIndex(index as u16 + 1)
        } else if let Some(index) = self.expanded_constants.iter().position(|x| x == &constant) {
            ConstantIndex((self.declared_count + index) as u16)
        } else {
            let is_wide = match constant {
                raw::Constant::Long(..) |
                raw::Constant::Double(..) => true,
                _ => false,
            };

            let index = ConstantIndex((self.declared_count + self.expanded_constants.len()) as u16);

            self.expanded_constants.push(constant);
            if is_wide {
                self.expanded_constants.push(raw::Constant::WidePlaceholder)
            };

            index
        }
    }

    pub(in codegen) fn push_string_constant(&mut self, str: String) -> ConstantIndex {
        self.push_expanded_constant(raw::Constant::Utf8(str))
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

    pub fn assemble_class(mut self, section: ClassSection) -> Reported<(Option<Vec<String>>, raw::Class)> {

        let mut reports = Reported::new();

        report_try!(reports; labels::expand_labels(&mut self, &section));

        let ClassSection {
            label, ident, span,
            this_class, super_class,
            top_level,
            constants,
            fields,
            methods,
        } = section;

        report_try!(reports; attrs::expand_this_and_super(&mut self, this_class, super_class));
        report_try!(reports; constants::expand_declared_constants(&mut self, constants));
        report_try!(reports; attrs::expand_class_attrs(&mut self, top_level));

        for field in fields {
            let field = report_cont!(reports; fields::expand_field(&mut self, field));
            self.fields.push(field);
        }

        for method in methods {
            let method = report_cont!(reports; methods::expand_method(&mut self, method));
            self.methods.push(method);
        }

        if !self.bootstrap_table.is_empty() {
            let name = self.push_string_constant(flags::attribute::ATTR_BOOTSTRAP_METHODS.into());
            let bootstrap_methods = mem::replace(&mut self.bootstrap_table, Vec::new());

            self.attributes.push(raw::Attribute {
                name,
                info: raw::AttributeInfo::BootstrapMethods(bootstrap_methods),
            });
        }

        reports.complete((
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
        ))
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
            out.push(report_cont!(reports; gen.assemble_class(class)));
        }

        reports.complete(out)
    }
}