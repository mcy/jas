use crate::ast::*;
use crate::phase::Phase;
use crate::reporting::*;
use crate::source_file::Span;

// represents a pile of instructions
// contiguously making up a class
pub struct ClassSection {
    pub label: Option<Ident>,
    pub ident: Ident,
    pub span: Span,
    pub this_class: Expr,
    pub super_class: Expr,
    pub top_level: Vec<MetaSection>,
    pub constants: Vec<ConstantSection>,
    pub fields: Vec<FieldSection>,
    pub methods: Vec<MethodSection>,
}

impl ClassSection {

    pub fn from_instructions(ast: Vec<Instruction>) -> Reported<Vec<ClassSection>> {

        let mut stack = ast.into_iter().rev().collect::<Vec<_>>();

        let mut reports = Reported::new();
        let mut result = Vec::new();

        while !stack.is_empty() {
            result.push(report_try!(reports; ClassSection::consume_class(&mut stack)));
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
                report_error!(reports; "found `{}` before `class` instruction", ident.name; ident.span);
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
                                InstructionBody::Code(..) => report_error!(reports; "unexpected code instruction `{}` in field", ident.name; ident.span)
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
                InstructionBody::Code(..) => report_error!(reports; "unexpected code instruction `{}`", ident.name; ident.span)
            }
        }

        reports.complete(ClassSection { label, ident, span, this_class, super_class, top_level, constants, fields, methods })
    }
}

impl Phase for ClassSection {

    type Input = Vec<Instruction>;
    type Output = ClassSection;

    fn run(input: Vec<Self::Input>) -> Reported<Vec<Self::Output>> {

        let mut reports = Reported::new();
        let mut out = Vec::new();

        for instructions in input {
            if let Some(classes) = reports.merge(ClassSection::from_instructions(instructions)) {
                for class in classes {
                    out.push(class);
                }
            }
        }

        reports.complete(out)
    }
}

pub struct MetaSection {
    pub label: Option<Ident>,
    pub ident: Ident,
    pub span: Span,
    pub body: MetaInstruction,
}

pub struct ConstantSection {
    pub label: Option<Ident>,
    pub ident: Ident,
    pub span: Span,
    pub body: ConstantInstruction,
}

pub struct FieldSection {
    pub label: Option<Ident>,
    pub ident: Ident,
    pub span: Span,
    pub name: Option<Expr>,
    pub descriptor: Expr,
    pub meta: Vec<MetaSection>,
}

pub struct MethodSection {
    pub label: Option<Ident>,
    pub ident: Ident,
    pub span: Span,
    pub name: Option<Expr>,
    pub descriptor: Expr,
    pub meta: Vec<MetaSection>,
    pub code: Vec<CodeSection>,
}

pub struct CodeSection {
    pub label: Option<Ident>,
    pub ident: Ident,
    pub span: Span,
    pub body: CodeInstruction,
}