use crate::ast::*;
use crate::consts;
use crate::consts::instructions;
use crate::consts::special;
use crate::codegen::Generator;
use crate::codegen::eval::{self, EvalContext};
use crate::codegen::convert;
use crate::codegen::labels::LabelKind;
use crate::sections::MetaSection;
use crate::source_file::Span;
use crate::reporting::*;

use classfile::raw;
use classfile::consts as flags;

use base64;

macro_rules! wrong_target {
    ($reports:ident; $inst:expr, $expected:expr; $span:expr) => {
        fatal_error!($reports; "`{}` may only appear after a `{}` instruction", $inst, $expected; $span)
    }
}

pub enum ClassMeta {
    Directive, // FIXME: make lazy somehow? boxed closures are ugh
    Flags(flags::class::Flags),
    Attr(raw::Attribute),
    Bootstrap(raw::BootstrapMethod),
}

pub enum FieldMeta {
    Flags(flags::field::Flags),
    Attr(raw::Attribute),
}

pub enum MethodMeta {
    Flags(flags::method::Flags),
    Attr(raw::Attribute),
    Stack(u16),
    Locals(u16),
    Catch(raw::ExceptionTableEntry),
    StackMap(raw::StackMapFrame),
}

pub fn expand_this_and_super(gen: &mut Generator, this_class: Expr, super_class: Expr) -> Reported<()> {

    let mut reports = Reported::new();

    let (this_index, super_index) = {
        let mut cx = eval::EvalContext::new(&mut *gen);
        (report_try!(reports; cx.eval_into_class(this_class)),
         report_try!(reports; cx.eval_into_class(super_class)))
    };

    gen.labels.insert("this".into(), LabelKind::Constant(this_index.0 as u32));
    gen.labels.insert("super".into(), LabelKind::Constant(super_index.0 as u32));

    gen.this_class = Some(this_index);
    gen.super_class = Some(super_index);

    reports.complete(())
}

pub fn expand_class_attrs(gen: &mut Generator, metas: Vec<MetaSection>) -> Reported<()> {
    let mut reports = Reported::new();

    for meta in metas {
        let span = meta.ident.span();
        match report_cont!(reports; expand_class_attr(gen, meta)) {
            ClassMeta::Directive => { /* nothing to do, it's already been handled */ },
            ClassMeta::Flags(flag) => {
                if let Some(..) = gen.flags {
                    report_warning!(reports; "`flags` instruction shadows previous one"; span);
                }
                gen.flags = Some(flag)
            }
            ClassMeta::Attr(attr) => {
                gen.attributes.push(attr);
            },
            ClassMeta::Bootstrap(method) => {
                gen.bootstrap_table.push(method);
            },
        }
    }

    reports.complete(())
}

pub fn expand_class_attr(gen: &mut Generator, meta: MetaSection) -> Reported<ClassMeta> {
    let mut reports = Reported::new();

    let mut cx = EvalContext::new(gen);
    let MetaSection { span, body, .. } = meta;

    let result = match body {
        MetaInstruction::Impl(exprs) => {
            for expr in exprs {
                let index = report_cont!(reports; cx.eval_into_class(expr));
                cx.gen.interfaces.push(index);
            }
            ClassMeta::Directive
        },
        MetaInstruction::Version { minor, major } => {
            if cx.gen.minor_major_version.is_some() {
                report_error!(reports; "duplicate `version` instruction"; span);
            } else {
                // FIXME: use something reasonable
                let minor_val = report_try!(reports; cx.eval_into_int(minor, "u16", convert::into_u16));
                let major_val = report_try!(reports; cx.eval_into_int(major, "u16", convert::into_u16));
                cx.gen.minor_major_version = Some((minor_val, major_val));
            }
            ClassMeta::Directive
        },
        MetaInstruction::Flags(exprs) => {
            let mut flag = flags::class::Flags::new();
            for expr in exprs {
                let ident = report_cont!(reports; cx.eval_into_bare_word(expr));
                match ident.name.as_str() {
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
                        continue
                    },
                };
            }
            ClassMeta::Flags(flag)
        },
        MetaInstruction::Stack(..) => wrong_target!(reports; instructions::STACK, instructions::METHOD; span),
        MetaInstruction::Locals(..) => wrong_target!(reports; instructions::LOCALS, instructions::METHOD; span),
        MetaInstruction::Catch {..} => wrong_target!(reports; instructions::CATCH, instructions::METHOD; span),
        MetaInstruction::ConstantValue(..) => wrong_target!(reports; instructions::CONST_VALUE, instructions::FIELD; span),
        MetaInstruction::Source(expr) => {
            let index = report_try!(reports; cx.eval_into_utf8(expr));
            let name = cx.gen.push_string_constant(flags::attribute::ATTR_SOURCE_FILE.into());
            ClassMeta::Attr(raw::Attribute {
                name, info: raw::AttributeInfo::SourceFile(index),
            })
        }
        MetaInstruction::Bootstrap(exprs) => {
            let mut iter = exprs.into_iter();
            let handle = report_try!(reports; cx.eval_into_const_index(iter.next().unwrap(), eval::FN_NONE));
            let mut args = Vec::new();
            for expr in iter {
                args.push(report_try!(reports; cx.eval_into_const_index(expr, eval::FN_NONE)));
            }
            ClassMeta::Bootstrap(raw::BootstrapMethod {
                method: handle,
                arguments: args
            })
        },
        MetaInstruction::StackMap(..) => wrong_target!(reports; instructions::STACK_MAP, instructions::METHOD; span),
        MetaInstruction::Attr(name, data) => {
            let name_index = report_try!(reports; cx.eval_into_utf8(name));
            match data {
                Expr::Str(str) => {
                    let span = str.span;
                    if let Ok(data) = base64::decode(str.value.as_str()) {
                        ClassMeta::Attr(raw::Attribute {
                            name: name_index,
                            info: raw::AttributeInfo::Other(data),
                        })
                    } else {
                        fatal_error!(reports; "expected base64 string"; span)
                    }
                },
                _ => fatal_error!(reports; "expected base64 string"; data.span()),
            }
        },
    };

    reports.complete(result)
}

pub fn expand_field_attr(gen: &mut Generator, meta: MetaSection) -> Reported<FieldMeta> {
    let mut reports = Reported::new();

    let mut cx = EvalContext::new(gen);
    let MetaSection { span, body, .. } = meta;

    let result = match body {
        MetaInstruction::Impl(..) =>  wrong_target!(reports; instructions::IMPL, instructions::CLASS; span),
        MetaInstruction::Version { .. } => wrong_target!(reports; instructions::VERSION, instructions::CLASS; span),
        MetaInstruction::Flags(exprs) => {
            let mut flag = flags::field::Flags::new();
            for expr in exprs {
                let ident = report_cont!(reports; cx.eval_into_bare_word(expr));
                match ident.name.as_str() {
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
                        continue
                    },
                };
            }
            FieldMeta::Flags(flag)
        },
        MetaInstruction::Stack(..) => wrong_target!(reports; instructions::STACK, instructions::METHOD; span),
        MetaInstruction::Locals(..) => wrong_target!(reports; instructions::LOCALS, instructions::METHOD; span),
        MetaInstruction::Catch { .. } => wrong_target!(reports; instructions::CATCH, instructions::METHOD; span),
        MetaInstruction::ConstantValue(expr) => {
            let index = report_try!(reports; cx.eval_into_const_index(expr, eval::FN_NONE));
            let name = cx.gen.push_string_constant(flags::attribute::ATTR_CONSTANT_VALUE.into());
            FieldMeta::Attr(raw::Attribute{
                name,
                info: raw::AttributeInfo::ConstantValue(index),
            })
        },
        MetaInstruction::Source(..) => wrong_target!(reports; instructions::SOURCE, instructions::CLASS; span),
        MetaInstruction::Bootstrap(..) => wrong_target!(reports; instructions::BOOTSTRAP, instructions::CLASS; span),
        MetaInstruction::StackMap(..) => wrong_target!(reports; instructions::STACK_MAP, instructions::METHOD; span),
        MetaInstruction::Attr(name, data) => {
            let name_index = report_try!(reports; cx.eval_into_utf8(name));
            match data {
                Expr::Str(str) => {
                    let span = str.span;
                    if let Ok(data) = base64::decode(str.value.as_str()) {
                        FieldMeta::Attr(raw::Attribute {
                            name: name_index,
                            info: raw::AttributeInfo::Other(data),
                        })
                    } else {
                        fatal_error!(reports; "expected base64 string"; span);
                    }
                },
                _ => fatal_error!(reports; "expected base64 string"; data.span()),
            }
        },
    };

    reports.complete(result)
}

pub fn expand_method_attr(gen: &mut Generator, meta: MetaSection) -> Reported<MethodMeta> {
    let mut reports = Reported::new();

    let mut cx = EvalContext::new(gen);
    let MetaSection { span, body, .. } = meta;

    let res = match body {
        MetaInstruction::Impl(..) => wrong_target!(reports; instructions::IMPL, instructions::CLASS; span),
        MetaInstruction::Version { .. } => wrong_target!(reports; instructions::VERSION, instructions::CLASS; span),
        MetaInstruction::Flags(exprs) => {
            let mut flag = flags::method::Flags::new();
            for expr in exprs {
                let ident = report_try!(reports; cx.eval_into_bare_word(expr));
                match ident.name.as_str() {
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
                        continue
                    },
                };
            }

            MethodMeta::Flags(flag)
        },
        MetaInstruction::Stack(expr) => {
            let val = report_try!(reports; cx.eval_into_int(expr, "u16", convert::into_u16));
            MethodMeta::Stack(val)
        },
        MetaInstruction::Locals(expr) => {
            let val = report_try!(reports; cx.eval_into_int(expr, "u16", convert::into_u16));
            MethodMeta::Locals(val)
        },
        MetaInstruction::Catch { start, end, handler, ty } => {
            let start_index = report_try!(reports; cx.eval_into_code_index(start));
            let end_index = report_try!(reports; cx.eval_into_code_index(end));
            let handler_index = report_try!(reports; cx.eval_into_code_index(handler));
            let ty_index = report_try!(reports; cx.eval_into_class(ty));

            MethodMeta::Catch(raw::ExceptionTableEntry {
                try_range: (start_index, end_index),
                handler_goto: handler_index,
                exception_type: ty_index,
            })
        },
        MetaInstruction::ConstantValue(..) => wrong_target!(reports; instructions::CONST_VALUE, instructions::FIELD; span),
        MetaInstruction::Source(..) => wrong_target!(reports; instructions::SOURCE, instructions::CLASS; span),
        MetaInstruction::Bootstrap(..) => wrong_target!(reports; instructions::BOOTSTRAP, instructions::CLASS; span),
        MetaInstruction::StackMap(exprs) => MethodMeta::StackMap(report_try!(reports; expand_stack_map(cx, exprs))),
        MetaInstruction::Attr(name, data) => {
            let name_index = report_try!(reports; cx.eval_into_utf8(name));
            match data {
                Expr::Str(str) => {
                    let span = str.span;
                    if let Ok(data) = base64::decode(str.value.as_str()) {
                        MethodMeta::Attr(raw::Attribute {
                            name: name_index,
                            info: raw::AttributeInfo::Other(data),
                        })
                    } else {
                        fatal_error!(reports; "expected base64 string"; span);
                    }
                },
                _ => fatal_error!(reports; "expected base64 string"; data.span()),
            }
        },
    };

    reports.complete(res)
}

fn expand_stack_map(mut cx: EvalContext<'_>, exprs: Vec<Expr>) -> Reported<raw::StackMapFrame> {
    let mut reports = Reported::new();
    let mut iter = exprs.into_iter();

    let frame_type = report_try!(reports; cx.eval_into_bare_word(iter.next().unwrap()));
    let (span, offset) = {
        let next = iter.next().unwrap();
        (next.span(), report_try!(reports; cx.eval_into_code_index(iter.next().unwrap())))
    };

    #[inline]
    fn parse_vty<'a, 'b, I: Iterator<Item = Expr>, T>(cx: &'b mut EvalContext<'a>, iter: &'b mut I, reports: &'b mut Reported<T>, span: &'b Span)
        -> Option<raw::VerificationType> {
        if let Some(expr) = iter.next() {
            let ident = reports.merge(cx.eval_into_bare_word(expr))?;
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
                        let ty = reports.merge(cx.eval_into_class(next))?;
                        Some(raw::VerificationType::Object(ty))
                    } else {
                        report_error!(reports; "expected argument"; ident);
                        None
                    }
                }
                special::VTYPE_UNINIT => {
                    if let Some(next) = iter.next() {
                        let index = reports.merge(cx.eval_into_code_index(next))?;
                        Some(raw::VerificationType::Uninitialized(index))
                    } else {
                        report_error!(reports; "expected argument"; ident);
                        None
                    }
                }
                _other => {
                    report_error!(reports; "expected verification type, found `{}`", ident.name; ident);
                    None
                }
            }
        } else {
            report_error!(reports; "expected identifier"; span);
            None
        }
    }

    macro_rules! error_on_extras {
        ($iter:expr) => {
            for extra in $iter {
                report_error!(reports; "unexpected argument"; extra);
            }
        }
    }

    let frame = match frame_type.name.as_str() {
        special::STACK_MAP_SAME => {
            if offset.0 > flags::attribute::STACK_MAP_SAME_MAX as u16 {
                report_error!(reports; "expected integer at most {}, found {}",
                                    flags::attribute::STACK_MAP_SAME_MAX, offset.0 ; span)
            }

            let frame = raw::StackMapFrame::Same(offset);
            error_on_extras!(iter);
            frame
        },
        special::STACK_MAP_SAME_EXT => {
            let frame = raw::StackMapFrame::SameExt(offset);
            error_on_extras!(iter);
            frame
        },
        special::STACK_MAP_SINGLE_STACK => {
            if offset.0 > (flags::attribute::STACK_MAP_SAME_MAX - flags::attribute::STACK_MAP_SAME_MIN) as u16 {
                report_error!(reports; "expected integer at most {}, found {}",
                                    flags::attribute::STACK_MAP_SAME_MAX - flags::attribute::STACK_MAP_SAME_MIN, offset.0 ; span)
            }
            if let Some(ty) = parse_vty(&mut cx, &mut iter, &mut reports, &frame_type.span) {
                let frame = raw::StackMapFrame::SingleStack(offset, ty);
                error_on_extras!(iter);
                frame
            } else {
                fatal_error!(reports; "expected verification type"; frame_type)
            }
        },
        special::STACK_MAP_SINGLE_STACK_EXT => {
            if let Some(ty) = parse_vty(&mut cx, &mut iter, &mut reports, &frame_type.span) {
                let frame = raw::StackMapFrame::SingleStackExt(offset, ty);
                error_on_extras!(iter);
                frame
            } else {
                fatal_error!(reports; "expected verification type"; frame_type)
            }
        },
        special::STACK_MAP_CHOP_1 => {
            let frame = raw::StackMapFrame::Chop1(offset);
            error_on_extras!(iter);
            frame
        },
        special::STACK_MAP_CHOP_2 => {
            let frame = raw::StackMapFrame::Chop2(offset);
            error_on_extras!(iter);
            frame
        },
        special::STACK_MAP_CHOP_3 => {
            let frame = raw::StackMapFrame::Chop3(offset);
            error_on_extras!(iter);
            frame
        },
        special::STACK_MAP_APPEND_1 => {
            let arg1 = parse_vty(&mut cx, &mut iter, &mut reports, &frame_type.span);
            if arg1.is_some() {
                let frame = raw::StackMapFrame::Append1(offset, arg1.unwrap());
                error_on_extras!(iter);
                frame
            } else {
                fatal_error!(reports; "expected 1 argument"; frame_type)
            }
        },
        special::STACK_MAP_APPEND_2 => {
            let arg1 = parse_vty(&mut cx, &mut iter, &mut reports, &frame_type.span);
            let arg2 = parse_vty(&mut cx, &mut iter, &mut reports, &frame_type.span);
            if arg1.is_some() && arg2.is_some() {
                let frame = raw::StackMapFrame::Append2(offset, arg1.unwrap(), arg2.unwrap());
                error_on_extras!(iter);
                frame
            } else {
                fatal_error!(reports; "expected 2 arguments"; frame_type)
            }
        },
        special::STACK_MAP_APPEND_3 => {
            let arg1 = parse_vty(&mut cx, &mut iter, &mut reports, &frame_type.span);
            let arg2 = parse_vty(&mut cx, &mut iter, &mut reports, &frame_type.span);
            let arg3 = parse_vty(&mut cx, &mut iter, &mut reports, &frame_type.span);
            if arg1.is_some() && arg2.is_some() && arg3.is_some() {
                let frame = raw::StackMapFrame::Append3(offset, arg1.unwrap(), arg2.unwrap(), arg3.unwrap());
                error_on_extras!(iter);
                frame
            } else {
                fatal_error!(reports; "expected 3 arguments"; frame_type)
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
                            ref _other => report_error!(reports; "expected identifier"; frame_type),
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
                            ref _other => {
                                if let Some(ty) = parse_vty(&mut cx, &mut iter, &mut reports, &frame_type.span) {
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
                            ref _other => {
                                if let Some(ty) = parse_vty(&mut cx, &mut iter, &mut reports, &frame_type.span) {
                                    locals.push(ty);
                                }
                            }
                        }
                    },
                }
            }
            raw::StackMapFrame::Full {
                offset, stack, locals,
            }
        },
        other => fatal_error!(reports; "expected `stack_map_frame` type, found `{}`", other; frame_type),
    };

    reports.complete(frame)
}