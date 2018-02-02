use ast::*;
use consts::special;
use codegen::Generator;
use codegen::eval::{self, EvalContext, Value};
use codegen::convert;
use sections::ConstantSection;
use reporting::*;

use classfile::raw;

pub fn expand_declared_constants(gen: &mut Generator, constants: Vec<ConstantSection>) -> Reported<()> {

    let mut reports = Reported::new();

    for constant in constants.into_iter() {
        let constant = report_cont!(reports; expand_constant(gen, constant));
        gen.push_declared_constant(constant);
    }

    reports.complete(())
}

pub fn expand_constant(gen: &mut Generator, constant: ConstantSection) -> Reported<raw::Constant> {

    let mut reports = Reported::new();

    let mut cx = EvalContext::new(gen);

    macro_rules! item_ref {
        ($ty:path; $class:expr, $name:expr, $desc:expr) => {{
            let class = $class;
            let name = $name;
            let desc = $desc;
            let class_index = report_try!(reports; cx.eval_into_class(class));
            let name_and_type = if let Some(desc) = desc {
                let name_index = report_try!(reports; cx.eval_into_utf8(name));
                let desc_index = report_try!(reports; cx.eval_into_utf8(desc));
                cx.gen.push_expanded_constant(raw::Constant::Signature {
                    name: name_index, descriptor: desc_index,
                })
            } else {
                report_try!(reports; cx.eval_into_const_index(name, eval::FN_NONE))
            };
            $ty { class: class_index, signature: name_and_type }
        }}
    }

    let ConstantSection { label, ident, span, body } = constant;

    let constant = match body {
        ConstantInstruction::ClassRef(class) => {
            let index = report_try!(reports; cx.eval_into_utf8(class));
            raw::Constant::Class(index)
        },
        ConstantInstruction::FieldRef { class, name, descriptor } => {
            item_ref!(raw::Constant::FieldRef; class, name, descriptor)
        },
        ConstantInstruction::MethodRef { class, name, descriptor } => {
            item_ref!(raw::Constant::MethodRef; class, name, descriptor)
        },
        ConstantInstruction::InterfaceMethodRef { class, name, descriptor } => {
            item_ref!(raw::Constant::InterfaceMethodRef; class, name, descriptor)
        },
        ConstantInstruction::String(str) => {
            let index = report_try!(reports; cx.eval_into_utf8(str));
            raw::Constant::String(index)
        },
        ConstantInstruction::Integer(expr) => {
            raw::Constant::Integer(
                report_try!(reports; cx.eval_into_int(expr, "i32", convert::into_i32))
            )
        },
        ConstantInstruction::Float(expr) => {
            raw::Constant::Float(
                report_try!(reports; cx.eval_into_float(expr, "f32", convert::into_f32))
            )
        },
        ConstantInstruction::Long(expr) => {
            raw::Constant::Long(
                report_try!(reports; cx.eval_into_int(expr, "i64", convert::into_i64))
            )
        },
        ConstantInstruction::Double(expr) => {
            raw::Constant::Double(
                report_try!(reports; cx.eval_into_float(expr, "f64", convert::into_f64))
            )
        },
        ConstantInstruction::NameAndType(name, descriptor) => {
            let name_index = report_try!(reports; cx.eval_into_utf8(name));
            let desc_index = report_try!(reports; cx.eval_into_utf8(descriptor));
            raw::Constant::Signature {
                name: name_index, descriptor: desc_index,
            }
        }
        ConstantInstruction::Utf8(expr) => {
            match report_try!(reports; cx.eval(expr)) {
                Value::Str(str, _) => {
                    raw::Constant::Utf8(str)
                },
                other => fatal_error!(reports; "expected string, found {}", other.report_name(); other),
            }
        }
        ConstantInstruction::MethodHandle { kind, referent } => {
            let ident = report_try!(reports; cx.eval_into_bare_word(kind));
            let kind = match ident.name.as_str() {
                special::HANDLE_KIND_GET_FIELD => raw::MethodHandleKind::GetField,
                special::HANDLE_KIND_GET_STATIC => raw::MethodHandleKind::GetStatic,
                special::HANDLE_KIND_PUT_FIELD => raw::MethodHandleKind::PutStatic,
                special::HANDLE_KIND_PUT_STATIC => raw::MethodHandleKind::PutStatic,

                special::HANDLE_KIND_INVOKE_VIRTUAL => raw::MethodHandleKind::InvokeVirtual,
                special::HANDLE_KIND_INVOKE_STATIC => raw::MethodHandleKind::InvokeStatic,
                special::HANDLE_KIND_INVOKE_SPECIAL => raw::MethodHandleKind::InvokeSpecial,
                special::HANDLE_KIND_NEW_INVOKE_SPECIAL => raw::MethodHandleKind::NewInvokeSpecial,
                special::HANDLE_KIND_INVOKE_INTERFACE => raw::MethodHandleKind::InvokeInterface,
                other => fatal_error!(reports; "`{}` is not a method handle kind", other; ident)
            };

            let referent_index = report_try!(reports; cx.eval_into_const_index(referent, eval::FN_NONE));

            raw::Constant::MethodHandle {
                kind, referent: referent_index,
            }
        },
        ConstantInstruction::MethodType(expr) => {
            let index = report_try!(reports; cx.eval_into_utf8(expr));
            raw::Constant::MethodType(index)
        },
        ConstantInstruction::DynamicTarget { bootstrap_method, name, descriptor } => {
            let index = report_try!(reports; cx.eval_into_const_index(bootstrap_method, eval::FN_NONE));

            let name_and_type = if let Some(descriptor) = descriptor {
                let name_index = report_try!(reports; cx.eval_into_utf8(name));
                let desc_index = report_try!(reports; cx.eval_into_utf8(descriptor));
                cx.gen.push_expanded_constant(raw::Constant::Signature {
                    name: name_index, descriptor: desc_index,
                })
            } else {
                report_try!(reports; cx.eval_into_const_index(name, eval::FN_NONE))
            };

            raw::Constant::InvokeDynamic { bootstrap_method: index, signature: name_and_type }
        },
    };

    reports.complete(constant)
}