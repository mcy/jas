use ast::*;
use consts::instructions;
use consts::special;
use codegen::Generator;
use codegen::eval::{self, EvalContext, Value};
use codegen::convert;
use codegen::attrs;
use codegen::labels::LabelKind;
use sections::{MethodSection, CodeSection};
use reporting::*;

use classfile::raw;
use classfile::consts as flags;
use classfile::indexing::*;

use std::collections::HashMap;

pub fn expand_method(gen: &mut Generator, method: MethodSection) -> Reported<raw::Method> {

    let mut reports = Reported::new();

    let MethodSection {
        label: m_label,
        ident,
        span,
        name: m_name,
        descriptor: m_desc,
        meta: metas,
        code,
    } = method;

    // this is here because nll
    let (
        name, desc, flags, mut attrs,
        max_stack, max_locals, exceptions, stack_map_table,
        code_len, code_array,
    ) = {
        let mut cx = EvalContext::new(gen);

        // run through and construct local indices
        let mut code_len = 0;
        let mut locals = HashMap::new();
        let mut opcodes = Vec::new();
        for (i, code) in code.iter().enumerate() {
            if let Some(ref label) = code.label {
                locals.insert(label.name.clone(), LabelKind::Code(i as u16));
            }
            opcodes.push(CodeIndex(code_len));
            code_len += code.body.len(code_len as usize) as u16;
        }

        cx.with_locals(locals).with_opcodes(opcodes);


        let name = if let Some(name) = m_name {
            if let Some(label) = m_label {
                report_warning!(reports; "`method` label ignored in favor of second argument"; label);
            }
            report_try!(reports; cx.eval_into_utf8(name))
        } else {
            if let Some(label) = m_label {
                cx.gen.push_string_constant(label.name)
            } else {
                fatal_error!(reports; "`method` requires either a label or a second argument"; ident)
            }
        };

        let desc = report_try!(reports; cx.eval_into_utf8(m_desc));


        let mut flags = None;
        let mut attrs = Vec::new();

        let mut max_stack = None;
        let mut max_locals = None;
        let mut exceptions = Vec::new();

        let mut stack_map_table = Vec::new();

        for meta in metas.into_iter() {
            let span = meta.ident.span();
            match report_try!(reports; attrs::expand_method_attr(cx.gen, meta)) {
                attrs::MethodMeta::Flags(flag) => {
                    if let Some(..) = flags {
                        report_warning!(reports; "`flags` instruction shadows previous one"; span);
                    }
                    flags = Some(flag)
                },
                attrs::MethodMeta::Attr(attr) => attrs.push(attr),
                attrs::MethodMeta::Stack(val) => {
                    if let Some(..) = max_stack {
                        report_warning!(reports; "`stack` instruction shadows previous one"; span);
                    }
                    max_stack = Some(val)
                }
                attrs::MethodMeta::Locals(val) => {
                    if let Some(..) = max_locals {
                        report_warning!(reports; "`locals` instruction shadows previous one"; span);
                    }
                    max_locals = Some(val)
                }
                attrs::MethodMeta::Catch(ex) => exceptions.push(ex),
                attrs::MethodMeta::StackMap(st) => stack_map_table.push(st),
            }
        }

        let (code_len, code_array) = report_try!(reports; expand_code(cx, code));

        (
            name, desc, flags, attrs,
            max_stack, max_locals, exceptions, stack_map_table,
            code_len, code_array,
        )
    };

    if max_stack.is_some() ||
        max_locals.is_some() ||
        !exceptions.is_empty() ||
        !code_array.is_empty() {

        let name = gen.push_string_constant(flags::attribute::ATTR_CODE.into());

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
        let name_index = gen.push_string_constant(flags::attribute::ATTR_STACK_MAP_TABLE.into());
        attrs.push(raw::Attribute { name: name_index, info: raw::AttributeInfo::StackMapTable(stack_map_table), })
    }

    reports.complete(raw::Method {
        flags, name, descriptor: desc, attributes: attrs,
    })
}

#[inline]
fn expand_code(mut cx: EvalContext, code: Vec<CodeSection>) -> Reported<(u16, Vec<raw::Instruction>)> {

    let mut reports = Reported::new();

    let mut code_len = 0u16;

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
                        let index = report_try!(reports; cx.eval_into_const_index(expr, eval::FN_NONE));
                        raw::Instruction::$any_const(index)
                    },
                )*
                $(
                    CodeInstruction::$class_const(expr) => {
                        let class_index = report_try!(reports; cx.eval_into_class(expr));
                        raw::Instruction::$class_const(class_index)
                    },
                )*
                $(
                    CodeInstruction::$var(expr) => {
                        let index = VarIndex(report_try!(reports; cx.eval_into_int(expr, "u8", convert::into_u8)));
                        raw::Instruction::$var(index)
                    },
                )*
                $(
                    CodeInstruction::$jump(expr) => {
                        let index = report_try!(reports; cx.eval_into_code_offset(expr, CodeIndex(code_len)));
                        raw::Instruction::$jump(index)
                    },
                )*
                $($tokens)*
            }
        };
    }

    for code in code {
        let CodeSection { label, ident, span, body, } = code;
        let code_index = body.len(code_len as usize) as u16;
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
                    let byte = report_try!(reports; cx.eval_into_int(expr, "i8", convert::into_i8));
                    raw::Instruction::PushByte(byte)
                },
                CodeInstruction::PushShort(expr) => {
                    let short = report_try!(reports; cx.eval_into_int(expr, "i16", convert::into_i16));
                    raw::Instruction::PushShort(short)
                },

                CodeInstruction::LoadConstant(expr) => {
                    let index = report_try!(reports; cx.eval_into_const_index(expr, eval::FN_NONE));
                    if index.0 > ::std::u8::MAX as u16 {
                        report_error!(reports; "attempted to `ldc` constant wider than a byte; try `ldc_w`?"; span)
                    }
                    let half = HalfConstantIndex(index.0 as u8);
                    raw::Instruction::LoadConstant(half)
                },
                CodeInstruction::WideLoadConstant(expr) => {
                    let index = report_try!(reports; cx.eval_into_const_index(expr, eval::FN_NONE));
                    raw::Instruction::WideLoadConstant(index)
                },
                CodeInstruction::WideLoadWideConstant(expr) => {
                    let index = report_try!(reports; cx.eval_into_const_index(expr, eval::FN_NONE));
                    raw::Instruction::WideLoadWideConstant(index)
                },

                CodeInstruction::IncInt(var, val) => {
                    let var = VarIndex(report_try!(reports; cx.eval_into_int(var, "u8", convert::into_u8)));
                    let val = report_try!(reports; cx.eval_into_int(val, "i8", convert::into_i8));
                    raw::Instruction::IncInt(var, val)
                },

                CodeInstruction::LookupSwitch {
                    default_offset, match_table,
                } => {
                    let default_index = report_try!(reports; cx.eval_into_wide_code_offset(default_offset, CodeIndex(code_len)));
                    let mut match_table_indices = Vec::new();
                    for (mtch, offset) in match_table {
                        let match_index = report_try!(reports; cx.eval_into_int(mtch, "i32", convert::into_i32));
                        let offset_index = report_try!(reports; cx.eval_into_wide_code_offset(offset, CodeIndex(code_len)));
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
                    let default_index = report_try!(reports; cx.eval_into_wide_code_offset(default_offset, CodeIndex(code_len)));
                    let match_start = report_try!(reports; cx.eval_into_int(match_range.0, "i32", convert::into_i32));
                    let match_end = report_try!(reports; cx.eval_into_int(match_range.1, "i32", convert::into_i32));
                    let mut offset_indices = Vec::new();
                    for offset in offset_table {
                        let offset_index = report_try!(reports; cx.eval_into_wide_code_offset(offset, CodeIndex(code_len)));
                        offset_indices.push(offset_index);
                    }
                    raw::Instruction::TableSwitch {
                        default_offset: default_index,
                        match_range: (match_start, match_end),
                        offset_table: offset_indices,
                    }
                },

                CodeInstruction::InvokeInterface(expr, count) => {
                    let index = report_try!(reports; cx.eval_into_const_index(expr, eval::FN_NONE));
                    let val = report_try!(reports; cx.eval_into_int(count, "u8", convert::into_u8));
                    raw::Instruction::InvokeInterface(index, val)
                },

                CodeInstruction::NewPrimitiveArray(expr) => {
                    let ident = report_try!(reports; cx.eval_into_bare_word(expr));
                    let ty = match ident.name.as_str() {
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
                    };
                    raw::Instruction::NewPrimitiveArray(ty)
                }

                CodeInstruction::Wide(..) => unimplemented!(), // FIXME: deal with this...

                CodeInstruction::NewRefMultiArray(class, dims) => {
                    let class_index = report_try!(reports; cx.eval_into_class(class));
                    let dims_val = report_try!(reports; cx.eval_into_int(dims, "u8", convert::into_u8));
                    raw::Instruction::NewRefMultiArray(class_index, dims_val)
                },

                CodeInstruction::WideGoto(expr) => {
                    let index = report_try!(reports; cx.eval_into_wide_code_offset(expr, CodeIndex(code_len)));
                    raw::Instruction::WideGoto(index)
                },
                CodeInstruction::WideJumpSub(expr) => {
                    let index = report_try!(reports; cx.eval_into_wide_code_offset(expr, CodeIndex(code_len)));
                    raw::Instruction::WideJumpSub(index)
                },
        ));
        code_len += code_index;
    };

    reports.complete((code_len, code_array))
}