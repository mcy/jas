use crate::consts::instructions;
use crate::consts::flags;
use crate::consts::special;

use classfile::raw;

use std::io::prelude::*;
use std::io;
use std::io::BufReader;
use std::fs;
use std::path::Path;

use base64;

pub fn disassemble_from_file<P: AsRef<Path>>(path: P) -> io::Result<String> {

    let abs = fs::canonicalize(path)?;
    let file = fs::OpenOptions::new().read(true).open(abs.as_path())?;
    let mut bytes = Vec::<u8>::new();
    BufReader::new(file).read_to_end(&mut bytes)?;
    let class = raw::io::parse_class(&mut bytes.as_slice())?;
    Ok(disassemble_class(&abs.to_string_lossy(), &class))
}

pub fn disassemble_class(path: &str, class: &raw::Class) -> String {
    let mut lines = Vec::new();

    lines.push(format!("; disassembled from {}", path));

    lines.push(format!("{: <15} {:#06x}, {:#06x},", instructions::CLASS, class.this_class.0, class.super_class.0));
    lines.push(format!("{: <15} {}, {},", instructions::VERSION, class.minor_version, class.major_version));

    macro_rules! print_flags {
        ($flags:expr; $($func:ident, $str:expr;)+) => {{
            let mut buf = Vec::new();

            $(
                if $flags.$func() {
                    buf.push(format!("{}, ", $str));
                }
            )+

            // pop off the extra space if it exists
            let mut str = buf.join("");
            str.pop();
            str
        }}
    }

    let flags = print_flags!(class.flags;
        is_public, flags::PUBLIC;
        is_final, flags::FINAL;
        is_super, flags::SUPER;
        is_interface, flags::INTERFACE;
        is_abstract, flags::ABSTRACT;
        is_synthetic, flags::SYNTHETIC;
        is_annotation, flags::ANNOTATION;
        is_enum, flags::ENUM;
    );
    if !flags.is_empty() {
        lines.push(format!("{: <15} {}", instructions::FLAGS, flags));
    }

    let interfaces = {
        let mut buf = Vec::new();
        for iface in &class.interfaces {
            buf.push(format!("{:#06x}, ", iface.0))
        }
        buf.join("")
    };
    if !interfaces.is_empty() {
        lines.push(format!("{: <15} {}", instructions::IMPL, interfaces));
    }

    lines.push("".into());
    lines.push("".into());
    lines.push("; constant pool".into());
    lines.push("".into());

    for constant in &class.constant_pool {
        match *constant {
            raw::Constant::Class(ref index) =>
                lines.push(format!("{: <15} {:#06x}", instructions::CLASS_REF, index.0)),
            raw::Constant::FieldRef { ref class, ref signature } =>
                lines.push(format!("{: <15} {:#06x}, {:#06x},", instructions::FIELD_REF, class.0, signature.0)),
            raw::Constant::MethodRef { ref class, ref signature } =>
                lines.push(format!("{: <15} {:#06x}, {:#06x},", instructions::METHOD_REF, class.0, signature.0)),
            raw::Constant::InterfaceMethodRef { ref class, ref signature } =>
                lines.push(format!("{: <15} {:#06x}, {:#06x},", instructions::INTERFACE_METHOD_REF, class.0, signature.0)),
            raw::Constant::String(ref index) =>
                lines.push(format!("{: <15} {:#06x}", instructions::STRING, index.0)),
            raw::Constant::Integer(ref val) =>
                lines.push(format!("{: <15} {}", instructions::INTEGER, val)),
            raw::Constant::Float(ref val) =>
                lines.push(format!("{: <15} {}", instructions::FLOAT, val)),
            raw::Constant::Long(ref val) =>
                lines.push(format!("{: <15} {}", instructions::LONG, val)),
            raw::Constant::Double(ref val) =>
                lines.push(format!("{: <15} {}", instructions::DOUBLE, val)),
            raw::Constant::WidePlaceholder => {},
            raw::Constant::Signature { ref name, ref descriptor } =>
                lines.push(format!("{: <15} {:#06x}, {:#06x},", instructions::NAME_AND_TYPE, name.0, descriptor.0)),
            raw::Constant::Utf8(ref val) =>
                lines.push(format!("{: <15} \"{}\"", instructions::UTF8, val)),
            raw::Constant::MethodHandle { ref kind, ref referent } => {
                let kind = match *kind {
                    raw::MethodHandleKind::GetField => special::HANDLE_KIND_GET_FIELD,
                    raw::MethodHandleKind::GetStatic => special::HANDLE_KIND_GET_STATIC,
                    raw::MethodHandleKind::PutField => special::HANDLE_KIND_PUT_FIELD,
                    raw::MethodHandleKind::PutStatic => special::HANDLE_KIND_PUT_STATIC,
                    raw::MethodHandleKind::InvokeVirtual => special::HANDLE_KIND_INVOKE_VIRTUAL,
                    raw::MethodHandleKind::InvokeStatic => special::HANDLE_KIND_INVOKE_STATIC,
                    raw::MethodHandleKind::InvokeInterface => special::HANDLE_KIND_INVOKE_INTERFACE,
                    raw::MethodHandleKind::InvokeSpecial => special::HANDLE_KIND_INVOKE_SPECIAL,
                    raw::MethodHandleKind::NewInvokeSpecial => special::HANDLE_KIND_NEW_INVOKE_SPECIAL,
                };
                lines.push(format!("{: <15} {}, {:#06x},", instructions::METHOD_HANDLE, kind, referent.0));
            },
            raw::Constant::MethodType(ref index) =>
                lines.push(format!("{: <15} {:#06x}", instructions::METHOD_TYPE, index.0)),
            raw::Constant::InvokeDynamic { ref bootstrap_method, ref signature } =>
                lines.push(format!("{: <15} {:#06x}, {:#06x},", instructions::DYNAMIC_TARGET, bootstrap_method.0, signature.0)),
        }
    }

    for attr in &class.attributes {
        let raw::Attribute { ref name, ref info } = *attr;
        match *info {
            raw::AttributeInfo::SourceFile(ref index) =>
                lines.push(format!("{: <15} {:#06x}", instructions::SOURCE, index.0)),
            raw::AttributeInfo::BootstrapMethods(ref methods) =>
                for &raw::BootstrapMethod { ref method, ref arguments } in methods {
                    let mut buf = Vec::new();
                    buf.push(format!("{: <15} {:#06x}", instructions::BOOTSTRAP, method.0));
                    for arg in arguments {
                        buf.push(format!(", {:#06x}", arg.0));
                    }
                    if !arguments.is_empty() {
                        buf.push(",".into())
                    }
                    lines.push(buf.join(""))
                }
            raw::AttributeInfo::Other(ref bytes) =>
                lines.push(format!("{: <15} {:#06x}, \"{}\",", instructions::ATTR, name.0, base64::encode(bytes))),
            _ => {}
        }
    }

    lines.push("".into());
    lines.push("".into());
    lines.push("; fields".into());
    lines.push("".into());

    for field in &class.fields {
        let raw::Field {
            ref name,
            ref descriptor,
            ref attributes,
            ..
        } = *field;

        {
            let name = if let raw::Constant::Utf8(ref str) = class.constant_pool[name.0 as usize - 1] {
                Some(str.clone())
            } else {
                None
            };

            let desc = if let raw::Constant::Utf8(ref str) = class.constant_pool[descriptor.0 as usize - 1] {
                Some(str.clone())
            } else {
                None
            };

            if name.is_some() && desc.is_some() {
                lines.push(format!("; {}:{}", name.unwrap(), desc.unwrap()));
            }
        }

        lines.push(format!("{: <15} {:#06x}, {:#06x},", instructions::FIELD, descriptor.0, name.0));

        let flags = print_flags!(field.flags;
            is_public, flags::PUBLIC;
            is_private, flags::PRIVATE;
            is_protected, flags::PROTECTED;
            is_static, flags::STATIC;
            is_final, flags::FINAL;
            is_volatile, flags::VOLATILE;
            is_transient, flags::TRANSIENT;
            is_synthetic, flags::SYNTHETIC;
            is_enum, flags::ENUM;
        );
        if !flags.is_empty() {
            lines.push(format!("{: <15} {}", instructions::FLAGS, flags));
        }

        for attr in attributes {
            let raw::Attribute { ref name, ref info } = *attr;
            match *info {
                raw::AttributeInfo::ConstantValue(ref index) =>
                    lines.push(format!("{: <15} {:#06x}", instructions::CONST_VALUE, index.0)),
                raw::AttributeInfo::Other(ref bytes) =>
                    lines.push(format!("{: <15} {:#06x}, \"{}\",", instructions::ATTR, name.0, base64::encode(bytes))),
                _ => {}
            }
        }

        lines.push("".into());
    }

    lines.push("".into());
    lines.push("; methods".into());
    lines.push("".into());

    for method in &class.methods {
        let raw::Method {
            ref name,
            ref descriptor,
            ref attributes,
            ..
        } = *method;

        {
            let name = if let raw::Constant::Utf8(ref str) = class.constant_pool[name.0 as usize - 1] {
                Some(str.clone())
            } else {
                None
            };

            let desc = if let raw::Constant::Utf8(ref str) = class.constant_pool[descriptor.0 as usize - 1] {
                Some(str.clone())
            } else {
                None
            };

            if name.is_some() && desc.is_some() {
                lines.push(format!("; {}:{}", name.unwrap(), desc.unwrap()));
            }
        }

        lines.push(format!("{: <15} {:#06x}, {:#06x},", instructions::METHOD, descriptor.0, name.0));

        let flags = print_flags!(method.flags;
            is_public, flags::PUBLIC;
            is_private, flags::PRIVATE;
            is_protected, flags::PROTECTED;
            is_static, flags::STATIC;
            is_final, flags::FINAL;
            is_synchronized, flags::SYNCHRONIZED;
            is_bridge, flags::BRIDGE;
            is_varargs, flags::VARARGS;
            is_native, flags::NATIVE;
            is_abstract, flags::ABSTRACT;
            is_strict, flags::STRICT;
            is_synthetic, flags::SYNTHETIC;
        );
        if !flags.is_empty() {
            lines.push(format!("{: <15} {}", instructions::FLAGS, flags));
        }

        for attr in attributes {
            let raw::Attribute { ref name, ref info } = *attr;
            match *info {
                raw::AttributeInfo::Code {
                    ref max_stack, ref max_locals,
                    ref code, ref exception_table, ..
                } => {
                    lines.push(format!("{: <15} {:#06x}", instructions::STACK, max_stack.0));
                    lines.push(format!("{: <15} {:#06x}", instructions::LOCALS, max_locals.0));

                    macro_rules! code_match {
                        ($arg:expr;
                            [no_args]
                                $($no_args:ident, $no_args_const:ident),* $(,)*
                            [one_index]
                                $($one_index:ident, $one_index_const:ident),* $(,)*
                            [one_var]
                                $($one_var:ident, $one_var_const:ident),* $(,)*
                            [other]
                                $($tokens:tt)*
                        ) => {
                            match $arg {
                                $(
                                    raw::Instruction::$no_args =>
                                        lines.push(format!("{: <15}", instructions::$no_args_const)),
                                )*
                                $(
                                    raw::Instruction::$one_index(ref index) =>
                                        lines.push(format!("{: <15} {:#06x}", instructions::$one_index_const, index.0)),
                                )*
                                $(
                                    raw::Instruction::$one_var(ref index) =>
                                        lines.push(format!("{: <15} {:#04x}", instructions::$one_var_const, index.0)),
                                )*
                                $($tokens)*
                            }
                        };
                    }

                    for inst in code {
                        code_match! { *inst;
                            [no_args]
                                Nop, NOP,

                                ConstRefNull, CONST_REF_NULL,
                                ConstIntM1, CONST_INT_M1,
                                ConstInt0, CONST_INT_0,
                                ConstInt1, CONST_INT_1,
                                ConstInt2, CONST_INT_2,
                                ConstInt3, CONST_INT_3,
                                ConstInt4, CONST_INT_4,
                                ConstInt5, CONST_INT_5,
                                ConstLong0, CONST_LONG_0,
                                ConstLong1, CONST_LONG_1,
                                ConstFloat0, CONST_FLOAT_0,
                                ConstFloat1, CONST_FLOAT_1,
                                ConstFloat2, CONST_FLOAT_2,
                                ConstDouble0, CONST_DOUBLE_0,
                                ConstDouble1, CONST_DOUBLE_1,

                                LoadInt0, LOAD_INT_0,
                                LoadInt1, LOAD_INT_1,
                                LoadInt2, LOAD_INT_2,
                                LoadInt3, LOAD_INT_3,

                                LoadLong0, LOAD_INT_0,
                                LoadLong1, LOAD_INT_1,
                                LoadLong2, LOAD_INT_2,
                                LoadLong3, LOAD_INT_3,

                                LoadFloat0, LOAD_FLOAT_0,
                                LoadFloat1, LOAD_FLOAT_1,
                                LoadFloat2, LOAD_FLOAT_2,
                                LoadFloat3, LOAD_FLOAT_3,

                                LoadDouble0, LOAD_DOUBLE_0,
                                LoadDouble1, LOAD_DOUBLE_1,
                                LoadDouble2, LOAD_DOUBLE_2,
                                LoadDouble3, LOAD_DOUBLE_3,

                                LoadRef0, LOAD_REF_0,
                                LoadRef1, LOAD_REF_1,
                                LoadRef2, LOAD_REF_2,
                                LoadRef3, LOAD_REF_3,

                                ArrayLoadInt, ARRAY_LOAD_INT,
                                ArrayLoadLong, ARRAY_LOAD_LONG,
                                ArrayLoadFloat, ARRAY_LOAD_FLOAT,
                                ArrayLoadDouble, ARRAY_LOAD_DOUBLE,
                                ArrayLoadRef, ARRAY_LOAD_REF,
                                ArrayLoadByte, ARRAY_LOAD_BYTE,
                                ArrayLoadChar, ARRAY_LOAD_CHAR,
                                ArrayLoadShort, ARRAY_LOAD_SHORT,

                                StoreInt0, STORE_INT_0,
                                StoreInt1, STORE_INT_1,
                                StoreInt2, STORE_INT_2,
                                StoreInt3, STORE_INT_3,

                                StoreLong0, STORE_LONG_0,
                                StoreLong1, STORE_LONG_1,
                                StoreLong2, STORE_LONG_2,
                                StoreLong3, STORE_LONG_3,

                                StoreFloat0, STORE_FLOAT_0,
                                StoreFloat1, STORE_FLOAT_1,
                                StoreFloat2, STORE_FLOAT_2,
                                StoreFloat3, STORE_FLOAT_3,

                                StoreDouble0, STORE_DOUBLE_0,
                                StoreDouble1, STORE_DOUBLE_1,
                                StoreDouble2, STORE_DOUBLE_2,
                                StoreDouble3, STORE_DOUBLE_3,

                                StoreRef0, STORE_REF_0,
                                StoreRef1, STORE_REF_1,
                                StoreRef2, STORE_REF_2,
                                StoreRef3, STORE_REF_3,

                                ArrayStoreInt, ARRAY_STORE_INT,
                                ArrayStoreLong, ARRAY_STORE_LONG,
                                ArrayStoreFloat, ARRAY_STORE_FLOAT,
                                ArrayStoreDouble, ARRAY_STORE_DOUBLE,
                                ArrayStoreRef, ARRAY_STORE_REF,
                                ArrayStoreByte, ARRAY_STORE_BYTE,
                                ArrayStoreChar, ARRAY_STORE_CHAR,
                                ArrayStoreShort, ARRAY_STORE_SHORT,


                                Pop, POP,
                                DoublePop, DOUBLE_POP,

                                Dup, DUP,
                                DupDown, DUP_DOWN,
                                DupDoubleDown, DUP_DOUBLE_DOWN,
                                DoubleDup, DOUBLE_DUP,
                                DoubleDupDown, DOUBLE_DUP_DOWN,
                                DoubleDupDoubleDown, DOUBLE_DUP_DOUBLE_DOWN,

                                Swap, SWAP,


                                AddInt, ADD_INT,
                                AddLong, ADD_LONG,
                                AddFloat, ADD_FLOAT,
                                AddDouble, ADD_DOUBLE,

                                SubInt, SUB_INT,
                                SubLong, SUB_LONG,
                                SubFloat, SUB_FLOAT,
                                SubDouble, SUB_DOUBLE,

                                MulInt, MUL_INT,
                                MulLong, MUL_LONG,
                                MulFloat, MUL_FLOAT,
                                MulDouble, MUL_DOUBLE,

                                DivInt, DIV_INT,
                                DivLong, DIV_LONG,
                                DivFloat, DIV_FLOAT,
                                DivDouble, DIV_DOUBLE,

                                RemInt, REM_INT,
                                RemLong, REM_LONG,
                                RemFloat, REM_FLOAT,
                                RemDouble, REM_DOUBLE,

                                NegInt, NEG_INT,
                                NegLong, NEG_LONG,
                                NegFloat, NEG_FLOAT,
                                NegDouble, NEG_DOUBLE,

                                LeftShiftInt, LEFT_SHIFT_INT,
                                LeftShiftLong, LEFT_SHIFT_LONG,
                                RightShiftInt, RIGHT_SHIFT_INT,
                                RightShiftLong, RIGHT_SHIFT_LONG,
                                URightShiftInt, URIGHT_SHIFT_INT,
                                URightShiftLong, URIGHT_SHIFT_LONG,

                                AndInt, AND_INT,
                                AndLong, AND_LONG,

                                OrInt, OR_INT,
                                OrLong, OR_LONG,

                                XorInt, XOR_INT,
                                XorLong, XOR_LONG,

                                IntToLong, INT_TO_LONG,
                                IntToFloat, INT_TO_FLOAT,
                                IntToDouble, INT_TO_DOUBLE,
                                LongToInt, LONG_TO_INT,
                                LongToFloat, LONG_TO_FLOAT,
                                LongToDouble, LONG_TO_DOUBLE,
                                FloatToInt, FLOAT_TO_INT,
                                FloatToLong, FLOAT_TO_LONG,
                                FloatToDouble, FLOAT_TO_DOUBLE,
                                DoubleToInt, DOUBLE_TO_INT,
                                DoubleToLong, DOUBLE_TO_LONG,
                                DoubleToFloat, DOUBLE_TO_FLOAT,

                                IntToByte, INT_TO_BYTE,
                                IntToChar, INT_TO_CHAR,
                                IntToShort, INT_TO_SHORT,


                                CompareLong, COMPARE_LONG,
                                CompareFloatL, COMPARE_FLOAT_L,
                                CompareFloatG, COMPARE_FLOAT_G,
                                CompareDoubleL, COMPARE_DOUBLE_L,
                                CompareDoubleG, COMPARE_DOUBLE_G,

                                ReturnInt, RETURN_INT,
                                ReturnLong, RETURN_LONG,
                                ReturnFloat, RETURN_FLOAT,
                                ReturnDouble, RETURN_DOUBLE,
                                ReturnRef, RETURN_REF,
                                ReturnVoid, RETURN_VOID,

                                ArrayLen, ARRAY_LEN,
                                Throw, THROW,

                                EnterMonitor, ENTER_MONITOR,
                                ExitMonitor, EXIT_MONITOR,

                                Breakpoint, BREAKPOINT,
                                ImplementationDefined1, IMPLEMENTATION_DEFINED_1,
                                ImplementationDefined2, IMPLEMENTATION_DEFINED_2,
                            [one_index]
                                // LoadConstant, LOAD_CONSTANT,
                                WideLoadConstant, WIDE_LOAD_CONSTANT,
                                WideLoadWideConstant, WIDE_LOAD_WIDE_CONSTANT,

                                IfIntEq0, IF_INT_EQ_0,
                                IfIntNe0, IF_INT_NE_0,
                                IfIntLt0, IF_INT_LT_0,
                                IfIntGe0, IF_INT_GE_0,
                                IfIntGt0, IF_INT_GT_0,
                                IfIntLe0, IF_INT_LE_0,

                                IfIntEq, IF_INT_EQ,
                                IfIntNe, IF_INT_NE,
                                IfIntLt, IF_INT_LT,
                                IfIntGe, IF_INT_GE,
                                IfIntGt, IF_INT_GT,
                                IfIntLe, IF_INT_LE,

                                IfRefEq, IF_REF_EQ,
                                IfRefNe, IF_REF_NE,

                                Goto, GOTO,
                                JumpSub, JUMP_SUB,

                                GetStaticField, GET_STATIC_FIELD,
                                PutStaticField, PUT_STATIC_FIELD,
                                GetField, GET_FIELD,
                                PutField, PUT_FIELD,

                                InvokeDynamic, INVOKE_DYNAMIC,
                                InvokeSpecial, INVOKE_SPECIAL,
                                InvokeStatic, INVOKE_STATIC,
                                InvokeVirtual, INVOKE_VIRTUAL,

                                New, NEW,
                                NewRefArray, NEW_REF_ARRAY,

                                CheckCast, CHECK_CAST,
                                InstanceOf, INSTANCE_OF,

                                IfRefNull, IF_REF_NULL,
                                IfRefNonNull, IF_REF_NON_NULL,

                            [one_var]

                                LoadConstant, LOAD_CONSTANT,

                                LoadInt, LOAD_INT,
                                LoadLong, LOAD_LONG,
                                LoadFloat, LOAD_FLOAT,
                                LoadDouble, LOAD_DOUBLE,
                                LoadRef, LOAD_REF,

                                StoreInt, STORE_INT,
                                StoreLong, STORE_LONG,
                                StoreFloat, STORE_FLOAT,
                                StoreDouble, STORE_DOUBLE,
                                StoreRef, STORE_REF,

                                RetSub, RET_SUB,

                            [other]
                                raw::Instruction::PushByte(val) =>
                                    lines.push(format!("{: <15} {}", instructions::PUSH_BYTE, val)),
                                raw::Instruction::PushShort(val) =>
                                    lines.push(format!("{: <15} {}", instructions::PUSH_SHORT, val)),
                                raw::Instruction::IncInt(ref index, val) =>
                                    lines.push(format!("{: <15} {:#04x}, {},", instructions::INC_INT, index.0, val)),
                                raw::Instruction::LookupSwitch {
                                    ref default_offset,
                                    ref match_table,
                                } => {
                                    lines.push(format!("{: <15}   \\", instructions::LOOKUP_SWITCH));
                                    lines.push(format!("          {:#06x}, \\", default_offset.0));
                                    for &(val, ref offset) in match_table {
                                        lines.push(format!("  {:06}, {:#06x}, \\", val, offset.0));
                                    }
                                    // pop off the last \
                                    let mut last = lines.pop().unwrap();
                                    last.pop();
                                    lines.push(last);
                                },
                                raw::Instruction::TableSwitch {
                                    ref default_offset,
                                    ref match_range,
                                    ref offset_table,
                                } => {
                                    lines.push(format!("{: <15}   \\", instructions::TABLE_SWITCH));
                                    lines.push(format!("          {:#06x}, \\", default_offset.0));
                                    lines.push(format!("  {:06}, {:06}, \\", match_range.0, match_range.1));
                                    for offset in offset_table {
                                        lines.push(format!("          {:#06x}, \\", offset.0));
                                    }
                                    // pop off the last \
                                    let mut last = lines.pop().unwrap();
                                    last.pop();
                                    lines.push(last);
                                },
                                raw::Instruction::InvokeInterface(ref index, ct) =>
                                    lines.push(format!("{: <15} {:#06x}, {},", instructions::INVOKE_INTERFACE, index.0, ct)),
                                raw::Instruction::NewPrimitiveArray(ref kind) => {
                                    let kind = match *kind {
                                        raw::ArrayPrimitive::Boolean => special::ARR_BOOLEAN,
                                        raw::ArrayPrimitive::Char => special::ARR_CHAR,
                                        raw::ArrayPrimitive::Float => special::ARR_FLOAT,
                                        raw::ArrayPrimitive::Double => special::ARR_DOUBLE,
                                        raw::ArrayPrimitive::Byte => special::ARR_BYTE,
                                        raw::ArrayPrimitive::Short => special::ARR_SHORT,
                                        raw::ArrayPrimitive::Int => special::ARR_INT,
                                        raw::ArrayPrimitive::Long => special::ARR_LONG,
                                    };
                                    lines.push(format!("{: <15} {}", instructions::NEW_PRIMITIVE_ARRAY, kind));
                                }
                                raw::Instruction::NewRefMultiArray(ref index, dim) =>
                                    lines.push(format!("{: <15} {:#06x}, {:#04x},", instructions::NEW_REF_MULTI_ARRAY, index.0, dim)),
                                raw::Instruction::WideGoto(ref index) =>
                                    lines.push(format!("{: <15} {:#010x}", instructions::WIDE_GOTO, index.0)),
                                raw::Instruction::WideJumpSub(ref index) =>
                                    lines.push(format!("{: <15} {:#010x}", instructions::WIDE_JUMP_SUB, index.0)),
                                raw::Instruction::Wide(..) => unimplemented!(), // FIXME
                        }
                    }

                    for ex in exception_table {
                        lines.push(format!("{: <15} {:#06x}, {:#06x}, {:#06x}, {:#06x},", instructions::CATCH, (ex.try_range.0).0, (ex.try_range.1).0, ex.handler_goto.0, ex.exception_type.0));
                    }

                    // FIXME: handle attrs
                }
                raw::AttributeInfo::Other(ref bytes) =>
                    lines.push(format!("{: <15} {:#06x}, \"{}\",", instructions::ATTR, name.0, base64::encode(bytes))),
                _ => {}
            }
        }

        lines.push("".into());
    }

    lines.join("\n")
}
