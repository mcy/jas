
use ast::*;
use reporting::*;
use source_file::Span;

#[derive(Clone, Debug)]
pub struct Instruction {
    pub label: Option<Ident>,
    pub ident: Ident,
    pub body: InstructionBody,
    pub span: Span,
}

impl Instruction {

    pub fn from_source(line: SourceLine) -> Reported<Instruction> {
        use consts::instructions as inst;

        use self::ItemInstruction as ItemI;
        use self::MetaInstruction as MetaI;
        use self::ConstantInstruction as ConstantI;
        use self::CodeInstruction as CodeI;

        let mut reports = Reported::new();
        let mut result: Option<Instruction> = None;

        #[inline]
        fn expected_args(reports: &mut Reported<Instruction>, line: &SourceLine, min: usize, max: usize) -> bool{
            let len = line.args.len();
            if len < min {
                report_error!(reports;
                    "not enough arguments for instruction `{}`, expected {}", line.instruction.name, min;
                    line.instruction
                );
                false
            } else if len > max {
                for i in max..len {
                    report_error!(reports;
                        "unexpected argument for instruction `{}`", line.instruction.name;
                        line.args[i]
                    );
                }
                false
            } else  {
                true
            }
        }

        #[inline]
        fn min_args(reports: &mut Reported<Instruction>, line: &SourceLine, n: usize) -> bool{
            let len = line.args.len();
            if len < n {
                report_error!(reports;
                    "not enough arguments for instruction `{}`, expected {}", line.instruction.name, n;
                    line.instruction
                );
                false
            } else  {
                true
            }
        }

        // FIXME: we can probably do a little better by
        // into_iter'ing args

        macro_rules! op_tuple {
            ($name:path, $ty:ident, $min:expr, $max:expr; $($field:expr),*) => {
                op_tuple!($name, $ty, $min, $max; $($field),*;)
            };
            ($name:path, $ty:ident, $min:expr, $max:expr; $($field:expr),*; $($opt:expr),*) => {
                if expected_args(&mut reports, &line, $min, $max) {
                    let op = {
                        let mut iter = line.args.iter();
                        $name (
                            $({$field; iter.next().unwrap().clone()}),*,
                            $({$opt; iter.next().cloned()}),*
                        )
                    };
                    let SourceLine {
                        label, instruction, args, span,
                    } = line;
                    result = Some(Instruction {
                        label,
                        ident: instruction,
                        body: InstructionBody::$ty(op),
                        span,
                    });
                }
            };
        }

        macro_rules! op_struct {
            ($name:path, $ty:ident, $min:expr, $max:expr; $($field:ident),*) => {
                op_struct!($name, $ty, $min, $max; $($field),*;)
            };
            ($name:path, $ty:ident, $min:expr, $max:expr; $($field:ident),*; $($opt:ident),*) => {
                if expected_args(&mut reports, &line, $min, $max) {
                    let op = {
                        let mut iter = line.args.iter();
                        $name {
                            $($field: iter.next().unwrap().clone(),)*
                            $($opt: iter.next().cloned(),)*
                        }
                    };
                    let SourceLine {
                        label, instruction, args, span,
                    } = line;
                    result = Some(Instruction {
                        label,
                        ident: instruction,
                        body: InstructionBody::$ty(op),
                        span,
                    });
                }
            };
        }

        macro_rules! op_0 {
            ($name:path, $ty:ident) => {
                if expected_args(&mut reports, &line, 0, 0) {
                    let op = $name;
                    let SourceLine {
                        label, instruction, args, span,
                    } = line;
                    result = Some(Instruction {
                        label,
                        ident: instruction,
                        body: InstructionBody::$ty(op),
                        span,
                    });
                }
            };
        }

        macro_rules! op_1 {
            ($name:path, $ty:ident) => (op_tuple!($name, $ty, 1, 1; 0));
        }

        macro_rules! op_2 {
            ($name:path, $ty:ident) => (op_tuple!($name, $ty, 2, 2; 0, 1));
        }

        macro_rules! op_vec {
            ($name:path, $ty:ident, $min:expr) => {
                if min_args(&mut reports, &line, $min) {
                    let op = $name(line.args.clone());
                    let SourceLine {
                        label, instruction, args, span,
                    } = line;
                    result = Some(Instruction {
                        label,
                        ident: instruction,
                        body: InstructionBody::$ty(op),
                        span,
                    });
                }
            };
        }

        match line.instruction.name.clone().as_str() {
            inst::CLASS => op_struct!(ItemI::Class, Item, 2, 2; this_class, super_class),
            inst::FIELD => op_struct!(ItemI::Field, Item, 1, 2; descriptor; name),
            inst::METHOD => op_struct!(ItemI::Method, Item, 1, 2; descriptor; name),

            inst::IMPL => op_vec!(MetaI::Impl, Meta, 1),
            inst::VERSION => op_struct!(MetaI::Version, Meta, 2, 2; minor, major),

            inst::FLAGS => op_vec!(MetaI::Flags, Meta, 1),

            inst::STACK => op_1!(MetaI::Stack, Meta),
            inst::LOCALS => op_1!(MetaI::Locals, Meta),
            inst::CATCH => op_struct!(MetaI::Catch, Meta, 4, 4; start, end, handler, ty),

            inst::CONST_VALUE => op_1!(MetaI::ConstantValue, Meta),

            inst::SOURCE => op_1!(MetaI::Source, Meta),

            inst::BOOTSTRAP => op_vec!(MetaI::Bootstrap, Meta, 1),

            inst::STACK_MAP => op_vec!(MetaI::StackMap, Meta, 2),

            inst::ATTR => op_2!(MetaI::Attr, Meta),

            inst::CLASS_REF => op_1!(ConstantI::ClassRef, Constant),
            inst::FIELD_REF => op_struct!(ConstantI::FieldRef, Constant, 2, 3; class, name; descriptor),
            inst::METHOD_REF => op_struct!(ConstantI::MethodRef, Constant, 2, 3; class, name; descriptor),
            inst::INTERFACE_METHOD_REF => op_struct!(ConstantI::InterfaceMethodRef, Constant, 2, 3; class, name; descriptor),
            inst::STRING => op_1!(ConstantI::String, Constant),
            inst::INTEGER => op_1!(ConstantI::Integer, Constant),
            inst::FLOAT => op_1!(ConstantI::Float, Constant),
            inst::LONG => op_1!(ConstantI::Long, Constant),
            inst::DOUBLE => op_1!(ConstantI::Double, Constant),
            inst::NAME_AND_TYPE => op_2!(ConstantI::NameAndType, Constant),
            inst::UTF8 => op_1!(ConstantI::Utf8, Constant),
            inst::METHOD_HANDLE => op_struct!(ConstantI::MethodHandle, Constant, 2, 2; kind, referent),
            inst::METHOD_TYPE => op_1!(ConstantI::MethodType, Constant),
            inst::DYNAMIC_TARGET => op_struct!(ConstantI::DynamicTarget, Constant, 2, 3; bootstrap_method, name; descriptor),

            inst::NOP => op_0!(CodeI::Nop, Code),

            inst::CONST_REF_NULL => op_0!(CodeI::ConstRefNull, Code),
            inst::CONST_INT_M1 => op_0!(CodeI::ConstIntM1, Code),
            inst::CONST_INT_0 => op_0!(CodeI::ConstInt0, Code),
            inst::CONST_INT_1 => op_0!(CodeI::ConstInt1, Code),
            inst::CONST_INT_2 => op_0!(CodeI::ConstInt2, Code),
            inst::CONST_INT_3 => op_0!(CodeI::ConstInt3, Code),
            inst::CONST_INT_4 => op_0!(CodeI::ConstInt4, Code),
            inst::CONST_INT_5 => op_0!(CodeI::ConstInt5, Code),
            inst::CONST_LONG_0 => op_0!(CodeI::ConstLong0, Code),
            inst::CONST_LONG_1 => op_0!(CodeI::ConstLong1, Code),
            inst::CONST_FLOAT_0 => op_0!(CodeI::ConstFloat0, Code),
            inst::CONST_FLOAT_1 => op_0!(CodeI::ConstFloat1, Code),
            inst::CONST_FLOAT_2 => op_0!(CodeI::ConstFloat2, Code),
            inst::CONST_DOUBLE_0 => op_0!(CodeI::ConstDouble0, Code),
            inst::CONST_DOUBLE_1 => op_0!(CodeI::ConstDouble1, Code),

            inst::PUSH_BYTE => op_1!(CodeI::PushByte, Code),
            inst::PUSH_SHORT => op_1!(CodeI::PushShort, Code),

            inst::LOAD_CONSTANT => op_1!(CodeI::LoadConstant, Code),
            inst::WIDE_LOAD_CONSTANT => op_1!(CodeI::WideLoadConstant, Code),
            inst::WIDE_LOAD_WIDE_CONSTANT => op_1!(CodeI::WideLoadWideConstant, Code),


            inst::LOAD_INT => op_1!(CodeI::LoadInt, Code),
            inst::LOAD_LONG => op_1!(CodeI::LoadLong, Code),
            inst::LOAD_FLOAT => op_1!(CodeI::LoadFloat, Code),
            inst::LOAD_DOUBLE => op_1!(CodeI::LoadDouble, Code),
            inst::LOAD_REF => op_1!(CodeI::LoadRef, Code),

            inst::LOAD_INT_0 => op_0!(CodeI::LoadInt0, Code),
            inst::LOAD_INT_1 => op_0!(CodeI::LoadInt1, Code),
            inst::LOAD_INT_2 => op_0!(CodeI::LoadInt2, Code),
            inst::LOAD_INT_3 => op_0!(CodeI::LoadInt3, Code),

            inst::LOAD_LONG_0 => op_0!(CodeI::LoadLong0, Code),
            inst::LOAD_LONG_1 => op_0!(CodeI::LoadLong1, Code),
            inst::LOAD_LONG_2 => op_0!(CodeI::LoadLong2, Code),
            inst::LOAD_LONG_3 => op_0!(CodeI::LoadLong3, Code),

            inst::LOAD_FLOAT_0 => op_0!(CodeI::LoadFloat0, Code),
            inst::LOAD_FLOAT_1 => op_0!(CodeI::LoadFloat1, Code),
            inst::LOAD_FLOAT_2 => op_0!(CodeI::LoadFloat2, Code),
            inst::LOAD_FLOAT_3 => op_0!(CodeI::LoadFloat3, Code),

            inst::LOAD_DOUBLE_0 => op_0!(CodeI::LoadDouble0, Code),
            inst::LOAD_DOUBLE_1 => op_0!(CodeI::LoadDouble1, Code),
            inst::LOAD_DOUBLE_2 => op_0!(CodeI::LoadDouble2, Code),
            inst::LOAD_DOUBLE_3 => op_0!(CodeI::LoadDouble3, Code),

            inst::LOAD_REF_0 => op_0!(CodeI::LoadRef0, Code),
            inst::LOAD_REF_1 => op_0!(CodeI::LoadRef1, Code),
            inst::LOAD_REF_2 => op_0!(CodeI::LoadRef2, Code),
            inst::LOAD_REF_3 => op_0!(CodeI::LoadRef3, Code),

            inst::ARRAY_LOAD_INT => op_0!(CodeI::ArrayLoadInt, Code),
            inst::ARRAY_LOAD_LONG => op_0!(CodeI::ArrayLoadLong, Code),
            inst::ARRAY_LOAD_FLOAT => op_0!(CodeI::ArrayLoadFloat, Code),
            inst::ARRAY_LOAD_DOUBLE => op_0!(CodeI::ArrayLoadDouble, Code),
            inst::ARRAY_LOAD_REF => op_0!(CodeI::ArrayLoadRef, Code),
            inst::ARRAY_LOAD_BYTE => op_0!(CodeI::ArrayLoadByte, Code),
            inst::ARRAY_LOAD_CHAR => op_0!(CodeI::ArrayLoadChar, Code),
            inst::ARRAY_LOAD_SHORT => op_0!(CodeI::ArrayLoadShort, Code),


            inst::STORE_INT => op_1!(CodeI::StoreInt, Code),
            inst::STORE_LONG => op_1!(CodeI::StoreLong, Code),
            inst::STORE_FLOAT => op_1!(CodeI::StoreFloat, Code),
            inst::STORE_DOUBLE => op_1!(CodeI::StoreDouble, Code),
            inst::STORE_REF => op_1!(CodeI::StoreRef, Code),

            inst::STORE_INT_0 => op_0!(CodeI::StoreInt0, Code),
            inst::STORE_INT_1 => op_0!(CodeI::StoreInt1, Code),
            inst::STORE_INT_2 => op_0!(CodeI::StoreInt2, Code),
            inst::STORE_INT_3 => op_0!(CodeI::StoreInt3, Code),

            inst::STORE_LONG_0 => op_0!(CodeI::StoreLong0, Code),
            inst::STORE_LONG_1 => op_0!(CodeI::StoreLong1, Code),
            inst::STORE_LONG_2 => op_0!(CodeI::StoreLong2, Code),
            inst::STORE_LONG_3 => op_0!(CodeI::StoreLong3, Code),

            inst::STORE_FLOAT_0 => op_0!(CodeI::StoreFloat0, Code),
            inst::STORE_FLOAT_1 => op_0!(CodeI::StoreFloat1, Code),
            inst::STORE_FLOAT_2 => op_0!(CodeI::StoreFloat2, Code),
            inst::STORE_FLOAT_3 => op_0!(CodeI::StoreFloat3, Code),

            inst::STORE_DOUBLE_0 => op_0!(CodeI::StoreDouble0, Code),
            inst::STORE_DOUBLE_1 => op_0!(CodeI::StoreDouble1, Code),
            inst::STORE_DOUBLE_2 => op_0!(CodeI::StoreDouble2, Code),
            inst::STORE_DOUBLE_3 => op_0!(CodeI::StoreDouble3, Code),

            inst::STORE_REF_0 => op_0!(CodeI::StoreRef0, Code),
            inst::STORE_REF_1 => op_0!(CodeI::StoreRef1, Code),
            inst::STORE_REF_2 => op_0!(CodeI::StoreRef2, Code),
            inst::STORE_REF_3 => op_0!(CodeI::StoreRef3, Code),

            inst::ARRAY_STORE_INT => op_0!(CodeI::ArrayStoreInt, Code),
            inst::ARRAY_STORE_LONG => op_0!(CodeI::ArrayStoreLong, Code),
            inst::ARRAY_STORE_FLOAT => op_0!(CodeI::ArrayStoreFloat, Code),
            inst::ARRAY_STORE_DOUBLE => op_0!(CodeI::ArrayStoreDouble, Code),
            inst::ARRAY_STORE_REF => op_0!(CodeI::ArrayStoreRef, Code),
            inst::ARRAY_STORE_BYTE => op_0!(CodeI::ArrayStoreByte, Code),
            inst::ARRAY_STORE_CHAR => op_0!(CodeI::ArrayStoreChar, Code),
            inst::ARRAY_STORE_SHORT => op_0!(CodeI::ArrayStoreShort, Code),


            inst::POP => op_0!(CodeI::Pop, Code),
            inst::DOUBLE_POP => op_0!(CodeI::DoublePop, Code),

            inst::DUP => op_0!(CodeI::Dup, Code),
            inst::DUP_DOWN => op_0!(CodeI::DupDown, Code),
            inst::DUP_DOUBLE_DOWN => op_0!(CodeI::DupDoubleDown, Code),
            inst::DOUBLE_DUP => op_0!(CodeI::DoubleDup, Code),
            inst::DOUBLE_DUP_DOWN => op_0!(CodeI::DoubleDupDown, Code),
            inst::DOUBLE_DUP_DOUBLE_DOWN => op_0!(CodeI::DoubleDupDoubleDown, Code),

            inst::SWAP => op_0!(CodeI::Swap, Code),


            inst::ADD_INT => op_0!(CodeI::AddInt, Code),
            inst::ADD_LONG => op_0!(CodeI::AddLong, Code),
            inst::ADD_FLOAT => op_0!(CodeI::AddFloat, Code),
            inst::ADD_DOUBLE => op_0!(CodeI::AddDouble, Code),

            inst::SUB_INT => op_0!(CodeI::SubInt, Code),
            inst::SUB_LONG => op_0!(CodeI::SubLong, Code),
            inst::SUB_FLOAT => op_0!(CodeI::SubFloat, Code),
            inst::SUB_DOUBLE => op_0!(CodeI::SubDouble, Code),

            inst::MUL_INT => op_0!(CodeI::MulInt, Code),
            inst::MUL_LONG => op_0!(CodeI::MulLong, Code),
            inst::MUL_FLOAT => op_0!(CodeI::MulFloat, Code),
            inst::MUL_DOUBLE => op_0!(CodeI::MulDouble, Code),

            inst::DIV_INT => op_0!(CodeI::DivInt, Code),
            inst::DIV_LONG => op_0!(CodeI::DivLong, Code),
            inst::DIV_FLOAT => op_0!(CodeI::DivFloat, Code),
            inst::DIV_DOUBLE => op_0!(CodeI::DivDouble, Code),

            inst::REM_INT => op_0!(CodeI::RemInt, Code),
            inst::REM_LONG => op_0!(CodeI::RemLong, Code),
            inst::REM_FLOAT => op_0!(CodeI::RemFloat, Code),
            inst::REM_DOUBLE => op_0!(CodeI::RemDouble, Code),

            inst::NEG_INT => op_0!(CodeI::NegInt, Code),
            inst::NEG_LONG => op_0!(CodeI::NegLong, Code),
            inst::NEG_FLOAT => op_0!(CodeI::NegFloat, Code),
            inst::NEG_DOUBLE => op_0!(CodeI::NegDouble, Code),

            inst::LEFT_SHIFT_INT => op_0!(CodeI::LeftShiftInt, Code),
            inst::LEFT_SHIFT_LONG => op_0!(CodeI::LeftShiftLong, Code),
            inst::RIGHT_SHIFT_INT => op_0!(CodeI::RightShiftInt, Code),
            inst::RIGHT_SHIFT_LONG => op_0!(CodeI::RightShiftLong, Code),
            inst::URIGHT_SHIFT_INT => op_0!(CodeI::URightShiftInt, Code),
            inst::URIGHT_SHIFT_LONG => op_0!(CodeI::URightShiftLong, Code),

            inst::AND_INT => op_0!(CodeI::AndInt, Code),
            inst::AND_LONG => op_0!(CodeI::AndLong, Code),

            inst::OR_INT => op_0!(CodeI::OrInt, Code),
            inst::OR_LONG => op_0!(CodeI::OrLong, Code),

            inst::XOR_INT => op_0!(CodeI::XorInt, Code),
            inst::XOR_LONG => op_0!(CodeI::XorLong, Code),

            inst::INC_INT => op_2!(CodeI::IncInt, Code),


            inst::INT_TO_LONG => op_0!(CodeI::IntToLong, Code),
            inst::INT_TO_FLOAT => op_0!(CodeI::IntToFloat, Code),
            inst::INT_TO_DOUBLE => op_0!(CodeI::IntToDouble, Code),
            inst::LONG_TO_INT => op_0!(CodeI::LongToInt, Code),
            inst::LONG_TO_FLOAT => op_0!(CodeI::LongToFloat, Code),
            inst::LONG_TO_DOUBLE => op_0!(CodeI::LongToDouble, Code),
            inst::FLOAT_TO_INT => op_0!(CodeI::FloatToInt, Code),
            inst::FLOAT_TO_LONG => op_0!(CodeI::FloatToLong, Code),
            inst::FLOAT_TO_DOUBLE => op_0!(CodeI::FloatToDouble, Code),
            inst::DOUBLE_TO_INT => op_0!(CodeI::DoubleToInt, Code),
            inst::DOUBLE_TO_LONG => op_0!(CodeI::DoubleToLong, Code),
            inst::DOUBLE_TO_FLOAT => op_0!(CodeI::DoubleToFloat, Code),

            inst::INT_TO_BYTE => op_0!(CodeI::IntToByte, Code),
            inst::INT_TO_CHAR => op_0!(CodeI::IntToChar, Code),
            inst::INT_TO_SHORT => op_0!(CodeI::IntToShort, Code),


            inst::COMPARE_LONG => op_0!(CodeI::CompareLong, Code),
            inst::COMPARE_FLOAT_L => op_0!(CodeI::CompareFloatL, Code),
            inst::COMPARE_FLOAT_G => op_0!(CodeI::CompareFloatG, Code),
            inst::COMPARE_DOUBLE_L => op_0!(CodeI::CompareDoubleL, Code),
            inst::COMPARE_DOUBLE_G => op_0!(CodeI::CompareDoubleG, Code),

            inst::IF_INT_EQ_0 => op_1!(CodeI::IfIntEq0, Code),
            inst::IF_INT_NE_0 => op_1!(CodeI::IfIntNe0, Code),
            inst::IF_INT_LT_0 => op_1!(CodeI::IfIntLt0, Code),
            inst::IF_INT_GE_0 => op_1!(CodeI::IfIntGe0, Code),
            inst::IF_INT_GT_0 => op_1!(CodeI::IfIntGt0, Code),
            inst::IF_INT_LE_0 => op_1!(CodeI::IfIntLe0, Code),

            inst::IF_INT_EQ => op_1!(CodeI::IfIntEq, Code),
            inst::IF_INT_NE => op_1!(CodeI::IfIntNe, Code),
            inst::IF_INT_LT => op_1!(CodeI::IfIntLt, Code),
            inst::IF_INT_GE => op_1!(CodeI::IfIntGe, Code),
            inst::IF_INT_GT => op_1!(CodeI::IfIntGt, Code),
            inst::IF_INT_LE => op_1!(CodeI::IfIntLe, Code),

            inst::IF_REF_EQ => op_1!(CodeI::IfRefEq, Code),
            inst::IF_REF_NE => op_1!(CodeI::IfRefNe, Code),

            inst::GOTO => op_1!(CodeI::Goto, Code),
            inst::JUMP_SUB => op_1!(CodeI::JumpSub, Code),
            inst::RET_SUB => op_1!(CodeI::RetSub, Code),

            inst::TABLE_SWITCH => {
                if min_args(&mut reports, &line, 3) {
                    let op = {
                        let mut iter = line.args.iter();
                        let default_offset = iter.next().unwrap().clone();
                        let match_start = iter.next().unwrap().clone();
                        let match_end = iter.next().unwrap().clone();
                        let mut offset_table = Vec::new();
                        for jump in iter {
                            offset_table.push(jump.clone());
                        }
                        CodeI::TableSwitch {
                            default_offset,
                            match_range: (match_start, match_end),
                            offset_table,
                        }
                    };
                    let SourceLine {
                        label, instruction, args, span,
                    } = line;
                    result = Some(Instruction {
                        label,
                        ident: instruction,
                        body: InstructionBody::Code(op),
                        span,
                    });
                }
            },
            inst::LOOKUP_SWITCH => {
                if min_args(&mut reports, &line, 1) {
                    let op = {
                        let mut iter = line.args.iter();
                        let default_offset = iter.next().unwrap().clone();
                        let mut match_table = Vec::new();
                        loop {
                            if let Some(index) = iter.next() {
                                if let Some(jump) = iter.next() {
                                    match_table.push((index.clone(), jump.clone()))
                                } else {
                                    report_error!(reports;
                                        "expected jump offset for match table row";
                                        index.span()
                                    );
                                }
                            } else {
                                break;
                            }
                        }
                        CodeI::LookupSwitch {
                            default_offset,
                            match_table,
                        }
                    };
                    let SourceLine {
                        label, instruction, args, span,
                    } = line;
                    result = Some(Instruction {
                        label,
                        ident: instruction,
                        body: InstructionBody::Code(op),
                        span,
                    });
                }
            },

            inst::RETURN_INT => op_0!(CodeI::ReturnInt, Code),
            inst::RETURN_LONG => op_0!(CodeI::ReturnLong, Code),
            inst::RETURN_FLOAT => op_0!(CodeI::ReturnFloat, Code),
            inst::RETURN_DOUBLE => op_0!(CodeI::ReturnDouble, Code),
            inst::RETURN_REF => op_0!(CodeI::ReturnRef, Code),
            inst::RETURN_VOID => op_0!(CodeI::ReturnVoid, Code),


            inst::GET_STATIC_FIELD => op_1!(CodeI::GetStaticField, Code),
            inst::PUT_STATIC_FIELD => op_1!(CodeI::PutStaticField, Code),
            inst::GET_FIELD => op_1!(CodeI::GetField, Code),
            inst::PUT_FIELD => op_1!(CodeI::PutField, Code),

            inst::INVOKE_VIRTUAL => op_1!(CodeI::InvokeVirtual, Code),
            inst::INVOKE_SPECIAL => op_1!(CodeI::InvokeSpecial, Code),
            inst::INVOKE_STATIC => op_1!(CodeI::InvokeStatic, Code),
            inst::INVOKE_INTERFACE => op_2!(CodeI::InvokeInterface, Code),
            inst::INVOKE_DYNAMIC => op_1!(CodeI::InvokeDynamic, Code),

            inst::NEW => op_1!(CodeI::New, Code),
            inst::NEW_PRIMITIVE_ARRAY => op_1!(CodeI::NewPrimitiveArray, Code),
            inst::NEW_REF_ARRAY => op_1!(CodeI::NewRefArray, Code),

            inst::ARRAY_LEN => op_0!(CodeI::ArrayLen, Code),

            inst::THROW => op_0!(CodeI::Throw, Code),

            inst::CHECK_CAST => op_1!(CodeI::CheckCast, Code),
            inst::INSTANCE_OF => op_1!(CodeI::InstanceOf, Code),

            inst::ENTER_MONITOR => op_0!(CodeI::EnterMonitor, Code),
            inst::EXIT_MONITOR => op_0!(CodeI::ExitMonitor, Code),


            inst::WIDE => {
                // for now, we're just going to panic
                unimplemented!();
                /*use super::WideInstruction as W;
                let wide_instruction = match bytes.read_u8()? {
                    inst::LOAD_INT => W::LoadInt(bytes.read_wide_var_index()?),
                    inst::LOAD_LONG => W::LoadLong(bytes.read_wide_var_index()?),
                    inst::LOAD_FLOAT => W::LoadFloat(bytes.read_wide_var_index()?),
                    inst::LOAD_DOUBLE => W::LoadDouble(bytes.read_wide_var_index()?),
                    inst::LOAD_REF => W::LoadRef(bytes.read_wide_var_index()?),

                    inst::STORE_INT => W::StoreInt(bytes.read_wide_var_index()?),
                    inst::STORE_LONG => W::StoreLong(bytes.read_wide_var_index()?),
                    inst::STORE_FLOAT => W::StoreFloat(bytes.read_wide_var_index()?),
                    inst::STORE_DOUBLE => W::StoreDouble(bytes.read_wide_var_index()?),
                    inst::STORE_REF => W::StoreRef(bytes.read_wide_var_index()?),

                    inst::INC_INT => W::IncInt(bytes.read_wide_var_index()?, bytes.read_i16::<BigEndian>()?),

                    inst::RET_SUB => W::RetSub(bytes.read_wide_var_index()?),

                    invalid => Err(invalid_data!("invalid wide opcode {} at instruction {}", invalid, i))?,
                };
                Wide(wide_instruction)*/
            },

            inst::NEW_REF_MULTI_ARRAY => op_2!(CodeI::NewRefMultiArray, Code),
            inst::IF_REF_NULL => op_1!(CodeI::IfRefNull, Code),
            inst::IF_REF_NON_NULL => op_1!(CodeI::IfRefNonNull, Code),
            inst::WIDE_GOTO => op_1!(CodeI::WideGoto, Code),
            inst::WIDE_JUMP_SUB => op_1!(CodeI::WideJumpSub, Code),


            inst::BREAKPOINT => op_0!(CodeI::Breakpoint, Code),
            inst::IMPLEMENTATION_DEFINED_1 => op_0!(CodeI::ImplementationDefined1, Code),
            inst::IMPLEMENTATION_DEFINED_2 => op_0!(CodeI::ImplementationDefined2, Code),

            other => fatal_error!(reports; "unknown instruction `{}`", other; line.instruction),
        }

        if let Some(inst) = result {
            reports.complete(inst)
        } else {
            reports
        }
    }

    pub fn label(&self) -> &Option<Ident> {
        &self.label
    }

    pub fn ident(&self) -> &Ident {
        &self.ident
    }

    pub fn body(&self) -> &InstructionBody {
        &self.body
    }

    pub fn into_body(self) -> InstructionBody {
        self.body
    }
}

impl Spannable for Instruction {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Clone, Debug)]
pub enum InstructionBody {
    Item(ItemInstruction),
    Meta(MetaInstruction),
    Constant(ConstantInstruction),
    Code(CodeInstruction),
}

#[derive(Clone, Debug)]
pub enum ItemInstruction {
    Class {
        this_class: Expr,
        super_class: Expr,
    },
    Field {
        descriptor: Expr,
        name: Option<Expr>,
    },
    Method {
        descriptor: Expr,
        name: Option<Expr>,
    },
}

#[derive(Clone, Debug)]
pub enum MetaInstruction {
    Impl(Vec<Expr>),

    Version {
        minor: Expr,
        major: Expr,
    },

    Flags(Vec<Expr>),
    Stack(Expr),
    Locals(Expr),
    Catch {
        start: Expr,
        end: Expr,
        handler: Expr,
        ty: Expr,
    },

    ConstantValue(Expr),

    Source(Expr),

    Bootstrap(Vec<Expr>),

    StackMap(Vec<Expr>),

    Attr(Expr, Expr),
}

#[derive(Clone, Debug)]
pub enum ConstantInstruction {
    ClassRef(Expr),
    FieldRef {
        class: Expr,
        name: Expr,
        descriptor: Option<Expr>,
    },
    MethodRef {
        class: Expr,
        name: Expr,
        descriptor: Option<Expr>,
    },
    InterfaceMethodRef {
        class: Expr,
        name: Expr,
        descriptor: Option<Expr>,
    },
    String(Expr),
    Integer(Expr),
    Float(Expr),
    Long(Expr),
    Double(Expr),
    NameAndType(Expr, Expr),
    Utf8(Expr),
    MethodHandle {
        kind: Expr,
        referent: Expr
    },
    MethodType(Expr),
    DynamicTarget{
        bootstrap_method: Expr,
        name: Expr,
        descriptor: Option<Expr>,
    },
}

#[derive(Clone, Debug)]
pub enum CodeInstruction {
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

    PushByte(Expr),
    PushShort(Expr),

    LoadConstant(Expr),
    WideLoadConstant(Expr),
    WideLoadWideConstant(Expr),


    LoadInt(Expr),
    LoadLong(Expr),
    LoadFloat(Expr),
    LoadDouble(Expr),
    LoadRef(Expr),

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


    StoreInt(Expr),
    StoreLong(Expr),
    StoreFloat(Expr),
    StoreDouble(Expr),
    StoreRef(Expr),

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

    IncInt(Expr, Expr),


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

    IfIntEq0(Expr),
    IfIntNe0(Expr),
    IfIntLt0(Expr),
    IfIntGe0(Expr),
    IfIntGt0(Expr),
    IfIntLe0(Expr),

    IfIntEq(Expr),
    IfIntNe(Expr),
    IfIntLt(Expr),
    IfIntGe(Expr),
    IfIntGt(Expr),
    IfIntLe(Expr),

    IfRefEq(Expr),
    IfRefNe(Expr),

    Goto(Expr),
    JumpSub(Expr),
    RetSub(Expr),

    LookupSwitch {
        default_offset: Expr,
        match_table: Vec<(Expr, Expr)>
    },

    TableSwitch {
        default_offset: Expr,
        match_range: (Expr, Expr),
        offset_table: Vec<Expr>,
    },

    ReturnInt,
    ReturnLong,
    ReturnFloat,
    ReturnDouble,
    ReturnRef,
    ReturnVoid,

    GetStaticField(Expr),
    PutStaticField(Expr),
    GetField(Expr),
    PutField(Expr),

    InvokeDynamic(Expr),
    InvokeInterface(Expr, Expr),
    InvokeSpecial(Expr),
    InvokeStatic(Expr),
    InvokeVirtual(Expr),

    New(Expr),
    NewPrimitiveArray(Expr),
    NewRefArray(Expr),

    ArrayLen,

    Throw,

    CheckCast(Expr),
    InstanceOf(Expr),

    EnterMonitor,
    ExitMonitor,

    Wide(Expr),

    NewRefMultiArray(Expr, Expr),
    IfRefNull(Expr),
    IfRefNonNull(Expr),
    WideGoto(Expr),
    WideJumpSub(Expr),


    Breakpoint,
    ImplementationDefined1,
    ImplementationDefined2,
}

impl CodeInstruction {

    #[inline]
    pub fn len(&self, prev: usize) -> usize {
        use self::CodeInstruction::*;
        match *self {


            PushByte(..) => 2,
            PushShort(..) => 3,

            LoadConstant(..) => 2,
            WideLoadConstant(..) => 3,
            WideLoadWideConstant(..) => 3,


            LoadInt(..) => 2,
            LoadLong(..) => 2,
            LoadFloat(..) => 2,
            LoadDouble(..) => 2,
            LoadRef(..) => 2,


            StoreInt(..) => 2,
            StoreLong(..) => 2,
            StoreFloat(..) => 2,
            StoreDouble(..) => 2,
            StoreRef(..) => 2,

            IfIntEq0(..) => 3,
            IfIntNe0(..) => 3,
            IfIntLt0(..) => 3,
            IfIntGe0(..) => 3,
            IfIntGt0(..) => 3,
            IfIntLe0(..) => 3,

            IfIntEq(..) => 3,
            IfIntNe(..) => 3,
            IfIntLt(..) => 3,
            IfIntGe(..) => 3,
            IfIntGt(..) => 3,
            IfIntLe(..) => 3,

            IfRefEq(..) => 3,
            IfRefNe(..) => 3,

            Goto(..) => 3,
            JumpSub(..) => 3,
            RetSub(..) => 2,

            LookupSwitch {
                ref default_offset,
                ref match_table
            } => {
                let mut bytes = prev + 1;
                while bytes % 4 != 0 {
                    bytes += 1;
                }
                bytes += 8;
                bytes += 8 * match_table.len();
                bytes - prev
            },

            TableSwitch {
                ref default_offset,
                ref match_range,
                ref offset_table,
            } => {
                let mut bytes = prev + 1;
                while bytes % 4 != 0 {
                    bytes += 1;
                }
                bytes += 12;
                bytes += 4 * offset_table.len();
                bytes - prev
            },

            GetStaticField(..) => 3,
            PutStaticField(..) => 3,
            GetField(..) => 3,
            PutField(..) => 3,

            InvokeDynamic(..) => 5,
            InvokeInterface(..) => 5,
            InvokeSpecial(..) => 3,
            InvokeStatic(..) => 3,
            InvokeVirtual(..) => 3,

            New(..) => 3,
            NewPrimitiveArray(..) => 2,
            NewRefArray(..) => 3,

            CheckCast(..) => 3,
            InstanceOf(..) => 3,

            Wide(..) => unimplemented!(), // FIXME

            NewRefMultiArray(..) => 4,
            IfRefNull(..) => 3,
            IfRefNonNull(..) => 3,
            WideGoto(..) => 5,
            WideJumpSub(..) => 5,

            _ => 1
        }
    }
}