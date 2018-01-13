use lexer::{Token, TokenType as TT};
use reporting::*;

use std::mem;

#[derive(Clone, Debug)]
pub struct Instruction {
    label: Option<Ident>,
    ident: Ident,
    instruction: InstructionType,
    span: Span,
}

impl Instruction {

    pub fn from_source(line: SourceLine) -> Reported<Instruction> {
        use consts::instructions as inst;
        use self::InstructionType::*;

        let mut reports = Reported::new();
        let mut result: Option<Instruction> = None;

        #[inline]
        fn expected_args(reports: &mut Reported<Instruction>, line: &SourceLine, min: usize, max: usize) -> bool{
            let len = line.args().len();
            if len < min {
                reports.report(report_error!(
                "not enough arguments for instruction `{}`, expected {}", line.instruction().value(), min;
                line.instruction().span()
            ));
                false
            } else if len > max {
                for i in max..len {
                    reports.report(report_error!(
                    "unexpected argument for instruction `{}`", line.instruction().value();
                    line.args()[i].span()
                ));
                }
                false
            } else  {
                true
            }
        }

        #[inline]
        fn min_args(reports: &mut Reported<Instruction>, line: &SourceLine, n: usize) -> bool{
            let len = line.args().len();
            if len < n {
                    reports.report(report_error!(
                    "not enough arguments for instruction `{}`, expected {}", line.instruction().value(), n;
                    line.instruction().span()
                ));
                false
            } else  {
                true
            }
        }

        // FIXME: we can probably do a little better by
        // into_iter'ing args

        macro_rules! op_tuple {
            ($name:ident, $min:expr, $max:expr; $($field:expr),*) => {
                op_tuple!($name, $min, $max; $($field),*;)
            };
            ($name:ident, $min:expr, $max:expr; $($field:expr),*; $($opt:expr),*) => {
                if expected_args(&mut reports, &line, $min, $max) {
                    let op = {
                        let mut iter = line.args().iter();
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
                        instruction: op,
                        span,
                    });
                }
            };
        }

        macro_rules! op_struct {
            ($name:ident, $min:expr, $max:expr; $($field:ident),*) => {
                op_struct!($name, $min, $max; $($field),*;)
            };
            ($name:ident, $min:expr, $max:expr; $($field:ident),*; $($opt:ident),*) => {
                if expected_args(&mut reports, &line, $min, $max) {
                    let op = {
                        let mut iter = line.args().iter();
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
                        instruction: op,
                        span,
                    });
                }
            };
        }

        macro_rules! op_0 {
            ($name:ident) => {
                if expected_args(&mut reports, &line, 0, 0) {
                    let op = $name;
                    let SourceLine {
                        label, instruction, args, span,
                    } = line;
                    result = Some(Instruction {
                        label,
                        ident: instruction,
                        instruction: op,
                        span,
                    });
                }
            };
        }

        macro_rules! op_1 {
            ($name:ident) => (op_tuple!($name, 1, 1; 0));
        }

        macro_rules! op_2 {
            ($name:ident) => (op_tuple!($name, 2, 2; 0, 1));
        }

        match line.instruction().value().clone().as_str() {
            inst::CLASS => op_1!(Class),
            inst::FIELD => op_struct!(Field, 1, 2; descriptor; name),
            inst::METHOD => op_struct!(Field, 1, 2; descriptor; name),

            inst::SUPER => op_1!(Super),
            inst::IMPL => op_1!(Impl),
            inst::VERSION => op_1!(Version),

            inst::FLAGS => {
                if min_args(&mut reports, &line, 1) {
                    let op = Flags(line.args().clone());
                    let SourceLine {
                        label, instruction, args, span,
                    } = line;
                    result = Some(Instruction {
                        label,
                        ident: instruction,
                        instruction: op,
                        span,
                    });
                }
            }
            inst::STACK => op_1!(Stack),
            inst::LOCALS => op_1!(Locals),
            inst::CATCH => op_struct!(Catch, 4, 4; start, end, handler, ty),

            inst::ATTR => op_2!(Attr),
            
            inst::CLASS_REF => op_1!(ClassRef),
            inst::FIELD_REF => op_struct!(FieldRef, 2, 3; class, name; descriptor),
            inst::METHOD_REF => op_struct!(MethodRef, 2, 3; class, name; descriptor),
            inst::INTERFACE_METHOD_REF => op_struct!(InterfaceMethodRef, 2, 3; class, name; descriptor),
            inst::STRING => op_1!(String),
            inst::INTEGER => op_1!(Integer),
            inst::FLOAT => op_1!(Float),
            inst::LONG => op_1!(Long),
            inst::DOUBLE => op_1!(Double),
            inst::NAME_AND_TYPE => op_2!(NameAndType),
            inst::UTF8 => op_1!(Utf8),
            inst::METHOD_HANDLE => op_struct!(MethodHandle, 2, 2; kind, referent),
            inst::METHOD_TYPE => op_1!(MethodType),
            inst::DYNAMIC_TARGET => op_struct!(DynamicTarget, 2, 3; bootstrap_method, name; descriptor),

            inst::NOP => op_0!(Nop),

            inst::CONST_REF_NULL => op_0!(ConstRefNull),
            inst::CONST_INT_M1 => op_0!(ConstIntM1),
            inst::CONST_INT_0 => op_0!(ConstInt0),
            inst::CONST_INT_1 => op_0!(ConstInt1),
            inst::CONST_INT_2 => op_0!(ConstInt2),
            inst::CONST_INT_3 => op_0!(ConstInt3),
            inst::CONST_INT_4 => op_0!(ConstInt4),
            inst::CONST_INT_5 => op_0!(ConstInt5),
            inst::CONST_LONG_0 => op_0!(ConstLong0),
            inst::CONST_LONG_1 => op_0!(ConstLong1),
            inst::CONST_FLOAT_0 => op_0!(ConstFloat0),
            inst::CONST_FLOAT_1 => op_0!(ConstFloat1),
            inst::CONST_FLOAT_2 => op_0!(ConstFloat2),
            inst::CONST_DOUBLE_0 => op_0!(ConstDouble0),
            inst::CONST_DOUBLE_1 => op_0!(ConstDouble1),

            inst::PUSH_BYTE => op_1!(PushByte),
            inst::PUSH_SHORT => op_1!(PushShort),

            inst::LOAD_CONSTANT => op_1!(LoadConstant),
            inst::WIDE_LOAD_CONSTANT => op_1!(WideLoadConstant),
            inst::WIDE_LOAD_WIDE_CONSTANT => op_1!(WideLoadWideConstant),


            inst::LOAD_INT => op_1!(LoadInt),
            inst::LOAD_LONG => op_1!(LoadLong),
            inst::LOAD_FLOAT => op_1!(LoadFloat),
            inst::LOAD_DOUBLE => op_1!(LoadDouble),
            inst::LOAD_REF => op_1!(LoadRef),

            inst::LOAD_INT_0 => op_0!(LoadInt0),
            inst::LOAD_INT_1 => op_0!(LoadInt1),
            inst::LOAD_INT_2 => op_0!(LoadInt2),
            inst::LOAD_INT_3 => op_0!(LoadInt3),

            inst::LOAD_LONG_0 => op_0!(LoadLong0),
            inst::LOAD_LONG_1 => op_0!(LoadLong1),
            inst::LOAD_LONG_2 => op_0!(LoadLong2),
            inst::LOAD_LONG_3 => op_0!(LoadLong3),

            inst::LOAD_FLOAT_0 => op_0!(LoadFloat0),
            inst::LOAD_FLOAT_1 => op_0!(LoadFloat1),
            inst::LOAD_FLOAT_2 => op_0!(LoadFloat2),
            inst::LOAD_FLOAT_3 => op_0!(LoadFloat3),

            inst::LOAD_DOUBLE_0 => op_0!(LoadDouble0),
            inst::LOAD_DOUBLE_1 => op_0!(LoadDouble1),
            inst::LOAD_DOUBLE_2 => op_0!(LoadDouble2),
            inst::LOAD_DOUBLE_3 => op_0!(LoadDouble3),

            inst::LOAD_REF_0 => op_0!(LoadRef0),
            inst::LOAD_REF_1 => op_0!(LoadRef1),
            inst::LOAD_REF_2 => op_0!(LoadRef2),
            inst::LOAD_REF_3 => op_0!(LoadRef3),

            inst::ARRAY_LOAD_INT => op_0!(ArrayLoadInt),
            inst::ARRAY_LOAD_LONG => op_0!(ArrayLoadLong),
            inst::ARRAY_LOAD_FLOAT => op_0!(ArrayLoadFloat),
            inst::ARRAY_LOAD_DOUBLE => op_0!(ArrayLoadDouble),
            inst::ARRAY_LOAD_REF => op_0!(ArrayLoadRef),
            inst::ARRAY_LOAD_BYTE => op_0!(ArrayLoadByte),
            inst::ARRAY_LOAD_CHAR => op_0!(ArrayLoadChar),
            inst::ARRAY_LOAD_SHORT => op_0!(ArrayLoadShort),


            inst::STORE_INT => op_1!(StoreInt),
            inst::STORE_LONG => op_1!(StoreLong),
            inst::STORE_FLOAT => op_1!(StoreFloat),
            inst::STORE_DOUBLE => op_1!(StoreDouble),
            inst::STORE_REF => op_1!(StoreRef),

            inst::STORE_INT_0 => op_0!(StoreInt0),
            inst::STORE_INT_1 => op_0!(StoreInt1),
            inst::STORE_INT_2 => op_0!(StoreInt2),
            inst::STORE_INT_3 => op_0!(StoreInt3),

            inst::STORE_LONG_0 => op_0!(StoreLong0),
            inst::STORE_LONG_1 => op_0!(StoreLong1),
            inst::STORE_LONG_2 => op_0!(StoreLong2),
            inst::STORE_LONG_3 => op_0!(StoreLong3),

            inst::STORE_FLOAT_0 => op_0!(StoreFloat0),
            inst::STORE_FLOAT_1 => op_0!(StoreFloat1),
            inst::STORE_FLOAT_2 => op_0!(StoreFloat2),
            inst::STORE_FLOAT_3 => op_0!(StoreFloat3),

            inst::STORE_DOUBLE_0 => op_0!(StoreDouble0),
            inst::STORE_DOUBLE_1 => op_0!(StoreDouble1),
            inst::STORE_DOUBLE_2 => op_0!(StoreDouble2),
            inst::STORE_DOUBLE_3 => op_0!(StoreDouble3),

            inst::STORE_REF_0 => op_0!(StoreRef0),
            inst::STORE_REF_1 => op_0!(StoreRef1),
            inst::STORE_REF_2 => op_0!(StoreRef2),
            inst::STORE_REF_3 => op_0!(StoreRef3),

            inst::ARRAY_STORE_INT => op_0!(ArrayStoreInt),
            inst::ARRAY_STORE_LONG => op_0!(ArrayStoreLong),
            inst::ARRAY_STORE_FLOAT => op_0!(ArrayStoreFloat),
            inst::ARRAY_STORE_DOUBLE => op_0!(ArrayStoreDouble),
            inst::ARRAY_STORE_REF => op_0!(ArrayStoreRef),
            inst::ARRAY_STORE_BYTE => op_0!(ArrayStoreByte),
            inst::ARRAY_STORE_CHAR => op_0!(ArrayStoreChar),
            inst::ARRAY_STORE_SHORT => op_0!(ArrayStoreShort),


            inst::POP => op_0!(Pop),
            inst::DOUBLE_POP => op_0!(DoublePop),

            inst::DUP => op_0!(Dup),
            inst::DUP_DOWN => op_0!(DupDown),
            inst::DUP_DOUBLE_DOWN => op_0!(DupDoubleDown),
            inst::DOUBLE_DUP => op_0!(DoubleDup),
            inst::DOUBLE_DUP_DOWN => op_0!(DoubleDupDown),
            inst::DOUBLE_DUP_DOUBLE_DOWN => op_0!(DoubleDupDoubleDown),

            inst::SWAP => op_0!(Swap),


            inst::ADD_INT => op_0!(AddInt),
            inst::ADD_LONG => op_0!(AddLong),
            inst::ADD_FLOAT => op_0!(AddFloat),
            inst::ADD_DOUBLE => op_0!(AddDouble),

            inst::SUB_INT => op_0!(SubInt),
            inst::SUB_LONG => op_0!(SubLong),
            inst::SUB_FLOAT => op_0!(SubFloat),
            inst::SUB_DOUBLE => op_0!(SubDouble),

            inst::MUL_INT => op_0!(MulInt),
            inst::MUL_LONG => op_0!(MulLong),
            inst::MUL_FLOAT => op_0!(MulFloat),
            inst::MUL_DOUBLE => op_0!(MulDouble),

            inst::DIV_INT => op_0!(DivInt),
            inst::DIV_LONG => op_0!(DivLong),
            inst::DIV_FLOAT => op_0!(DivFloat),
            inst::DIV_DOUBLE => op_0!(DivDouble),

            inst::REM_INT => op_0!(RemInt),
            inst::REM_LONG => op_0!(RemLong),
            inst::REM_FLOAT => op_0!(RemFloat),
            inst::REM_DOUBLE => op_0!(RemDouble),

            inst::NEG_INT => op_0!(NegInt),
            inst::NEG_LONG => op_0!(NegLong),
            inst::NEG_FLOAT => op_0!(NegFloat),
            inst::NEG_DOUBLE => op_0!(NegDouble),

            inst::LEFT_SHIFT_INT => op_0!(LeftShiftInt),
            inst::LEFT_SHIFT_LONG => op_0!(LeftShiftLong),
            inst::RIGHT_SHIFT_INT => op_0!(RightShiftInt),
            inst::RIGHT_SHIFT_LONG => op_0!(RightShiftLong),
            inst::URIGHT_SHIFT_INT => op_0!(URightShiftInt),
            inst::URIGHT_SHIFT_LONG => op_0!(URightShiftLong),

            inst::AND_INT => op_0!(AndInt),
            inst::AND_LONG => op_0!(AndLong),

            inst::OR_INT => op_0!(OrInt),
            inst::OR_LONG => op_0!(OrLong),

            inst::XOR_INT => op_0!(XorInt),
            inst::XOR_LONG => op_0!(XorLong),

            inst::INC_INT => op_2!(IncInt),


            inst::INT_TO_LONG => op_0!(IntToLong),
            inst::INT_TO_FLOAT => op_0!(IntToFloat),
            inst::INT_TO_DOUBLE => op_0!(IntToDouble),
            inst::LONG_TO_INT => op_0!(LongToInt),
            inst::LONG_TO_FLOAT => op_0!(LongToFloat),
            inst::LONG_TO_DOUBLE => op_0!(LongToDouble),
            inst::FLOAT_TO_INT => op_0!(FloatToInt),
            inst::FLOAT_TO_LONG => op_0!(FloatToLong),
            inst::FLOAT_TO_DOUBLE => op_0!(FloatToDouble),
            inst::DOUBLE_TO_INT => op_0!(DoubleToInt),
            inst::DOUBLE_TO_LONG => op_0!(DoubleToLong),
            inst::DOUBLE_TO_FLOAT => op_0!(DoubleToFloat),

            inst::INT_TO_BYTE => op_0!(IntToByte),
            inst::INT_TO_CHAR => op_0!(IntToChar),
            inst::INT_TO_SHORT => op_0!(IntToShort),


            inst::COMPARE_LONG => op_0!(CompareLong),
            inst::COMPARE_FLOAT_L => op_0!(CompareFloatL),
            inst::COMPARE_FLOAT_G => op_0!(CompareFloatG),
            inst::COMPARE_DOUBLE_L => op_0!(CompareDoubleL),
            inst::COMPARE_DOUBLE_G => op_0!(CompareDoubleG),

            inst::IF_INT_EQ_0 => op_1!(IfIntEq0),
            inst::IF_INT_NE_0 => op_1!(IfIntNe0),
            inst::IF_INT_LT_0 => op_1!(IfIntLt0),
            inst::IF_INT_GE_0 => op_1!(IfIntGe0),
            inst::IF_INT_GT_0 => op_1!(IfIntGt0),
            inst::IF_INT_LE_0 => op_1!(IfIntLe0),

            inst::IF_INT_EQ => op_1!(IfIntEq),
            inst::IF_INT_NE => op_1!(IfIntNe),
            inst::IF_INT_LT => op_1!(IfIntLt),
            inst::IF_INT_GE => op_1!(IfIntGe),
            inst::IF_INT_GT => op_1!(IfIntGt),
            inst::IF_INT_LE => op_1!(IfIntLe),

            inst::IF_REF_EQ => op_1!(IfRefEq),
            inst::IF_REF_NE => op_1!(IfRefNe),

            inst::GOTO => op_1!(Goto),
            inst::JUMP_SUB => op_1!(JumpSub),
            inst::RET_SUB => op_1!(RetSub),

            inst::TABLE_SWITCH => {
                if min_args(&mut reports, &line, 3) {
                    let op = {
                        let mut iter = line.args().iter();
                        let default_offset = iter.next().unwrap().clone();
                        let match_start = iter.next().unwrap().clone();
                        let match_end = iter.next().unwrap().clone();
                        let mut offset_table = Vec::new();
                        for jump in iter {
                            offset_table.push(jump.clone());
                        }
                        TableSwitch {
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
                        instruction: op,
                        span,
                    });
                }
            },
            inst::LOOKUP_SWITCH => {
                if min_args(&mut reports, &line, 1) {
                    let op = {
                        let mut iter = line.args().iter();
                        let default_offset = iter.next().unwrap().clone();
                        let mut match_table = Vec::new();
                        loop {
                            if let Some(index) = iter.next() {
                                if let Some(jump) = iter.next() {
                                    match_table.push((index.clone(), jump.clone()))
                                } else {
                                    reports.report(report_error!(
                                    "expected jump offset for match table row";
                                    index.span()
                                ));
                                }
                            } else {
                                break;
                            }
                        }
                        LookupSwitch {
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
                        instruction: op,
                        span,
                    });
                }
            },

            inst::RETURN_INT => op_0!(ReturnInt),
            inst::RETURN_LONG => op_0!(ReturnLong),
            inst::RETURN_FLOAT => op_0!(ReturnFloat),
            inst::RETURN_DOUBLE => op_0!(ReturnDouble),
            inst::RETURN_REF => op_0!(ReturnRef),
            inst::RETURN_VOID => op_0!(ReturnVoid),


            inst::GET_STATIC_FIELD => op_1!(GetStaticField),
            inst::PUT_STATIC_FIELD => op_1!(PutStaticField),
            inst::GET_FIELD => op_1!(GetField),
            inst::PUT_FIELD => op_1!(PutField),

            inst::INVOKE_VIRTUAL => op_1!(InvokeVirtual),
            inst::INVOKE_SPECIAL => op_1!(InvokeSpecial),
            inst::INVOKE_STATIC => op_1!(InvokeStatic),
            inst::INVOKE_INTERFACE => op_2!(InvokeInterface),
            inst::INVOKE_DYNAMIC => op_1!(InvokeDynamic),

            inst::NEW => op_1!(New),
            inst::NEW_PRIMITIVE_ARRAY => op_1!(NewPrimitiveArray),
            inst::NEW_REF_ARRAY => op_1!(NewRefArray),

            inst::ARRAY_LEN => op_0!(ArrayLen),

            inst::THROW => op_0!(Throw),

            inst::CHECK_CAST => op_1!(CheckCast),
            inst::INSTANCE_OF => op_1!(InstanceOf),

            inst::ENTER_MONITOR => op_0!(EnterMonitor),
            inst::EXIT_MONITOR => op_0!(ExitMonitor),


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

            inst::NEW_REF_MULTI_ARRAY => op_2!(NewRefMultiArray),
            inst::IF_REF_NULL => op_1!(IfRefNull),
            inst::IF_REF_NON_NULL => op_1!(IfRefNonNull),
            inst::WIDE_GOTO => op_1!(WideGoto),
            inst::WIDE_JUMP_SUB => op_1!(WideJumpSub),


            inst::BREAKPOINT => op_0!(Breakpoint),
            inst::IMPLEMENTATION_DEFINED_1 => op_0!(ImplementationDefined1),
            inst::IMPLEMENTATION_DEFINED_2 => op_0!(ImplementationDefined2),

            other => fatal_error!(reports; "unknown instruction `{}`", other),
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

    pub fn instruction(&self) -> &InstructionType {
        &self.instruction
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn into_instruction(self) -> InstructionType {
        self.instruction
    }
}

#[derive(Clone, Debug)]
pub enum InstructionType {
    Class(Expr),
    Field {
        descriptor: Expr,
        name: Option<Expr>,
    },
    Method {
        descriptor: Expr,
        name: Option<Expr>,
    },

    Super(Expr),
    Impl(Expr),

    Version(Expr),

    Flags(Vec<Expr>),
    Stack(Expr),
    Locals(Expr),
    Catch {
        start: Expr,
        end: Expr,
        handler: Expr,
        ty: Expr,
    },

    Attr(Expr, Expr),

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

    Wide(Expr, ),

    NewRefMultiArray(Expr, Expr),
    IfRefNull(Expr),
    IfRefNonNull(Expr),
    WideGoto(Expr),
    WideJumpSub(Expr),


    Breakpoint,
    ImplementationDefined1,
    ImplementationDefined2,
}

impl InstructionType {

    pub fn category(&self) -> InstructionCategory {
        use self::InstructionType::*;
        match *self {
            Class(..) | Field {..} | Method {..} => InstructionCategory::Item,
            Super(..) | Impl(..) | Version(..) |
            Flags(..) | Stack(..) | Locals(..) |
            Catch {..} | Attr (..) => InstructionCategory::Meta,
            ClassRef(..) | FieldRef {..} | MethodRef {..} |
            InterfaceMethodRef {..} | String(..) | Integer(..) |
            Float(..) | Double(..) | NameAndType(..) |
            Utf8(..) | MethodHandle {..} | MethodType(..) |
            DynamicTarget {..} => InstructionCategory::Constant,
            _ => InstructionCategory::Code,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum InstructionCategory {
    Item, Meta, Constant, Code,
}

#[derive(Clone, Debug)]
pub struct SourceLine {
    label: Option<Ident>,
    instruction: Ident,
    args: Vec<Expr>,
    span: Span,
}

impl SourceLine {

    pub fn label(&self) -> &Option<Ident> {
        &self.label
    }

    pub fn instruction(&self) -> &Ident {
        &self.instruction
    }

    pub fn args(&self) -> &Vec<Expr> {
        &self.args
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Clone, Debug)]
pub enum Expr {
    Ref(Ident),
    Int(IntLit),
    Float(FloatLit),
    Str(StrLit),
    Char(CharLit),
    Bracket(Box<Instruction>),
    BinOp(Box<Expr>, BinOpKind, Box<Expr>),
    //UnOp(UnOpKind, Box<Expr>),
}

impl Expr {

    pub fn span(&self) -> Span {
        match *self {
            Expr::Ref(ref ident) => ident.span(),
            Expr::Int(ref int) => int.span(),
            Expr::Float(ref float) => float.span(),
            Expr::Str(ref str) => str.span(),
            Expr::Char(ref char) => char.span(),
            Expr::Bracket(ref line) => line.span(), // FIXME
            Expr::BinOp(ref first, _, ref second) => first.span().extend_to(second.span().end),
            //Expr::UnOp(_, ref expr) => expr.span(), // FIXME
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum BinOpKind {
    Add, Sub, Mul, Div,
    Rem, And, Or, Xor,
}

impl BinOpKind {

    pub fn from_str(val: &str) -> Option<BinOpKind> {
        use self::BinOpKind as K;
        match val {
            "+" => Some(K::Add),
            "-" => Some(K::Sub),
            "*" => Some(K::Mul),
            "/" => Some(K::Div),
            "%" => Some(K::Rem),
            "&" => Some(K::And),
            "|" => Some(K::Or),
            "^" => Some(K::Xor),
            _ => None,
        }
    }
}

use std::cmp::Ordering;
impl PartialOrd for BinOpKind {
    #[inline]
    fn partial_cmp(&self, other: &BinOpKind) -> Option<Ordering> {
        #[inline]
        fn precedence(val: BinOpKind) -> i8 {
            use self::BinOpKind as K;
            match val {
                K::And | K::Or | K::Xor => 0,
                K::Add | K::Sub => 1,
                K::Mul | K::Div | K::Rem => 2,
            }
        }

        if self == other {
            Some(Ordering::Equal)
        } else {
            match precedence(*self).cmp(&precedence(*other)) {
                Ordering::Equal => None,
                x => Some(x),
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum UnOpKind {
    Plus, Neg, Not
}

#[derive(Clone, Debug)]
pub struct Ident {
    name: String,
    span: Span,
}

impl Ident {

    pub fn value(&self) -> &String {
        &self.name
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Clone, Debug)]
pub struct IntLit {
    value: i64,
    span: Span,
}

impl IntLit {

    pub fn value(&self) -> i64 {
        self.value
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Clone, Debug)]
pub struct FloatLit {
    value: f64,
    span: Span,
}

impl FloatLit {

    pub fn value(&self) -> f64 {
        self.value
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Clone, Debug)]
pub struct StrLit {
    value: String,
    span: Span,
}

impl StrLit {

    pub fn value(&self) -> &String {
        &self.value
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Clone, Debug)]
pub struct CharLit {
    value: u16, // java chars, utf16!
    span: Span,
}

impl CharLit {

    pub fn value(&self) -> u16 {
        self.value
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

pub struct Parser {
    token_stack: Vec<Token>,
}

impl Parser {

    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            token_stack: tokens.into_iter().rev().collect(),
        }
    }

    fn pop_token(&mut self, what: &str) -> Reported<Token> {

        let mut reports = Reported::new();

        if let Some(token) = self.token_stack.pop() {
            reports.complete(token)
        } else {
            fatal_error!(reports; "expected {} but found eof", what)
        }
    }

    pub fn consume_ident(&mut self) -> Reported<Ident> {

        let mut reports = Reported::new();

        loop {
            let token = merge_reports!(reports, self.pop_token("identifier"));
            let span = token.span;
            match token.ty {
                TT::AlphaNum => {
                    break match merge_reports!(reports, AlphaNum::parse(token)) {
                        AlphaNum::Ident(ident) => reports.complete(ident),
                        _ => {
                            fatal_error!(reports; "expected identifier but found literal"; span)
                        }
                    };
                },
                TT::Str | TT::Char => {
                    fatal_error!(reports; "expected identifier but found literal"; span)
                },
                TT::Punct => {
                    fatal_error!(reports; "expected identifier but found unexpected symbol"; span)
                },
                TT::LineBreak => {}
            }
        }
    }

    pub fn consume_expr(&mut self, is_bracket: bool) -> Reported<Expr> {
        // the shunting-yard algorithm
        // https://en.wikipedia.org/wiki/Shunting-yard_algorithm

        let mut reports = Reported::new();

        let mut output = Vec::new();
        let mut stack = Vec::<StackElement>::new();

        #[derive(Debug)]
        enum StackElement {
            Expr(Expr),
            BinOp(BinOpKind, Span),
            Delim,
        }

        while let Some(token) = { self.token_stack.pop() } {
            match token.ty {
                TT::AlphaNum => {
                    let expr = match merge_reports!(reports, AlphaNum::parse(token)) {
                        AlphaNum::Ident(ident) => Expr::Ref(ident),
                        AlphaNum::Int(int) => Expr::Int(int),
                        AlphaNum::Float(float) => Expr::Float(float),
                    };
                    output.push(StackElement::Expr(expr));
                },
                TT::Str => {
                    let Token { value, span, .. } = token;
                    let unquoted = value[1..value.len() - 1].into();
                    // FIXME: resolve escapes
                    let expr = Expr::Str(StrLit {
                        value: unquoted,
                        span,
                    });
                    output.push(StackElement::Expr(expr));
                },
                TT::Char => {
                    let Token { value, span, .. } = token;
                    // FIXME: resolve escapes
                    let unquoted = &value[1..value.len() - 1].encode_utf16().collect::<Vec<_>>();
                    if unquoted.len() != 1 {
                        fatal_error!(reports; "character literals must contain exactly one utf16 character"; span);
                    }
                    let expr = Expr::Char(CharLit {
                        value: unquoted[0],
                        span,
                    });
                    output.push(StackElement::Expr(expr));
                },
                TT::LineBreak => {
                    if !is_bracket {
                        self.token_stack.push(token);
                        break;
                    }
                }
                TT::Punct => {
                    match token.value.as_str() {
                        // TODO: handle unary ops
                        "+" | "-" | "*" | "/" |
                        "%" | "&" | "|" | "^" => {
                            let op = BinOpKind::from_str(&token.value).unwrap();
                            while let Some(top) = { stack.pop() } {
                                match top {
                                    StackElement::BinOp(op2, _) if !(op2 < op) => {
                                        output.push(top);
                                    },
                                    _ => {
                                        stack.push(top);
                                        break
                                    },
                                }
                            }
                            stack.push(StackElement::BinOp(op, token.span));
                        },
                        "(" => {
                            stack.push(StackElement::Delim);
                        },
                        ")" => {
                            let mut parens_match = false;
                            while let Some(top) = { stack.pop() } {
                                match top {
                                    StackElement::Delim => {
                                        parens_match = true;
                                        break;
                                    },
                                    _ => output.push(top),
                                }
                            }
                            if !parens_match {
                                fatal_error!(reports; "unmatched parens")
                            }
                        },
                        "[" => {
                            let line = merge_reports!(reports, self.consume_source_line(true));
                            let instruction = merge_reports!(reports, Instruction::from_source(line));
                            output.push(StackElement::Expr(Expr::Bracket(Box::new(instruction))));
                        },
                        "]" => {
                            if is_bracket {
                                self.token_stack.push(token);
                                break;
                            }
                        },
                        "," => {
                            self.token_stack.push(token);
                            break;
                        }
                        _ => {
                            fatal_error!(reports; "unexpected symbol {}", token.value; token.span)
                        },
                    }
                }
            }
        }

        while let Some(top) = stack.pop() {
            output.push(top);
        }

        fn consume_output(stack: &mut Vec<StackElement>) -> Reported<Expr> {
            let mut reports = Reported::new();
            match stack.pop() {
                Some(StackElement::Expr(expr)) => reports.complete(expr),
                Some(StackElement::BinOp(op, span)) => {
                    // backwards, since we're popping off the back
                    let second = merge_reports!(reports, consume_output(stack));
                    let first = merge_reports!(reports, consume_output(stack));
                    reports.complete(Expr::BinOp(Box::new(first), op, Box::new(second)))
                },
                // delimiters can't appear in the operand stack
                Some(_) => unreachable!(),
                // hmm... we ran out of stuff to parse
                None => {
                    fatal_error!(reports; "")
                } // FIXME: this error
            }
        }

        let expr = merge_reports!(reports, consume_output(&mut output));

        if !output.is_empty() {
            fatal_error!(reports; "orphaned expr"; match output.pop().unwrap() {
                StackElement::Expr(expr) => expr.span(),
                StackElement::BinOp(_, span) => span,
                _ => unreachable!(),
            })
        }

        reports.complete(expr)
    }

    pub fn consume_source_line(&mut self, is_bracket: bool) -> Reported<SourceLine> {
        let mut reports = Reported::new();

        let (label, instruction, mut span) = {
            let maybe_label = merge_reports!(reports, self.consume_ident());
            let maybe_colon = merge_reports!(reports, self.pop_token("expression"));
            if maybe_colon.value == ":" {
                let instruction = merge_reports!(reports, self.consume_ident());
                let span = maybe_label.span().extend_to(instruction.span().end);
                (Some(maybe_label), instruction, span)
            } else {
                self.token_stack.push(maybe_colon);
                let span = maybe_label.span();
                (None, maybe_label, span)
            }
        };

        let mut args = Vec::new();
        let mut scanning_for_comma = false;

        while let Some(token) = { self.token_stack.pop() } {
            match token.ty {
                TT::LineBreak => {
                    if !is_bracket {
                        break;
                    }
                },
                TT::Punct => {
                    match token.value.as_str() {
                        "," => {
                            if scanning_for_comma {
                                scanning_for_comma = false;
                                span.extend_to(token.span.end);
                            } else {
                                fatal_error!(reports; "expected expression, found `,`"; token.span)
                            }
                        },
                        "]" => {
                            if is_bracket {
                                break;
                            }
                        }
                        _ => {
                            self.token_stack.push(token);
                            let expr = merge_reports!(reports, self.consume_expr(is_bracket));
                            span.extend_to_mut(expr.span().end);
                            args.push(expr);
                            scanning_for_comma = true;
                        },
                    }
                },
                _ => {
                    self.token_stack.push(token);
                    let expr = merge_reports!(reports, self.consume_expr(is_bracket));
                    span.extend_to_mut(expr.span().end);
                    args.push(expr);
                    scanning_for_comma = true;
                },
            }
        }

        reports.complete(SourceLine{
            label, instruction, args, span,
        })
    }

    pub fn consume_all(&mut self) -> Reported<Vec<Instruction>> {
        let mut reports = Reported::new();
        let mut lines = Vec::new();
        while !self.token_stack.is_empty() {
            let line = reports.merge(self.consume_source_line(false));
            if let Some(line) = line {
                let instruction = reports.merge(Instruction::from_source(line));
                if let Some(inst) = instruction {
                    lines.push(inst);
                }
            }
        }
       reports.complete(lines)
    }

}

enum AlphaNum {
    Ident(Ident),
    Int(IntLit),
    Float(FloatLit),
}

impl AlphaNum {
    fn parse(token: Token) -> Reported<AlphaNum> {
        let mut reports = Reported::new();

        let Token { value, span, .. } = token;

        let mut iter = value.chars();
        let first = iter.next().unwrap();
        match first {
            '0' ... '9' => {
                if value.contains('.') ||
                    value.contains('e') {
                    use std::str::FromStr;
                    if let Ok(num) = f64::from_str(&value[..]) {
                        reports.complete(AlphaNum::Float(FloatLit{ value: num, span }))
                    } else {
                        fatal_error!(reports; "invalid numeric literal"; span)
                    }
                } else {
                    let num =
                        if value.starts_with("0b") {
                            i64::from_str_radix(&value[2..], 2)
                        } else if value.starts_with("0o") {
                            i64::from_str_radix(&value[2..], 8)
                        } else if value.starts_with("0x") {
                            i64::from_str_radix(&value[2..], 16)
                        } else {
                            i64::from_str_radix(&value[..], 10)
                        };
                    if let Ok(num) = num {
                        reports.complete(AlphaNum::Int(IntLit{ value: num, span }))
                    } else {
                        fatal_error!(reports; "invalid numeric literal"; span)
                    }
                }
            },
            _ => {
                // only two magic identifiers can
                // have angle brackets
                if (value.contains('<') ||
                    value.contains('>')) &&
                    !(value == "<init>" || value == "<clinit>"){
                    fatal_error!(reports; "only the identifiers `<init>` and `<clinit>` may contain `<` or `>`"; span)
                }
                reports.complete(AlphaNum::Ident(Ident { name: value.clone(), span }))
            },
        }
    }
}
