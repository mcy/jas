use consts::code;
use indexing::*;

#[derive(Debug)]
pub enum Instruction {

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
    
    PushByte(i8),
    PushShort(i16),
    
    LoadConstant(HalfConstantIndex),
    WideLoadConstant(ConstantIndex),
    WideLoadWideConstant(ConstantIndex),
    
    
    LoadInt(VarIndex),
    LoadLong(VarIndex),
    LoadFloat(VarIndex),
    LoadDouble(VarIndex),
    LoadRef(VarIndex),
    
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
    
    
    StoreInt(VarIndex),
    StoreLong(VarIndex),
    StoreFloat(VarIndex),
    StoreDouble(VarIndex),
    StoreRef(VarIndex),
    
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
    
    IncInt(VarIndex, i8),
    
    
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
    
    IfIntEq0(CodeIndex),
    IfIntNe0(CodeIndex),
    IfIntLt0(CodeIndex),
    IfIntGe0(CodeIndex),
    IfIntGt0(CodeIndex),
    IfIntLe0(CodeIndex),
    
    IfIntEq(CodeIndex),
    IfIntNe(CodeIndex),
    IfIntLt(CodeIndex),
    IfIntGe(CodeIndex),
    IfIntGt(CodeIndex),
    IfIntLe(CodeIndex),
    
    IfRefEq(CodeIndex),
    IfRefNe(CodeIndex),
    
    Goto(CodeIndex),
    JumpSub(CodeIndex),
    RetSub(VarIndex),

    LookupSwitch {
        default_offset: WideCodeIndex,
        match_table: Vec<(i32, WideCodeIndex)>
    },

    TableSwitch {
        default_offset: WideCodeIndex,
        match_range: (i32, i32),
        offset_table: Vec<WideCodeIndex>,
    },
    
    ReturnInt,
    ReturnLong,
    ReturnFloat,
    ReturnDouble,
    ReturnRef,
    ReturnVoid,
    
    GetStaticField(ConstantIndex),
    PutStaticField(ConstantIndex),
    GetField(ConstantIndex),
    PutField(ConstantIndex),

    InvokeDynamic(ConstantIndex),
    InvokeInterface(ConstantIndex, u8),
    InvokeSpecial(ConstantIndex),
    InvokeStatic(ConstantIndex),
    InvokeVirtual(ConstantIndex),
    
    New(ConstantIndex),
    NewPrimitiveArray(ArrayPrimitive),
    NewRefArray(ConstantIndex),
    
    ArrayLen,
    
    Throw,
    
    CheckCast(ConstantIndex),
    InstanceOf(ConstantIndex),
    
    EnterMonitor,
    ExitMonitor,
    
    Wide(WideInstruction),
    
    NewRefMultiArray(ConstantIndex, u8),
    IfRefNull(CodeIndex),
    IfRefNonNull(CodeIndex),
    WideGoto(WideCodeIndex),
    WideJumpSub(WideCodeIndex),
    
    
    Breakpoint,
    ImplementationDefined1,
    ImplementationDefined2,
}

impl Instruction {
    
    pub fn opcode(&self) -> u8 {
        match *self {
            Instruction::Nop => code::OPCODE_NOP,

            Instruction::ConstRefNull => code::OPCODE_CONST_REF_NULL,
            Instruction::ConstIntM1 => code::OPCODE_CONST_INT_M1,
            Instruction::ConstInt0 => code::OPCODE_CONST_INT_0,
            Instruction::ConstInt1 => code::OPCODE_CONST_INT_1,
            Instruction::ConstInt2 => code::OPCODE_CONST_INT_2,
            Instruction::ConstInt3 => code::OPCODE_CONST_INT_3,
            Instruction::ConstInt4 => code::OPCODE_CONST_INT_4,
            Instruction::ConstInt5 => code::OPCODE_CONST_INT_5,
            Instruction::ConstLong0 => code::OPCODE_CONST_LONG_0,
            Instruction::ConstLong1 => code::OPCODE_CONST_LONG_1,
            Instruction::ConstFloat0 => code::OPCODE_CONST_FLOAT_0,
            Instruction::ConstFloat1 => code::OPCODE_CONST_FLOAT_1,
            Instruction::ConstFloat2 => code::OPCODE_CONST_FLOAT_2,
            Instruction::ConstDouble0 => code::OPCODE_CONST_DOUBLE_0,
            Instruction::ConstDouble1 => code::OPCODE_CONST_DOUBLE_1,

            Instruction::PushByte(..) => code::OPCODE_PUSH_BYTE,
            Instruction::PushShort(..) => code::OPCODE_PUSH_SHORT,

            Instruction::LoadConstant(..) => code::OPCODE_LOAD_CONSTANT,
            Instruction::WideLoadConstant(..) => code::OPCODE_WIDE_LOAD_CONSTANT,
            Instruction::WideLoadWideConstant(..) => code::OPCODE_WIDE_LOAD_WIDE_CONSTANT,


            Instruction::LoadInt(..) => code::OPCODE_LOAD_INT,
            Instruction::LoadLong(..) => code::OPCODE_LOAD_LONG,
            Instruction::LoadFloat(..) => code::OPCODE_LOAD_FLOAT,
            Instruction::LoadDouble(..) => code::OPCODE_LOAD_DOUBLE,
            Instruction::LoadRef(..) => code::OPCODE_LOAD_REF,

            Instruction::LoadInt0 => code::OPCODE_LOAD_INT_0,
            Instruction::LoadInt1 => code::OPCODE_LOAD_INT_1,
            Instruction::LoadInt2 => code::OPCODE_LOAD_INT_2,
            Instruction::LoadInt3 => code::OPCODE_LOAD_INT_3,

            Instruction::LoadLong0 => code::OPCODE_LOAD_LONG_0,
            Instruction::LoadLong1 => code::OPCODE_LOAD_LONG_1,
            Instruction::LoadLong2 => code::OPCODE_LOAD_LONG_2,
            Instruction::LoadLong3 => code::OPCODE_LOAD_LONG_3,

            Instruction::LoadFloat0 => code::OPCODE_LOAD_FLOAT_0,
            Instruction::LoadFloat1 => code::OPCODE_LOAD_FLOAT_1,
            Instruction::LoadFloat2 => code::OPCODE_LOAD_FLOAT_2,
            Instruction::LoadFloat3 => code::OPCODE_LOAD_FLOAT_3,

            Instruction::LoadDouble0 => code::OPCODE_LOAD_DOUBLE_0,
            Instruction::LoadDouble1 => code::OPCODE_LOAD_DOUBLE_1,
            Instruction::LoadDouble2 => code::OPCODE_LOAD_DOUBLE_2,
            Instruction::LoadDouble3 => code::OPCODE_LOAD_DOUBLE_3,

            Instruction::LoadRef0 => code::OPCODE_LOAD_REF_0,
            Instruction::LoadRef1 => code::OPCODE_LOAD_REF_1,
            Instruction::LoadRef2 => code::OPCODE_LOAD_REF_2,
            Instruction::LoadRef3 => code::OPCODE_LOAD_REF_3,

            Instruction::ArrayLoadInt => code::OPCODE_ARRAY_LOAD_INT,
            Instruction::ArrayLoadLong => code::OPCODE_ARRAY_LOAD_LONG,
            Instruction::ArrayLoadFloat => code::OPCODE_ARRAY_LOAD_FLOAT,
            Instruction::ArrayLoadDouble => code::OPCODE_ARRAY_LOAD_DOUBLE,
            Instruction::ArrayLoadRef => code::OPCODE_ARRAY_LOAD_REF,
            Instruction::ArrayLoadByte => code::OPCODE_ARRAY_LOAD_BYTE,
            Instruction::ArrayLoadChar => code::OPCODE_ARRAY_LOAD_CHAR,
            Instruction::ArrayLoadShort => code::OPCODE_ARRAY_LOAD_SHORT,


            Instruction::StoreInt(..) => code::OPCODE_STORE_INT,
            Instruction::StoreLong(..) => code::OPCODE_STORE_LONG,
            Instruction::StoreFloat(..) => code::OPCODE_STORE_FLOAT,
            Instruction::StoreDouble(..) => code::OPCODE_STORE_DOUBLE,
            Instruction::StoreRef(..) => code::OPCODE_STORE_REF,

            Instruction::StoreInt0 => code::OPCODE_STORE_INT_0,
            Instruction::StoreInt1 => code::OPCODE_STORE_INT_1,
            Instruction::StoreInt2 => code::OPCODE_STORE_INT_2,
            Instruction::StoreInt3 => code::OPCODE_STORE_INT_3,

            Instruction::StoreLong0 => code::OPCODE_STORE_LONG_0,
            Instruction::StoreLong1 => code::OPCODE_STORE_LONG_1,
            Instruction::StoreLong2 => code::OPCODE_STORE_LONG_2,
            Instruction::StoreLong3 => code::OPCODE_STORE_LONG_3,

            Instruction::StoreFloat0 => code::OPCODE_STORE_FLOAT_0,
            Instruction::StoreFloat1 => code::OPCODE_STORE_FLOAT_1,
            Instruction::StoreFloat2 => code::OPCODE_STORE_FLOAT_2,
            Instruction::StoreFloat3 => code::OPCODE_STORE_FLOAT_3,

            Instruction::StoreDouble0 => code::OPCODE_STORE_DOUBLE_0,
            Instruction::StoreDouble1 => code::OPCODE_STORE_DOUBLE_1,
            Instruction::StoreDouble2 => code::OPCODE_STORE_DOUBLE_2,
            Instruction::StoreDouble3 => code::OPCODE_STORE_DOUBLE_3,

            Instruction::StoreRef0 => code::OPCODE_STORE_REF_0,
            Instruction::StoreRef1 => code::OPCODE_STORE_REF_1,
            Instruction::StoreRef2 => code::OPCODE_STORE_REF_2,
            Instruction::StoreRef3 => code::OPCODE_STORE_REF_3,

            Instruction::ArrayStoreInt => code::OPCODE_ARRAY_STORE_INT,
            Instruction::ArrayStoreLong => code::OPCODE_ARRAY_STORE_LONG,
            Instruction::ArrayStoreFloat => code::OPCODE_ARRAY_STORE_FLOAT,
            Instruction::ArrayStoreDouble => code::OPCODE_ARRAY_STORE_DOUBLE,
            Instruction::ArrayStoreRef => code::OPCODE_ARRAY_STORE_REF,
            Instruction::ArrayStoreByte => code::OPCODE_ARRAY_STORE_BYTE,
            Instruction::ArrayStoreChar => code::OPCODE_ARRAY_STORE_CHAR,
            Instruction::ArrayStoreShort => code::OPCODE_ARRAY_STORE_SHORT,


            Instruction::Pop => code::OPCODE_POP,
            Instruction::DoublePop => code::OPCODE_DOUBLE_POP,

            Instruction::Dup => code::OPCODE_DUP,
            Instruction::DupDown => code::OPCODE_DUP_DOWN,
            Instruction::DupDoubleDown => code::OPCODE_DUP_DOUBLE_DOWN,
            Instruction::DoubleDup => code::OPCODE_DOUBLE_DUP,
            Instruction::DoubleDupDown => code::OPCODE_DOUBLE_DUP_DOWN,
            Instruction::DoubleDupDoubleDown => code::OPCODE_DOUBLE_DUP_DOUBLE_DOWN,

            Instruction::Swap => code::OPCODE_SWAP,


            Instruction::AddInt => code::OPCODE_ADD_INT,
            Instruction::AddLong => code::OPCODE_ADD_LONG,
            Instruction::AddFloat => code::OPCODE_ADD_FLOAT,
            Instruction::AddDouble => code::OPCODE_ADD_DOUBLE,

            Instruction::SubInt => code::OPCODE_SUB_INT,
            Instruction::SubLong => code::OPCODE_SUB_LONG,
            Instruction::SubFloat => code::OPCODE_SUB_FLOAT,
            Instruction::SubDouble => code::OPCODE_SUB_DOUBLE,

            Instruction::MulInt => code::OPCODE_MUL_INT,
            Instruction::MulLong => code::OPCODE_MUL_LONG,
            Instruction::MulFloat => code::OPCODE_MUL_FLOAT,
            Instruction::MulDouble => code::OPCODE_MUL_DOUBLE,

            Instruction::DivInt => code::OPCODE_DIV_INT,
            Instruction::DivLong => code::OPCODE_DIV_LONG,
            Instruction::DivFloat => code::OPCODE_DIV_FLOAT,
            Instruction::DivDouble => code::OPCODE_DIV_DOUBLE,

            Instruction::RemInt => code::OPCODE_REM_INT,
            Instruction::RemLong => code::OPCODE_REM_LONG,
            Instruction::RemFloat => code::OPCODE_REM_FLOAT,
            Instruction::RemDouble => code::OPCODE_REM_DOUBLE,

            Instruction::NegInt => code::OPCODE_NEG_INT,
            Instruction::NegLong => code::OPCODE_NEG_LONG,
            Instruction::NegFloat => code::OPCODE_NEG_FLOAT,
            Instruction::NegDouble => code::OPCODE_NEG_DOUBLE,

            Instruction::LeftShiftInt => code::OPCODE_LEFT_SHIFT_INT,
            Instruction::LeftShiftLong => code::OPCODE_LEFT_SHIFT_LONG,
            Instruction::RightShiftInt => code::OPCODE_RIGHT_SHIFT_INT,
            Instruction::RightShiftLong => code::OPCODE_RIGHT_SHIFT_LONG,
            Instruction::URightShiftInt => code::OPCODE_URIGHT_SHIFT_INT,
            Instruction::URightShiftLong => code::OPCODE_URIGHT_SHIFT_LONG,

            Instruction::AndInt => code::OPCODE_AND_INT,
            Instruction::AndLong => code::OPCODE_AND_LONG,

            Instruction::OrInt => code::OPCODE_OR_INT,
            Instruction::OrLong => code::OPCODE_OR_LONG,

            Instruction::XorInt => code::OPCODE_XOR_INT,
            Instruction::XorLong => code::OPCODE_XOR_LONG,

            Instruction::IncInt(..) => code::OPCODE_INC_INT,


            Instruction::IntToLong => code::OPCODE_INT_TO_LONG,
            Instruction::IntToFloat => code::OPCODE_INT_TO_FLOAT,
            Instruction::IntToDouble => code::OPCODE_INT_TO_DOUBLE,
            Instruction::LongToInt => code::OPCODE_LONG_TO_INT,
            Instruction::LongToFloat => code::OPCODE_LONG_TO_FLOAT,
            Instruction::LongToDouble => code::OPCODE_LONG_TO_DOUBLE,
            Instruction::FloatToInt => code::OPCODE_FLOAT_TO_INT,
            Instruction::FloatToLong => code::OPCODE_FLOAT_TO_LONG,
            Instruction::FloatToDouble => code::OPCODE_FLOAT_TO_DOUBLE,
            Instruction::DoubleToInt => code::OPCODE_DOUBLE_TO_INT,
            Instruction::DoubleToLong => code::OPCODE_DOUBLE_TO_LONG,
            Instruction::DoubleToFloat => code::OPCODE_DOUBLE_TO_FLOAT,

            Instruction::IntToByte => code::OPCODE_INT_TO_BYTE,
            Instruction::IntToChar => code::OPCODE_INT_TO_CHAR,
            Instruction::IntToShort => code::OPCODE_INT_TO_SHORT,


            Instruction::CompareLong => code::OPCODE_COMPARE_LONG,
            Instruction::CompareFloatL => code::OPCODE_COMPARE_FLOAT_L,
            Instruction::CompareFloatG => code::OPCODE_COMPARE_FLOAT_G,
            Instruction::CompareDoubleL => code::OPCODE_COMPARE_DOUBLE_L,
            Instruction::CompareDoubleG => code::OPCODE_COMPARE_DOUBLE_G,

            Instruction::IfIntEq0(..) => code::OPCODE_IF_INT_EQ_0,
            Instruction::IfIntNe0(..) => code::OPCODE_IF_INT_NE_0,
            Instruction::IfIntLt0(..) => code::OPCODE_IF_INT_LT_0,
            Instruction::IfIntGe0(..) => code::OPCODE_IF_INT_GE_0,
            Instruction::IfIntGt0(..) => code::OPCODE_IF_INT_GT_0,
            Instruction::IfIntLe0(..) => code::OPCODE_IF_INT_LE_0,

            Instruction::IfIntEq(..) => code::OPCODE_IF_INT_EQ,
            Instruction::IfIntNe(..) => code::OPCODE_IF_INT_NE,
            Instruction::IfIntLt(..) => code::OPCODE_IF_INT_LT,
            Instruction::IfIntGe(..) => code::OPCODE_IF_INT_GE,
            Instruction::IfIntGt(..) => code::OPCODE_IF_INT_GT,
            Instruction::IfIntLe(..) => code::OPCODE_IF_INT_LE,

            Instruction::IfRefEq(..) => code::OPCODE_IF_REF_EQ,
            Instruction::IfRefNe(..) => code::OPCODE_IF_REF_NE,

            Instruction::Goto(..) => code::OPCODE_GOTO,
            Instruction::JumpSub(..) => code::OPCODE_JUMP_SUB,
            Instruction::RetSub(..) => code::OPCODE_RET_SUB,

            Instruction::TableSwitch {..} => code::OPCODE_TABLE_SWITCH,
            Instruction::LookupSwitch {..} => code::OPCODE_LOOKUP_SWITCH,

            Instruction::ReturnInt => code::OPCODE_RETURN_INT,
            Instruction::ReturnLong => code::OPCODE_RETURN_LONG,
            Instruction::ReturnFloat => code::OPCODE_RETURN_FLOAT,
            Instruction::ReturnDouble => code::OPCODE_RETURN_DOUBLE,
            Instruction::ReturnRef => code::OPCODE_RETURN_REF,
            Instruction::ReturnVoid => code::OPCODE_RETURN_VOID,


            Instruction::GetStaticField(..) => code::OPCODE_GET_STATIC_FIELD,
            Instruction::PutStaticField(..) => code::OPCODE_PUT_STATIC_FIELD,
            Instruction::GetField(..) => code::OPCODE_GET_FIELD,
            Instruction::PutField(..) => code::OPCODE_PUT_FIELD,

            Instruction::InvokeVirtual(..) => code::OPCODE_INVOKE_VIRTUAL,
            Instruction::InvokeSpecial(..) => code::OPCODE_INVOKE_SPECIAL,
            Instruction::InvokeStatic(..) => code::OPCODE_INVOKE_STATIC,
            Instruction::InvokeInterface(..) => code::OPCODE_INVOKE_INTERFACE,
            Instruction::InvokeDynamic(..) => code::OPCODE_INVOKE_DYNAMIC,

            Instruction::New(..) => code::OPCODE_NEW,
            Instruction::NewPrimitiveArray(..) => code::OPCODE_NEW_PRIMITIVE_ARRAY,
            Instruction::NewRefArray(..) => code::OPCODE_NEW_REF_ARRAY,

            Instruction::ArrayLen => code::OPCODE_ARRAY_LEN,

            Instruction::Throw => code::OPCODE_THROW,

            Instruction::CheckCast(..) => code::OPCODE_CHECK_CAST,
            Instruction::InstanceOf(..) => code::OPCODE_INSTANCE_OF,

            Instruction::EnterMonitor => code::OPCODE_ENTER_MONITOR,
            Instruction::ExitMonitor => code::OPCODE_EXIT_MONITOR,

            Instruction::Wide(..) => code::OPCODE_WIDE,

            Instruction::NewRefMultiArray(..) => code::OPCODE_NEW_REF_MULTI_ARRAY,
            Instruction::IfRefNull(..) => code::OPCODE_IF_REF_NULL,
            Instruction::IfRefNonNull(..) => code::OPCODE_IF_REF_NON_NULL,
            Instruction::WideGoto(..) => code::OPCODE_WIDE_GOTO,
            Instruction::WideJumpSub(..) => code::OPCODE_WIDE_JUMP_SUB,


            Instruction::Breakpoint => code::OPCODE_BREAKPOINT,
            Instruction::ImplementationDefined1 => code::OPCODE_IMPLEMENTATION_DEFINED_1,
            Instruction::ImplementationDefined2 => code::OPCODE_IMPLEMENTATION_DEFINED_2,
        }
    }
}

#[derive(Debug)]
pub enum ArrayPrimitive {
    Boolean,
    Char,
    Float,
    Double,
    Byte,
    Short,
    Int,
    Long,
}

impl ArrayPrimitive {

    pub fn to_byte(&self) -> u8 {
        use self::ArrayPrimitive::*;
        match *self {
            Boolean => code::ARRAY_BOOLEAN,
            Char => code::ARRAY_CHAR,
            Float => code::ARRAY_FLOAT,
            Double => code::ARRAY_DOUBLE,
            Byte => code::ARRAY_BYTE,
            Short => code::ARRAY_SHORT,
            Int => code::ARRAY_INT,
            Long => code::ARRAY_LONG,
        }
    }

    pub fn from_byte(byte: u8) -> Option<ArrayPrimitive> {
        use self::ArrayPrimitive::*;
        match byte {
            code::ARRAY_BOOLEAN => Some(Boolean),
            code::ARRAY_CHAR => Some(Char),
            code::ARRAY_FLOAT => Some(Float),
            code::ARRAY_DOUBLE => Some(Double),
            code::ARRAY_BYTE => Some(Byte),
            code::ARRAY_SHORT => Some(Short),
            code::ARRAY_INT => Some(Int),
            code::ARRAY_LONG => Some(Long),
            _ => None
        }
    }
}

#[derive(Debug)]
pub enum WideInstruction {

    LoadInt(WideVarIndex),
    LoadLong(WideVarIndex),
    LoadFloat(WideVarIndex),
    LoadDouble(WideVarIndex),
    LoadRef(WideVarIndex),

    StoreInt(WideVarIndex),
    StoreLong(WideVarIndex),
    StoreFloat(WideVarIndex),
    StoreDouble(WideVarIndex),
    StoreRef(WideVarIndex),

    IncInt(WideVarIndex, i16),

    RetSub(WideVarIndex),
}

impl WideInstruction {

    pub fn opcode(&self) -> u8 {
        match *self {
            WideInstruction::LoadInt(..) => code::OPCODE_LOAD_INT,
            WideInstruction::LoadLong(..) => code::OPCODE_LOAD_LONG,
            WideInstruction::LoadFloat(..) => code::OPCODE_LOAD_FLOAT,
            WideInstruction::LoadDouble(..) => code::OPCODE_LOAD_DOUBLE,
            WideInstruction::LoadRef(..) => code::OPCODE_LOAD_REF,

            WideInstruction::StoreInt(..) => code::OPCODE_STORE_INT,
            WideInstruction::StoreLong(..) => code::OPCODE_STORE_LONG,
            WideInstruction::StoreFloat(..) => code::OPCODE_STORE_FLOAT,
            WideInstruction::StoreDouble(..) => code::OPCODE_STORE_DOUBLE,
            WideInstruction::StoreRef(..) => code::OPCODE_STORE_REF,

            WideInstruction::IncInt(..) => code::OPCODE_INC_INT,

            WideInstruction::RetSub(..) => code::OPCODE_RET_SUB,
        }
    }
}

#[derive(Debug)]
pub struct ExceptionTableEntry {
    pub try_range: (CodeIndex, CodeIndex),
    pub handler_goto: CodeIndex,
    pub exception_type: ConstantIndex
}