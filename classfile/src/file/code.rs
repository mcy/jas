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
    
    LoadConstant(ConstantIndex),
    WideLoadConstant(WideConstantIndex),
    WideLoadWideConstant(WideConstantIndex),
    
    
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
        use self::Instruction::*;
        match *self {
            Nop => 0x00,

            ConstRefNull => 0x01,
            ConstIntM1 => 0x02,
            ConstInt0 => 0x03,
            ConstInt1 => 0x04,
            ConstInt2 => 0x05,
            ConstInt3 => 0x06,
            ConstInt4 => 0x07,
            ConstInt5 => 0x08,
            ConstLong0 => 0x09,
            ConstLong1 => 0x0a,
            ConstFloat0 => 0x0b,
            ConstFloat1 => 0x0c,
            ConstFloat2 => 0x0d,
            ConstDouble0 => 0x0e,
            ConstDouble1 => 0x0f,

            PushByte(..) => 0x10,
            PushShort(..) => 0x11,

            LoadConstant(..) => 0x12,
            WideLoadConstant(..) => 0x13,
            WideLoadWideConstant(..) => 0x14,


            LoadInt(..) => 0x15,
            LoadLong(..) => 0x16,
            LoadFloat(..) => 0x17,
            LoadDouble(..) => 0x18,
            LoadRef(..) => 0x19,

            LoadInt0 => 0x1a,
            LoadInt1 => 0x1b,
            LoadInt2 => 0x1c,
            LoadInt3 => 0x1d,

            LoadLong0 => 0x1e,
            LoadLong1 => 0x1f,
            LoadLong2 => 0x20,
            LoadLong3 => 0x21,

            LoadFloat0 => 0x22,
            LoadFloat1 => 0x23,
            LoadFloat2 => 0x24,
            LoadFloat3 => 0x25,

            LoadDouble0 => 0x26,
            LoadDouble1 => 0x27,
            LoadDouble2 => 0x28,
            LoadDouble3 => 0x29,

            LoadRef0 => 0x2a,
            LoadRef1 => 0x2b,
            LoadRef2 => 0x2c,
            LoadRef3 => 0x2d,

            ArrayLoadInt => 0x2e,
            ArrayLoadLong => 0x2f,
            ArrayLoadFloat => 0x30,
            ArrayLoadDouble => 0x31,
            ArrayLoadRef => 0x32,
            ArrayLoadByte => 0x33,
            ArrayLoadChar => 0x34,
            ArrayLoadShort => 0x35,


            StoreInt(..) => 0x36,
            StoreLong(..) => 0x37,
            StoreFloat(..) => 0x38,
            StoreDouble(..) => 0x39,
            StoreRef(..) => 0x3a,

            StoreInt0 => 0x3b,
            StoreInt1 => 0x3c,
            StoreInt2 => 0x3d,
            StoreInt3 => 0x3e,

            StoreLong0 => 0x3f,
            StoreLong1 => 0x40,
            StoreLong2 => 0x41,
            StoreLong3 => 0x42,

            StoreFloat0 => 0x43,
            StoreFloat1 => 0x44,
            StoreFloat2 => 0x45,
            StoreFloat3 => 0x46,

            StoreDouble0 => 0x47,
            StoreDouble1 => 0x48,
            StoreDouble2 => 0x49,
            StoreDouble3 => 0x4a,

            StoreRef0 => 0x4b,
            StoreRef1 => 0x4c,
            StoreRef2 => 0x4d,
            StoreRef3 => 0x4e,

            ArrayStoreInt => 0x4f,
            ArrayStoreLong => 0x50,
            ArrayStoreFloat => 0x51,
            ArrayStoreDouble => 0x52,
            ArrayStoreRef => 0x53,
            ArrayStoreByte => 0x54,
            ArrayStoreChar => 0x55,
            ArrayStoreShort => 0x56,


            Pop => 0x57,
            DoublePop => 0x58,

            Dup => 0x59,
            DupDown => 0x5a,
            DupDoubleDown => 0x5b,
            DoubleDup => 0x5c,
            DoubleDupDown => 0x5d,
            DoubleDupDoubleDown => 0x5e,

            Swap => 0x5f,


            AddInt => 0x60,
            AddLong => 0x61,
            AddFloat => 0x62,
            AddDouble => 0x63,

            SubInt => 0x64,
            SubLong => 0x65,
            SubFloat => 0x66,
            SubDouble => 0x67,

            MulInt => 0x68,
            MulLong => 0x69,
            MulFloat => 0x6a,
            MulDouble => 0x6b,

            DivInt => 0x6c,
            DivLong => 0x6d,
            DivFloat => 0x6e,
            DivDouble => 0x6f,

            RemInt => 0x70,
            RemLong => 0x71,
            RemFloat => 0x72,
            RemDouble => 0x73,

            NegInt => 0x74,
            NegLong => 0x75,
            NegFloat => 0x76,
            NegDouble => 0x77,

            LeftShiftInt => 0x78,
            LeftShiftLong => 0x79,
            RightShiftInt => 0x7a,
            RightShiftLong => 0x7b,
            URightShiftInt => 0x7c,
            URightShiftLong => 0x7d,

            AndInt => 0x7e,
            AndLong => 0x7f,

            OrInt => 0x80,
            OrLong => 0x81,

            XorInt => 0x82,
            XorLong => 0x83,

            IncInt(..) => 0x84,


            IntToLong => 0x85,
            IntToFloat => 0x86,
            IntToDouble => 0x87,
            LongToInt => 0x88,
            LongToFloat => 0x89,
            LongToDouble => 0x8a,
            FloatToInt => 0x8b,
            FloatToLong => 0x8c,
            FloatToDouble => 0x8d,
            DoubleToInt => 0x8e,
            DoubleToLong => 0x8f,
            DoubleToFloat => 0x90,

            IntToByte => 0x91,
            IntToChar => 0x92,
            IntToShort => 0x93,


            CompareLong => 0x94,
            CompareFloatL => 0x95,
            CompareFloatG => 0x96,
            CompareDoubleL => 0x97,
            CompareDoubleG => 0x98,

            IfIntEq0(..) => 0x99,
            IfIntNe0(..) => 0x9a,
            IfIntLt0(..) => 0x9b,
            IfIntGe0(..) => 0x9c,
            IfIntGt0(..) => 0x9d,
            IfIntLe0(..) => 0x9e,

            IfIntEq(..) => 0x9f,
            IfIntNe(..) => 0xa0,
            IfIntLt(..) => 0xa1,
            IfIntGe(..) => 0xa2,
            IfIntGt(..) => 0xa3,
            IfIntLe(..) => 0xa4,

            IfRefEq(..) => 0xa5,
            IfRefNe(..) => 0xa6,

            Goto(..) => 0xa7,
            JumpSub(..) => 0xa8,
            RetSub(..) => 0xa9,

            TableSwitch { .. } => 0xaa,
            LookupSwitch { .. } => 0xab,

            ReturnInt => 0xac,
            ReturnLong => 0xad,
            ReturnFloat => 0xae,
            ReturnDouble => 0xaf,
            ReturnRef => 0xb0,
            ReturnVoid => 0xb1,


            GetStaticField(..) => 0xb2,
            PutStaticField(..) => 0xb3,
            GetField(..) => 0xb4,
            PutField(..) => 0xb5,

            InvokeVirtual(..) => 0xb6,
            InvokeSpecial(..) => 0xb7,
            InvokeStatic(..) => 0xb8,
            InvokeInterface(..) => 0xb9,
            InvokeDynamic(..) => 0xc0,

            New(..) => 0xbb,
            NewPrimitiveArray(..) => 0xbc,
            NewRefArray(..) => 0xbd,

            ArrayLen => 0xbe,

            Throw => 0xbf,

            CheckCast(..) => 0xc0,
            InstanceOf(..) => 0xc1,

            EnterMonitor => 0xc2,
            ExitMonitor => 0xc3,

            Wide(..) => 0xc4,

            NewRefMultiArray(..) => 0xc5,
            IfRefNull(..) => 0xc6,
            IfRefNonNull(..) => 0xc7,
            WideGoto(..) => 0xc8,
            WideJumpSub(..) => 0xc9,


            Breakpoint => 0xca,
            ImplementationDefined1 => 0xfe,
            ImplementationDefined2 => 0xff,
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
            Boolean => 4,
            Char => 5,
            Float => 6,
            Double => 7,
            Byte => 8,
            Short => 9,
            Int => 10,
            Long => 11,
        }
    }

    pub fn from_byte(byte: u8) -> Option<ArrayPrimitive> {
        use self::ArrayPrimitive::*;
        match byte {
            4 => Some(Boolean),
            5 => Some(Char),
            6 => Some(Float),
            7 => Some(Double),
            8 => Some(Byte),
            9 => Some(Short),
            10 => Some(Int),
            11 => Some(Long),
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
        use self::WideInstruction::*;
        match *self {
            LoadInt(..) => 0x15,
            LoadLong(..) => 0x16,
            LoadFloat(..) => 0x17,
            LoadDouble(..) => 0x18,
            LoadRef(..) => 0x19,

            StoreInt(..) => 0x36,
            StoreLong(..) => 0x37,
            StoreFloat(..) => 0x38,
            StoreDouble(..) => 0x39,
            StoreRef(..) => 0x3a,

            IncInt(..) => 0x84,

            RetSub(..) => 0xa9,

        }
    }
}

#[derive(Debug)]
pub struct ExceptionTableEntry {
    pub try_range: (CodeIndex, CodeIndex),
    pub handler_goto: CodeIndex,
    pub exception_type: ConstantIndex
}