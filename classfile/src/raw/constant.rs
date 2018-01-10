use consts::constant;
use indexing::ConstantIndex;

#[derive(Debug)]
pub enum Constant {
    Class(ConstantIndex),

    FieldRef {
        class: ConstantIndex,
        signature: ConstantIndex,
    },

    MethodRef {
        class: ConstantIndex,
        signature: ConstantIndex,
    },

    InterfaceMethodRef {
        class: ConstantIndex,
        signature: ConstantIndex,
    },

    String(ConstantIndex),

    Integer(i32),
    Float(f32),

    Long(i64),
    Double(f64),

    WidePlaceholder,

    Signature {
        name: ConstantIndex,
        descriptor: ConstantIndex,
    },

    Utf8(String),

    MethodHandle {
        kind: MethodHandleKind,
        referent: ConstantIndex,
    },

    MethodType(ConstantIndex),

    InvokeDynamic {
        bootstrap_method: ConstantIndex,
        signature: ConstantIndex,
    },
}

impl Constant {
    pub fn tag(&self) -> u8 {
        use self::Constant::*;

        match *self {
            Class(..) => constant::TAG_CLASS,
            FieldRef { .. } => constant::TAG_FIELD_REF,
            MethodRef { .. } => constant::TAG_METHOD_REF,
            InterfaceMethodRef { .. } => constant::TAG_INTERFACE_METHOD_REF,
            String(..) => constant::TAG_STRING,
            Integer(..) => constant::TAG_INTEGER,
            Float(..) => constant::TAG_FLOAT,
            Long(..) => constant::TAG_LONG,
            Double(..) => constant::TAG_DOUBLE,
            WidePlaceholder => 0, // FIXME
            Signature { .. } => constant::TAG_TYPE_AND_NAME,
            Utf8(..) => constant::TAG_UTF8,
            MethodHandle { .. } => constant::TAG_METHOD_HANDLE,
            MethodType { .. } => constant::TAG_METHOD_TYPE,
            InvokeDynamic { .. } => constant::TAG_INVOKE_DYNAMIC,
        }
    }
}

#[derive(Debug)]
pub enum MethodHandleKind {
    GetField,
    GetStatic,
    PutField,
    PutStatic,

    InvokeVirtual,
    InvokeStatic,
    InvokeSpecial,
    NewInvokeSpecial,
    InvokeInterface,
}

impl MethodHandleKind {
    pub fn as_byte(&self) -> u8 {
        use self::MethodHandleKind::*;
        match *self {
            GetField => constant::HANDLE_GET_FIELD,
            GetStatic => constant::HANDLE_GET_STATIC,
            PutField => constant::HANDLE_PUT_FIELD,
            PutStatic => constant::HANDLE_PUT_STATIC,

            InvokeVirtual => constant::HANDLE_INVOKE_VIRTUAL,
            InvokeStatic => constant::HANDLE_INVOKE_STATIC,
            InvokeSpecial => constant::HANDLE_INVOKE_SPECIAL,
            NewInvokeSpecial => constant::HANDLE_NEW_INVOKE_SPECIAL,
            InvokeInterface => constant::HANDLE_INVOKE_INTERFACE,
        }
    }

    pub fn from_byte(byte: u8) -> Option<MethodHandleKind> {
        use self::MethodHandleKind::*;
        match byte {
            constant::HANDLE_GET_FIELD => Some(GetField),
            constant::HANDLE_GET_STATIC => Some(GetStatic),
            constant::HANDLE_PUT_FIELD => Some(PutField),
            constant::HANDLE_PUT_STATIC => Some(PutStatic),

            constant::HANDLE_INVOKE_VIRTUAL => Some(InvokeVirtual),
            constant::HANDLE_INVOKE_STATIC => Some(InvokeStatic),
            constant::HANDLE_INVOKE_SPECIAL => Some(InvokeSpecial),
            constant::HANDLE_NEW_INVOKE_SPECIAL => Some(NewInvokeSpecial),
            constant::HANDLE_INVOKE_INTERFACE => Some(InvokeInterface),

            _ => None,
        }
    }
}
