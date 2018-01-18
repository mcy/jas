
macro_rules! flag_getter {
    ($name:ident, $flag:ident) => {
        // TODO: docs
        pub fn $name(&self) -> bool {
            self.0 & $flag != 0
        }
    }
}

macro_rules! flag_setter {
    ($name:ident, $flag:ident) => {
        // TODO: docs
        pub fn $name(&mut self, flag: bool) -> &mut Self {
            if flag {
                self.0 |= $flag;
            } else {
                self.0 &= !$flag;
            }
            self
        }
    }
}

pub mod class {
    pub const MAGIC: u32 = 0xcafebabe;

    pub const ACC_PUBLIC: u16 = 0x0001;
    pub const ACC_FINAL: u16 = 0x0010;
    pub const ACC_SUPER: u16 = 0x0020;
    pub const ACC_INTERFACE: u16 = 0x0200;
    pub const ACC_ABSTRACT: u16 = 0x0400;
    pub const ACC_SYNTHETIC: u16 = 0x1000;
    pub const ACC_ANNOTATION: u16 = 0x2000;
    pub const ACC_ENUM: u16 = 0x4000;

    #[derive(Clone, Copy, Debug)]
    pub struct Flags(u16);

    impl Flags {

        pub fn new() -> Flags {
            Flags(0)
        }

        pub fn from_bits(bits: u16) -> Flags {
            Flags(bits)
        }

        pub fn bits(&self) -> u16 {
            self.0
        }

        pub fn len() -> usize {
            2
        }

        flag_getter!(is_public, ACC_PUBLIC);
        flag_getter!(is_final, ACC_FINAL);
        flag_getter!(is_super, ACC_SUPER);
        flag_getter!(is_interface, ACC_INTERFACE);
        flag_getter!(is_abstract, ACC_ABSTRACT);
        flag_getter!(is_synthetic, ACC_SYNTHETIC);
        flag_getter!(is_annotation, ACC_ANNOTATION);
        flag_getter!(is_enum, ACC_ENUM);

        flag_setter!(set_public, ACC_PUBLIC);
        flag_setter!(set_final, ACC_FINAL);
        flag_setter!(set_super, ACC_SUPER);
        flag_setter!(set_interface, ACC_INTERFACE);
        flag_setter!(set_abstract, ACC_ABSTRACT);
        flag_setter!(set_synthetic, ACC_SYNTHETIC);
        flag_setter!(set_annotation, ACC_ANNOTATION);
        flag_setter!(set_enum, ACC_ENUM);
    }

}

pub mod field {
    pub const ACC_PUBLIC: u16 = 0x0001;
    pub const ACC_PRIVATE: u16 = 0x0002;
    pub const ACC_PROTECTED: u16 = 0x0004;
    pub const ACC_STATIC: u16 = 0x0008;
    pub const ACC_FINAL: u16 = 0x0010;
    pub const ACC_VOLATILE: u16 = 0x0040;
    pub const ACC_TRANSIENT: u16 = 0x0080;
    pub const ACC_SYNTHETIC: u16 = 0x1000;
    pub const ACC_ENUM: u16 = 0x4000;

    #[derive(Clone, Copy, Debug)]
    pub struct Flags(u16);

    impl Flags {

        pub fn new() -> Flags {
            Flags(0)
        }

        pub fn from_bits(bits: u16) -> Flags {
            Flags(bits)
        }

        pub fn bits(&self) -> u16 {
            self.0
        }

        pub fn len() -> usize {
            2
        }

        flag_getter!(is_public, ACC_PUBLIC);
        flag_getter!(is_private, ACC_PRIVATE);
        flag_getter!(is_protected, ACC_PROTECTED);
        flag_getter!(is_static, ACC_STATIC);
        flag_getter!(is_final, ACC_FINAL);
        flag_getter!(is_volatile, ACC_VOLATILE);
        flag_getter!(is_transient, ACC_TRANSIENT);
        flag_getter!(is_synthetic, ACC_SYNTHETIC);
        flag_getter!(is_enum, ACC_ENUM);

        flag_setter!(set_public, ACC_PUBLIC);
        flag_setter!(set_private, ACC_PRIVATE);
        flag_setter!(set_protected, ACC_PROTECTED);
        flag_setter!(set_static, ACC_STATIC);
        flag_setter!(set_final, ACC_FINAL);
        flag_setter!(set_volatile, ACC_VOLATILE);
        flag_setter!(set_transient, ACC_TRANSIENT);
        flag_setter!(set_synthetic, ACC_SYNTHETIC);
        flag_setter!(set_enum, ACC_ENUM);
    }
}

pub mod method {
    pub const ACC_PUBLIC: u16 = 0x0001;
    pub const ACC_PRIVATE: u16 = 0x0002;
    pub const ACC_PROTECTED: u16 = 0x0004;
    pub const ACC_STATIC: u16 = 0x0008;
    pub const ACC_FINAL: u16 = 0x0010;
    pub const ACC_SYNCHRONIZED: u16 = 0x0020;
    pub const ACC_BRIDGE: u16 = 0x0040;
    pub const ACC_VARARGS: u16 = 0x0080;
    pub const ACC_NATIVE: u16 = 0x0100;
    pub const ACC_ABSTRACT: u16 = 0x0400;
    pub const ACC_STRICT: u16 = 0x0800;
    pub const ACC_SYNTHETIC: u16 = 0x1000;

    #[derive(Clone, Copy, Debug)]
    pub struct Flags(u16);

    impl Flags {

        pub fn new() -> Flags {
            Flags(0)
        }

        pub fn from_bits(bits: u16) -> Flags {
            Flags(bits)
        }

        pub fn bits(&self) -> u16 {
            self.0
        }

        pub fn len() -> usize {
            2
        }

        flag_getter!(is_public, ACC_PUBLIC);
        flag_getter!(is_private, ACC_PRIVATE);
        flag_getter!(is_protected, ACC_PROTECTED);
        flag_getter!(is_static, ACC_STATIC);
        flag_getter!(is_final, ACC_FINAL);
        flag_getter!(is_synchronized, ACC_SYNCHRONIZED);
        flag_getter!(is_bridge, ACC_BRIDGE);
        flag_getter!(is_varargs, ACC_VARARGS);
        flag_getter!(is_native, ACC_NATIVE);
        flag_getter!(is_abstract, ACC_ABSTRACT);
        flag_getter!(is_strict, ACC_STRICT);
        flag_getter!(is_synthetic, ACC_SYNTHETIC);

        flag_setter!(set_public, ACC_PUBLIC);
        flag_setter!(set_private, ACC_PRIVATE);
        flag_setter!(set_protected, ACC_PROTECTED);
        flag_setter!(set_static, ACC_STATIC);
        flag_setter!(set_final, ACC_FINAL);
        flag_setter!(set_synchronized, ACC_SYNCHRONIZED);
        flag_setter!(set_bridge, ACC_BRIDGE);
        flag_setter!(set_varargs, ACC_VARARGS);
        flag_setter!(set_native, ACC_NATIVE);
        flag_setter!(set_abstract, ACC_ABSTRACT);
        flag_setter!(set_strict, ACC_STRICT);
        flag_setter!(set_synthetic, ACC_SYNTHETIC);
    }
}

pub mod constant {
    pub const TAG_CLASS: u8 = 7;
    pub const TAG_FIELD_REF: u8 = 9;
    pub const TAG_METHOD_REF: u8 = 10;
    pub const TAG_INTERFACE_METHOD_REF: u8 = 11;
    pub const TAG_STRING: u8 = 8;
    pub const TAG_INTEGER: u8 = 3;
    pub const TAG_FLOAT: u8 = 4;
    pub const TAG_LONG: u8 = 5;
    pub const TAG_DOUBLE: u8 = 6;
    pub const TAG_TYPE_AND_NAME: u8 = 12;
    pub const TAG_UTF8: u8 = 1;
    pub const TAG_METHOD_HANDLE: u8 = 15;
    pub const TAG_METHOD_TYPE: u8 = 16;
    pub const TAG_INVOKE_DYNAMIC: u8 = 18;

    pub const HANDLE_GET_FIELD: u8 = 1;
    pub const HANDLE_GET_STATIC: u8 = 2;
    pub const HANDLE_PUT_FIELD: u8 = 3;
    pub const HANDLE_PUT_STATIC: u8 = 4;

    pub const HANDLE_INVOKE_VIRTUAL: u8 = 5;
    pub const HANDLE_INVOKE_STATIC: u8 = 6;
    pub const HANDLE_INVOKE_SPECIAL: u8 = 7;
    pub const HANDLE_NEW_INVOKE_SPECIAL: u8 = 8;
    pub const HANDLE_INVOKE_INTERFACE: u8 = 9;
}

pub mod attribute {
    pub const ATTR_CONSTANT_VALUE: &'static str = "ConstantValue";
    pub const ATTR_CODE: &'static str = "Code";
    pub const ATTR_STACK_MAP_TABLE: &'static str = "StackMapTable";
    pub const ATTR_EXCEPTIONS: &'static str = "Exceptions";
    pub const ATTR_INNER_CLASSES: &'static str = "InnerClasses";
    pub const ATTR_ENCLOSING_METHOD: &'static str = "EnclosingMethod";
    pub const ATTR_SYNTHETIC: &'static str = "Synthetic";
    pub const ATTR_SIGNATURE: &'static str = "Signature";
    pub const ATTR_SOURCE_FILE: &'static str = "SourceFile";
    pub const ATTR_SOURCE_DEBUG_EXTENSION: &'static str = "SourceDebugExtension";
    pub const ATTR_LINE_NUMBER_TABLE: &'static str = "LineNumberTable";
    pub const ATTR_LOCAL_VARIABLE_TABLE: &'static str = "LocalVariableTable";
    pub const ATTR_LOCAL_VARIABLE_TYPE_TABLE: &'static str = "LocalVariableTypeTable";
    pub const ATTR_DEPRECATED: &'static str = "Deprecated";
    pub const ATTR_RUNTIME_VISIBLE_ANNOTATIONS: &'static str = "RuntimeVisibleAnnotations";
    pub const ATTR_RUNTIME_INVISIBLE_ANNOTATIONS: &'static str = "RuntimeInvisibleAnnotations";
    pub const ATTR_RUNTIME_VISIBLE_PARAMETER_ANNOTATIONS: &'static str = "RuntimeVisibleParameterAnnotations";
    pub const ATTR_RUNTIME_INVISIBLE_PARAMETER_ANNOTATIONS: &'static str = "RuntimeInvisibleParameterAnnotations";
    pub const ATTR_ANNOTATION_DEFAULT: &'static str = "AnnotationDefault";
    pub const ATTR_BOOTSTRAP_METHODS: &'static str = "BootstrapMethods";

    pub const STACK_MAP_SAME_OFFSET: u8 = 0;
    pub const STACK_MAP_SAME_MIN: u8 = 0;
    pub const STACK_MAP_SAME_MAX: u8 = 63;
    pub const STACK_MAP_SAME_EXT: u8 = 251;
    pub const STACK_MAP_SINGLE_STACK_OFFSET: u8 = 64;
    pub const STACK_MAP_SINGLE_STACK_MIN: u8 = 64;
    pub const STACK_MAP_SINGLE_STACK_MAX: u8 = 127;
    pub const STACK_MAP_SINGLE_STACK_EXT: u8 = 247;
    pub const STACK_MAP_CHOP_3: u8 = 248;
    pub const STACK_MAP_CHOP_2: u8 = 249;
    pub const STACK_MAP_CHOP_1: u8 = 250;
    pub const STACK_MAP_APPEND_3: u8 = 252;
    pub const STACK_MAP_APPEND_2: u8 = 253;
    pub const STACK_MAP_APPEND_1: u8 = 254;
    pub const STACK_MAP_FULL: u8 = 255;

    pub const VTYPE_TOP: u8 = 0;
    pub const VTYPE_INT: u8 = 1;
    pub const VTYPE_FLOAT: u8 = 2;
    pub const VTYPE_LONG: u8 = 4; // [sic]
    pub const VTYPE_DOUBLE: u8 = 3;
    pub const VTYPE_NULL: u8 = 5;
    pub const VTYPE_UNINIT_THIS: u8 = 6;
    pub const VTYPE_OBJ: u8 = 7;
    pub const VTYPE_UNINIT: u8 = 8;
}

pub mod code {
    pub const OPCODE_NOP: u8 = 0x00;

    pub const OPCODE_CONST_REF_NULL: u8 = 0x01;
    pub const OPCODE_CONST_INT_M1: u8 = 0x02;
    pub const OPCODE_CONST_INT_0: u8 = 0x03;
    pub const OPCODE_CONST_INT_1: u8 = 0x04;
    pub const OPCODE_CONST_INT_2: u8 = 0x05;
    pub const OPCODE_CONST_INT_3: u8 = 0x06;
    pub const OPCODE_CONST_INT_4: u8 = 0x07;
    pub const OPCODE_CONST_INT_5: u8 = 0x08;
    pub const OPCODE_CONST_LONG_0: u8 = 0x09;
    pub const OPCODE_CONST_LONG_1: u8 = 0x0a;
    pub const OPCODE_CONST_FLOAT_0: u8 = 0x0b;
    pub const OPCODE_CONST_FLOAT_1: u8 = 0x0c;
    pub const OPCODE_CONST_FLOAT_2: u8 = 0x0d;
    pub const OPCODE_CONST_DOUBLE_0: u8 = 0x0e;
    pub const OPCODE_CONST_DOUBLE_1: u8 = 0x0f;

    pub const OPCODE_PUSH_BYTE: u8 = 0x10;
    pub const OPCODE_PUSH_SHORT: u8 = 0x11;

    pub const OPCODE_LOAD_CONSTANT: u8 = 0x12;
    pub const OPCODE_WIDE_LOAD_CONSTANT: u8 = 0x13;
    pub const OPCODE_WIDE_LOAD_WIDE_CONSTANT: u8 = 0x14;


    pub const OPCODE_LOAD_INT: u8 = 0x15;
    pub const OPCODE_LOAD_LONG: u8 = 0x16;
    pub const OPCODE_LOAD_FLOAT: u8 = 0x17;
    pub const OPCODE_LOAD_DOUBLE: u8 = 0x18;
    pub const OPCODE_LOAD_REF: u8 = 0x19;

    pub const OPCODE_LOAD_INT_0: u8 = 0x1a;
    pub const OPCODE_LOAD_INT_1: u8 = 0x1b;
    pub const OPCODE_LOAD_INT_2: u8 = 0x1c;
    pub const OPCODE_LOAD_INT_3: u8 = 0x1d;

    pub const OPCODE_LOAD_LONG_0: u8 = 0x1e;
    pub const OPCODE_LOAD_LONG_1: u8 = 0x1f;
    pub const OPCODE_LOAD_LONG_2: u8 = 0x20;
    pub const OPCODE_LOAD_LONG_3: u8 = 0x21;

    pub const OPCODE_LOAD_FLOAT_0: u8 = 0x22;
    pub const OPCODE_LOAD_FLOAT_1: u8 = 0x23;
    pub const OPCODE_LOAD_FLOAT_2: u8 = 0x24;
    pub const OPCODE_LOAD_FLOAT_3: u8 = 0x25;

    pub const OPCODE_LOAD_DOUBLE_0: u8 = 0x26;
    pub const OPCODE_LOAD_DOUBLE_1: u8 = 0x27;
    pub const OPCODE_LOAD_DOUBLE_2: u8 = 0x28;
    pub const OPCODE_LOAD_DOUBLE_3: u8 = 0x29;

    pub const OPCODE_LOAD_REF_0: u8 = 0x2a;
    pub const OPCODE_LOAD_REF_1: u8 = 0x2b;
    pub const OPCODE_LOAD_REF_2: u8 = 0x2c;
    pub const OPCODE_LOAD_REF_3: u8 = 0x2d;

    pub const OPCODE_ARRAY_LOAD_INT: u8 = 0x2e;
    pub const OPCODE_ARRAY_LOAD_LONG: u8 = 0x2f;
    pub const OPCODE_ARRAY_LOAD_FLOAT: u8 = 0x30;
    pub const OPCODE_ARRAY_LOAD_DOUBLE: u8 = 0x31;
    pub const OPCODE_ARRAY_LOAD_REF: u8 = 0x32;
    pub const OPCODE_ARRAY_LOAD_BYTE: u8 = 0x33;
    pub const OPCODE_ARRAY_LOAD_CHAR: u8 = 0x34;
    pub const OPCODE_ARRAY_LOAD_SHORT: u8 = 0x35;


    pub const OPCODE_STORE_INT: u8 = 0x36;
    pub const OPCODE_STORE_LONG: u8 = 0x37;
    pub const OPCODE_STORE_FLOAT: u8 = 0x38;
    pub const OPCODE_STORE_DOUBLE: u8 = 0x39;
    pub const OPCODE_STORE_REF: u8 = 0x3a;

    pub const OPCODE_STORE_INT_0: u8 = 0x3b;
    pub const OPCODE_STORE_INT_1: u8 = 0x3c;
    pub const OPCODE_STORE_INT_2: u8 = 0x3d;
    pub const OPCODE_STORE_INT_3: u8 = 0x3e;

    pub const OPCODE_STORE_LONG_0: u8 = 0x3f;
    pub const OPCODE_STORE_LONG_1: u8 = 0x40;
    pub const OPCODE_STORE_LONG_2: u8 = 0x41;
    pub const OPCODE_STORE_LONG_3: u8 = 0x42;

    pub const OPCODE_STORE_FLOAT_0: u8 = 0x43;
    pub const OPCODE_STORE_FLOAT_1: u8 = 0x44;
    pub const OPCODE_STORE_FLOAT_2: u8 = 0x45;
    pub const OPCODE_STORE_FLOAT_3: u8 = 0x46;

    pub const OPCODE_STORE_DOUBLE_0: u8 = 0x47;
    pub const OPCODE_STORE_DOUBLE_1: u8 = 0x48;
    pub const OPCODE_STORE_DOUBLE_2: u8 = 0x49;
    pub const OPCODE_STORE_DOUBLE_3: u8 = 0x4a;

    pub const OPCODE_STORE_REF_0: u8 = 0x4b;
    pub const OPCODE_STORE_REF_1: u8 = 0x4c;
    pub const OPCODE_STORE_REF_2: u8 = 0x4d;
    pub const OPCODE_STORE_REF_3: u8 = 0x4e;

    pub const OPCODE_ARRAY_STORE_INT: u8 = 0x4f;
    pub const OPCODE_ARRAY_STORE_LONG: u8 = 0x50;
    pub const OPCODE_ARRAY_STORE_FLOAT: u8 = 0x51;
    pub const OPCODE_ARRAY_STORE_DOUBLE: u8 = 0x52;
    pub const OPCODE_ARRAY_STORE_REF: u8 = 0x53;
    pub const OPCODE_ARRAY_STORE_BYTE: u8 = 0x54;
    pub const OPCODE_ARRAY_STORE_CHAR: u8 = 0x55;
    pub const OPCODE_ARRAY_STORE_SHORT: u8 = 0x56;


    pub const OPCODE_POP: u8 = 0x57;
    pub const OPCODE_DOUBLE_POP: u8 = 0x58;

    pub const OPCODE_DUP: u8 = 0x59;
    pub const OPCODE_DUP_DOWN: u8 = 0x5a;
    pub const OPCODE_DUP_DOUBLE_DOWN: u8 = 0x5b;
    pub const OPCODE_DOUBLE_DUP: u8 = 0x5c;
    pub const OPCODE_DOUBLE_DUP_DOWN: u8 = 0x5d;
    pub const OPCODE_DOUBLE_DUP_DOUBLE_DOWN: u8 = 0x5e;

    pub const OPCODE_SWAP: u8 = 0x5f;


    pub const OPCODE_ADD_INT: u8 = 0x60;
    pub const OPCODE_ADD_LONG: u8 = 0x61;
    pub const OPCODE_ADD_FLOAT: u8 = 0x62;
    pub const OPCODE_ADD_DOUBLE: u8 = 0x63;

    pub const OPCODE_SUB_INT: u8 = 0x64;
    pub const OPCODE_SUB_LONG: u8 = 0x65;
    pub const OPCODE_SUB_FLOAT: u8 = 0x66;
    pub const OPCODE_SUB_DOUBLE: u8 = 0x67;

    pub const OPCODE_MUL_INT: u8 = 0x68;
    pub const OPCODE_MUL_LONG: u8 = 0x69;
    pub const OPCODE_MUL_FLOAT: u8 = 0x6a;
    pub const OPCODE_MUL_DOUBLE: u8 = 0x6b;

    pub const OPCODE_DIV_INT: u8 = 0x6c;
    pub const OPCODE_DIV_LONG: u8 = 0x6d;
    pub const OPCODE_DIV_FLOAT: u8 = 0x6e;
    pub const OPCODE_DIV_DOUBLE: u8 = 0x6f;

    pub const OPCODE_REM_INT: u8 = 0x70;
    pub const OPCODE_REM_LONG: u8 = 0x71;
    pub const OPCODE_REM_FLOAT: u8 = 0x72;
    pub const OPCODE_REM_DOUBLE: u8 = 0x73;

    pub const OPCODE_NEG_INT: u8 = 0x74;
    pub const OPCODE_NEG_LONG: u8 = 0x75;
    pub const OPCODE_NEG_FLOAT: u8 = 0x76;
    pub const OPCODE_NEG_DOUBLE: u8 = 0x77;

    pub const OPCODE_LEFT_SHIFT_INT: u8 = 0x78;
    pub const OPCODE_LEFT_SHIFT_LONG: u8 = 0x79;
    pub const OPCODE_RIGHT_SHIFT_INT: u8 = 0x7a;
    pub const OPCODE_RIGHT_SHIFT_LONG: u8 = 0x7b;
    pub const OPCODE_URIGHT_SHIFT_INT: u8 = 0x7c;
    pub const OPCODE_URIGHT_SHIFT_LONG: u8 = 0x7d;

    pub const OPCODE_AND_INT: u8 = 0x7e;
    pub const OPCODE_AND_LONG: u8 = 0x7f;

    pub const OPCODE_OR_INT: u8 = 0x80;
    pub const OPCODE_OR_LONG: u8 = 0x81;

    pub const OPCODE_XOR_INT: u8 = 0x82;
    pub const OPCODE_XOR_LONG: u8 = 0x83;

    pub const OPCODE_INC_INT: u8 = 0x84;


    pub const OPCODE_INT_TO_LONG: u8 = 0x85;
    pub const OPCODE_INT_TO_FLOAT: u8 = 0x86;
    pub const OPCODE_INT_TO_DOUBLE: u8 = 0x87;
    pub const OPCODE_LONG_TO_INT: u8 = 0x88;
    pub const OPCODE_LONG_TO_FLOAT: u8 = 0x89;
    pub const OPCODE_LONG_TO_DOUBLE: u8 = 0x8a;
    pub const OPCODE_FLOAT_TO_INT: u8 = 0x8b;
    pub const OPCODE_FLOAT_TO_LONG: u8 = 0x8c;
    pub const OPCODE_FLOAT_TO_DOUBLE: u8 = 0x8d;
    pub const OPCODE_DOUBLE_TO_INT: u8 = 0x8e;
    pub const OPCODE_DOUBLE_TO_LONG: u8 = 0x8f;
    pub const OPCODE_DOUBLE_TO_FLOAT: u8 = 0x90;

    pub const OPCODE_INT_TO_BYTE: u8 = 0x91;
    pub const OPCODE_INT_TO_CHAR: u8 = 0x92;
    pub const OPCODE_INT_TO_SHORT: u8 = 0x93;


    pub const OPCODE_COMPARE_LONG: u8 = 0x94;
    pub const OPCODE_COMPARE_FLOAT_L: u8 = 0x95;
    pub const OPCODE_COMPARE_FLOAT_G: u8 = 0x96;
    pub const OPCODE_COMPARE_DOUBLE_L: u8 = 0x97;
    pub const OPCODE_COMPARE_DOUBLE_G: u8 = 0x98;

    pub const OPCODE_IF_INT_EQ_0: u8 = 0x99;
    pub const OPCODE_IF_INT_NE_0: u8 = 0x9a;
    pub const OPCODE_IF_INT_LT_0: u8 = 0x9b;
    pub const OPCODE_IF_INT_GE_0: u8 = 0x9c;
    pub const OPCODE_IF_INT_GT_0: u8 = 0x9d;
    pub const OPCODE_IF_INT_LE_0: u8 = 0x9e;

    pub const OPCODE_IF_INT_EQ: u8 = 0x9f;
    pub const OPCODE_IF_INT_NE: u8 = 0xa0;
    pub const OPCODE_IF_INT_LT: u8 = 0xa1;
    pub const OPCODE_IF_INT_GE: u8 = 0xa2;
    pub const OPCODE_IF_INT_GT: u8 = 0xa3;
    pub const OPCODE_IF_INT_LE: u8 = 0xa4;

    pub const OPCODE_IF_REF_EQ: u8 = 0xa5;
    pub const OPCODE_IF_REF_NE: u8 = 0xa6;

    pub const OPCODE_GOTO: u8 = 0xa7;
    pub const OPCODE_JUMP_SUB: u8 = 0xa8;
    pub const OPCODE_RET_SUB: u8 = 0xa9;

    pub const OPCODE_TABLE_SWITCH: u8 = 0xaa;
    pub const OPCODE_LOOKUP_SWITCH: u8 = 0xab;

    pub const OPCODE_RETURN_INT: u8 = 0xac;
    pub const OPCODE_RETURN_LONG: u8 = 0xad;
    pub const OPCODE_RETURN_FLOAT: u8 = 0xae;
    pub const OPCODE_RETURN_DOUBLE: u8 = 0xaf;
    pub const OPCODE_RETURN_REF: u8 = 0xb0;
    pub const OPCODE_RETURN_VOID: u8 = 0xb1;


    pub const OPCODE_GET_STATIC_FIELD: u8 = 0xb2;
    pub const OPCODE_PUT_STATIC_FIELD: u8 = 0xb3;
    pub const OPCODE_GET_FIELD: u8 = 0xb4;
    pub const OPCODE_PUT_FIELD: u8 = 0xb5;

    pub const OPCODE_INVOKE_VIRTUAL: u8 = 0xb6;
    pub const OPCODE_INVOKE_SPECIAL: u8 = 0xb7;
    pub const OPCODE_INVOKE_STATIC: u8 = 0xb8;
    pub const OPCODE_INVOKE_INTERFACE: u8 = 0xb9;
    pub const OPCODE_INVOKE_DYNAMIC: u8 = 0xba;

    pub const OPCODE_NEW: u8 = 0xbb;
    pub const OPCODE_NEW_PRIMITIVE_ARRAY: u8 = 0xbc;
    pub const OPCODE_NEW_REF_ARRAY: u8 = 0xbd;

    pub const OPCODE_ARRAY_LEN: u8 = 0xbe;

    pub const OPCODE_THROW: u8 = 0xbf;

    pub const OPCODE_CHECK_CAST: u8 = 0xc0;
    pub const OPCODE_INSTANCE_OF: u8 = 0xc1;

    pub const OPCODE_ENTER_MONITOR: u8 = 0xc2;
    pub const OPCODE_EXIT_MONITOR: u8 = 0xc3;

    pub const OPCODE_WIDE: u8 = 0xc4;

    pub const OPCODE_NEW_REF_MULTI_ARRAY: u8 = 0xc5;
    pub const OPCODE_IF_REF_NULL: u8 = 0xc6;
    pub const OPCODE_IF_REF_NON_NULL: u8 = 0xc7;
    pub const OPCODE_WIDE_GOTO: u8 = 0xc8;
    pub const OPCODE_WIDE_JUMP_SUB: u8 = 0xc9;


    pub const OPCODE_BREAKPOINT: u8 = 0xca;
    pub const OPCODE_IMPLEMENTATION_DEFINED_1: u8 = 0xfe;
    pub const OPCODE_IMPLEMENTATION_DEFINED_2: u8 = 0xff;



    pub const ARRAY_BOOLEAN: u8 = 4;
    pub const ARRAY_CHAR: u8 = 5;
    pub const ARRAY_FLOAT: u8 = 6;
    pub const ARRAY_DOUBLE: u8 = 7;
    pub const ARRAY_BYTE: u8 = 8;
    pub const ARRAY_SHORT: u8 = 9;
    pub const ARRAY_INT: u8 = 10;
    pub const ARRAY_LONG: u8 = 11;
}