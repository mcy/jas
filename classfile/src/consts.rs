
pub const MAGIC: u32 = 0xcafebabe;

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
}