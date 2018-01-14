pub mod instructions {

    // assembler directives
    pub const CLASS: &'static str = "class";
    pub const FIELD: &'static str = "field";
    pub const METHOD: &'static str = "method";

    pub const SUPER: &'static str = "super";
    pub const IMPL: &'static str = "impl";

    pub const VERSION: &'static str = "version";

    pub const FLAGS: &'static str = "flags";

    pub const STACK: &'static str = "stack";
    pub const LOCALS: &'static str = "locals";
    pub const CATCH: &'static str = "catch";

    pub const CONST_VALUE: &'static str = "const_val";

    pub const ATTR: &'static str = "attr";

    // constants
    pub const CLASS_REF: &'static str = "class_ref";
    pub const FIELD_REF: &'static str = "field_ref";
    pub const METHOD_REF: &'static str = "method_ref";
    pub const INTERFACE_METHOD_REF: &'static str = "imethod_ref";
    pub const STRING: &'static str = "string";
    pub const INTEGER: &'static str = "int";
    pub const FLOAT: &'static str = "float";
    pub const LONG: &'static str = "long";
    pub const DOUBLE: &'static str = "double";
    pub const NAME_AND_TYPE: &'static str = "name_and_type";
    pub const UTF8: &'static str = "utf8";
    pub const METHOD_HANDLE: &'static str = "method_handle";
    pub const METHOD_TYPE: &'static str = "method_type";
    pub const DYNAMIC_TARGET: &'static str = "dynamic_target"; // this is CONSTANT_InvokeDynamic

    // opcodes
    pub const NOP: &'static str = "nop";

    pub const CONST_REF_NULL: &'static str = "aconst_null";
    pub const CONST_INT_M1: &'static str = "iconst_m1";
    pub const CONST_INT_0: &'static str = "iconst_0";
    pub const CONST_INT_1: &'static str = "iconst_1";
    pub const CONST_INT_2: &'static str = "iconst_2";
    pub const CONST_INT_3: &'static str = "iconst_3";
    pub const CONST_INT_4: &'static str = "iconst_4";
    pub const CONST_INT_5: &'static str = "iconst_5";
    pub const CONST_LONG_0: &'static str = "lconst_0";
    pub const CONST_LONG_1: &'static str = "lconst_1";
    pub const CONST_FLOAT_0: &'static str = "fconst_0";
    pub const CONST_FLOAT_1: &'static str = "fconst_1";
    pub const CONST_FLOAT_2: &'static str = "fconst_2";
    pub const CONST_DOUBLE_0: &'static str = "dconst_0";
    pub const CONST_DOUBLE_1: &'static str = "dconst_1";

    pub const PUSH_BYTE: &'static str = "bipush";
    pub const PUSH_SHORT: &'static str = "sipush";

    pub const LOAD_CONSTANT: &'static str = "ldc";
    pub const WIDE_LOAD_CONSTANT: &'static str = "ldc_w";
    pub const WIDE_LOAD_WIDE_CONSTANT: &'static str = "ldc2_w";


    pub const LOAD_INT: &'static str = "iload";
    pub const LOAD_LONG: &'static str = "lload";
    pub const LOAD_FLOAT: &'static str = "fload";
    pub const LOAD_DOUBLE: &'static str = "dload";
    pub const LOAD_REF: &'static str = "aload";

    pub const LOAD_INT_0: &'static str = "iload_0";
    pub const LOAD_INT_1: &'static str = "iload_1";
    pub const LOAD_INT_2: &'static str = "iload_2";
    pub const LOAD_INT_3: &'static str = "iload_3";

    pub const LOAD_LONG_0: &'static str = "lload_0";
    pub const LOAD_LONG_1: &'static str = "lload_1";
    pub const LOAD_LONG_2: &'static str = "lload_2";
    pub const LOAD_LONG_3: &'static str = "lload_3";

    pub const LOAD_FLOAT_0: &'static str = "fload_0";
    pub const LOAD_FLOAT_1: &'static str = "fload_1";
    pub const LOAD_FLOAT_2: &'static str = "fload_2";
    pub const LOAD_FLOAT_3: &'static str = "fload_3";

    pub const LOAD_DOUBLE_0: &'static str = "dload_0";
    pub const LOAD_DOUBLE_1: &'static str = "dload_1";
    pub const LOAD_DOUBLE_2: &'static str = "dload_2";
    pub const LOAD_DOUBLE_3: &'static str = "dload_3";

    pub const LOAD_REF_0: &'static str = "aload_0";
    pub const LOAD_REF_1: &'static str = "aload_1";
    pub const LOAD_REF_2: &'static str = "aload_2";
    pub const LOAD_REF_3: &'static str = "aload_3";

    pub const ARRAY_LOAD_INT: &'static str = "iaload";
    pub const ARRAY_LOAD_LONG: &'static str = "laload";
    pub const ARRAY_LOAD_FLOAT: &'static str = "faload";
    pub const ARRAY_LOAD_DOUBLE: &'static str = "daload";
    pub const ARRAY_LOAD_REF: &'static str = "aaload";
    pub const ARRAY_LOAD_BYTE: &'static str = "baload";
    pub const ARRAY_LOAD_CHAR: &'static str = "caload";
    pub const ARRAY_LOAD_SHORT: &'static str = "saload";


    pub const STORE_INT: &'static str = "istore";
    pub const STORE_LONG: &'static str = "lstore";
    pub const STORE_FLOAT: &'static str = "fstore";
    pub const STORE_DOUBLE: &'static str = "dstore";
    pub const STORE_REF: &'static str = "astore";

    pub const STORE_INT_0: &'static str = "istore_0";
    pub const STORE_INT_1: &'static str = "istore_1";
    pub const STORE_INT_2: &'static str = "istore_2";
    pub const STORE_INT_3: &'static str = "istore_3";

    pub const STORE_LONG_0: &'static str = "lstore_0";
    pub const STORE_LONG_1: &'static str = "lstore_1";
    pub const STORE_LONG_2: &'static str = "lstore_2";
    pub const STORE_LONG_3: &'static str = "lstore_3";

    pub const STORE_FLOAT_0: &'static str = "fstore_0";
    pub const STORE_FLOAT_1: &'static str = "fstore_1";
    pub const STORE_FLOAT_2: &'static str = "fstore_2";
    pub const STORE_FLOAT_3: &'static str = "fstore_3";

    pub const STORE_DOUBLE_0: &'static str = "dstore_0";
    pub const STORE_DOUBLE_1: &'static str = "dstore_1";
    pub const STORE_DOUBLE_2: &'static str = "dstore_2";
    pub const STORE_DOUBLE_3: &'static str = "dstore_3";

    pub const STORE_REF_0: &'static str = "astore_0";
    pub const STORE_REF_1: &'static str = "astore_1";
    pub const STORE_REF_2: &'static str = "astore_2";
    pub const STORE_REF_3: &'static str = "astore_3";

    pub const ARRAY_STORE_INT: &'static str = "iastore";
    pub const ARRAY_STORE_LONG: &'static str = "lastore";
    pub const ARRAY_STORE_FLOAT: &'static str = "fastore";
    pub const ARRAY_STORE_DOUBLE: &'static str = "dastore";
    pub const ARRAY_STORE_REF: &'static str = "aastore";
    pub const ARRAY_STORE_BYTE: &'static str = "bastore";
    pub const ARRAY_STORE_CHAR: &'static str = "castore";
    pub const ARRAY_STORE_SHORT: &'static str = "sastore";


    pub const POP: &'static str = "pop";
    pub const DOUBLE_POP: &'static str = "pop2";

    pub const DUP: &'static str = "dup";
    pub const DUP_DOWN: &'static str = "dup_x1";
    pub const DUP_DOUBLE_DOWN: &'static str = "dup_x2";
    pub const DOUBLE_DUP: &'static str = "dup2";
    pub const DOUBLE_DUP_DOWN: &'static str = "dup2_x1";
    pub const DOUBLE_DUP_DOUBLE_DOWN: &'static str = "dup2_x2";

    pub const SWAP: &'static str = "swap";


    pub const ADD_INT: &'static str = "iadd";
    pub const ADD_LONG: &'static str = "ladd";
    pub const ADD_FLOAT: &'static str = "fadd";
    pub const ADD_DOUBLE: &'static str = "dadd";

    pub const SUB_INT: &'static str = "isub";
    pub const SUB_LONG: &'static str = "lsub";
    pub const SUB_FLOAT: &'static str = "fsub";
    pub const SUB_DOUBLE: &'static str = "dsub";

    pub const MUL_INT: &'static str = "imul";
    pub const MUL_LONG: &'static str = "lmul";
    pub const MUL_FLOAT: &'static str = "fmul";
    pub const MUL_DOUBLE: &'static str = "dmul";

    pub const DIV_INT: &'static str = "idiv";
    pub const DIV_LONG: &'static str = "ldiv";
    pub const DIV_FLOAT: &'static str = "fdiv";
    pub const DIV_DOUBLE: &'static str = "ddiv";

    pub const REM_INT: &'static str = "irem";
    pub const REM_LONG: &'static str = "lrem";
    pub const REM_FLOAT: &'static str = "frem";
    pub const REM_DOUBLE: &'static str = "drem";

    pub const NEG_INT: &'static str = "ineg";
    pub const NEG_LONG: &'static str = "lneg";
    pub const NEG_FLOAT: &'static str = "fneg";
    pub const NEG_DOUBLE: &'static str = "dneg";

    pub const LEFT_SHIFT_INT: &'static str = "ishl";
    pub const LEFT_SHIFT_LONG: &'static str = "lshl";
    pub const RIGHT_SHIFT_INT: &'static str = "ishr";
    pub const RIGHT_SHIFT_LONG: &'static str = "lshr";
    pub const URIGHT_SHIFT_INT: &'static str = "iushr";
    pub const URIGHT_SHIFT_LONG: &'static str = "lushr";

    pub const AND_INT: &'static str = "iand";
    pub const AND_LONG: &'static str = "land";

    pub const OR_INT: &'static str = "ior";
    pub const OR_LONG: &'static str = "lor";

    pub const XOR_INT: &'static str = "ixor";
    pub const XOR_LONG: &'static str = "lxor";

    pub const INC_INT: &'static str = "iinc";


    pub const INT_TO_LONG: &'static str = "i2l";
    pub const INT_TO_FLOAT: &'static str = "i2f";
    pub const INT_TO_DOUBLE: &'static str = "i2d";
    pub const LONG_TO_INT: &'static str = "l2i";
    pub const LONG_TO_FLOAT: &'static str = "l2f";
    pub const LONG_TO_DOUBLE: &'static str = "l2d";
    pub const FLOAT_TO_INT: &'static str = "f2i";
    pub const FLOAT_TO_LONG: &'static str = "f2l";
    pub const FLOAT_TO_DOUBLE: &'static str = "f2d";
    pub const DOUBLE_TO_INT: &'static str = "d2i";
    pub const DOUBLE_TO_LONG: &'static str = "d2l";
    pub const DOUBLE_TO_FLOAT: &'static str = "d2f";

    pub const INT_TO_BYTE: &'static str = "i2b";
    pub const INT_TO_CHAR: &'static str = "i2c";
    pub const INT_TO_SHORT: &'static str = "i2s";


    pub const COMPARE_LONG: &'static str = "lcmp";
    pub const COMPARE_FLOAT_L: &'static str = "fcmpl";
    pub const COMPARE_FLOAT_G: &'static str = "fcmpg";
    pub const COMPARE_DOUBLE_L: &'static str = "dcmpl";
    pub const COMPARE_DOUBLE_G: &'static str = "dcmpg";

    pub const IF_INT_EQ_0: &'static str = "ifeq";
    pub const IF_INT_NE_0: &'static str = "ifne";
    pub const IF_INT_LT_0: &'static str = "iflt";
    pub const IF_INT_GE_0: &'static str = "ifge";
    pub const IF_INT_GT_0: &'static str = "ifgt";
    pub const IF_INT_LE_0: &'static str = "ifle";

    pub const IF_INT_EQ: &'static str = "if_icmpeq";
    pub const IF_INT_NE: &'static str = "if_icmpne";
    pub const IF_INT_LT: &'static str = "if_icmplt";
    pub const IF_INT_GE: &'static str = "if_icmpge";
    pub const IF_INT_GT: &'static str = "if_icmpgt";
    pub const IF_INT_LE: &'static str = "if_icmple";

    pub const IF_REF_EQ: &'static str = "if_acmpeq";
    pub const IF_REF_NE: &'static str = "if_acmpne";

    pub const GOTO: &'static str = "goto";
    pub const JUMP_SUB: &'static str = "jsr";
    pub const RET_SUB: &'static str = "ret";

    pub const TABLE_SWITCH: &'static str = "tableswitch";
    pub const LOOKUP_SWITCH: &'static str = "lookupswitch";

    pub const RETURN_INT: &'static str = "ireturn";
    pub const RETURN_LONG: &'static str = "lreturn";
    pub const RETURN_FLOAT: &'static str = "freturn";
    pub const RETURN_DOUBLE: &'static str = "dreturn";
    pub const RETURN_REF: &'static str = "areturn";
    pub const RETURN_VOID: &'static str = "return";


    pub const GET_STATIC_FIELD: &'static str = "getstatic";
    pub const PUT_STATIC_FIELD: &'static str = "putstatic";
    pub const GET_FIELD: &'static str = "getfield";
    pub const PUT_FIELD: &'static str = "setfield";

    pub const INVOKE_VIRTUAL: &'static str = "invokevirtual";
    pub const INVOKE_SPECIAL: &'static str = "invokespecial";
    pub const INVOKE_STATIC: &'static str = "invokestatic";
    pub const INVOKE_INTERFACE: &'static str = "invokeinterface";
    pub const INVOKE_DYNAMIC: &'static str = "invokedynamic";

    pub const NEW: &'static str = "new";
    pub const NEW_PRIMITIVE_ARRAY: &'static str = "newarray";
    pub const NEW_REF_ARRAY: &'static str = "anewarray";

    pub const ARRAY_LEN: &'static str = "arraylength";

    pub const THROW: &'static str = "athrow";

    pub const CHECK_CAST: &'static str = "checkcast";
    pub const INSTANCE_OF: &'static str = "instanceof";

    pub const ENTER_MONITOR: &'static str = "monitorenter";
    pub const EXIT_MONITOR: &'static str = "monitorexit";

    pub const WIDE: &'static str = "wide";

    pub const NEW_REF_MULTI_ARRAY: &'static str = "multinewarray";
    pub const IF_REF_NULL: &'static str = "ifnull";
    pub const IF_REF_NON_NULL: &'static str = "ifnonnull";
    pub const WIDE_GOTO: &'static str = "goto_w";
    pub const WIDE_JUMP_SUB: &'static str = "jsr_w";


    pub const BREAKPOINT: &'static str = "breakpoint";
    pub const IMPLEMENTATION_DEFINED_1: &'static str = "impdep1";
    pub const IMPLEMENTATION_DEFINED_2: &'static str = "impdep2";
}

pub mod special {

    pub const HANDLE_KIND_GET_FIELD: &'static str = "getfield";
    pub const HANDLE_KIND_GET_STATIC: &'static str = "getstatic";
    pub const HANDLE_KIND_PUT_FIELD: &'static str = "setfield";
    pub const HANDLE_KIND_PUT_STATIC: &'static str = "setstatic";

    pub const HANDLE_KIND_INVOKE_VIRTUAL: &'static str = "invokevirtual";
    pub const HANDLE_KIND_INVOKE_STATIC: &'static str = "invokestatic";
    pub const HANDLE_KIND_INVOKE_SPECIAL: &'static str = "invokespecial";
    pub const HANDLE_KIND_NEW_INVOKE_SPECIAL: &'static str = "new_invokespecial";
    pub const HANDLE_KIND_INVOKE_INTERFACE: &'static str = "invokeinterface";

    pub const ARR_BOOLEAN: &'static str = "boolean";
    pub const ARR_CHAR: &'static str = "char";
    pub const ARR_FLOAT: &'static str = "float";
    pub const ARR_DOUBLE: &'static str = "double";
    pub const ARR_BYTE: &'static str = "byte";
    pub const ARR_SHORT: &'static str = "short";
    pub const ARR_INT: &'static str = "int";
    pub const ARR_LONG: &'static str = "long";
}

pub mod flags {

    pub const PUBLIC: &'static str = "public";
    pub const PROTECTED: &'static str = "protected";
    pub const PRIVATE: &'static str = "private";
    pub const FINAL: &'static str = "final";
    pub const SUPER: &'static str = "super";
    pub const INTERFACE: &'static str = "interface";
    pub const ABSTRACT: &'static str = "abstract";
    pub const SYNTHETIC: &'static str = "synthetic";
    pub const ANNOTATION: &'static str = "annotation";
    pub const ENUM: &'static str = "enum";
    pub const STATIC: &'static str = "static";
    pub const VOLATILE: &'static str = "volatile";
    pub const TRANSIENT: &'static str = "transient";
    pub const SYNCHRONIZED: &'static str = "synchronized";
    pub const BRIDGE: &'static str = "bridge";
    pub const VARARGS: &'static str = "varargs";
    pub const NATIVE: &'static str = "native";
    pub const STRICT: &'static str = "strictfp";
}