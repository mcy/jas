use byteorder::*;
use std::io::*;

use super::*;

use consts;
use indexing::*;

macro_rules! invalid_data {
    ($fmt:expr $(,$args:expr)*) =>
        {Error::new(ErrorKind::InvalidData, format!($fmt $(,$args)*))};
}

pub fn parse_class<R: Read>(bytes: &mut R) -> Result<Class> {

    let magic = bytes.read_u32::<BigEndian>()?;
    if magic != consts::class::MAGIC {
        Err(invalid_data!("classfile did not start with magic bytes"))?;
    }

    let minor_version = bytes.read_u16::<BigEndian>()?;
    let major_version = bytes.read_u16::<BigEndian>()?;

    let num_constants = bytes.read_u16::<BigEndian>()?;

    let mut constant_pool = Vec::with_capacity(num_constants as usize);

    let mut is_wide = false;
    for i in 1..num_constants {
        if is_wide {
            constant_pool.push(Constant::WidePlaceholder);
            is_wide = false;
        } else {
            let constant = parse_constant(bytes, i)?;
            constant_pool.push(constant.0);
            is_wide = constant.1;
        }
    }

    let flags = consts::class::Flags::from_bits(bytes.read_u16::<BigEndian>()?);

    let this_class = bytes.read_constant_index()?;
    let super_class = bytes.read_constant_index()?;

    let num_interfaces = bytes.read_u16::<BigEndian>()?;
    let mut interfaces = Vec::with_capacity(num_interfaces as usize);

    for _ in 0..num_interfaces {
        interfaces.push(bytes.read_constant_index()?);
    }

    let num_fields = bytes.read_u16::<BigEndian>()?;
    let mut fields = Vec::with_capacity(num_fields as usize);

    for i in 0..num_fields {
        fields.push(parse_field(bytes, i, &constant_pool)?);
    }

    let num_methods = bytes.read_u16::<BigEndian>()?;
    let mut methods = Vec::with_capacity(num_methods as usize);

    for i in 0..num_methods {
        methods.push(parse_method(bytes, i, &constant_pool)?);
    }

    let num_attrs = bytes.read_u16::<BigEndian>()?;
    let mut attributes = Vec::with_capacity(num_attrs as usize);

    for i in 0..num_attrs {
        attributes.push(parse_attribute(bytes, i, &constant_pool)?);
    }

    Ok(Class{
        minor_version,
        major_version,
        constant_pool,
        flags,
        this_class,
        super_class,
        interfaces,
        fields,
        methods,
        attributes
    })
}

fn parse_constant<R: Read>(bytes: &mut R, index: u16) -> Result<(Constant, bool)> {
    let mut is_wide_constant = false;
    let constant = match bytes.read_u8()? {
        7 => Constant::Class(bytes.read_constant_index()?),
        9 => Constant::FieldRef {
            class: bytes.read_constant_index()?,
            signature: bytes.read_constant_index()?,
        },
        10 => Constant::MethodRef {
            class: bytes.read_constant_index()?,
            signature: bytes.read_constant_index()?,
        },
        11 => Constant::InterfaceMethodRef {
            class: bytes.read_constant_index()?,
            signature: bytes.read_constant_index()?,
        },
        8 => Constant::String(bytes.read_constant_index()?),
        3 => Constant::Integer(bytes.read_i32::<BigEndian>()?),
        4 => Constant::Float(bytes.read_f32::<BigEndian>()?),
        5 => {
            is_wide_constant = true;
            Constant::Long(bytes.read_i64::<BigEndian>()?)
        },
        6 => {
            is_wide_constant = true;
            Constant::Double(bytes.read_f64::<BigEndian>()?)
        },
        12 => Constant::Signature {
            name: bytes.read_constant_index()?,
            descriptor: bytes.read_constant_index()?,
        },
        1 => {
            let len = bytes.read_u16::<BigEndian>()?;
            let mut buf = vec![0; len as usize];
            bytes.read_exact(&mut buf[..])?;
            let string = String::from_utf8(buf)
                .map_err(
                    |_| invalid_data!("constant {} was a UTF8 constant but did not contain UTF8 data", index))?;
            Constant::Utf8(string)
        },
        15 => Constant::MethodHandle {
            kind: {
                let kind = bytes.read_u8()?;
                MethodHandleKind::from_byte(kind).ok_or(
                    invalid_data!("constant {} was a MethodHandle with invalid kind byte {}", index, kind))?
            },
            referent: bytes.read_constant_index()?,
        },
        16 => Constant::MethodType(bytes.read_constant_index()?),
        18 => Constant::InvokeDynamic {
            bootstrap_method: bytes.read_constant_index()?,
            signature: bytes.read_constant_index()?,
        },
        invalid => Err(invalid_data!("constant {} has invalid tag {}", index, invalid))?,
    };

    Ok((constant, is_wide_constant))
}

fn parse_field<R: Read>(bytes: &mut R, index: u16, constants: &Vec<Constant>) -> Result<Field> {
    let flags = consts::field::Flags::from_bits(bytes.read_u16::<BigEndian>()?);
    let name = bytes.read_constant_index()?;
    let descriptor = bytes.read_constant_index()?;
    let num_attrs = bytes.read_u16::<BigEndian>()?;
    let mut attributes = Vec::with_capacity(num_attrs as usize);
    for i in 0..num_attrs {
        attributes.push(parse_attribute(bytes, i, constants)?);
    }
    Ok(Field {
        flags, name, descriptor, attributes,
    })
}

fn parse_method<R: Read>(bytes: &mut R, index: u16, constants: &Vec<Constant>) -> Result<Method> {
    let flags = consts::method::Flags::from_bits(bytes.read_u16::<BigEndian>()?);
    let name = bytes.read_constant_index()?;
    let descriptor = bytes.read_constant_index()?;
    let num_attrs = bytes.read_u16::<BigEndian>()?;
    let mut attributes = Vec::with_capacity(num_attrs as usize);
    for i in 0..num_attrs {
        attributes.push(parse_attribute(bytes, i, constants)?);
    }
    Ok(Method {
        flags, name, descriptor, attributes,
    })
}

fn parse_attribute<R: Read>(bytes: &mut R, index: u16, constants: &Vec<Constant>) -> Result<Attribute> {
    let name_index = bytes.read_constant_index()?;
    let len = bytes.read_u32::<BigEndian>()?;

    let name: &str = match constants[(name_index.0 - 1) as usize] {
        Constant::Utf8(ref string) => &string,
        _ => Err(invalid_data!("attribute {}'s name points to non-Utf8 constant {}", index, name_index.0))?,
    };

    use consts::attribute as attr;
    let info = match name {
        attr::ATTR_CONSTANT_VALUE => AttributeInfo::ConstantValue(bytes.read_constant_index()?),
        attr::ATTR_CODE => {
            let max_stack = bytes.read_code_index()?;
            let max_locals = bytes.read_code_index()?;
            let code_len = bytes.read_u32::<BigEndian>()?;
            let code = parse_code(bytes, code_len)?;
            let num_exceptions = bytes.read_u16::<BigEndian>()?;
            let mut exception_table = Vec::with_capacity(num_exceptions as usize);
            for _ in 0..num_exceptions {
                let try_range = (bytes.read_code_index()?, bytes.read_code_index()?);
                let handler_goto = bytes.read_code_index()?;
                let exception_type = bytes.read_constant_index()?;

                exception_table.push(ExceptionTableEntry { try_range, handler_goto, exception_type, });
            }
            let num_attributes = bytes.read_u16::<BigEndian>()?;
            let mut attributes = Vec::with_capacity(num_exceptions as usize);
            for i in 0..num_attributes {
                attributes.push(parse_attribute(bytes, i, constants)?);
            }
            AttributeInfo::Code {
                max_stack,
                max_locals,
                code_len,
                code,
                exception_table,
                attributes,
            }
        },
        /*"StackMapTable" => unimplemented!(),*/
        attr::ATTR_STACK_MAP_TABLE => {
            fn parse_ty<R: Read>(bytes: &mut R) -> Result<VerificationType> {
                let tag = bytes.read_u8()?;
                Ok(match tag {
                    attr::VTYPE_TOP => VerificationType::Top,
                    attr::VTYPE_INT => VerificationType::Int,
                    attr::VTYPE_FLOAT => VerificationType::Float,
                    attr::VTYPE_LONG => VerificationType::Long,
                    attr::VTYPE_DOUBLE => VerificationType::Double,
                    attr::VTYPE_NULL => VerificationType::Null,
                    attr::VTYPE_OBJ => VerificationType::Object(bytes.read_constant_index()?),
                    attr::VTYPE_UNINIT => VerificationType::Uninitialized(bytes.read_code_index()?),
                    attr::VTYPE_UNINIT_THIS => VerificationType::UninitializedThis,
                    _ => Err(invalid_data!("unknown verification type {}", tag))?,
                })
            }
            let len = bytes.read_u16::<BigEndian>()?;
            let mut frames = Vec::with_capacity(len as usize);
            for _ in 0..len {
                let frame_ty = bytes.read_u8()?;
                let frame = match frame_ty {
                    attr::STACK_MAP_SAME_MIN ...
                    attr::STACK_MAP_SAME_MAX =>
                        StackMapFrame::Same(CodeIndex(frame_ty as u16 + attr::STACK_MAP_SAME_OFFSET as u16 )),
                    attr::STACK_MAP_SAME_EXT =>
                        StackMapFrame::SameExt(bytes.read_code_index()?),
                    attr::STACK_MAP_SINGLE_STACK_MIN ...
                    attr::STACK_MAP_SINGLE_STACK_MAX =>
                        StackMapFrame::SingleStack(CodeIndex(frame_ty as u16 + attr::STACK_MAP_SINGLE_STACK_OFFSET as u16 ), parse_ty(bytes)?),
                    attr::STACK_MAP_SINGLE_STACK_EXT =>
                        StackMapFrame::SingleStackExt(bytes.read_code_index()?, parse_ty(bytes)?),
                    attr::STACK_MAP_CHOP_1 =>
                        StackMapFrame::Chop1(bytes.read_code_index()?),
                    attr::STACK_MAP_CHOP_2 =>
                        StackMapFrame::Chop2(bytes.read_code_index()?),
                    attr::STACK_MAP_CHOP_3 =>
                        StackMapFrame::Chop3(bytes.read_code_index()?),
                    attr::STACK_MAP_APPEND_1 =>
                        StackMapFrame::Append1(bytes.read_code_index()?, parse_ty(bytes)?),
                    attr::STACK_MAP_APPEND_2 =>
                        StackMapFrame::Append2(bytes.read_code_index()?, parse_ty(bytes)?, parse_ty(bytes)?),
                    attr::STACK_MAP_APPEND_3 =>
                        StackMapFrame::Append3(bytes.read_code_index()?, parse_ty(bytes)?, parse_ty(bytes)?, parse_ty(bytes)?),
                    attr::STACK_MAP_FULL => {
                        let offset = bytes.read_code_index()?;
                        let locals_len = bytes.read_u16::<BigEndian>()?;
                        let mut locals = Vec::with_capacity(locals_len as usize);
                        for _ in 0..locals_len {
                            locals.push(parse_ty(bytes)?);
                        }
                        let stack_len = bytes.read_u16::<BigEndian>()?;
                        let mut stack = Vec::with_capacity(stack_len as usize);
                        for _ in 0..stack_len {
                            stack.push(parse_ty(bytes)?);
                        }
                        StackMapFrame::Full {
                            offset, locals, stack,
                        }
                    },
                    _ => Err(invalid_data!("unknown stack map frame type {}", frame_ty))?,
                };
                frames.push(frame);
            }
            AttributeInfo::StackMapTable(frames)
        },
        attr::ATTR_EXCEPTIONS => {
            let num_exceptions = bytes.read_u16::<BigEndian>()?;
            let mut exceptions = Vec::with_capacity(num_exceptions as usize);
            for _ in 0..num_exceptions {
                exceptions.push(bytes.read_constant_index()?);
            }
            AttributeInfo::Exceptions(exceptions)
        },
        attr::ATTR_INNER_CLASSES => {
            let num_classes = bytes.read_u16::<BigEndian>()?;
            let mut classes = Vec::with_capacity(num_classes as usize);
            for _ in 0..num_classes {
                let inner_class = bytes.read_constant_index()?;
                let outer_class = bytes.read_constant_index()?;
                let inner_name = bytes.read_constant_index()?;
                let inner_flags = consts::class::Flags::from_bits(bytes.read_u16::<BigEndian>()?);
                classes.push(InnerClass {
                    inner_class, outer_class,
                    inner_name, inner_flags,
                });
            }
            AttributeInfo::InnerClasses(classes)
        },
        attr::ATTR_ENCLOSING_METHOD => AttributeInfo::EnclosingMethod {
            class: bytes.read_constant_index()?,
            method: bytes.read_constant_index()?,
        },
        attr::ATTR_SYNTHETIC => AttributeInfo::Synthetic,
        attr::ATTR_SIGNATURE => AttributeInfo::Signature(bytes.read_constant_index()?),
        attr::ATTR_SOURCE_FILE => AttributeInfo::SourceFile(bytes.read_constant_index()?),
        attr::ATTR_SOURCE_DEBUG_EXTENSION => {
            let mut buf = vec![0; len as usize];
            bytes.read_exact(&mut buf[..])?;
            AttributeInfo::SourceDebugExtension(buf)
        },
        attr::ATTR_LINE_NUMBER_TABLE => {
            let num_line_numbers = bytes.read_u16::<BigEndian>()?;
            let mut line_numbers = Vec::with_capacity(num_line_numbers as usize);
            for _ in 0..num_line_numbers {
                let instruction = bytes.read_code_index()?;
                let line_number = bytes.read_u16::<BigEndian>()?;
                line_numbers.push(LineNumber {
                    instruction, line_number
                });
            }
            AttributeInfo::LineNumberTable(line_numbers)
        },
        attr::ATTR_LOCAL_VARIABLE_TABLE => {
            let num_local_vars = bytes.read_u16::<BigEndian>()?;
            let mut local_vars = Vec::with_capacity(num_local_vars as usize);
            for _ in 0..num_local_vars {
                let valid_start = bytes.read_code_index()?;
                let valid_length = bytes.read_code_index()?;
                let name = bytes.read_constant_index()?;
                let descriptor = bytes.read_constant_index()?;
                let variable_index = bytes.read_var_index()?;

                local_vars.push(LocalVariable {
                    valid_start, valid_length, name,
                    descriptor, variable_index,
                });
            }
            AttributeInfo::LocalVariableTable(local_vars)
        },
        attr::ATTR_LOCAL_VARIABLE_TYPE_TABLE => {
            let num_local_vars = bytes.read_u16::<BigEndian>()?;
            let mut local_vars = Vec::with_capacity(num_local_vars as usize);
            for _ in 0..num_local_vars {
                let valid_start = bytes.read_code_index()?;
                let valid_length = bytes.read_code_index()?;
                let name = bytes.read_constant_index()?;
                let descriptor = bytes.read_constant_index()?;
                let variable_index = bytes.read_var_index()?;

                local_vars.push(LocalVariableType {
                    valid_start, valid_length, name,
                    descriptor, variable_index,
                });
            }
            AttributeInfo::LocalVariableTypeTable(local_vars)
        },
        attr::ATTR_DEPRECATED => AttributeInfo::Deprecated,
        /*
        "RuntimeVisibleAnnotations" => unimplemented!(),
        "RuntimeInvisibleAnnotations" => unimplemented!(),
        "RuntimeVisibleParameterAnnotations" => unimplemented!(),
        "RuntimeInvisibleParameterAnnotations" => unimplemented!(),
        "AnnotationDefault" => unimplemented!(),
        "BootstrapMethods" => unimplemented!(),
        */
        _ => {
            let mut buf = vec![0; len as usize];
            bytes.read_exact(&mut buf[..])?;
            AttributeInfo::Other(buf)
        }
    };

    let attr = Ok(Attribute { name: name_index, info });
    attr
}

fn parse_code<R: Read>(bytes: &mut R, len: u32) -> Result<Vec<Instruction>> {
    let len = len as usize;
    let mut instructions = Vec::with_capacity(len);

    struct CountingReader<'a, R: Read + 'a> {
        reader: &'a mut R,
        count: usize,
    }
    impl<'a, R> Read for CountingReader<'a, R> where R: Read {

        fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
            let read_bytes = self.reader.read(buf)?;
            self.count += read_bytes;
            Ok(read_bytes)
        }
    }

    let mut bytes = CountingReader { reader: bytes, count: 0, };
    let mut i = 0;

    while bytes.count < len {
        use super::code::Instruction::*;
        use consts::code;
        let instruction = match bytes.read_u8()? {
            code::OPCODE_NOP => Nop,

            code::OPCODE_CONST_REF_NULL => ConstRefNull,
            code::OPCODE_CONST_INT_M1 => ConstIntM1,
            code::OPCODE_CONST_INT_0 => ConstInt0,
            code::OPCODE_CONST_INT_1 => ConstInt1,
            code::OPCODE_CONST_INT_2 => ConstInt2,
            code::OPCODE_CONST_INT_3 => ConstInt3,
            code::OPCODE_CONST_INT_4 => ConstInt4,
            code::OPCODE_CONST_INT_5 => ConstInt5,
            code::OPCODE_CONST_LONG_0 => ConstLong0,
            code::OPCODE_CONST_LONG_1 => ConstLong1,
            code::OPCODE_CONST_FLOAT_0 => ConstFloat0,
            code::OPCODE_CONST_FLOAT_1 => ConstFloat1,
            code::OPCODE_CONST_FLOAT_2 => ConstFloat2,
            code::OPCODE_CONST_DOUBLE_0 => ConstDouble0,
            code::OPCODE_CONST_DOUBLE_1 => ConstDouble1,

            code::OPCODE_PUSH_BYTE => PushByte(bytes.read_i8()?),
            code::OPCODE_PUSH_SHORT => PushShort(bytes.read_i16::<BigEndian>()?),

            code::OPCODE_LOAD_CONSTANT => LoadConstant(bytes.read_half_constant_index()?),
            code::OPCODE_WIDE_LOAD_CONSTANT => WideLoadConstant(bytes.read_constant_index()?),
            code::OPCODE_WIDE_LOAD_WIDE_CONSTANT => WideLoadWideConstant(bytes.read_constant_index()?),


            code::OPCODE_LOAD_INT => LoadInt(bytes.read_var_index()?),
            code::OPCODE_LOAD_LONG => LoadLong(bytes.read_var_index()?),
            code::OPCODE_LOAD_FLOAT => LoadFloat(bytes.read_var_index()?),
            code::OPCODE_LOAD_DOUBLE => LoadDouble(bytes.read_var_index()?),
            code::OPCODE_LOAD_REF => LoadRef(bytes.read_var_index()?),

            code::OPCODE_LOAD_INT_0 => LoadInt0,
            code::OPCODE_LOAD_INT_1 => LoadInt1,
            code::OPCODE_LOAD_INT_2 => LoadInt2,
            code::OPCODE_LOAD_INT_3 => LoadInt3,

            code::OPCODE_LOAD_LONG_0 => LoadLong0,
            code::OPCODE_LOAD_LONG_1 => LoadLong1,
            code::OPCODE_LOAD_LONG_2 => LoadLong2,
            code::OPCODE_LOAD_LONG_3 => LoadLong3,

            code::OPCODE_LOAD_FLOAT_0 => LoadFloat0,
            code::OPCODE_LOAD_FLOAT_1 => LoadFloat1,
            code::OPCODE_LOAD_FLOAT_2 => LoadFloat2,
            code::OPCODE_LOAD_FLOAT_3 => LoadFloat3,

            code::OPCODE_LOAD_DOUBLE_0 => LoadDouble0,
            code::OPCODE_LOAD_DOUBLE_1 => LoadDouble1,
            code::OPCODE_LOAD_DOUBLE_2 => LoadDouble2,
            code::OPCODE_LOAD_DOUBLE_3 => LoadDouble3,

            code::OPCODE_LOAD_REF_0 => LoadRef0,
            code::OPCODE_LOAD_REF_1 => LoadRef1,
            code::OPCODE_LOAD_REF_2 => LoadRef2,
            code::OPCODE_LOAD_REF_3 => LoadRef3,

            code::OPCODE_ARRAY_LOAD_INT => ArrayLoadInt,
            code::OPCODE_ARRAY_LOAD_LONG => ArrayLoadLong,
            code::OPCODE_ARRAY_LOAD_FLOAT => ArrayLoadFloat,
            code::OPCODE_ARRAY_LOAD_DOUBLE => ArrayLoadDouble,
            code::OPCODE_ARRAY_LOAD_REF => ArrayLoadRef,
            code::OPCODE_ARRAY_LOAD_BYTE => ArrayLoadByte,
            code::OPCODE_ARRAY_LOAD_CHAR => ArrayLoadChar,
            code::OPCODE_ARRAY_LOAD_SHORT => ArrayLoadShort,


            code::OPCODE_STORE_INT => StoreInt(bytes.read_var_index()?),
            code::OPCODE_STORE_LONG => StoreLong(bytes.read_var_index()?),
            code::OPCODE_STORE_FLOAT => StoreFloat(bytes.read_var_index()?),
            code::OPCODE_STORE_DOUBLE => StoreDouble(bytes.read_var_index()?),
            code::OPCODE_STORE_REF => StoreRef(bytes.read_var_index()?),

            code::OPCODE_STORE_INT_0 => StoreInt0,
            code::OPCODE_STORE_INT_1 => StoreInt1,
            code::OPCODE_STORE_INT_2 => StoreInt2,
            code::OPCODE_STORE_INT_3 => StoreInt3,

            code::OPCODE_STORE_LONG_0 => StoreLong0,
            code::OPCODE_STORE_LONG_1 => StoreLong1,
            code::OPCODE_STORE_LONG_2 => StoreLong2,
            code::OPCODE_STORE_LONG_3 => StoreLong3,

            code::OPCODE_STORE_FLOAT_0 => StoreFloat0,
            code::OPCODE_STORE_FLOAT_1 => StoreFloat1,
            code::OPCODE_STORE_FLOAT_2 => StoreFloat2,
            code::OPCODE_STORE_FLOAT_3 => StoreFloat3,

            code::OPCODE_STORE_DOUBLE_0 => StoreDouble0,
            code::OPCODE_STORE_DOUBLE_1 => StoreDouble1,
            code::OPCODE_STORE_DOUBLE_2 => StoreDouble2,
            code::OPCODE_STORE_DOUBLE_3 => StoreDouble3,

            code::OPCODE_STORE_REF_0 => StoreRef0,
            code::OPCODE_STORE_REF_1 => StoreRef1,
            code::OPCODE_STORE_REF_2 => StoreRef2,
            code::OPCODE_STORE_REF_3 => StoreRef3,

            code::OPCODE_ARRAY_STORE_INT => ArrayStoreInt,
            code::OPCODE_ARRAY_STORE_LONG => ArrayStoreLong,
            code::OPCODE_ARRAY_STORE_FLOAT => ArrayStoreFloat,
            code::OPCODE_ARRAY_STORE_DOUBLE => ArrayStoreDouble,
            code::OPCODE_ARRAY_STORE_REF => ArrayStoreRef,
            code::OPCODE_ARRAY_STORE_BYTE => ArrayStoreByte,
            code::OPCODE_ARRAY_STORE_CHAR => ArrayStoreChar,
            code::OPCODE_ARRAY_STORE_SHORT => ArrayStoreShort,


            code::OPCODE_POP => Pop,
            code::OPCODE_DOUBLE_POP => DoublePop,

            code::OPCODE_DUP => Dup,
            code::OPCODE_DUP_DOWN => DupDown,
            code::OPCODE_DUP_DOUBLE_DOWN => DupDoubleDown,
            code::OPCODE_DOUBLE_DUP => DoubleDup,
            code::OPCODE_DOUBLE_DUP_DOWN => DoubleDupDown,
            code::OPCODE_DOUBLE_DUP_DOUBLE_DOWN => DoubleDupDoubleDown,

            code::OPCODE_SWAP => Swap,


            code::OPCODE_ADD_INT => AddInt,
            code::OPCODE_ADD_LONG => AddLong,
            code::OPCODE_ADD_FLOAT => AddFloat,
            code::OPCODE_ADD_DOUBLE => AddDouble,

            code::OPCODE_SUB_INT => SubInt,
            code::OPCODE_SUB_LONG => SubLong,
            code::OPCODE_SUB_FLOAT => SubFloat,
            code::OPCODE_SUB_DOUBLE => SubDouble,

            code::OPCODE_MUL_INT => MulInt,
            code::OPCODE_MUL_LONG => MulLong,
            code::OPCODE_MUL_FLOAT => MulFloat,
            code::OPCODE_MUL_DOUBLE => MulDouble,

            code::OPCODE_DIV_INT => DivInt,
            code::OPCODE_DIV_LONG => DivLong,
            code::OPCODE_DIV_FLOAT => DivFloat,
            code::OPCODE_DIV_DOUBLE => DivDouble,

            code::OPCODE_REM_INT => RemInt,
            code::OPCODE_REM_LONG => RemLong,
            code::OPCODE_REM_FLOAT => RemFloat,
            code::OPCODE_REM_DOUBLE => RemDouble,

            code::OPCODE_NEG_INT => NegInt,
            code::OPCODE_NEG_LONG => NegLong,
            code::OPCODE_NEG_FLOAT => NegFloat,
            code::OPCODE_NEG_DOUBLE => NegDouble,

            code::OPCODE_LEFT_SHIFT_INT => LeftShiftInt,
            code::OPCODE_LEFT_SHIFT_LONG => LeftShiftLong,
            code::OPCODE_RIGHT_SHIFT_INT => RightShiftInt,
            code::OPCODE_RIGHT_SHIFT_LONG => RightShiftLong,
            code::OPCODE_URIGHT_SHIFT_INT => URightShiftInt,
            code::OPCODE_URIGHT_SHIFT_LONG => URightShiftLong,

            code::OPCODE_AND_INT => AndInt,
            code::OPCODE_AND_LONG => AndLong,

            code::OPCODE_OR_INT => OrInt,
            code::OPCODE_OR_LONG => OrLong,

            code::OPCODE_XOR_INT => XorInt,
            code::OPCODE_XOR_LONG => XorLong,

            code::OPCODE_INC_INT => IncInt(bytes.read_var_index()?, bytes.read_i8()?),


            code::OPCODE_INT_TO_LONG => IntToLong,
            code::OPCODE_INT_TO_FLOAT => IntToFloat,
            code::OPCODE_INT_TO_DOUBLE => IntToDouble,
            code::OPCODE_LONG_TO_INT => LongToInt,
            code::OPCODE_LONG_TO_FLOAT => LongToFloat,
            code::OPCODE_LONG_TO_DOUBLE => LongToDouble,
            code::OPCODE_FLOAT_TO_INT => FloatToInt,
            code::OPCODE_FLOAT_TO_LONG => FloatToLong,
            code::OPCODE_FLOAT_TO_DOUBLE => FloatToDouble,
            code::OPCODE_DOUBLE_TO_INT => DoubleToInt,
            code::OPCODE_DOUBLE_TO_LONG => DoubleToLong,
            code::OPCODE_DOUBLE_TO_FLOAT => DoubleToFloat,

            code::OPCODE_INT_TO_BYTE => IntToByte,
            code::OPCODE_INT_TO_CHAR => IntToChar,
            code::OPCODE_INT_TO_SHORT => IntToShort,


            code::OPCODE_COMPARE_LONG => CompareLong,
            code::OPCODE_COMPARE_FLOAT_L => CompareFloatL,
            code::OPCODE_COMPARE_FLOAT_G => CompareFloatG,
            code::OPCODE_COMPARE_DOUBLE_L => CompareDoubleL,
            code::OPCODE_COMPARE_DOUBLE_G => CompareDoubleG,

            code::OPCODE_IF_INT_EQ_0 => IfIntEq0(bytes.read_code_offset()?),
            code::OPCODE_IF_INT_NE_0 => IfIntNe0(bytes.read_code_offset()?),
            code::OPCODE_IF_INT_LT_0 => IfIntLt0(bytes.read_code_offset()?),
            code::OPCODE_IF_INT_GE_0 => IfIntGe0(bytes.read_code_offset()?),
            code::OPCODE_IF_INT_GT_0 => IfIntGt0(bytes.read_code_offset()?),
            code::OPCODE_IF_INT_LE_0 => IfIntLe0(bytes.read_code_offset()?),

            code::OPCODE_IF_INT_EQ => IfIntEq(bytes.read_code_offset()?),
            code::OPCODE_IF_INT_NE => IfIntNe(bytes.read_code_offset()?),
            code::OPCODE_IF_INT_LT => IfIntLt(bytes.read_code_offset()?),
            code::OPCODE_IF_INT_GE => IfIntGe(bytes.read_code_offset()?),
            code::OPCODE_IF_INT_GT => IfIntGt(bytes.read_code_offset()?),
            code::OPCODE_IF_INT_LE => IfIntLe(bytes.read_code_offset()?),

            code::OPCODE_IF_REF_EQ => IfRefEq(bytes.read_code_offset()?),
            code::OPCODE_IF_REF_NE => IfRefNe(bytes.read_code_offset()?),

            code::OPCODE_GOTO => Goto(bytes.read_code_offset()?),
            code::OPCODE_JUMP_SUB => JumpSub(bytes.read_code_offset()?),
            code::OPCODE_RET_SUB => RetSub(bytes.read_var_index()?),

            code::OPCODE_TABLE_SWITCH => {
                while bytes.count % 4 != 0 {
                    let _ = bytes.read_u8()?;
                }
                let default_offset = bytes.read_wide_code_offset()?;
                let match_range = (bytes.read_i32::<BigEndian>()?, bytes.read_i32::<BigEndian>()?);
                let offset_count = (match_range.1 - match_range.0 + 1) as usize;
                let mut offset_table = Vec::with_capacity(offset_count);
                for _ in 0..offset_count {
                    offset_table.push(bytes.read_wide_code_offset()?);
                }
                TableSwitch {
                    default_offset, match_range, offset_table,
                }
            },
            code::OPCODE_LOOKUP_SWITCH => {
                while bytes.count % 4 != 0 {
                    let _ = bytes.read_u8()?;
                }
                let default_offset = bytes.read_wide_code_offset()?;
                let match_count = bytes.read_u32::<BigEndian>()? as usize;
                let mut match_table = Vec::with_capacity(match_count);
                for _ in 0..match_count {
                    match_table.push((bytes.read_i32::<BigEndian>()?, bytes.read_wide_code_offset()?));
                }
                LookupSwitch {
                    default_offset, match_table,
                }
            },

            code::OPCODE_RETURN_INT => ReturnInt,
            code::OPCODE_RETURN_LONG => ReturnLong,
            code::OPCODE_RETURN_FLOAT => ReturnFloat,
            code::OPCODE_RETURN_DOUBLE => ReturnDouble,
            code::OPCODE_RETURN_REF => ReturnRef,
            code::OPCODE_RETURN_VOID => ReturnVoid,


            code::OPCODE_GET_STATIC_FIELD => GetStaticField(bytes.read_constant_index()?),
            code::OPCODE_PUT_STATIC_FIELD => PutStaticField(bytes.read_constant_index()?),
            code::OPCODE_GET_FIELD => GetField(bytes.read_constant_index()?),
            code::OPCODE_PUT_FIELD => PutField(bytes.read_constant_index()?),

            code::OPCODE_INVOKE_VIRTUAL => InvokeVirtual(bytes.read_constant_index()?),
            code::OPCODE_INVOKE_SPECIAL => InvokeSpecial(bytes.read_constant_index()?),
            code::OPCODE_INVOKE_STATIC=> InvokeStatic(bytes.read_constant_index()?),
            code::OPCODE_INVOKE_INTERFACE => {
                let index = bytes.read_constant_index()?;
                let count = bytes.read_u8()?;
                let _ = bytes.read_u8()?;

                InvokeInterface(bytes.read_constant_index()?, count)
            },
            code::OPCODE_INVOKE_DYNAMIC => {
                let index = bytes.read_constant_index()?;
                let _ = bytes.read_u8()?;
                let _ = bytes.read_u8()?;

                InvokeDynamic(index)
            },

            code::OPCODE_NEW => New(bytes.read_constant_index()?),
            code::OPCODE_NEW_PRIMITIVE_ARRAY => {
                let arr_type = bytes.read_u8()?;
                NewPrimitiveArray(ArrayPrimitive::from_byte(arr_type)
                    .ok_or(invalid_data!("invalid primitive array type {} at instruction {}", arr_type, i))?)
            },
            code::OPCODE_NEW_REF_ARRAY => NewRefArray(bytes.read_constant_index()?),

            code::OPCODE_ARRAY_LEN => ArrayLen,

            code::OPCODE_THROW => Throw,

            code::OPCODE_CHECK_CAST => CheckCast(bytes.read_constant_index()?),
            code::OPCODE_INSTANCE_OF => InstanceOf(bytes.read_constant_index()?),

            code::OPCODE_ENTER_MONITOR => EnterMonitor,
            code::OPCODE_EXIT_MONITOR => ExitMonitor,


            code::OPCODE_WIDE => {
                use super::WideInstruction as W;
                let wide_instruction = match bytes.read_u8()? {
                    code::OPCODE_LOAD_INT => W::LoadInt(bytes.read_wide_var_index()?),
                    code::OPCODE_LOAD_LONG => W::LoadLong(bytes.read_wide_var_index()?),
                    code::OPCODE_LOAD_FLOAT => W::LoadFloat(bytes.read_wide_var_index()?),
                    code::OPCODE_LOAD_DOUBLE => W::LoadDouble(bytes.read_wide_var_index()?),
                    code::OPCODE_LOAD_REF => W::LoadRef(bytes.read_wide_var_index()?),

                    code::OPCODE_STORE_INT => W::StoreInt(bytes.read_wide_var_index()?),
                    code::OPCODE_STORE_LONG => W::StoreLong(bytes.read_wide_var_index()?),
                    code::OPCODE_STORE_FLOAT => W::StoreFloat(bytes.read_wide_var_index()?),
                    code::OPCODE_STORE_DOUBLE => W::StoreDouble(bytes.read_wide_var_index()?),
                    code::OPCODE_STORE_REF => W::StoreRef(bytes.read_wide_var_index()?),

                    code::OPCODE_INC_INT => W::IncInt(bytes.read_wide_var_index()?, bytes.read_i16::<BigEndian>()?),

                    code::OPCODE_RET_SUB => W::RetSub(bytes.read_wide_var_index()?),

                    invalid => Err(invalid_data!("invalid wide opcode {} at instruction {}", invalid, i))?,
                };
                Wide(wide_instruction)
            },

            code::OPCODE_NEW_REF_MULTI_ARRAY => NewRefMultiArray(bytes.read_constant_index()?, bytes.read_u8()?),
            code::OPCODE_IF_REF_NULL => IfRefNull(bytes.read_code_offset()?),
            code::OPCODE_IF_REF_NON_NULL => IfRefNonNull(bytes.read_code_offset()?),
            code::OPCODE_WIDE_GOTO => WideGoto(bytes.read_wide_code_offset()?),
            code::OPCODE_WIDE_JUMP_SUB => WideJumpSub(bytes.read_wide_code_offset()?),


            code::OPCODE_BREAKPOINT => Breakpoint,
            code::OPCODE_IMPLEMENTATION_DEFINED_1 => ImplementationDefined1,
            code::OPCODE_IMPLEMENTATION_DEFINED_2 => ImplementationDefined2,

            invalid => Err(invalid_data!("invalid opcode {} at instruction {}", invalid, i))?,
        };
        instructions.push(instruction);
        i += 1;
    }

    Ok(instructions)
}

pub fn emit_class<W: Write>(class: &Class, out: &mut W) -> Result<()> {
    out.write_u32::<BigEndian>(consts::class::MAGIC)?;

    out.write_u16::<BigEndian>(class.minor_version)?;
    out.write_u16::<BigEndian>(class.major_version)?;

    out.write_u16::<BigEndian>((class.constant_pool.len() + 1) as u16)?;
    for constant in class.constant_pool.iter() {
        emit_constant(constant, out)?;
    }

    out.write_u16::<BigEndian>(class.flags.bits())?;

    out.write_constant_index(&class.this_class)?;
    out.write_constant_index(&class.super_class)?;

    out.write_u16::<BigEndian>(class.interfaces.len() as u16)?;
    for interface in class.interfaces.iter() {
        out.write_constant_index(interface)?;
    }

    out.write_u16::<BigEndian>(class.fields.len() as u16)?;
    for field in class.fields.iter() {
        emit_field(field, out)?;
    }

    out.write_u16::<BigEndian>(class.methods.len() as u16)?;
    for method in class.methods.iter() {
        emit_method(method, out)?;
    }

    out.write_u16::<BigEndian>(class.attributes.len() as u16)?;
    for attr in class.attributes.iter() {
        emit_attribute(attr, out)?;
    }

    Ok(())
}

fn emit_constant<W: Write>(constant: &Constant, out: &mut W) -> Result<()> {
    out.write_u8(constant.tag())?;
    match *constant {
        Constant::Class(ref index) => out.write_constant_index(index)?,
        Constant::FieldRef { ref class, ref signature, } => {
            out.write_constant_index(class)?;
            out.write_constant_index(signature)?;
        },
        Constant::MethodRef { ref class, ref signature, } => {
            out.write_constant_index(class)?;
            out.write_constant_index(signature)?;
        },
        Constant::InterfaceMethodRef { ref class, ref signature, } => {
            out.write_constant_index(class)?;
            out.write_constant_index(signature)?;
        },
        Constant::String(ref index) => out.write_constant_index(index)?,
        Constant::Integer(val) => out.write_i32::<BigEndian>(val)?,
        Constant::Float(val) => out.write_f32::<BigEndian>(val)?,
        Constant::Long(val) => out.write_i64::<BigEndian>(val)?,
        Constant::Double(val) => out.write_f64::<BigEndian>(val)?,
        Constant::Signature { ref name, ref descriptor, } => {
            out.write_constant_index(name)?;
            out.write_constant_index(descriptor)?;
        },
        Constant::Utf8(ref string) => {
            // FIXME: make sure string can't be too long
            let bytes = string.as_bytes();
            out.write_u16::<BigEndian>(bytes.len() as u16)?;
            out.write_all(bytes)?;
        },
        Constant::MethodHandle { ref kind, ref referent, } => {
            out.write_u8(kind.as_byte())?;
            out.write_constant_index(referent)?;
        }
        Constant::MethodType(ref index) => out.write_constant_index(index)?,
        Constant::InvokeDynamic { ref bootstrap_method, ref signature, } => {
            out.write_constant_index(bootstrap_method)?;
            out.write_constant_index(signature)?;
        },
        Constant::WidePlaceholder => {},
    };

    Ok(())
}

fn emit_field<W: Write>(field: &Field, out: &mut W) -> Result<()> {
    out.write_u16::<BigEndian>(field.flags.bits())?;
    out.write_constant_index(&field.name)?;
    out.write_constant_index(&field.descriptor)?;
    out.write_u16::<BigEndian>(field.attributes.len() as u16)?;
    for attr in field.attributes.iter() {
        emit_attribute(attr, out);
    }
    Ok(())
}

fn emit_method<W: Write>(method: &Method, out: &mut W) -> Result<()> {
    out.write_u16::<BigEndian>(method.flags.bits())?;
    out.write_constant_index(&method.name)?;
    out.write_constant_index(&method.descriptor)?;
    out.write_u16::<BigEndian>(method.attributes.len() as u16)?;
    for attr in method.attributes.iter() {
        emit_attribute(attr, out)?;
    }
    Ok(())
}

fn emit_attribute<W: Write>(attribute: &Attribute, out: &mut W) -> Result<()> {

    out.write_constant_index(&attribute.name)?;
    out.write_u32::<BigEndian>(attribute.info.len() as u32)?;

    match attribute.info {
        AttributeInfo::ConstantValue(ref index) => out.write_constant_index(index)?,
        AttributeInfo::Code {
            ref max_stack, ref max_locals, code_len,
            ref code, ref exception_table, ref attributes,
        } => {
            out.write_code_index(max_stack)?;
            out.write_code_index(max_locals)?;
            out.write_u32::<BigEndian>(code_len)?;
            emit_code(code, out)?;
            out.write_u16::<BigEndian>(exception_table.len() as u16)?;
            for ex in exception_table.iter() {
                out.write_code_index(&ex.try_range.0)?;
                out.write_code_index(&ex.try_range.1)?;
                out.write_code_index(&ex.handler_goto)?;
                out.write_constant_index(&ex.exception_type)?;
            }
            out.write_u16::<BigEndian>(attributes.len() as u16)?;
            for attr in attributes.iter() {
                emit_attribute(&attr, out)?;
            }
        },
        AttributeInfo::StackMapTable(ref frames) => {
            out.write_u16::<BigEndian>(frames.len() as u16)?;
            #[inline]
            fn emit_ty<W: Write>(ty: &VerificationType, out: &mut W) -> Result<()> {
                out.write_u8(ty.tag());
                match *ty {
                    VerificationType::Object(ref index) => out.write_constant_index(index)?,
                    VerificationType::Uninitialized(ref index) => out.write_code_index(index)?,
                    _ => {}
                }
                Ok(())
            }
            for frame in frames.iter() {
                match *frame {
                    StackMapFrame::Same(ref offset) =>
                        out.write_u8(offset.0 as u8 + consts::attribute::STACK_MAP_SAME_OFFSET)?,
                    StackMapFrame::SameExt(ref offset) => {
                        out.write_u8(consts::attribute::STACK_MAP_SAME_EXT)?;
                        out.write_code_index(offset)?;
                    },
                    StackMapFrame::SingleStack(ref offset, ref ty) => {
                        out.write_u8(offset.0 as u8 + consts::attribute::STACK_MAP_SINGLE_STACK_OFFSET)?;
                        emit_ty(ty, out)?;
                    },
                    StackMapFrame::SingleStackExt(ref offset, ref ty) => {
                        out.write_u8(consts::attribute::STACK_MAP_SINGLE_STACK_EXT)?;
                        out.write_code_index(offset)?;
                        emit_ty(ty, out)?;
                    },
                    StackMapFrame::Chop1(ref offset) => {
                        out.write_u8(consts::attribute::STACK_MAP_CHOP_1)?;
                        out.write_code_index(offset)?;
                    },
                    StackMapFrame::Chop2(ref offset) => {
                        out.write_u8(consts::attribute::STACK_MAP_CHOP_2)?;
                        out.write_code_index(offset)?;
                    },
                    StackMapFrame::Chop3(ref offset) => {
                        out.write_u8(consts::attribute::STACK_MAP_CHOP_3)?;
                        out.write_code_index(offset)?;
                    },
                    StackMapFrame::Append1(ref offset, ref ty1) => {
                        out.write_u8(consts::attribute::STACK_MAP_APPEND_1)?;
                        out.write_code_index(offset)?;
                        emit_ty(ty1, out)?;
                    },
                    StackMapFrame::Append2(ref offset, ref ty1, ref ty2) => {
                        out.write_u8(consts::attribute::STACK_MAP_APPEND_2)?;
                        out.write_code_index(offset)?;
                        emit_ty(ty1, out)?;
                        emit_ty(ty2, out)?;
                    },
                    StackMapFrame::Append3(ref offset, ref ty1, ref ty2, ref ty3) => {
                        out.write_u8(consts::attribute::STACK_MAP_APPEND_3)?;
                        out.write_code_index(offset)?;
                        emit_ty(ty1, out)?;
                        emit_ty(ty2, out)?;
                        emit_ty(ty3, out)?;
                    },
                    StackMapFrame::Full {
                        ref offset, ref locals, ref stack,
                    } => {
                        out.write_u8(consts::attribute::STACK_MAP_FULL)?;
                        out.write_code_index(offset)?;
                        out.write_u16::<BigEndian>(locals.len() as u16)?;
                        for ty in locals.iter() {
                            emit_ty(ty, out)?;
                        }
                        out.write_u16::<BigEndian>(stack.len() as u16)?;
                        for ty in stack.iter() {
                            emit_ty(ty, out)?;
                        }
                    }
                }
            }
        }
        AttributeInfo::Exceptions(ref exceptions) => {
            out.write_u16::<BigEndian>(exceptions.len() as u16)?;
            for ex in exceptions.iter() {
                out.write_constant_index(&ex)?;
            }
        },
        AttributeInfo::InnerClasses(ref classes) => {
            out.write_u16::<BigEndian>(classes.len() as u16)?;
            for class in classes.iter() {
                out.write_constant_index(&class.inner_class)?;
                out.write_constant_index(&class.outer_class)?;
                out.write_constant_index(&class.inner_name)?;
                out.write_u16::<BigEndian>(class.inner_flags.bits())?;
            }
        },
        AttributeInfo::EnclosingMethod { ref class, ref method, } => {
            out.write_constant_index(class)?;
            out.write_constant_index(method)?;
        },
        AttributeInfo::Synthetic => {},
        AttributeInfo::Signature(ref index) => out.write_constant_index(index)?,
        AttributeInfo::SourceFile(ref index) => out.write_constant_index(index)?,
        AttributeInfo::SourceDebugExtension(ref buf) => out.write_all(&buf[..])?,
        AttributeInfo::LineNumberTable(ref lines) => {
            out.write_u16::<BigEndian>(lines.len() as u16)?;
            for line in lines.iter() {
                out.write_code_index(&line.instruction)?;
                out.write_u16::<BigEndian>(line.line_number)?;
            }
        }
        AttributeInfo::LocalVariableTable(ref vars) => {
            out.write_u16::<BigEndian>(vars.len() as u16)?;
            for var in vars.iter() {
                out.write_code_index(&var.valid_start)?;
                out.write_code_index(&var.valid_length)?;
                out.write_constant_index(&var.name)?;
                out.write_constant_index(&var.descriptor)?;
                out.write_var_index(&var.variable_index)?;
            }
        }

        AttributeInfo::LocalVariableTypeTable(ref vars) => {
            out.write_u16::<BigEndian>(vars.len() as u16)?;
            for var in vars.iter() {
                out.write_code_index(&var.valid_start)?;
                out.write_code_index(&var.valid_length)?;
                out.write_constant_index(&var.name)?;
                out.write_constant_index(&var.descriptor)?;
                out.write_var_index(&var.variable_index)?;
            }
        },

        AttributeInfo::Deprecated => {}

        AttributeInfo::Other(ref buf) => out.write_all(&buf[..])?,

        AttributeInfo::BootstrapMethods(ref methods) => {
            out.write_u16::<BigEndian>(methods.len() as u16)?;
            for method in methods.iter() {
                out.write_constant_index(&method.method)?;
                out.write_u16::<BigEndian>(method.arguments.len() as u16)?;
                for arg in method.arguments.iter() {
                    out.write_constant_index(arg)?;
                }
            }
        }

        // FIXME: emit the rest of the attributes
        _ => unimplemented!(),
    }

    Ok(())
}

fn emit_code<W: Write>(instructions: &Vec<Instruction>, out: &mut W) -> Result<()> {

    struct CountingWriter<'a, W: Write + 'a> {
        writer: &'a mut W,
        count: usize,
    }
    impl<'a, W> Write for CountingWriter<'a, W> where W: Write {

        fn write(&mut self, buf: &[u8]) -> Result<usize> {
            let written_bytes = self.writer.write(buf)?;
            self.count += written_bytes;
            Ok(written_bytes)
        }

        fn flush(&mut self) -> Result<()> {
            self.writer.flush()
        }
    }

    let mut out = CountingWriter { writer: out, count: 0, };

    for op in instructions.iter() {
        out.write_u8(op.opcode())?;
        use super::code::Instruction::*;
        match *op {
            
            PushByte(byte) => out.write_i8(byte)?,
            PushShort(short) => out.write_i16::<BigEndian>(short)?,
            
            LoadConstant(ref index) => out.write_half_constant_index(index)?,
            WideLoadConstant(ref index) => out.write_constant_index(index)?,
            WideLoadWideConstant(ref index) => out.write_constant_index(index)?,
            
            LoadInt(ref index) => out.write_var_index(index)?,
            LoadLong(ref index) => out.write_var_index(index)?,
            LoadFloat(ref index) => out.write_var_index(index)?,
            LoadDouble(ref index) => out.write_var_index(index)?,
            LoadRef(ref index) => out.write_var_index(index)?,

            StoreInt(ref index) => out.write_var_index(index)?,
            StoreLong(ref index) => out.write_var_index(index)?,
            StoreFloat(ref index) => out.write_var_index(index)?,
            StoreDouble(ref index) => out.write_var_index(index)?,
            StoreRef(ref index) => out.write_var_index(index)?,

            IncInt(ref index, byte) => {
                out.write_var_index(index)?;
                out.write_i8(byte)?;  
            },

            IfIntEq0(ref index) => out.write_code_offset(index)?,
            IfIntNe0(ref index) => out.write_code_offset(index)?,
            IfIntLt0(ref index) => out.write_code_offset(index)?,
            IfIntGe0(ref index) => out.write_code_offset(index)?,
            IfIntGt0(ref index) => out.write_code_offset(index)?,
            IfIntLe0(ref index) => out.write_code_offset(index)?,

            IfIntEq(ref index) => out.write_code_offset(index)?,
            IfIntNe(ref index) => out.write_code_offset(index)?,
            IfIntLt(ref index) => out.write_code_offset(index)?,
            IfIntGe(ref index) => out.write_code_offset(index)?,
            IfIntGt(ref index) => out.write_code_offset(index)?,
            IfIntLe(ref index) => out.write_code_offset(index)?,

            IfRefEq(ref index) => out.write_code_offset(index)?,
            IfRefNe(ref index) => out.write_code_offset(index)?,
            
            Goto(ref index) => out.write_code_offset(&index)?,
            JumpSub(ref index) => out.write_code_offset(index)?,
            RetSub(ref index) => out.write_var_index(index)?,
            
            TableSwitch { ref default_offset, ref match_range, ref offset_table, } => {
                while out.count % 4 != 0 {
                    out.write_u8(0)?;
                }
                out.write_wide_code_offset(default_offset)?;
                out.write_i32::<BigEndian>(match_range.0)?;
                out.write_i32::<BigEndian>(match_range.1)?;
                
                for ref offset in offset_table.iter() {
                    out.write_wide_code_offset(offset)?;
                }
            },

            LookupSwitch { ref default_offset, ref match_table, } => {
                while out.count % 4 != 0 {
                    out.write_u8(0)?;
                }
                out.write_wide_code_offset(default_offset)?;
                out.write_u32::<BigEndian>(match_table.len() as u32)?;
                for &(index, ref offset) in match_table.iter() {
                    out.write_i32::<BigEndian>(index)?;
                    out.write_wide_code_offset(offset)?;
                }
            }

            GetStaticField(ref index) => out.write_constant_index(index)?,
            PutStaticField(ref index) => out.write_constant_index(index)?,
            GetField(ref index) => out.write_constant_index(index)?,
            PutField(ref index) => out.write_constant_index(index)?,
            
            InvokeVirtual(ref index) => out.write_constant_index(index)?,
            InvokeSpecial(ref index) => out.write_constant_index(index)?,
            InvokeStatic(ref index) => out.write_constant_index(index)?,
            InvokeInterface(ref index, count) => {
                out.write_constant_index(index)?;
                out.write_u8(count)?;
                out.write_u8(0)?;
            }
            InvokeDynamic(ref index) => {
                out.write_constant_index(index)?;
                out.write_u8(0)?;
                out.write_u8(0)?;
            }

            New(ref index) => out.write_constant_index(index)?,
            NewPrimitiveArray(ref ty) => out.write_u8(ty.to_byte())?,
            NewRefArray(ref index) => out.write_constant_index(index)?,

            CheckCast(ref index) => out.write_constant_index(index)?,
            InstanceOf(ref index) => out.write_constant_index(index)?,

            Wide(ref wide_instruction) => {
                out.write_u8(wide_instruction.opcode())?;
                match *wide_instruction {
                    WideInstruction::LoadInt(ref index) => out.write_wide_var_index(index)?,
                    WideInstruction::LoadLong(ref index) => out.write_wide_var_index(index)?,
                    WideInstruction::LoadFloat(ref index) => out.write_wide_var_index(index)?,
                    WideInstruction::LoadDouble(ref index) => out.write_wide_var_index(index)?,
                    WideInstruction::LoadRef(ref index) => out.write_wide_var_index(index)?,

                    WideInstruction::StoreInt(ref index) => out.write_wide_var_index(index)?,
                    WideInstruction::StoreLong(ref index) => out.write_wide_var_index(index)?,
                    WideInstruction::StoreFloat(ref index) => out.write_wide_var_index(index)?,
                    WideInstruction::StoreDouble(ref index) => out.write_wide_var_index(index)?,
                    WideInstruction::StoreRef(ref index) => out.write_wide_var_index(index)?,

                    WideInstruction::IncInt(ref index, short) => {
                        out.write_wide_var_index(index)?;
                        out.write_i16::<BigEndian>(short)?;
                    }

                    WideInstruction::RetSub(ref index) => out.write_wide_var_index(index)?,

                };
            },

            NewRefMultiArray(ref index, dims) => {
                out.write_constant_index(index)?;
                out.write_u8(dims)?;
            }
            IfRefNull(ref index) => out.write_code_offset(index)?,
            IfRefNonNull(ref index) => out.write_code_offset(index)?,
            WideGoto(ref index) => out.write_wide_code_offset(index)?,
            WideJumpSub(ref index) => out.write_wide_code_offset(index)?,

            _ => {},
        };
    }
    Ok(())
}