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
    if magic != consts::MAGIC {
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

    let info = match name {
        "ConstantValue" => AttributeInfo::ConstantValue(bytes.read_constant_index()?),
        "Code" => {
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
        "Exceptions" => {
            let num_exceptions = bytes.read_u16::<BigEndian>()?;
            let mut exceptions = Vec::with_capacity(num_exceptions as usize);
            for _ in 0..num_exceptions {
                exceptions.push(bytes.read_constant_index()?);
            }
            AttributeInfo::Exceptions(exceptions)
        },
        "InnerClasses" => {
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
        "EnclosingMethod" => AttributeInfo::EnclosingMethod {
            class: bytes.read_constant_index()?,
            method: bytes.read_constant_index()?,
        },
        "Synthetic" => AttributeInfo::Synthetic,
        "Signature" => AttributeInfo::Signature(bytes.read_constant_index()?),
        "SourceFile" => AttributeInfo::SourceFile(bytes.read_constant_index()?),
        "SourceDebugExtension" => {
            let mut buf = vec![0; len as usize];
            bytes.read_exact(&mut buf[..])?;
            AttributeInfo::SourceDebugExtension(buf)
        },
        "LineNumberTable" => {
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
        "LocalVariableTable" => {
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
        "LocalVariableTypeTable" => {
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
        "Deprecated" => AttributeInfo::Deprecated,
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
        let instruction = match bytes.read_u8()? {
            0x00 => Nop,

            0x01 => ConstRefNull,
            0x02 => ConstIntM1,
            0x03 => ConstInt0,
            0x04 => ConstInt1,
            0x05 => ConstInt2,
            0x06 => ConstInt3,
            0x07 => ConstInt4,
            0x08 => ConstInt5,
            0x09 => ConstLong0,
            0x0a => ConstLong1,
            0x0b => ConstFloat0,
            0x0c => ConstFloat1,
            0x0d => ConstFloat2,
            0x0e => ConstDouble0,
            0x0f => ConstDouble1,

            0x10 => PushByte(bytes.read_i8()?),
            0x11 => PushShort(bytes.read_i16::<BigEndian>()?),

            0x12 => LoadConstant(bytes.read_constant_index()?),
            0x13 => WideLoadConstant(bytes.read_wide_constant_index()?),
            0x14 => WideLoadWideConstant(bytes.read_wide_constant_index()?),


            0x15 => LoadInt(bytes.read_var_index()?),
            0x16 => LoadLong(bytes.read_var_index()?),
            0x17 => LoadFloat(bytes.read_var_index()?),
            0x18 => LoadDouble(bytes.read_var_index()?),
            0x19 => LoadRef(bytes.read_var_index()?),

            0x1a => LoadInt0,
            0x1b => LoadInt1,
            0x1c => LoadInt2,
            0x1d => LoadInt3,

            0x1e => LoadLong0,
            0x1f => LoadLong1,
            0x20 => LoadLong2,
            0x21 => LoadLong3,

            0x22 => LoadFloat0,
            0x23 => LoadFloat1,
            0x24 => LoadFloat2,
            0x25 => LoadFloat3,

            0x26 => LoadDouble0,
            0x27 => LoadDouble1,
            0x28 => LoadDouble2,
            0x29 => LoadDouble3,

            0x2a => LoadRef0,
            0x2b => LoadRef1,
            0x2c => LoadRef2,
            0x2d => LoadRef3,

            0x2e => ArrayLoadInt,
            0x2f => ArrayLoadLong,
            0x30 => ArrayLoadFloat,
            0x31 => ArrayLoadDouble,
            0x32 => ArrayLoadRef,
            0x33 => ArrayLoadByte,
            0x34 => ArrayLoadChar,
            0x35 => ArrayLoadShort,


            0x36 => StoreInt(bytes.read_var_index()?),
            0x37 => StoreLong(bytes.read_var_index()?),
            0x38 => StoreFloat(bytes.read_var_index()?),
            0x39 => StoreDouble(bytes.read_var_index()?),
            0x3a => StoreRef(bytes.read_var_index()?),

            0x3b => StoreInt0,
            0x3c => StoreInt1,
            0x3d => StoreInt2,
            0x3e => StoreInt3,

            0x3f => StoreLong0,
            0x40 => StoreLong1,
            0x41 => StoreLong2,
            0x42 => StoreLong3,

            0x43 => StoreFloat0,
            0x44 => StoreFloat1,
            0x45 => StoreFloat2,
            0x46 => StoreFloat3,

            0x47 => StoreDouble0,
            0x48 => StoreDouble1,
            0x49 => StoreDouble2,
            0x4a => StoreDouble3,

            0x4b => StoreRef0,
            0x4c => StoreRef1,
            0x4d => StoreRef2,
            0x4e => StoreRef3,

            0x4f => ArrayStoreInt,
            0x50 => ArrayStoreLong,
            0x51 => ArrayStoreFloat,
            0x52 => ArrayStoreDouble,
            0x53 => ArrayStoreRef,
            0x54 => ArrayStoreByte,
            0x55 => ArrayStoreChar,
            0x56 => ArrayStoreShort,


            0x57 => Pop,
            0x58 => DoublePop,

            0x59 => Dup,
            0x5a => DupDown,
            0x5b => DupDoubleDown,
            0x5c => DoubleDup,
            0x5d => DoubleDupDown,
            0x5e => DoubleDupDoubleDown,

            0x5f => Swap,


            0x60 => AddInt,
            0x61 => AddLong,
            0x62 => AddFloat,
            0x63 => AddDouble,

            0x64 => SubInt,
            0x65 => SubLong,
            0x66 => SubFloat,
            0x67 => SubDouble,

            0x68 => MulInt,
            0x69 => MulLong,
            0x6a => MulFloat,
            0x6b => MulDouble,

            0x6c => DivInt,
            0x6d => DivLong,
            0x6e => DivFloat,
            0x6f => DivDouble,

            0x70 => RemInt,
            0x71 => RemLong,
            0x72 => RemFloat,
            0x73 => RemDouble,

            0x74 => NegInt,
            0x75 => NegLong,
            0x76 => NegFloat,
            0x77 => NegDouble,

            0x78 => LeftShiftInt,
            0x79 => LeftShiftLong,
            0x7a => RightShiftInt,
            0x7b => RightShiftLong,
            0x7c => URightShiftInt,
            0x7d => URightShiftLong,

            0x7e => AndInt,
            0x7f => AndLong,

            0x80 => OrInt,
            0x81 => OrLong,

            0x82 => XorInt,
            0x83 => XorLong,

            0x84 => IncInt(bytes.read_var_index()?, bytes.read_i8()?),


            0x85 => IntToLong,
            0x86 => IntToFloat,
            0x87 => IntToDouble,
            0x88 => LongToInt,
            0x89 => LongToFloat,
            0x8a => LongToDouble,
            0x8b => FloatToInt,
            0x8c => FloatToLong,
            0x8d => FloatToDouble,
            0x8e => DoubleToInt,
            0x8f => DoubleToLong,
            0x90 => DoubleToFloat,

            0x91 => IntToByte,
            0x92 => IntToChar,
            0x93 => IntToShort,


            0x94 => CompareLong,
            0x95 => CompareFloatL,
            0x96 => CompareFloatG,
            0x97 => CompareDoubleL,
            0x98 => CompareDoubleG,

            0x99 => IfIntEq0(bytes.read_code_index()?),
            0x9a => IfIntNe0(bytes.read_code_index()?),
            0x9b => IfIntLt0(bytes.read_code_index()?),
            0x9c => IfIntGe0(bytes.read_code_index()?),
            0x9d => IfIntGt0(bytes.read_code_index()?),
            0x9e => IfIntLe0(bytes.read_code_index()?),

            0x9f => IfIntEq(bytes.read_code_index()?),
            0xa0 => IfIntNe(bytes.read_code_index()?),
            0xa1 => IfIntLt(bytes.read_code_index()?),
            0xa2 => IfIntGe(bytes.read_code_index()?),
            0xa3 => IfIntGt(bytes.read_code_index()?),
            0xa4 => IfIntLe(bytes.read_code_index()?),

            0xa5 => IfRefEq(bytes.read_code_index()?),
            0xa6 => IfRefNe(bytes.read_code_index()?),

            0xa7 => Goto(bytes.read_code_index()?),
            0xa8 => JumpSub(bytes.read_code_index()?),
            0xa9 => RetSub(bytes.read_var_index()?),

            0xaa => {
                while bytes.count % 4 != 0 {
                    let _ = bytes.read_u8()?;
                }
                let default_offset = bytes.read_wide_code_index()?;
                let match_range = (bytes.read_i32::<BigEndian>()?, bytes.read_i32::<BigEndian>()?);
                let offset_count = (match_range.1 - match_range.0) as usize;
                let mut offset_table = Vec::with_capacity(offset_count);
                for _ in 0..offset_count {
                    offset_table.push(bytes.read_wide_code_index()?);
                }
                TableSwitch {
                    default_offset, match_range, offset_table,
                }
            },
            0xab => {
                while bytes.count % 4 != 0 {
                    let _ = bytes.read_u8()?;
                }
                let default_offset = bytes.read_wide_code_index()?;
                let match_count = bytes.read_u32::<BigEndian>()? as usize;
                let mut match_table = Vec::with_capacity(match_count);
                for _ in 0..match_count {
                    match_table.push((bytes.read_i32::<BigEndian>()?, bytes.read_wide_code_index()?));
                }
                LookupSwitch {
                    default_offset, match_table,
                }
            },

            0xac => ReturnInt,
            0xad => ReturnLong,
            0xae => ReturnFloat,
            0xaf => ReturnDouble,
            0xb0 => ReturnRef,
            0xb1 => ReturnVoid,


            0xb2 => GetStaticField(bytes.read_constant_index()?),
            0xb3 => PutStaticField(bytes.read_constant_index()?),
            0xb4 => GetField(bytes.read_constant_index()?),
            0xb5 => PutField(bytes.read_constant_index()?),

            0xb6 => InvokeVirtual(bytes.read_constant_index()?),
            0xb7 => InvokeSpecial(bytes.read_constant_index()?),
            0xb8 => InvokeStatic(bytes.read_constant_index()?),
            0xb9 => {
                let index = bytes.read_constant_index()?;
                let count = bytes.read_u8()?;
                let _ = bytes.read_u8()?;

                InvokeInterface(bytes.read_constant_index()?, count)
            },
            0xba => {
                let index = bytes.read_constant_index()?;
                let _ = bytes.read_u8()?;
                let _ = bytes.read_u8()?;

                InvokeDynamic(bytes.read_constant_index()?)
            },

            0xbb => New(bytes.read_constant_index()?),
            0xbc => {
                let arr_type = bytes.read_u8()?;
                NewPrimitiveArray(ArrayPrimitive::from_byte(arr_type)
                    .ok_or(invalid_data!("invalid primitive array type {} at instruction {}", arr_type, i))?)
            },
            0xbd => NewRefArray(bytes.read_constant_index()?),

            0xbe => ArrayLen,

            0xbf => Throw,

            0xc0 => CheckCast(bytes.read_constant_index()?),
            0xc1 => InstanceOf(bytes.read_constant_index()?),

            0xc2 => EnterMonitor,
            0xc3 => ExitMonitor,


            0xc4 => {
                use super::WideInstruction as W;
                let wide_instruction = match bytes.read_u8()? {
                    0x15 => W::LoadInt(bytes.read_wide_var_index()?),
                    0x16 => W::LoadLong(bytes.read_wide_var_index()?),
                    0x17 => W::LoadFloat(bytes.read_wide_var_index()?),
                    0x18 => W::LoadDouble(bytes.read_wide_var_index()?),
                    0x19 => W::LoadRef(bytes.read_wide_var_index()?),

                    0x36 => W::StoreInt(bytes.read_wide_var_index()?),
                    0x37 => W::StoreLong(bytes.read_wide_var_index()?),
                    0x38 => W::StoreFloat(bytes.read_wide_var_index()?),
                    0x39 => W::StoreDouble(bytes.read_wide_var_index()?),
                    0x3a => W::StoreRef(bytes.read_wide_var_index()?),

                    0x84 => W::IncInt(bytes.read_wide_var_index()?, bytes.read_i16::<BigEndian>()?),

                    0xa9 => W::RetSub(bytes.read_wide_var_index()?),

                    invalid => Err(invalid_data!("invalid wide opcode {} at instruction {}", invalid, i))?,
                };
                Wide(wide_instruction)
            },

            0xc5 => NewRefMultiArray(bytes.read_constant_index()?, bytes.read_u8()?),
            0xc6 => IfRefNull(bytes.read_code_index()?),
            0xc7 => IfRefNonNull(bytes.read_code_index()?),
            0xc8 => WideGoto(bytes.read_wide_code_index()?),
            0xc9 => WideJumpSub(bytes.read_wide_code_index()?),


            0xca => Breakpoint,
            0xfe => ImplementationDefined1,
            0xff => ImplementationDefined2,

            invalid => Err(invalid_data!("invalid opcode {} at instruction {}", invalid, i))?,
        };
        instructions.push(instruction);
        i += 1;
    }

    Ok(instructions)
}

pub fn emit_class<W: Write>(class: &Class, out: &mut W) -> Result<()> {
    out.write_u32::<BigEndian>(consts::MAGIC)?;

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
            
            LoadConstant(ref index) => out.write_constant_index(index)?,
            WideLoadConstant(ref index) => out.write_wide_constant_index(index)?,
            WideLoadWideConstant(ref index) => out.write_wide_constant_index(index)?,
            
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

            IfIntEq0(ref index) => out.write_code_index(index)?,
            IfIntNe0(ref index) => out.write_code_index(index)?,
            IfIntLt0(ref index) => out.write_code_index(index)?,
            IfIntGe0(ref index) => out.write_code_index(index)?,
            IfIntGt0(ref index) => out.write_code_index(index)?,
            IfIntLe0(ref index) => out.write_code_index(index)?,

            IfIntEq(ref index) => out.write_code_index(index)?,
            IfIntNe(ref index) => out.write_code_index(index)?,
            IfIntLt(ref index) => out.write_code_index(index)?,
            IfIntGe(ref index) => out.write_code_index(index)?,
            IfIntGt(ref index) => out.write_code_index(index)?,
            IfIntLe(ref index) => out.write_code_index(index)?,

            IfRefEq(ref index) => out.write_code_index(index)?,
            IfRefNe(ref index) => out.write_code_index(index)?,
            
            Goto(ref index) => out.write_code_index(&index)?,
            JumpSub(ref index) => out.write_code_index(index)?,
            RetSub(ref index) => out.write_var_index(index)?,
            
            TableSwitch { ref default_offset, ref match_range, ref offset_table, } => {
                while out.count % 4 != 0 {
                    out.write_u8(0)?;
                }
                out.write_wide_code_index(default_offset)?;
                out.write_i32::<BigEndian>(match_range.0)?;
                out.write_i32::<BigEndian>(match_range.1)?;
                
                for ref offset in offset_table.iter() {
                    out.write_wide_code_index(offset)?;
                }
            },

            LookupSwitch { ref default_offset, ref match_table, } => {
                while out.count % 4 != 0 {
                    out.write_u8(0)?;
                }
                out.write_wide_code_index(default_offset)?;
                out.write_u32::<BigEndian>(match_table.len() as u32)?;
                for &(index, ref offset) in match_table.iter() {
                    out.write_i32::<BigEndian>(index)?;
                    out.write_wide_code_index(offset)?;
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
                use super::WideInstruction as W;
                match *wide_instruction {
                    W::LoadInt(ref index) => out.write_wide_var_index(index)?,
                    W::LoadLong(ref index) => out.write_wide_var_index(index)?,
                    W::LoadFloat(ref index) => out.write_wide_var_index(index)?,
                    W::LoadDouble(ref index) => out.write_wide_var_index(index)?,
                    W::LoadRef(ref index) => out.write_wide_var_index(index)?,

                    W::StoreInt(ref index) => out.write_wide_var_index(index)?,
                    W::StoreLong(ref index) => out.write_wide_var_index(index)?,
                    W::StoreFloat(ref index) => out.write_wide_var_index(index)?,
                    W::StoreDouble(ref index) => out.write_wide_var_index(index)?,
                    W::StoreRef(ref index) => out.write_wide_var_index(index)?,

                    W::IncInt(ref index, short) => {
                        out.write_wide_var_index(index)?;
                        out.write_i16::<BigEndian>(short)?;
                    }

                    W::RetSub(ref index) => out.write_wide_var_index(index)?,

                };
            },

            NewRefMultiArray(ref index, dims) => {
                out.write_constant_index(index)?;
                out.write_u8(dims)?;
            }
            IfRefNull(ref index) => out.write_code_index(index)?,
            IfRefNonNull(ref index) => out.write_code_index(index)?,
            WideGoto(ref index) => out.write_wide_code_index(index)?,
            WideJumpSub(ref index) => out.write_wide_code_index(index)?,

            _ => {},
        };
    }
    Ok(())
}