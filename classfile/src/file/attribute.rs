use super::*;

use consts::class;
use indexing::*;

#[derive(Debug)]
pub struct Attribute {
    pub name: ConstantIndex,
    pub info: AttributeInfo,
}

#[derive(Debug)]
pub enum AttributeInfo {
    ConstantValue(ConstantIndex),

    Code {
        max_stack: CodeIndex,
        max_locals: CodeIndex,
        code_len: u32,
        code: Vec<Instruction>,
        exception_table: Vec<ExceptionTableEntry>,
        attributes: Vec<Attribute>,
    },

    //StackMapTable(Vec<StackMapFrame>),

    Exceptions(Vec<ConstantIndex>),

    InnerClasses(Vec<InnerClass>),

    EnclosingMethod {
        class: ConstantIndex,
        method: ConstantIndex,
    },

    Synthetic,

    Signature(ConstantIndex),

    SourceFile(ConstantIndex),
    SourceDebugExtension(Vec<u8>),

    LineNumberTable(Vec<LineNumber>),
    LocalVariableTable(Vec<LocalVariable>),
    LocalVariableTypeTable(Vec<LocalVariableType>),

    Deprecated,

    RuntimeVisibleAnnotations(Vec<Annotation>),
    RuntimeInvisibleAnnotations(Vec<Annotation>),

    RuntimeVisibleParameterAnnotations(Vec<Vec<Annotation>>),
    RuntimeInvisibleParameterAnnotations(Vec<Vec<Annotation>>),

    AnnotationDefault(AnnotationElement),

    BootstrapMethods(Vec<BootstrapMethod>),

    Other(Vec<u8>),
}

impl AttributeInfo {

    pub fn len(&self) -> usize {
        match *self {
            AttributeInfo::ConstantValue(..) => ConstantIndex::len(),

            AttributeInfo::Code {
                ref max_stack, ref max_locals, code_len, ref code, ref exception_table, ref attributes,
            } => {
                2 * CodeIndex::len() +
                    4 + code_len as usize +
                    2 + exception_table.len() * (CodeIndex::len() * 3 + ConstantIndex::len()) +
                    2 + attributes.iter().map(|attr| 2 + 4 + attr.info.len()).sum::<usize>()
            },

            AttributeInfo::Exceptions(ref indices) => 2 + indices.len() * ConstantIndex::len(),

            AttributeInfo::InnerClasses(ref classes) =>
                2 + classes.len() * (ConstantIndex::len() * 3 + class::Flags::len()),

            AttributeInfo::EnclosingMethod {..} => 2 * ConstantIndex::len() ,

            AttributeInfo::Synthetic => 0,

            AttributeInfo::Signature(..) => ConstantIndex::len(),

            AttributeInfo::SourceFile(..) => ConstantIndex::len(),
            AttributeInfo::SourceDebugExtension(ref bytes) => bytes.len(),

            AttributeInfo::LineNumberTable(ref lines) =>
                2 + lines.len() * (CodeIndex::len() + 2),
            AttributeInfo::LocalVariableTable(ref vars) =>
                2 + vars.len() * (CodeIndex::len() * 2 + ConstantIndex::len() * 2 + VarIndex::len()),
            AttributeInfo::LocalVariableTypeTable(ref vars) =>
                2 + vars.len() * (CodeIndex::len() * 2 + ConstantIndex::len() * 2 + VarIndex::len()),

            AttributeInfo::Deprecated => 0,

            AttributeInfo::RuntimeVisibleAnnotations(ref ans) =>
                2 + ans.iter().map(|an| an.len()).sum::<usize>(),
            AttributeInfo::RuntimeInvisibleAnnotations(ref ans) =>
                2 + ans.iter().map(|an| an.len()).sum::<usize>(),

            AttributeInfo::RuntimeVisibleParameterAnnotations(ref ans) =>
                2 + ans.iter().map(|an| an.len()).sum::<usize>(),
            AttributeInfo::RuntimeInvisibleParameterAnnotations(ref ans) =>
                2 + ans.iter().map(|an| an.len()).sum::<usize>(),

            AttributeInfo::AnnotationDefault(ref an) => 4 + an.len(),

            AttributeInfo::BootstrapMethods(ref methods) =>
                methods.iter().map(|meth| ConstantIndex::len() * (meth.arguments.len() + 1)).sum(),

            AttributeInfo::Other(ref bytes) => bytes.len(),
        }
    }
}

#[derive(Debug)]
pub struct StackMapFrame {
    frame_type: u8,
    data: StackMapFrameData
}

#[derive(Debug)]
pub enum StackMapFrameData {
    SameFrame, /* 0-63 */
    SameFrameOneItem { /* 64-127 */
        stack: VerificationType,
    },
    SameFrameOneItemExtended { /* 247 */
        offset_delta: u16,
        stack: VerificationType,
    },
    ChopFrame { /* 248-250 */
        offset_delta: u16,
    },
    FrameExtended { /* 251 */
        offset_delta: u16,
    },
    AppendFrame { /* 252-254 */
        offset_delta: u16,
        locals: Vec<VerificationType>,
    },
    FullFrame { /* 255 */
        offset_delta: u16,
        locals: Vec<VerificationType>,
        stack: Vec<VerificationType>,
    }
}

#[derive(Debug)]
pub enum VerificationType {
    Top,
    Int,
    Float,
    Long,
    Double,
    Null,
    UninitializedThis,
    Object(ConstantIndex),
    Uninitialized(CodeIndex),
}

#[derive(Debug)]
pub struct InnerClass {
    pub inner_class: ConstantIndex,
    pub outer_class: ConstantIndex,
    pub inner_name: ConstantIndex,
    pub inner_flags: class::Flags,
}

#[derive(Debug)]
pub struct LineNumber {
    pub instruction: CodeIndex,
    pub line_number: u16,
}

#[derive(Debug)]
pub struct LocalVariable {
    pub valid_start: CodeIndex,
    pub valid_length: CodeIndex,
    pub name: ConstantIndex,
    pub descriptor: ConstantIndex,
    pub variable_index: VarIndex,
}

#[derive(Debug)]
pub struct LocalVariableType {
    pub valid_start: CodeIndex,
    pub valid_length: CodeIndex,
    pub name: ConstantIndex,
    pub descriptor: ConstantIndex,
    pub variable_index: VarIndex,
}

#[derive(Debug)]
pub struct BootstrapMethod {
    pub method: ConstantIndex,
    pub arguments: Vec<ConstantIndex>,
}