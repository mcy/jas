use super::*;

use consts::class;
use consts::attribute as attr;
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

    StackMapTable(Vec<StackMapFrame>),

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

            AttributeInfo::StackMapTable(ref frames) => {
                2 + frames.iter().map(|frame| {
                    match *frame {
                        StackMapFrame::Same(..) => 1,
                        StackMapFrame::SameExt(..) => 3,
                        StackMapFrame::SingleStack(_, ref ty) => 1 + ty.len(),
                        StackMapFrame::SingleStackExt(_, ref ty) => 3 + ty.len(),
                        StackMapFrame::Chop1(..) => 3,
                        StackMapFrame::Chop2(..) => 3,
                        StackMapFrame::Chop3(..) => 3,
                        StackMapFrame::Append1(_, ref ty1) => 3 + ty1.len(),
                        StackMapFrame::Append2(_, ref ty1, ref ty2) => 3 + ty1.len() + ty2.len(),
                        StackMapFrame::Append3(_, ref ty1, ref ty2, ref ty3) => 3 + ty1.len() + ty2.len() + ty3.len(),
                        StackMapFrame::Full { offset: _, ref locals, ref stack } => {
                            1 + 2 + 2 + 2 +
                                locals.iter().map(VerificationType::len).sum::<usize>() +
                                stack.iter().map(VerificationType::len).sum::<usize>()
                        },
                    }
                }).sum::<usize>()
            }

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
                2 + methods.iter().map(|meth| 2 + ConstantIndex::len() * (meth.arguments.len() + 1)).sum::<usize>(),

            AttributeInfo::Other(ref bytes) => bytes.len(),
        }
    }
}

#[derive(Debug)]
pub enum StackMapFrame {
    Same(CodeIndex),
    SameExt(CodeIndex),
    SingleStack(CodeIndex, VerificationType),
    SingleStackExt(CodeIndex, VerificationType),
    Chop1(CodeIndex),
    Chop2(CodeIndex),
    Chop3(CodeIndex),
    Append1(CodeIndex, VerificationType),
    Append2(CodeIndex, VerificationType, VerificationType),
    Append3(CodeIndex, VerificationType, VerificationType, VerificationType),
    Full {
        offset: CodeIndex,
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

impl VerificationType {
    pub fn tag(&self) -> u8 {
        match *self {
            VerificationType::Top => attr::VTYPE_TOP,
            VerificationType::Int => attr::VTYPE_INT,
            VerificationType::Float => attr::VTYPE_FLOAT,
            VerificationType::Long => attr::VTYPE_LONG,
            VerificationType::Double => attr::VTYPE_DOUBLE,
            VerificationType::Null => attr::VTYPE_NULL,
            VerificationType::UninitializedThis => attr::VTYPE_UNINIT_THIS,
            VerificationType::Object(..) => attr::VTYPE_OBJ,
            VerificationType::Uninitialized(..) => attr::VTYPE_UNINIT,
        }
    }

    pub fn len(&self) -> usize {
        match *self {
            VerificationType::Object(..) => 3,
            VerificationType::Uninitialized(..) => 3,
            _ => 1,
        }
    }
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