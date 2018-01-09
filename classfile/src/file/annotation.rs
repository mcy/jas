use indexing::ConstantIndex;

#[derive(Debug)]
pub struct Annotation {
    class: ConstantIndex,
    values: Vec<(u16, AnnotationElement)>,
}

impl Annotation {

    pub fn len(&self) -> usize {
        ConstantIndex::len() + 2 +
            self.values.len() +
            self.values.iter().map(|&(_, ref an)| 2 + an.len()).sum::<usize>()
    }
}

#[derive(Debug)]
pub enum AnnotationElement {
    Byte(ConstantIndex),
    Short(ConstantIndex),
    Int(ConstantIndex),
    Long(ConstantIndex),

    Float(ConstantIndex),
    Double(ConstantIndex),

    Boolean(ConstantIndex),
    Char(ConstantIndex),
    String(ConstantIndex),

    Enum {
        class: ConstantIndex,
        name: ConstantIndex,
    },

    Class(ConstantIndex),

    Annotation(Annotation),

    Array(Vec<AnnotationElement>),
}

impl AnnotationElement {
    pub fn len(&self) -> usize {
        use self::AnnotationElement::*;
        1 + match *self {
            Byte(..) => ConstantIndex::len(),
            Short(..) => ConstantIndex::len(),
            Int(..) => ConstantIndex::len(),
            Long(..) => ConstantIndex::len(),

            Float(..) => ConstantIndex::len(),
            Double(..) => ConstantIndex::len(),

            Boolean(..) => ConstantIndex::len(),
            Char(..) => ConstantIndex::len(),
            String(..) => ConstantIndex::len(),

            Enum {..} => 2 * ConstantIndex::len(),

            Class(..) => ConstantIndex::len(),

            Annotation(ref an) => an.len(),

            Array(ref xs) => xs.iter().map(|an| 2 + an.len()).sum(),
        }
    }
}