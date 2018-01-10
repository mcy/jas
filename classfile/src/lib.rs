extern crate byteorder;

pub mod consts;
pub mod raw;
pub mod indexing;

#[derive(Clone)]
pub struct ClassName(pub Vec<String>);

impl ClassName {

    pub fn from_str(class: &str) -> ClassName {
        ClassName(class.split("/").map(|s| s.into()).collect::<Vec<_>>())
    }
}

#[derive(Clone)]
pub enum JavaType {
    Byte,
    Short,
    Int,
    Long,

    Float,
    Double,

    Boolean,
    Char,

    Class(ClassName),
    Array(Box<JavaType>),
}

impl JavaType {

    fn name(&self) -> String {
        use self::JavaType::*;
        match *self {
            Byte => "B".into(),
            Short => "S".into(),
            Int => "I".into(),
            Long => "J".into(),

            Float => "F".into(),
            Double => "D".into(),

            Boolean => "Z".into(),
            Char => "C".into(),

            Class(ClassName(ref path)) => format!("L{};", path.join("/")),
            Array(ref element) => format!("[{}", element.name()),
        }
    }

    fn array(&self) -> JavaType {
        JavaType::Array(Box::new(self.clone()))
    }
}