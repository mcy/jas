use super::*;

use consts::class::*;
use indexing::ConstantIndex;

#[derive(Debug)]
pub struct Class {
    pub minor_version: u16,
    pub major_version: u16,

    pub constant_pool: Vec<Constant>,

    pub flags: Flags,

    pub this_class: ConstantIndex,
    pub super_class: ConstantIndex,
    pub interfaces: Vec<ConstantIndex>,

    pub fields: Vec<Field>,
    pub methods: Vec<Method>,
    pub attributes: Vec<Attribute>,
}

