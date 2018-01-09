use super::*;

use consts::field::*;
use indexing::ConstantIndex;

#[derive(Debug)]
pub struct Field {
    pub flags: Flags,
    pub name: ConstantIndex,
    pub descriptor: ConstantIndex,
    pub attributes: Vec<Attribute>,
}
