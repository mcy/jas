use super::*;

use consts::method::*;
use indexing::ConstantIndex;

#[derive(Debug)]
pub struct Method {
    pub flags: Flags,
    pub name: ConstantIndex,
    pub descriptor: ConstantIndex,
    pub attributes: Vec<Attribute>,
}