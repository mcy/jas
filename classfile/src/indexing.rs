
use byteorder::*;
use std::io;

#[derive(Clone, Debug)]
pub struct ConstantIndex(pub u16);

impl ConstantIndex {
    pub fn len() -> usize {
        2
    }
}

#[derive(Clone, Debug)]
pub struct WideConstantIndex(pub u32);

impl WideConstantIndex {
    pub fn len() -> usize {
        4
    }
}

#[derive(Clone, Debug)]
pub struct CodeIndex(pub u16);

impl CodeIndex {
    pub fn len() -> usize {
        2
    }
}

#[derive(Clone, Debug)]
pub struct WideCodeIndex(pub u32);

impl WideCodeIndex {
    pub fn len() -> usize {
        4
    }
}

#[derive(Clone, Debug)]
pub struct VarIndex(pub u8);

impl VarIndex {
    pub fn len() -> usize {
        1
    }
}

#[derive(Clone, Debug)]
pub struct WideVarIndex(pub u16);

impl WideVarIndex {
    pub fn len() -> usize {
        2
    }
}

pub trait ReadIndexExt: io::Read {

    fn read_constant_index(&mut self) -> io::Result<ConstantIndex>;

    fn read_wide_constant_index(&mut self) -> io::Result<WideConstantIndex>;

    fn read_code_index(&mut self) -> io::Result<CodeIndex>;

    fn read_wide_code_index(&mut self) -> io::Result<WideCodeIndex>;

    fn read_var_index(&mut self) -> io::Result<VarIndex>;

    fn read_wide_var_index(&mut self) -> io::Result<WideVarIndex>;
}

impl<R> ReadIndexExt for R where R: io::Read  {

    fn read_constant_index(&mut self) -> io::Result<ConstantIndex> {
        Ok(ConstantIndex(self.read_u16::<BigEndian>()?))
    }

    fn read_wide_constant_index(&mut self) -> io::Result<WideConstantIndex> {
        Ok(WideConstantIndex(self.read_u32::<BigEndian>()?))
    }

    fn read_code_index(&mut self) -> io::Result<CodeIndex> {
        Ok(CodeIndex(self.read_u16::<BigEndian>()?))
    }

    fn read_wide_code_index(&mut self) -> io::Result<WideCodeIndex> {
        Ok(WideCodeIndex(self.read_u32::<BigEndian>()?))
    }

    fn read_var_index(&mut self) -> io::Result<VarIndex> {
        Ok(VarIndex(self.read_u8()?))
    }

    fn read_wide_var_index(&mut self) -> io::Result<WideVarIndex> {
        Ok(WideVarIndex(self.read_u16::<BigEndian>()?))
    }
}

pub trait WriteIndexExt: io::Write {

    fn write_constant_index(&mut self, index: &ConstantIndex) -> io::Result<()>;

    fn write_wide_constant_index(&mut self, index: &WideConstantIndex) -> io::Result<()>;

    fn write_code_index(&mut self, index: &CodeIndex) -> io::Result<()>;

    fn write_wide_code_index(&mut self, index: &WideCodeIndex) -> io::Result<()>;

    fn write_var_index(&mut self, index: &VarIndex) -> io::Result<()>;

    fn write_wide_var_index(&mut self, index: &WideVarIndex) -> io::Result<()>;
}

impl<W> WriteIndexExt for W where W: io::Write  {

    fn write_constant_index(&mut self, index: &ConstantIndex) -> io::Result<()> {
        self.write_u16::<BigEndian>(index.0)
    }

    fn write_wide_constant_index(&mut self, index: &WideConstantIndex) -> io::Result<()> {
        self.write_u32::<BigEndian>(index.0)
    }

    fn write_code_index(&mut self, index: &CodeIndex) -> io::Result<()> {
        self.write_u16::<BigEndian>(index.0)
    }

    fn write_wide_code_index(&mut self, index: &WideCodeIndex) -> io::Result<()> {
        self.write_u32::<BigEndian>(index.0)
    }

    fn write_var_index(&mut self, index: &VarIndex) -> io::Result<()> {
        self.write_u8(index.0)
    }

    fn write_wide_var_index(&mut self, index: &WideVarIndex) -> io::Result<()> {
        self.write_u16::<BigEndian>(index.0)
    }
}