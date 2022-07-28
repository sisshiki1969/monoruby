use super::*;

mod builtins;
mod bytecodegen;
mod compiler;
mod globals;
mod inst;
mod interp;
mod op;
pub use builtins::*;
use bytecodegen::*;
pub use compiler::*;
pub use globals::*;
use inst::*;
pub use interp::*;
use op::*;

type Result<T> = std::result::Result<T, MonorubyErr>;
pub type BuiltinFn = extern "C" fn(&mut Interp, &mut Globals, Arg, usize) -> Option<Value>;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub(crate) struct SlotId(u16);

impl SlotId {
    pub(crate) fn new(reg: u16) -> Self {
        Self(reg)
    }

    pub(crate) fn self_() -> Self {
        Self(0)
    }

    pub(crate) fn is_zero(&self) -> bool {
        self.0 == 0
    }
}

impl std::fmt::Display for SlotId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::ops::Add<u16> for SlotId {
    type Output = Self;
    fn add(self, rhs: u16) -> Self {
        Self(self.0 + rhs)
    }
}
