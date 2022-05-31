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
pub use interp::Interp;
use op::*;

type Result<T> = std::result::Result<T, MonorubyErr>;
pub type BuiltinFn = extern "C" fn(&mut Interp, &mut Globals, Arg, usize) -> Value;

#[derive(Debug, Clone, PartialEq)]
pub enum MonorubyErr {
    UndefinedLocal(IdentId),
    MethodNotFound(IdentId),
    WrongArguments(String),
    Syntax(ParseErr),
    Syntax2(String),
    Unimplemented(String),
}

impl MonorubyErr {
    pub fn parse(error: ParseErr) -> MonorubyErr {
        MonorubyErr::Syntax(error)
    }

    pub fn escape_from_eval() -> MonorubyErr {
        MonorubyErr::Syntax2("can't escape from eval.".to_string())
    }

    pub fn undefined_local(ident: IdentId) -> MonorubyErr {
        MonorubyErr::UndefinedLocal(ident)
    }

    pub fn method_not_found(name: IdentId) -> MonorubyErr {
        MonorubyErr::MethodNotFound(name)
    }

    pub fn wrong_arguments(expected: usize, actual: usize) -> MonorubyErr {
        MonorubyErr::WrongArguments(format!(
            "number of arguments mismatch. expected:{} actual:{}",
            expected, actual
        ))
    }

    pub fn unsupported_parameter_kind(param: ParamKind) -> MonorubyErr {
        MonorubyErr::Unimplemented(format!("unsupported parameter kind {:?}", param))
    }

    pub fn unsupported_operator(op: BinOp) -> MonorubyErr {
        MonorubyErr::Unimplemented(format!("unsupported operator {:?}", op))
    }

    pub fn unsupported_lhs(lhs: NodeKind) -> MonorubyErr {
        MonorubyErr::Unimplemented(format!("unsupported lhs {:?}", lhs))
    }

    pub fn unsupported_node(expr: NodeKind) -> MonorubyErr {
        MonorubyErr::Unimplemented(format!("unsupported nodekind {:?}", expr))
    }
}

///
/// ID of instruction.
///
#[derive(Clone, Copy, PartialEq, Eq, Hash, Default)]
struct InstId(pub u32);

impl std::fmt::Debug for InstId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ":{:05}", self.0)
    }
}

///
/// ID of register.
///
#[derive(Clone, Copy, PartialEq, Eq)]
enum BcReg {
    Self_,
    Local(BcLocal),
    Temp(BcTemp),
}

impl std::fmt::Debug for BcReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Self_ => write!(f, "S"),
            Self::Local(local) => write!(f, "{:?}", local),
            Self::Temp(temp) => write!(f, "{:?}", temp),
        }
    }
}

impl std::convert::From<BcLocal> for BcReg {
    fn from(local: BcLocal) -> Self {
        BcReg::Local(local)
    }
}

impl std::convert::From<BcTemp> for BcReg {
    fn from(temp: BcTemp) -> Self {
        BcReg::Temp(temp)
    }
}

///
/// ID of temporary register.
///
#[derive(Clone, Copy, PartialEq, Eq, Hash, Default)]
struct BcTemp(pub u16);

impl std::fmt::Debug for BcTemp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.0)
    }
}

impl std::ops::AddAssign<usize> for BcTemp {
    fn add_assign(&mut self, rhs: usize) {
        self.0 = self.0 + rhs as u16;
    }
}

///
/// ID of local variable.
///
#[derive(Clone, Copy, PartialEq, Eq, Hash, Default)]
struct BcLocal(pub u16);

impl std::fmt::Debug for BcLocal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "L{}", self.0)
    }
}
