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
pub struct MonorubyErr {
    pub kind: MonorubyErrKind,
    pub loc: Vec<(Loc, SourceInfoRef)>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MonorubyErrKind {
    UndefinedLocal(IdentId),
    MethodNotFound(IdentId),
    WrongArguments(String),
    Syntax(ParseErrKind),
    Syntax2(String),
    Unimplemented(String),
}

impl MonorubyErr {
    pub fn show_loc(&self) {
        match self.loc.first() {
            Some((loc, sourceinfo)) => {
                sourceinfo.show_loc(loc);
            }
            None => {}
        }
    }
}

// Parser level errors.
impl MonorubyErr {
    pub fn parse(error: ParseErr) -> MonorubyErr {
        MonorubyErr {
            kind: MonorubyErrKind::Syntax(error.kind),
            loc: vec![(error.loc, error.source_info)],
        }
    }
}

// Bytecode compiler level errors.
impl MonorubyErr {
    pub fn unsupported_parameter_kind(
        param: ParamKind,
        loc: Loc,
        sourceinfo: SourceInfoRef,
    ) -> MonorubyErr {
        MonorubyErr {
            kind: MonorubyErrKind::Unimplemented(format!("unsupported parameter kind {:?}", param)),
            loc: vec![(loc, sourceinfo)],
        }
    }

    pub fn unsupported_operator(op: BinOp, loc: Loc, sourceinfo: SourceInfoRef) -> MonorubyErr {
        MonorubyErr {
            kind: MonorubyErrKind::Unimplemented(format!("unsupported operator {:?}", op)),
            loc: vec![(loc, sourceinfo)],
        }
    }

    pub fn unsupported_lhs(lhs: Node, sourceinfo: SourceInfoRef) -> MonorubyErr {
        MonorubyErr {
            kind: MonorubyErrKind::Unimplemented(format!("unsupported lhs {:?}", lhs.kind)),
            loc: vec![(lhs.loc, sourceinfo)],
        }
    }

    pub fn unsupported_node(expr: Node, sourceinfo: SourceInfoRef) -> MonorubyErr {
        MonorubyErr {
            kind: MonorubyErrKind::Unimplemented(format!("unsupported nodekind {:?}", expr.kind)),
            loc: vec![(expr.loc, sourceinfo)],
        }
    }

    pub fn escape_from_eval(loc: Loc, sourceinfo: SourceInfoRef) -> MonorubyErr {
        MonorubyErr {
            kind: MonorubyErrKind::Syntax2("can't escape from eval.".to_string()),
            loc: vec![(loc, sourceinfo)],
        }
    }

    pub fn undefined_local(ident: IdentId, loc: Loc, sourceinfo: SourceInfoRef) -> MonorubyErr {
        MonorubyErr {
            kind: MonorubyErrKind::UndefinedLocal(ident),
            loc: vec![(loc, sourceinfo)],
        }
    }
}

// Executor level errors.
impl MonorubyErr {
    pub fn method_not_found(name: IdentId) -> MonorubyErr {
        MonorubyErr {
            kind: MonorubyErrKind::MethodNotFound(name),
            loc: vec![],
        }
    }

    pub fn wrong_arguments(expected: usize, actual: usize) -> MonorubyErr {
        MonorubyErr {
            kind: MonorubyErrKind::WrongArguments(format!(
                "number of arguments mismatch. expected:{} actual:{}",
                expected, actual
            )),
            loc: vec![],
        }
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
