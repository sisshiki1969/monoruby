use super::*;

#[derive(Debug, Clone, PartialEq)]
pub struct MonorubyErr {
    pub kind: MonorubyErrKind,
    pub loc: Vec<(Loc, SourceInfoRef)>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MonorubyErrKind {
    UndefinedLocal(String),
    MethodNotFound(IdentId),
    WrongArguments(String),
    Syntax(ParseErrKind),
    Syntax2(String),
    Unimplemented(String),
    UninitConst(IdentId),
    DivideByZero,
    Range(String),
}

impl MonorubyErr {
    fn new(kind: MonorubyErrKind) -> Self {
        MonorubyErr {
            kind: kind,
            loc: vec![],
        }
    }

    fn new_with_loc(kind: MonorubyErrKind, loc: Loc, sourceinfo: SourceInfoRef) -> Self {
        MonorubyErr {
            kind: kind,
            loc: vec![(loc, sourceinfo)],
        }
    }

    pub fn show_all_loc(&self) {
        for (loc, sourceinfo) in &self.loc {
            sourceinfo.show_loc(loc);
        }
    }

    pub fn show_loc(&self) {
        if let Some((loc, sourceinfo)) = self.loc.first() {
            sourceinfo.show_loc(loc);
        } else {
            eprintln!("location not defined.");
        }
    }

    pub fn get_error_message(&self, globals: &Globals) -> String {
        globals.get_error_message(self)
    }
}

// Parser level errors.
impl MonorubyErr {
    pub fn parse(error: ParseErr) -> MonorubyErr {
        MonorubyErr::new_with_loc(
            MonorubyErrKind::Syntax(error.kind),
            error.loc,
            error.source_info,
        )
    }
}

// Bytecode compiler level errors.
impl MonorubyErr {
    pub fn unsupported_parameter_kind(
        param: ParamKind,
        loc: Loc,
        sourceinfo: SourceInfoRef,
    ) -> MonorubyErr {
        MonorubyErr::new_with_loc(
            MonorubyErrKind::Unimplemented(format!("unsupported parameter kind {:?}", param)),
            loc,
            sourceinfo,
        )
    }

    pub fn unsupported_operator(op: BinOp, loc: Loc, sourceinfo: SourceInfoRef) -> MonorubyErr {
        MonorubyErr::new_with_loc(
            MonorubyErrKind::Unimplemented(format!("unsupported operator {:?}", op)),
            loc,
            sourceinfo,
        )
    }

    pub fn unsupported_lhs(lhs: Node, sourceinfo: SourceInfoRef) -> MonorubyErr {
        MonorubyErr::new_with_loc(
            MonorubyErrKind::Unimplemented(format!("unsupported lhs {:?}", lhs.kind)),
            lhs.loc,
            sourceinfo,
        )
    }

    pub fn unsupported_node(expr: Node, sourceinfo: SourceInfoRef) -> MonorubyErr {
        MonorubyErr::new_with_loc(
            MonorubyErrKind::Unimplemented(format!("unsupported nodekind {:?}", expr.kind)),
            expr.loc,
            sourceinfo,
        )
    }

    pub fn escape_from_eval(loc: Loc, sourceinfo: SourceInfoRef) -> MonorubyErr {
        MonorubyErr::new_with_loc(
            MonorubyErrKind::Syntax2("can't escape from eval.".to_string()),
            loc,
            sourceinfo,
        )
    }

    pub fn undefined_local(ident: String, loc: Loc, sourceinfo: SourceInfoRef) -> MonorubyErr {
        MonorubyErr::new_with_loc(MonorubyErrKind::UndefinedLocal(ident), loc, sourceinfo)
    }
}

// Executor level errors.
impl MonorubyErr {
    pub fn method_not_found(name: IdentId) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::MethodNotFound(name))
    }

    pub fn wrong_arguments(expected: usize, actual: usize) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::WrongArguments(format!(
            "number of arguments mismatch. expected:{} actual:{}",
            expected, actual
        )))
    }

    pub fn divide_by_zero() -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::DivideByZero)
    }

    pub fn range(msg: String) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Range(msg))
    }

    pub fn uninitialized_constant(name: IdentId) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::UninitConst(name))
    }
}
