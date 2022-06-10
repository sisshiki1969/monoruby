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
    DivideByZero,
}

impl MonorubyErr {
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

    pub fn undefined_local(ident: String, loc: Loc, sourceinfo: SourceInfoRef) -> MonorubyErr {
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

    pub fn divide_by_zero() -> MonorubyErr {
        MonorubyErr {
            kind: MonorubyErrKind::DivideByZero,
            loc: vec![],
        }
    }
}
