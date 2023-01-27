use super::*;

//
// error handlers
//
impl Globals {
    pub fn set_error(&mut self, err: MonorubyErr) {
        self.error = Some(err);
    }

    pub(crate) fn err_method_not_found(&mut self, name: IdentId, obj: Value) {
        self.set_error(MonorubyErr::method_not_found(name, obj))
    }

    pub(crate) fn err_divide_by_zero(&mut self) {
        self.set_error(MonorubyErr::divide_by_zero());
    }

    pub(crate) fn err_uninitialized_constant(&mut self, name: IdentId) {
        self.set_error(MonorubyErr::uninitialized_constant(name));
    }

    pub(crate) fn err_bad_range(&mut self, start: Value, end: Value) {
        self.set_error(MonorubyErr::bad_range(start, end));
    }

    ///
    /// Set RangeError with message "*val* out of char range".
    ///
    pub(crate) fn err_char_out_of_range(&mut self, val: Value) {
        self.set_error(MonorubyErr::range(format!(
            "{} out of char range",
            self.val_tos(val)
        )));
    }

    ///
    /// Set TypeError with message "no implicit conversion of *actual* into *expected*".
    ///
    pub(crate) fn err_no_implict_conv(&mut self, actual: Value, expect: ClassId) {
        let actual = actual.get_real_class_name(self);
        self.set_error(MonorubyErr::typeerr(format!(
            "no implicit conversion of {} into {}",
            actual,
            expect.get_name(self),
        )));
    }

    ///
    /// Set TypeError with message "*name* is not a class".
    ///
    pub(crate) fn err_is_not_class(&mut self, name: String) {
        self.set_error(MonorubyErr::typeerr(format!("{name} is not a class",)));
    }

    ///
    /// Set TypeError with message "superclass mismatch for class *name*".
    ///
    pub(crate) fn err_superclass_mismatch(&mut self, name: IdentId) {
        self.set_error(MonorubyErr::typeerr(format!(
            "superclass mismatch for class {}",
            IdentId::get_name(name)
        )));
    }

    ///
    /// Set TypeError with message "*name* is not Symbol nor String".
    ///
    pub(crate) fn err_is_not_symbol_nor_string(&mut self, val: Value) {
        self.set_error(MonorubyErr::typeerr(format!(
            "{} is not a symbol nor a string",
            self.val_tos(val)
        )));
    }

    ///
    /// Set TypeError with message "*name* is not Regexp nor String".
    ///
    pub(crate) fn err_is_not_regexp_nor_string(&mut self, val: Value) {
        self.set_error(MonorubyErr::typeerr(format!(
            "{} is not a regexp nor a string",
            self.val_tos(val)
        )));
    }

    ///
    /// Set TypeError with message "no_implicit_conversion of {} into {}".
    ///
    pub(crate) fn err_no_implicit_conversion(&mut self, val: Value, target_class: ClassId) {
        self.set_error(MonorubyErr::typeerr(format!(
            "no implicit conversion of {} into {}",
            val.get_real_class_name(self),
            target_class.get_name(self),
        )));
    }

    ///
    /// Set TypeError with message "can't convert *class of val* into Float".
    ///
    pub(crate) fn err_cant_conert_into_float(&mut self, val: Value) {
        self.set_error(MonorubyErr::typeerr(format!(
            "can't convert {} into Float",
            val.get_real_class_name(self)
        )));
    }

    pub(crate) fn err_argument(&mut self, msg: &str) {
        self.set_error(MonorubyErr::argumenterr(msg.to_string()));
    }

    pub(crate) fn err_internal(&mut self, msg: String) {
        self.set_error(MonorubyErr::internalerr(msg));
    }

    pub(crate) fn err_regex(&mut self, msg: String) {
        self.set_error(MonorubyErr::regexerr(msg));
    }

    pub(crate) fn err_create_proc_no_block(&mut self) {
        self.err_argument("tried to create Proc object without a block");
    }

    pub(crate) fn check_number_of_arguments(
        &mut self,
        given: usize,
        range: std::ops::RangeInclusive<usize>,
    ) -> Option<()> {
        if range.contains(&given) {
            Some(())
        } else {
            if range.start() == range.end() {
                self.set_error(MonorubyErr::wrong_arguments(*range.start(), given));
            } else {
                self.err_argument(&format!(
                    "wrong number of arguments (given {given}, expeted {:?})",
                    range
                ));
            };
            None
        }
    }

    ///
    /// Set IndexError with message "index *actual* too small for array; minimum: *minimum*".
    ///
    pub(crate) fn err_index_too_small(&mut self, actual: i64, minimum: i64) {
        self.set_error(MonorubyErr::indexerr(format!(
            "index {} too small for array; minimum: {}",
            actual, minimum,
        )));
    }

    ///
    /// Set FrozenError with message "can't modify frozen Integer: 5".
    ///
    pub(crate) fn err_cant_modify_frozen(&mut self, val: Value) {
        self.set_error(MonorubyErr::frozenerr(format!(
            "can't modify frozen {}: {}",
            val.get_real_class_name(self),
            self.val_tos(val),
        )));
    }

    ///
    /// Set LoadError with message "can't load '*file*'".
    ///
    pub(crate) fn err_cant_load(&mut self, err: Option<std::io::Error>, path: &std::path::Path) {
        self.set_error(MonorubyErr::loaderr(match err {
            Some(err) => format!("can't load {:?}. {}", path, err,),
            None => format!("can't load {:?}", path),
        }));
    }

    pub(crate) fn take_error(&mut self) -> Option<MonorubyErr> {
        std::mem::take(&mut self.error)
    }

    pub(crate) fn push_error_location(&mut self, loc: Loc, sourceinfo: SourceInfoRef) {
        match &mut self.error {
            Some(err) => {
                err.loc.push((loc, sourceinfo));
            }
            None => unreachable!(),
        };
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MonorubyErr {
    pub kind: MonorubyErrKind,
    pub loc: Vec<(Loc, SourceInfoRef)>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MonorubyErrKind {
    //UndefinedLocal(String),
    MethodNotFound(IdentId, Value),
    Arguments(String),
    Syntax(ParseErrKind),
    Syntax2(String),
    Unimplemented(String),
    UninitConst(IdentId),
    DivideByZero,
    Range(String),
    Type(String),
    Index(String),
    Frozen(String),
    Load(String),
    Internal(String),
    Regex(String),
}

impl MonorubyErr {
    fn new(kind: MonorubyErrKind) -> Self {
        MonorubyErr { kind, loc: vec![] }
    }

    fn new_with_loc(kind: MonorubyErrKind, loc: Loc, sourceinfo: SourceInfoRef) -> Self {
        MonorubyErr {
            kind,
            loc: vec![(loc, sourceinfo)],
        }
    }

    pub fn is_eof(&self) -> bool {
        self.kind == MonorubyErrKind::Syntax(ParseErrKind::UnexpectedEOF)
    }

    pub fn show_all_loc(&self) {
        for (loc, sourceinfo) in &self.loc {
            sourceinfo.show_loc(loc);
        }
    }

    fn show_loc(&self) {
        if let Some((loc, sourceinfo)) = self.loc.first() {
            sourceinfo.show_loc(loc);
        } else {
            eprintln!("location not defined.");
        }
    }

    pub fn show_error_message_and_all_loc(&self, globals: &Globals) {
        eprintln!("{}", self.get_error_message(globals));
        self.show_all_loc();
    }

    pub fn show_error_message_and_loc(&self, globals: &Globals) {
        eprintln!("{}", self.get_error_message(globals));
        self.show_loc();
    }

    pub fn get_error_message(&self, globals: &Globals) -> String {
        match &self.kind {
            /*MonorubyErrKind::UndefinedLocal(ident) => {
                format!("undefined local variable or method `{}'", ident)
            }*/
            MonorubyErrKind::MethodNotFound(name, obj) => {
                format!(
                    "undefined method `{}' for {}:{}",
                    IdentId::get_name(*name),
                    obj.to_s(globals),
                    obj.get_real_class_name(globals)
                )
            }
            MonorubyErrKind::Arguments(name) => name.to_string(),
            MonorubyErrKind::Syntax(kind) => match kind {
                ParseErrKind::SyntaxError(msg) => msg.to_string(),
                ParseErrKind::UnexpectedEOF => "unexpected end-of-file.".to_string(),
            },
            MonorubyErrKind::Syntax2(msg) => msg.to_string(),
            MonorubyErrKind::Unimplemented(msg) => msg.to_string(),
            MonorubyErrKind::UninitConst(name) => {
                format!("uninitialized constant {}", IdentId::get_name(*name))
            }
            MonorubyErrKind::DivideByZero => "divided by 0".to_string(),
            MonorubyErrKind::Range(msg) => msg.to_string(),
            MonorubyErrKind::Type(msg) => msg.to_string(),
            MonorubyErrKind::Index(msg) => msg.to_string(),
            MonorubyErrKind::Frozen(msg) => msg.to_string(),
            MonorubyErrKind::Load(msg) => msg.to_string(),
            MonorubyErrKind::Internal(msg) => msg.to_string(),
            MonorubyErrKind::Regex(msg) => msg.to_string(),
        }
    }
}

// Parser level errors.
impl MonorubyErr {
    pub(crate) fn parse(error: ParseErr) -> MonorubyErr {
        MonorubyErr::new_with_loc(
            MonorubyErrKind::Syntax(error.kind),
            error.loc,
            error.source_info,
        )
    }
}

// Bytecode compiler level errors.
impl MonorubyErr {
    pub(crate) fn unsupported_parameter_kind(
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

    pub(crate) fn unsupported_lhs(lhs: &Node, sourceinfo: SourceInfoRef) -> MonorubyErr {
        MonorubyErr::new_with_loc(
            MonorubyErrKind::Unimplemented(format!("unsupported lhs {:?}", lhs.kind)),
            lhs.loc,
            sourceinfo,
        )
    }

    pub(crate) fn unsupported_block_param(lhs: &Node, sourceinfo: SourceInfoRef) -> MonorubyErr {
        MonorubyErr::new_with_loc(
            MonorubyErrKind::Unimplemented(format!(
                "unsupported block parameter type {:?}",
                lhs.kind
            )),
            lhs.loc,
            sourceinfo,
        )
    }

    pub(crate) fn unsupported_node(expr: Node, sourceinfo: SourceInfoRef) -> MonorubyErr {
        MonorubyErr::new_with_loc(
            MonorubyErrKind::Unimplemented(format!("unsupported nodekind {:?}", expr.kind)),
            expr.loc,
            sourceinfo,
        )
    }

    pub(crate) fn unsupported_feature(
        msg: &str,
        loc: Loc,
        sourceinfo: SourceInfoRef,
    ) -> MonorubyErr {
        MonorubyErr::new_with_loc(
            MonorubyErrKind::Unimplemented(msg.to_string()),
            loc,
            sourceinfo,
        )
    }

    pub(crate) fn syntax(msg: String, loc: Loc, sourceinfo: SourceInfoRef) -> MonorubyErr {
        MonorubyErr::new_with_loc(
            MonorubyErrKind::Syntax(ParseErrKind::SyntaxError(msg)),
            loc,
            sourceinfo,
        )
    }

    pub(crate) fn escape_from_eval(loc: Loc, sourceinfo: SourceInfoRef) -> MonorubyErr {
        MonorubyErr::new_with_loc(
            MonorubyErrKind::Syntax2("can't escape from eval.".to_string()),
            loc,
            sourceinfo,
        )
    }
}

// Executor level errors.
impl MonorubyErr {
    pub(crate) fn method_not_found(name: IdentId, obj: Value) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::MethodNotFound(name, obj))
    }

    pub(crate) fn wrong_arguments(expected: usize, given: usize) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Arguments(format!(
            "wrong number of arguments (given {given}, expected {expected})"
        )))
    }

    pub(crate) fn bad_range(start: Value, end: Value) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Arguments(format!(
            "bad value for range. start:{:?} end:{:?}",
            start, end
        )))
    }

    pub(crate) fn divide_by_zero() -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::DivideByZero)
    }

    pub(crate) fn range(msg: String) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Range(msg))
    }

    pub(crate) fn uninitialized_constant(name: IdentId) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::UninitConst(name))
    }

    pub(crate) fn typeerr(msg: String) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Type(msg))
    }

    pub(crate) fn argumenterr(msg: String) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Arguments(msg))
    }

    pub(crate) fn indexerr(msg: String) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Index(msg))
    }

    pub(crate) fn frozenerr(msg: String) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Frozen(msg))
    }

    pub(crate) fn loaderr(msg: String) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Load(msg))
    }

    pub(crate) fn internalerr(msg: String) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Internal(msg))
    }

    pub(crate) fn regexerr(msg: String) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Regex(msg))
    }
}

#[cfg(test)]
mod test {
    use crate::tests::*;

    #[test]
    fn error() {
        run_test_error(
            r##"
        A=3
        class A
        end
        "##,
        );
        run_test_error(
            r##"
        A=3
        class C < A
        end
        "##,
        );
        run_test_error(
            r##"
        class S1; end
        class S2; end
        class C < S1; end
        class C < S2; end
        end
        "##,
        );
        run_test_error(
            r##"
        attr_accessor 2
        "##,
        );
        run_test_error(
            r##"
        require 2
        "##,
        );
        run_test_error(
            r##"
        Math.sqrt("e")
        "##,
        );
        run_test_error(
            r##"
        require "ffff"
        "##,
        );
    }
}
