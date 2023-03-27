use super::*;

//
// error handlers
//
impl Globals {
    pub fn set_error(&mut self, err: MonorubyErr) {
        self.error = Some(err);
    }

    pub(crate) fn err_method_not_found(&mut self, name: IdentId, obj: Value) {
        self.set_error(MonorubyErr::method_not_found(self, name, obj))
    }

    pub(crate) fn err_private_method_called(&mut self, name: IdentId, obj: Value) {
        self.set_error(MonorubyErr::private_method_called(self, name, obj))
    }

    /*pub(crate) fn err_protected_method_called(&mut self, name: IdentId, obj: Value) {
        self.set_error(MonorubyErr::method_not_found(format!(
            "protected method `{}' called for {}:{}",
            IdentId::get_name(name),
            obj.to_s(self),
            obj.get_real_class_name(self)
        )))
    }*/

    pub(crate) fn err_divide_by_zero(&mut self) {
        self.set_error(MonorubyErr::divide_by_zero());
    }

    pub(crate) fn err_no_block_given(&mut self) {
        self.set_error(MonorubyErr::no_block_given());
    }

    pub(crate) fn check_number_of_arguments(
        given: usize,
        range: std::ops::RangeInclusive<usize>,
    ) -> Result<()> {
        if range.contains(&given) {
            Ok(())
        } else {
            let err = if range.start() == range.end() {
                MonorubyErr::wrong_arguments(*range.start(), given)
            } else {
                MonorubyErr::wrong_number_of_arg_range(given, range)
            };
            Err(err)
        }
    }

    pub(crate) fn err_wrong_number_of_arg_range(
        &mut self,
        given: usize,
        range: std::ops::RangeInclusive<usize>,
    ) {
        self.set_error(MonorubyErr::wrong_number_of_arg_range(given, range))
    }

    pub(crate) fn check_min_number_of_arguments(given: usize, min: usize) -> Result<()> {
        if given >= min {
            return Ok(());
        }
        Err(MonorubyErr::wrong_number_of_arg_min(given, min))
    }

    pub(crate) fn err_internal(&mut self, msg: String) {
        self.set_error(MonorubyErr::internalerr(msg));
    }

    ///
    /// Set FrozenError with message "can't modify frozen Integer: 5".
    ///
    pub(crate) fn err_cant_modify_frozen(&mut self, val: Value) {
        self.set_error(MonorubyErr::cant_modify_frozen(self, val));
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
    NotMethod(String),
    Arguments(String),
    Syntax(ParseErrKind),
    Syntax2(String),
    Unimplemented(String),
    UninitConst(String),
    DivideByZero,
    LocalJump(String),
    Range(String),
    Type(String),
    Index(String),
    Frozen(String),
    Load(String),
    Internal(String),
    Regex(String),
    Runtime(String),
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

    pub fn show_error_message_and_all_loc(&self) {
        eprintln!("{}", self.get_error_message());
        self.show_all_loc();
    }

    pub fn show_error_message_and_loc(&self) {
        eprintln!("{}", self.get_error_message());
        self.show_loc();
    }

    pub fn get_error_message(&self) -> String {
        match &self.kind {
            /*MonorubyErrKind::UndefinedLocal(ident) => {
                format!("undefined local variable or method `{}'", ident)
            }*/
            MonorubyErrKind::NotMethod(msg) => format!("{msg} (NoMethodError)"),
            MonorubyErrKind::Arguments(msg) => format!("{msg} (ArgumentError)"),
            MonorubyErrKind::Syntax(kind) => match kind {
                ParseErrKind::SyntaxError(msg) => msg.to_string(),
                ParseErrKind::UnexpectedEOF => "unexpected end-of-file.".to_string(),
            },
            MonorubyErrKind::Syntax2(msg) => msg.to_string(),
            MonorubyErrKind::Unimplemented(msg) => msg.to_string(),
            MonorubyErrKind::UninitConst(msg) => msg.to_string(),
            MonorubyErrKind::DivideByZero => "divided by 0 (ZeroDivisionError)".to_string(),
            MonorubyErrKind::LocalJump(msg) => format!("{msg} (LocalJumpError)"),
            MonorubyErrKind::Range(msg) => format!("{msg} (RangeError)"),
            MonorubyErrKind::Type(msg) => format!("{msg} (TypeError)"),
            MonorubyErrKind::Index(msg) => format!("{msg} (IndexError)"),
            MonorubyErrKind::Frozen(msg) => format!("{msg} (FrozenError)"),
            MonorubyErrKind::Load(msg) => format!("{msg} (LoadError)"),
            MonorubyErrKind::Internal(msg) => msg.to_string(),
            MonorubyErrKind::Regex(msg) => format!("{msg} (RegexError)"),
            MonorubyErrKind::Runtime(msg) => format!("{msg} (RuntimeError)"),
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
            MonorubyErrKind::Unimplemented(format!("unsupported parameter kind {param:?}")),
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

    pub(crate) fn cant_set_variable(id: usize, loc: Loc, sourceinfo: SourceInfoRef) -> MonorubyErr {
        // 0 => $&
        // 1 => $'
        // 100 + n => $n
        Self::syntax(
            format!(
                "can't set variable ${}.",
                match id {
                    0 => "&".to_string(),
                    1 => "'".to_string(),
                    n => (n - 100).to_string(),
                }
            ),
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
    pub(crate) fn method_not_found(globals: &Globals, name: IdentId, obj: Value) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::NotMethod(format!(
            "undefined method `{}' for {}:{}",
            IdentId::get_name(name),
            obj.to_s(globals),
            obj.get_real_class_name(globals)
        )))
    }

    pub(crate) fn method_not_found_for_class(
        globals: &Globals,
        name: IdentId,
        class: ClassId,
    ) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::NotMethod(format!(
            "undefined method `{}' for {}",
            IdentId::get_name(name),
            class.get_name(globals)
        )))
    }

    pub(crate) fn private_method_called(
        globals: &Globals,
        name: IdentId,
        obj: Value,
    ) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::NotMethod(format!(
            "private method `{}' called for {}:{}",
            IdentId::get_name(name),
            obj.to_s(globals),
            obj.get_real_class_name(globals)
        )))
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

    pub(crate) fn no_block_given() -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::LocalJump(
            "no block given (yield).".to_string(),
        ))
    }

    /*pub(crate) fn range(msg: String) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Range(msg))
    }*/

    pub(crate) fn char_out_of_range(globals: &Globals, val: Value) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Range(format!(
            "{} out of char range",
            globals.val_tos(val)
        )))
    }

    pub(crate) fn uninitialized_constant(name: IdentId) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::UninitConst(format!(
            "uninitialized constant {}",
            IdentId::get_name(name)
        )))
    }

    pub(crate) fn typeerr(msg: String) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Type(msg))
    }

    pub(crate) fn no_implicit_conversion(
        globals: &Globals,
        val: Value,
        target_class: ClassId,
    ) -> MonorubyErr {
        MonorubyErr::typeerr(format!(
            "no implicit conversion of {} into {}",
            val.get_real_class_name(globals),
            target_class.get_name(globals),
        ))
    }

    pub(crate) fn is_not_class_nor_module(name: String) -> MonorubyErr {
        MonorubyErr::typeerr(format!("{name} is not a class nor a module",))
    }

    pub(crate) fn is_not_class(name: String) -> MonorubyErr {
        MonorubyErr::typeerr(format!("{name} is not a class"))
    }

    pub(crate) fn superclass_mismatch(name: IdentId) -> MonorubyErr {
        MonorubyErr::typeerr(format!(
            "superclass mismatch for class {}",
            IdentId::get_name(name)
        ))
    }

    ///
    /// Set TypeError with message "*name* is not Symbol nor String".
    ///
    pub(crate) fn is_not_symbol_nor_string(globals: &Globals, val: Value) -> MonorubyErr {
        MonorubyErr::typeerr(format!(
            "{} is not a symbol nor a string",
            globals.val_tos(val)
        ))
    }

    ///
    /// Set TypeError with message "*name* is not Regexp nor String".
    ///
    pub(crate) fn is_not_regexp_nor_string(globals: &Globals, val: Value) -> MonorubyErr {
        MonorubyErr::typeerr(format!(
            "{} is not a regexp nor a string",
            globals.val_tos(val)
        ))
    }

    ///
    /// Set TypeError with message "can't convert *class of val* into Float".
    ///
    pub(crate) fn cant_convert_into_float(globals: &Globals, val: Value) -> MonorubyErr {
        MonorubyErr::typeerr(format!(
            "can't convert {} into Float",
            val.get_real_class_name(globals)
        ))
    }

    pub(crate) fn argumenterr(msg: String) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Arguments(msg))
    }

    pub(crate) fn zero_width_padding() -> MonorubyErr {
        MonorubyErr::argumenterr("zero width padding".to_string())
    }

    pub(crate) fn negative_argument() -> MonorubyErr {
        MonorubyErr::argumenterr("negative_argument".to_string())
    }

    pub(crate) fn create_proc_no_block() -> MonorubyErr {
        MonorubyErr::argumenterr("tried to create Proc object without a block".to_string())
    }

    pub(crate) fn wrong_number_of_arg_range(
        given: usize,
        range: std::ops::RangeInclusive<usize>,
    ) -> MonorubyErr {
        Self::argumenterr(format!(
            "wrong number of arguments (given {given}, expeted {range:?})"
        ))
    }

    pub(crate) fn wrong_number_of_arg_min(given: usize, min: usize) -> MonorubyErr {
        Self::argumenterr(format!(
            "wrong number of arguments (given {given}, expeted {min}+)"
        ))
    }

    pub(crate) fn indexerr(msg: String) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Index(msg))
    }

    pub(crate) fn index_too_small(actual: i64, minimum: i64) -> MonorubyErr {
        MonorubyErr::indexerr(format!(
            "index {} too small for array; minimum: {}",
            actual, minimum,
        ))
    }

    pub(crate) fn frozenerr(msg: String) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Frozen(msg))
    }

    pub(crate) fn cant_modify_frozen(globals: &Globals, val: Value) -> MonorubyErr {
        MonorubyErr::frozenerr(format!(
            "can't modify frozen {}: {}",
            val.get_real_class_name(globals),
            globals.val_tos(val),
        ))
    }

    pub(crate) fn loaderr(msg: String) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Load(msg))
    }

    pub(crate) fn cant_load(err: Option<std::io::Error>, path: &std::path::Path) -> MonorubyErr {
        MonorubyErr::loaderr(match err {
            Some(err) => format!("can't load {path:?}. {err}"),
            None => format!("can't load {path:?}"),
        })
    }

    pub(crate) fn internalerr(msg: String) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Internal(msg))
    }

    pub(crate) fn regexerr(msg: String) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Regex(msg))
    }

    pub(crate) fn runtimeerr(msg: String) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Runtime(msg))
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
