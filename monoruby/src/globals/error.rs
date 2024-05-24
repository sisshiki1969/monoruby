use ruruby_parse::{Loc, ParamKind, ParseErr, ParseErrKind, SourceInfoRef};

use super::*;

///
/// Exception information which is stored in *Executor*.
///
#[derive(Debug, Clone, PartialEq)]
pub struct MonorubyErr {
    kind: MonorubyErrKind,
    msg: String,
    trace: Vec<(Loc, SourceInfoRef)>,
}

impl MonorubyErr {
    pub fn push_trace(&mut self, loc: Loc, sourceinfo: SourceInfoRef) {
        self.trace.push((loc, sourceinfo));
    }
}

impl MonorubyErr {
    pub fn new(kind: MonorubyErrKind, msg: impl ToString) -> Self {
        MonorubyErr {
            kind,
            msg: msg.to_string(),
            trace: vec![],
        }
    }

    pub fn new_from_exception(ex: &ExceptionInner) -> Self {
        let kind = ex.kind().clone();
        let msg = ex.msg().to_string();
        let trace = ex.trace();
        MonorubyErr { kind, msg, trace }
    }

    fn new_with_loc(
        kind: MonorubyErrKind,
        msg: String,
        loc: Loc,
        sourceinfo: SourceInfoRef,
    ) -> Self {
        MonorubyErr {
            kind,
            msg,
            trace: vec![(loc, sourceinfo)],
        }
    }

    pub fn kind(&self) -> &MonorubyErrKind {
        &self.kind
    }

    pub fn msg(&self) -> &str {
        &self.msg
    }

    pub fn set_msg(&mut self, msg: String) {
        self.msg = msg;
    }

    pub fn trace(&self) -> &[(Loc, SourceInfoRef)] {
        &self.trace
    }

    pub fn take_trace(&mut self) -> Vec<(Loc, SourceInfoRef)> {
        std::mem::take(&mut self.trace)
    }

    pub fn show_all_loc(&self) {
        for (loc, sourceinfo) in &self.trace {
            sourceinfo.show_loc(loc);
        }
    }

    fn show_loc(&self) {
        if let Some((loc, sourceinfo)) = self.trace.first() {
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
        format!("{} ({})", self.show(globals), self.get_class_name())
    }

    pub fn show(&self, globals: &Globals) -> String {
        format!("{}{}", self.msg, self.kind.show(globals),)
    }

    pub fn get_class_name(&self) -> &str {
        match &self.kind {
            MonorubyErrKind::Exception => "Exception",
            MonorubyErrKind::NotMethod(_) => "NoMethodError",
            MonorubyErrKind::Arguments => "ArgumentError",
            MonorubyErrKind::Syntax => "SyntaxError",
            MonorubyErrKind::Unimplemented => "RuntimeError",
            MonorubyErrKind::Name => "NameError",
            MonorubyErrKind::DivideByZero => "ZeroDivisionError",
            MonorubyErrKind::LocalJump => "LocalJumpError",
            MonorubyErrKind::Range => "RangeError",
            MonorubyErrKind::Type(_) => "TypeError",
            MonorubyErrKind::Index => "IndexError",
            MonorubyErrKind::Frozen => "FrozenError",
            MonorubyErrKind::Load => "LoadError",
            MonorubyErrKind::Internal => "InternalError",
            MonorubyErrKind::Regex => "RegexError",
            MonorubyErrKind::Runtime => "RuntimeError",
            MonorubyErrKind::Key => "KeyError",
            MonorubyErrKind::Fiber => "FiberError",
            MonorubyErrKind::StopIteration => "StopIteration",
            MonorubyErrKind::SystemExit(..) => "SystemExit",
            MonorubyErrKind::MethodReturn(..) => unreachable!(),
        }
    }
}

// Parser level errors.
impl MonorubyErr {
    pub fn parse(error: ParseErr) -> MonorubyErr {
        let msg = match error.kind {
            ParseErrKind::SyntaxError(msg) => msg.to_string(),
            ParseErrKind::UnexpectedEOF => "unexpected end-of-file.".to_string(),
        };
        MonorubyErr::new_with_loc(MonorubyErrKind::Syntax, msg, error.loc, error.source_info)
    }

    pub fn is_unexpected_eof(&self) -> bool {
        self.kind == MonorubyErrKind::Syntax && self.msg == "unexpected end-of-file."
    }
}

// Bytecodegen level errors.
impl MonorubyErr {
    pub(crate) fn unsupported_parameter_kind(
        param: ParamKind,
        loc: Loc,
        sourceinfo: SourceInfoRef,
    ) -> MonorubyErr {
        let msg = format!("unsupported parameter kind {param:?}");
        eprintln!(
            "{}",
            ansi_term::Colour::Red.paint(format!("warning: {msg}"))
        );
        MonorubyErr::new_with_loc(MonorubyErrKind::Unimplemented, msg, loc, sourceinfo)
    }

    pub(crate) fn unsupported_lhs(lhs: &Node, sourceinfo: SourceInfoRef) -> MonorubyErr {
        let msg = format!("unsupported lhs {:?}", lhs.kind);
        eprintln!(
            "{}",
            ansi_term::Colour::Red.paint(format!("warning: {msg}"))
        );
        MonorubyErr::new_with_loc(MonorubyErrKind::Unimplemented, msg, lhs.loc, sourceinfo)
    }

    pub(crate) fn unsupported_node(expr: Node, sourceinfo: SourceInfoRef) -> MonorubyErr {
        let msg = format!("unsupported nodekind {:?}", expr.kind);
        eprintln!(
            "{}",
            ansi_term::Colour::Red.paint(format!("warning: {msg}"))
        );
        MonorubyErr::new_with_loc(MonorubyErrKind::Unimplemented, msg, expr.loc, sourceinfo)
    }

    pub(crate) fn unsupported_feature(
        msg: &str,
        loc: Loc,
        sourceinfo: SourceInfoRef,
    ) -> MonorubyErr {
        let msg = msg.to_string();
        eprintln!(
            "{}",
            ansi_term::Colour::Red.paint(format!("warning: {msg}"))
        );
        MonorubyErr::new_with_loc(MonorubyErrKind::Unimplemented, msg, loc, sourceinfo)
    }

    pub(crate) fn syntax(msg: String, loc: Loc, sourceinfo: SourceInfoRef) -> MonorubyErr {
        MonorubyErr::new_with_loc(MonorubyErrKind::Syntax, msg, loc, sourceinfo)
    }

    pub(crate) fn cant_set_variable(id: u32, loc: Loc, sourceinfo: SourceInfoRef) -> MonorubyErr {
        // 0 => $&
        // 1 => $'
        // 100 + n => $n
        Self::syntax(
            format!(
                "can't set variable ${}.",
                match id {
                    ruruby_parse::SPECIAL_LASTMATCH => "&".to_string(),
                    ruruby_parse::SPECIAL_POSTMATCH => "'".to_string(),
                    ruruby_parse::SPECIAL_LOADPATH => "LOAD_PATH".to_string(),
                    ruruby_parse::SPECIAL_LOADEDFEATURES => "LOADED_FEATURES".to_string(),
                    n => (n - 100).to_string(),
                }
            ),
            loc,
            sourceinfo,
        )
    }

    pub(crate) fn escape_from_eval(loc: Loc, sourceinfo: SourceInfoRef) -> MonorubyErr {
        MonorubyErr::new_with_loc(
            MonorubyErrKind::Syntax,
            "can't escape from eval.".to_string(),
            loc,
            sourceinfo,
        )
    }
}

// Executor level errors.
impl MonorubyErr {
    pub(crate) fn method_return(val: Value, target_lfp: Lfp) -> MonorubyErr {
        MonorubyErr::new(
            MonorubyErrKind::MethodReturn(val, target_lfp),
            String::new(),
        )
    }

    pub(crate) fn method_not_found(name: IdentId, obj: Value) -> MonorubyErr {
        MonorubyErr::new(
            MonorubyErrKind::NotMethod(NoMethodErrKind::MethodNotFound { name, obj }),
            "",
        )
    }

    pub(crate) fn method_not_found_for_class(name: IdentId, class: ClassId) -> MonorubyErr {
        MonorubyErr::new(
            MonorubyErrKind::NotMethod(NoMethodErrKind::MethodNotFoundForClass { name, class }),
            "",
        )
    }

    pub(crate) fn private_method_called(name: IdentId, obj: Value) -> MonorubyErr {
        MonorubyErr::new(
            MonorubyErrKind::NotMethod(NoMethodErrKind::PrivateMethodCalled { name, obj }),
            "",
        )
    }

    pub fn wrong_number_of_arg(expected: usize, given: usize) -> MonorubyErr {
        Self::argumenterr(format!(
            "wrong number of arguments (given {given}, expected {expected})"
        ))
    }

    pub fn wrong_number_of_arg_range(
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

    pub(crate) fn bad_range(start: Value, end: Value) -> MonorubyErr {
        MonorubyErr::new(
            MonorubyErrKind::Arguments,
            format!("bad value for range. start:{:?} end:{:?}", start, end),
        )
    }

    pub(crate) fn divide_by_zero() -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::DivideByZero, "divided by 0".to_string())
    }

    pub(crate) fn no_block_given() -> MonorubyErr {
        MonorubyErr::new(
            MonorubyErrKind::LocalJump,
            "no block given (yield).".to_string(),
        )
    }

    pub(crate) fn fibererr(msg: String) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Fiber, msg)
    }

    pub(crate) fn char_out_of_range(globals: &Globals, val: Value) -> MonorubyErr {
        MonorubyErr::new(
            MonorubyErrKind::Range,
            format!("{} out of char range", val.to_s(globals)),
        )
    }

    pub(crate) fn uninitialized_constant(name: IdentId) -> MonorubyErr {
        MonorubyErr::new(
            MonorubyErrKind::Name,
            format!("uninitialized constant {name}"),
        )
    }

    pub(crate) fn uninitialized_cvar(name: IdentId, class_name: IdentId) -> MonorubyErr {
        MonorubyErr::new(
            MonorubyErrKind::Name,
            format!("uninitialized class variable {name} in {class_name}"),
        )
    }

    pub(crate) fn identifier_must_be_constant(name: &str) -> MonorubyErr {
        MonorubyErr::new(
            MonorubyErrKind::Name,
            format!("identifier {name} needs to be constant"),
        )
    }

    pub(crate) fn typeerr(msg: impl ToString, kind: TypeErrKind) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Type(kind), msg)
    }

    pub(crate) fn no_implicit_conversion(val: Value, target_class: ClassId) -> MonorubyErr {
        MonorubyErr::typeerr("", TypeErrKind::NoImpricitConversion { val, target_class })
    }

    pub(crate) fn is_not_class_nor_module(name: String) -> MonorubyErr {
        MonorubyErr::typeerr(
            format!("{name} is not a class nor a module"),
            TypeErrKind::Other,
        )
    }

    /*pub(crate) fn is_not_class_nor_module_rescue() -> MonorubyErr {
        MonorubyErr::typeerr(format!("class or module required for rescue clause",))
    }*/

    pub(crate) fn is_not_class(name: String) -> MonorubyErr {
        MonorubyErr::typeerr(format!("{name} is not a class"), TypeErrKind::Other)
    }

    pub(crate) fn superclass_mismatch(name: IdentId) -> MonorubyErr {
        MonorubyErr::typeerr(
            format!("superclass mismatch for class {name}"),
            TypeErrKind::Other,
        )
    }

    ///
    /// Set TypeError with message "*name* is not Symbol nor String".
    ///
    pub(crate) fn is_not_symbol_nor_string(val: Value) -> MonorubyErr {
        MonorubyErr::typeerr("", TypeErrKind::NotSymbolNorString { val })
    }

    ///
    /// Set TypeError with message "*name* is not Regexp nor String".
    ///
    pub(crate) fn is_not_regexp_nor_string(val: Value) -> MonorubyErr {
        MonorubyErr::typeerr("", TypeErrKind::NotRegexpNorString { val })
    }

    ///
    /// Set TypeError with message "can't convert *class of val* into Float".
    ///
    pub(crate) fn cant_convert_into_float(val: Value) -> MonorubyErr {
        MonorubyErr::typeerr("", TypeErrKind::CantConverFloat { val })
    }

    ///
    /// Set TypeError with message "{op}: *class of val* can't be coerced into {`msg`}".
    ///
    pub(crate) fn cant_coerced_into(op: IdentId, val: Value, msg: &'static str) -> MonorubyErr {
        MonorubyErr::typeerr("", TypeErrKind::CantCoerced { op, val, msg })
    }

    pub(crate) fn argumenterr(msg: impl ToString) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Arguments, msg)
    }

    pub(crate) fn zero_width_padding() -> MonorubyErr {
        MonorubyErr::argumenterr("zero width padding")
    }

    pub(crate) fn negative_argument() -> MonorubyErr {
        MonorubyErr::argumenterr("negative argument")
    }

    pub(crate) fn negative_array_size() -> MonorubyErr {
        MonorubyErr::argumenterr("negative array size")
    }

    pub(crate) fn create_proc_no_block() -> MonorubyErr {
        MonorubyErr::argumenterr("tried to create Proc object without a block")
    }

    pub(crate) fn indexerr(msg: impl ToString) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Index, msg)
    }

    pub(crate) fn keyerr(msg: String) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Key, msg)
    }

    pub(crate) fn stopiterationerr(msg: String) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::StopIteration, msg)
    }

    pub(crate) fn index_too_small(actual: i64, minimum: i64) -> MonorubyErr {
        MonorubyErr::indexerr(format!(
            "index {} too small for array; minimum: {}",
            actual, minimum,
        ))
    }

    pub(crate) fn frozenerr(msg: impl ToString) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Frozen, msg)
    }

    pub(crate) fn cant_modify_frozen(globals: &Globals, val: Value) -> MonorubyErr {
        MonorubyErr::frozenerr(format!(
            "can't modify frozen {}: {}",
            val.get_real_class_name(globals),
            val.to_s(globals),
        ))
    }

    pub(crate) fn loaderr(msg: impl ToString) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Load, msg)
    }

    pub(crate) fn cant_load(err: Option<std::io::Error>, path: &std::path::Path) -> MonorubyErr {
        MonorubyErr::loaderr(match err {
            Some(err) => format!("can't load {path:?}. {err}"),
            None => format!("can't load {path:?}"),
        })
    }

    pub(crate) fn internalerr(msg: impl ToString) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Internal, msg)
    }

    pub(crate) fn regexerr(msg: impl ToString) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Regex, msg)
    }

    pub(crate) fn runtimeerr(msg: impl ToString) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Runtime, msg)
    }

    pub(crate) fn rangeerr(msg: impl ToString) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Range, msg)
    }

    pub(crate) fn float_out_of_range_of_integer(f: f64) -> MonorubyErr {
        MonorubyErr::rangeerr(format!("float {:?} out of range of integer", f))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum MonorubyErrKind {
    MethodReturn(Value, Lfp),
    NotMethod(NoMethodErrKind),
    Arguments,
    Syntax,
    Unimplemented,
    Name,
    DivideByZero,
    LocalJump,
    Range,
    Type(TypeErrKind),
    Index,
    Frozen,
    Load,
    Internal,
    Regex,
    Runtime,
    Key,
    Fiber,
    StopIteration,
    Exception,
    SystemExit(u8),
}

impl MonorubyErrKind {
    pub fn show(&self, globals: &Globals) -> String {
        match self {
            MonorubyErrKind::Type(kind) => kind.show(globals),
            MonorubyErrKind::NotMethod(kind) => kind.show(globals),
            _ => String::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum NoMethodErrKind {
    MethodNotFound { name: IdentId, obj: Value },
    MethodNotFoundForClass { name: IdentId, class: ClassId },
    PrivateMethodCalled { name: IdentId, obj: Value },
}

impl NoMethodErrKind {
    pub fn show(&self, globals: &Globals) -> String {
        match self {
            NoMethodErrKind::MethodNotFound { name, obj } => format!(
                "undefined method `{name}' for {}:{}",
                obj.inspect(globals),
                obj.get_real_class_name(globals)
            ),
            NoMethodErrKind::MethodNotFoundForClass { name, class } => format!(
                "undefined method `{name}' for {}",
                globals.get_class_name(*class)
            ),
            NoMethodErrKind::PrivateMethodCalled { name, obj } => format!(
                "private method `{name}' called for {}:{}",
                obj.inspect(globals),
                obj.get_real_class_name(globals)
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeErrKind {
    NoImpricitConversion {
        val: Value,
        target_class: ClassId,
    },
    NotSymbolNorString {
        val: Value,
    },
    NotRegexpNorString {
        val: Value,
    },
    CantConverFloat {
        val: Value,
    },
    CantCoerced {
        op: IdentId,
        val: Value,
        msg: &'static str,
    },
    WrongArgumentType {
        val: Value,
        expected: &'static str,
    },
    Other,
}

impl TypeErrKind {
    pub fn show(&self, globals: &Globals) -> String {
        match self {
            TypeErrKind::NoImpricitConversion { val, target_class } => format!(
                "no implicit conversion of {} into {}",
                val.get_real_class_name(globals),
                globals.get_class_name(*target_class)
            ),
            TypeErrKind::NotSymbolNorString { val } => {
                format!("{} is not a symbol nor a string", val.to_s(globals))
            }
            TypeErrKind::NotRegexpNorString { val } => {
                format!("{} is not a regexp nor a string", val.to_s(globals))
            }
            TypeErrKind::CantConverFloat { val } => {
                format!(
                    "can't convert {} into Float",
                    val.get_real_class_name(globals)
                )
            }
            TypeErrKind::CantCoerced { op, val, msg } => {
                format!(
                    "{op}: {} can't be coerced into {msg}",
                    val.get_real_class_name(globals)
                )
            }
            TypeErrKind::WrongArgumentType { val, expected } => {
                format!(
                    "wrong argument type {} (expected {expected})",
                    val.get_real_class_name(globals)
                )
            }
            TypeErrKind::Other => "".to_string(),
        }
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
        class S; end
        class C < S
            def f
                super
            end
        end
        C.new.f
        "##,
        );
        run_test_error(
            r##"
        class C
            def f
                yield
            end
        end
        C.new.f
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
