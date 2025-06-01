use ruruby_parse::{Loc, ParseErr, ParseErrKind, SourceInfoRef};

use super::*;

///
/// Exception information which is stored in *Executor*.
///
#[derive(Debug, Clone, PartialEq)]
pub struct MonorubyErr {
    kind: MonorubyErrKind,
    message: String,
    trace: Vec<(Option<(Loc, SourceInfoRef)>, Option<FuncId>)>,
}

impl MonorubyErr {
    pub fn push_trace(&mut self, loc: Loc, sourceinfo: SourceInfoRef, fid: FuncId) {
        self.trace.push((Some((loc, sourceinfo)), Some(fid)));
    }

    pub fn push_internal_trace(&mut self, fid: FuncId) {
        self.trace.push((None, Some(fid)));
    }
}

impl MonorubyErr {
    pub fn new(kind: MonorubyErrKind, msg: impl ToString) -> Self {
        MonorubyErr {
            kind,
            message: msg.to_string(),
            trace: vec![],
        }
    }

    pub fn new_from_exception(ex: &ExceptionInner) -> Self {
        let kind = ex.kind().clone();
        let msg = ex.message().to_string();
        let trace = ex.trace().to_vec();
        MonorubyErr {
            kind,
            message: msg,
            trace,
        }
    }

    fn new_with_loc(
        kind: MonorubyErrKind,
        msg: String,
        loc: Loc,
        sourceinfo: SourceInfoRef,
        func_id: impl Into<Option<FuncId>>,
    ) -> Self {
        let func_id = func_id.into();
        MonorubyErr {
            kind,
            message: msg,
            trace: vec![(Some((loc, sourceinfo)), func_id)],
        }
    }

    pub fn kind(&self) -> &MonorubyErrKind {
        &self.kind
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn set_msg(&mut self, msg: String) {
        self.message = msg;
    }

    pub fn trace(&self) -> &[(Option<(Loc, SourceInfoRef)>, Option<FuncId>)] {
        &self.trace
    }

    pub fn take_trace(&mut self) -> Vec<(Option<(Loc, SourceInfoRef)>, Option<FuncId>)> {
        std::mem::take(&mut self.trace)
    }

    pub fn show_error_message_and_all_loc(&self, store: &Store) {
        let mut loc_flag = false;
        if let Some((source_loc, func_id)) = self.trace.first() {
            let error_message = self.get_error_message(store);
            if let Some((loc, source)) = source_loc {
                eprintln!(
                    "{}: {error_message}",
                    store.location(func_id.clone(), source, *loc),
                );
                source.show_loc(loc);
                loc_flag = true;
            } else if let Some(func_id) = func_id {
                eprintln!("{}: {error_message}", store.internal_location(*func_id),);
            } else {
                eprintln!("<internal>: {error_message}");
            }
        } else {
            eprintln!("location not defined.");
        }
        if self.trace.len() > 1 {
            for (source_loc, func_id) in &self.trace[1..] {
                if let Some((loc, source)) = source_loc {
                    eprintln!(
                        "        from: {}",
                        store.location(func_id.clone(), source, *loc)
                    );
                    if !loc_flag {
                        source.show_loc(loc);
                        loc_flag = true;
                    }
                } else {
                    eprintln!("        {}", store.internal_location(func_id.unwrap()))
                }
            }
        }
    }

    pub fn get_error_message(&self, store: &Store) -> String {
        format!("{} ({})", self.message, self.class_name(store))
    }

    pub fn class_name(&self, store: &Store) -> String {
        match &self.kind {
            MonorubyErrKind::Exception => "Exception",
            MonorubyErrKind::NotMethod => "NoMethodError",
            MonorubyErrKind::Arguments => "ArgumentError",
            MonorubyErrKind::Syntax => "SyntaxError",
            MonorubyErrKind::Unimplemented => "RuntimeError",
            MonorubyErrKind::Name => "NameError",
            MonorubyErrKind::DivideByZero => "ZeroDivisionError",
            MonorubyErrKind::LocalJump => "LocalJumpError",
            MonorubyErrKind::Range => "RangeError",
            MonorubyErrKind::Type => "TypeError",
            MonorubyErrKind::Index => "IndexError",
            MonorubyErrKind::Frozen => "FrozenError",
            MonorubyErrKind::Load(_) => "LoadError",
            MonorubyErrKind::Internal => "InternalError",
            MonorubyErrKind::Regex => "RegexpError",
            MonorubyErrKind::Runtime => "RuntimeError",
            MonorubyErrKind::Key => "KeyError",
            MonorubyErrKind::Fiber => "FiberError",
            MonorubyErrKind::StopIteration => "StopIteration",
            MonorubyErrKind::SystemExit(..) => "SystemExit",
            MonorubyErrKind::Other(class_id) => return class_id.get_name(store),
            MonorubyErrKind::MethodReturn(..) => unreachable!(),
        }
        .to_string()
    }

    pub fn class_id(&self) -> ClassId {
        match &self.kind {
            MonorubyErrKind::Exception => EXCEPTION_CLASS,
            MonorubyErrKind::NotMethod => NO_METHOD_ERROR_CLASS,
            MonorubyErrKind::Arguments => ARGUMENTS_ERROR_CLASS,
            MonorubyErrKind::Syntax => SYNTAX_ERROR_CLASS,
            MonorubyErrKind::Unimplemented => UNIMPLEMENTED_ERROR_CLASS,
            MonorubyErrKind::Name => NAME_ERROR_CLASS,
            MonorubyErrKind::DivideByZero => ZERO_DIVISION_ERROR_CLASS,
            MonorubyErrKind::LocalJump => LOCAL_JUMP_ERROR_CLASS,
            MonorubyErrKind::Range => RANGE_ERROR_CLASS,
            MonorubyErrKind::Type => TYPE_ERROR_CLASS,
            MonorubyErrKind::Index => INDEX_ERROR_CLASS,
            MonorubyErrKind::Frozen => FROZEN_ERROR_CLASS,
            MonorubyErrKind::Load(_) => LOAD_ERROR_CLASS,
            MonorubyErrKind::Internal => INTERNAL_ERROR_CLASS,
            MonorubyErrKind::Regex => REGEX_ERROR_CLASS,
            MonorubyErrKind::Runtime => RUNTIME_ERROR_CLASS,
            MonorubyErrKind::Key => KEY_ERROR_CLASS,
            MonorubyErrKind::Fiber => FIBER_ERROR_CLASS,
            MonorubyErrKind::StopIteration => STOP_ITERATION_CLASS,
            MonorubyErrKind::SystemExit(..) => SYSTEM_EXIT_ERROR_CLASS,
            MonorubyErrKind::Other(class_id) => *class_id,
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
        MonorubyErr::new_with_loc(
            MonorubyErrKind::Syntax,
            msg,
            error.loc,
            error.source_info,
            None,
        )
    }

    pub fn is_unexpected_eof(&self) -> bool {
        self.kind == MonorubyErrKind::Syntax && self.message == "unexpected end-of-file."
    }
}

// Bytecodegen level errors.
impl MonorubyErr {
    pub(crate) fn unsupported_lhs(
        lhs: &Node,
        sourceinfo: SourceInfoRef,
        func_id: FuncId,
    ) -> MonorubyErr {
        let msg = format!("unsupported lhs {:?}", lhs.kind);
        MonorubyErr::new_with_loc(
            MonorubyErrKind::Unimplemented,
            msg,
            lhs.loc,
            sourceinfo,
            func_id,
        )
    }

    pub(crate) fn unsupported_node(
        expr: &Node,
        sourceinfo: SourceInfoRef,
        func_id: FuncId,
    ) -> MonorubyErr {
        let msg = format!("unsupported nodekind {:?}", expr.kind);
        MonorubyErr::new_with_loc(
            MonorubyErrKind::Unimplemented,
            msg,
            expr.loc,
            sourceinfo,
            func_id,
        )
    }

    pub(crate) fn unsupported_feature(
        msg: &str,
        loc: Loc,
        sourceinfo: SourceInfoRef,
        func_id: FuncId,
    ) -> MonorubyErr {
        let msg = msg.to_string();
        MonorubyErr::new_with_loc(
            MonorubyErrKind::Unimplemented,
            msg,
            loc,
            sourceinfo,
            func_id,
        )
    }

    pub(crate) fn syntax(
        msg: String,
        loc: Loc,
        sourceinfo: SourceInfoRef,
        func_id: FuncId,
    ) -> MonorubyErr {
        MonorubyErr::new_with_loc(MonorubyErrKind::Syntax, msg, loc, sourceinfo, func_id)
    }

    pub(crate) fn escape_from_eval(
        msg: &str,
        loc: Loc,
        sourceinfo: SourceInfoRef,
        func_id: FuncId,
    ) -> MonorubyErr {
        MonorubyErr::new_with_loc(
            MonorubyErrKind::Syntax,
            format!("can't escape from eval with {}.", msg),
            loc,
            sourceinfo,
            func_id,
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

    pub(crate) fn method_not_found(store: &Store, name: IdentId, obj: Value) -> MonorubyErr {
        MonorubyErr::new(
            MonorubyErrKind::NotMethod,
            format!(
                "undefined method `{name}' for {}",
                obj.get_real_class_name(store)
            ),
        )
    }

    pub(crate) fn method_not_found_for_class(
        store: &Store,
        name: IdentId,
        class: ClassId,
    ) -> MonorubyErr {
        MonorubyErr::new(
            MonorubyErrKind::NotMethod,
            format!(
                "undefined method `{name}' for {}",
                store.get_class_name(class)
            ),
        )
    }

    pub(crate) fn private_method_called(store: &Store, name: IdentId, obj: Value) -> MonorubyErr {
        MonorubyErr::new(
            MonorubyErrKind::NotMethod,
            format!(
                "private method `{name}' called for {}:{}",
                obj.inspect(store),
                obj.get_real_class_name(store)
            ),
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

    pub(crate) fn char_out_of_range(store: &Store, val: Value) -> MonorubyErr {
        MonorubyErr::new(
            MonorubyErrKind::Range,
            format!("{} out of char range", val.to_s(store)),
        )
    }

    pub(crate) fn nameerr(msg: impl ToString) -> MonorubyErr {
        Self::new(MonorubyErrKind::Name, msg)
    }

    pub(crate) fn uninitialized_constant(name: IdentId) -> MonorubyErr {
        Self::nameerr(format!("uninitialized constant {name}"))
    }

    pub(crate) fn uninitialized_cvar(name: IdentId, class_name: String) -> MonorubyErr {
        Self::nameerr(format!(
            "uninitialized class variable {name} in {class_name}"
        ))
    }

    pub(crate) fn identifier_must_be_constant(name: &str) -> MonorubyErr {
        Self::nameerr(format!("identifier {name} needs to be constant"))
    }

    pub(crate) fn undefined_method(method_name: IdentId, class_name: String) -> MonorubyErr {
        Self::nameerr(format!(
            "undefined method `{}' for class `{}'",
            method_name.get_name(),
            class_name,
        ))
    }

    pub(crate) fn typeerr(msg: impl ToString) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Type, msg)
    }

    pub(crate) fn no_implicit_conversion(
        store: &Store,
        val: Value,
        target_class: ClassId,
    ) -> MonorubyErr {
        MonorubyErr::typeerr(format!(
            "no implicit conversion of {} into {}",
            val.get_real_class_name(store),
            store.get_class_name(target_class)
        ))
    }

    pub(crate) fn is_not_class_nor_module(name: String) -> MonorubyErr {
        MonorubyErr::typeerr(format!("{name} is not a class nor a module"))
    }

    /*pub(crate) fn is_not_class_nor_module_rescue() -> MonorubyErr {
        MonorubyErr::typeerr(format!("class or module required for rescue clause",))
    }*/

    pub(crate) fn is_not_class(name: String) -> MonorubyErr {
        MonorubyErr::typeerr(format!("{name} is not a class"))
    }

    pub(crate) fn superclass_mismatch(name: IdentId) -> MonorubyErr {
        MonorubyErr::typeerr(format!("superclass mismatch for class {name}"))
    }

    ///
    /// Set TypeError with message "*name* is not Symbol nor String".
    ///
    pub(crate) fn is_not_symbol_nor_string(store: &Store, val: Value) -> MonorubyErr {
        MonorubyErr::typeerr(format!("{} is not a symbol nor a string", val.to_s(store)))
    }

    ///
    /// Set TypeError with message "*name* is not Regexp nor String".
    ///
    pub(crate) fn is_not_regexp_nor_string(store: &Store, val: Value) -> MonorubyErr {
        MonorubyErr::typeerr(format!("{} is not a regexp nor a string", val.to_s(store)))
    }

    ///
    /// Set TypeError with message "can't convert *class of val* into Float".
    ///
    pub(crate) fn cant_convert_into_float(store: &Store, val: Value) -> MonorubyErr {
        MonorubyErr::typeerr(format!(
            "can't convert {} into Float",
            val.get_real_class_name(store)
        ))
    }

    ///
    /// Set TypeError with message "{op}: *class of val* can't be coerced into {`msg`}".
    ///
    pub(crate) fn cant_coerced_into(
        store: &Store,
        op: IdentId,
        val: Value,
        msg: &'static str,
    ) -> MonorubyErr {
        MonorubyErr::typeerr(format!(
            "{op}: {} can't be coerced into {msg}",
            val.get_real_class_name(store)
        ))
    }

    pub(crate) fn wrong_argument_type(
        store: &Store,
        val: Value,
        expected: &'static str,
    ) -> MonorubyErr {
        MonorubyErr::typeerr(format!(
            "wrong argument type {} (expected {expected})",
            val.get_real_class_name(store),
        ))
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

    pub(crate) fn cant_modify_frozen(store: &Store, val: Value) -> MonorubyErr {
        MonorubyErr::frozenerr(format!(
            "can't modify frozen {}: {}",
            val.get_real_class_name(store),
            val.to_s(store),
        ))
    }

    pub(crate) fn loaderr(msg: impl ToString, path: PathBuf) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Load(path), msg)
    }

    pub(crate) fn cant_load(err: Option<std::io::Error>, path: &std::path::Path) -> MonorubyErr {
        MonorubyErr::loaderr(
            match err {
                Some(err) => format!("can't load {path:?}. {err}"),
                None => format!("can't load {path:?}"),
            },
            path.into(),
        )
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
    Exception,
    NotMethod,
    Arguments,
    Syntax,
    Unimplemented,
    Name,
    DivideByZero,
    LocalJump,
    Range,
    Type,
    Index,
    Frozen,
    Load(PathBuf),
    Internal,
    Regex,
    Runtime,
    Key,
    Fiber,
    StopIteration,
    SystemExit(u8),
    Other(ClassId),
    MethodReturn(Value, Lfp),
}

impl MonorubyErrKind {
    pub fn from_class_id(class_id: ClassId) -> Self {
        match class_id {
            EXCEPTION_CLASS => MonorubyErrKind::Exception,
            NO_METHOD_ERROR_CLASS => MonorubyErrKind::NotMethod,
            ARGUMENTS_ERROR_CLASS => MonorubyErrKind::Arguments,
            SYNTAX_ERROR_CLASS => MonorubyErrKind::Syntax,
            UNIMPLEMENTED_ERROR_CLASS => MonorubyErrKind::Unimplemented,
            NAME_ERROR_CLASS => MonorubyErrKind::Name,
            ZERO_DIVISION_ERROR_CLASS => MonorubyErrKind::DivideByZero,
            LOCAL_JUMP_ERROR_CLASS => MonorubyErrKind::LocalJump,
            RANGE_ERROR_CLASS => MonorubyErrKind::Range,
            TYPE_ERROR_CLASS => MonorubyErrKind::Type,
            INDEX_ERROR_CLASS => MonorubyErrKind::Index,
            FROZEN_ERROR_CLASS => MonorubyErrKind::Frozen,
            LOAD_ERROR_CLASS => MonorubyErrKind::Load(PathBuf::new()),
            INTERNAL_ERROR_CLASS => MonorubyErrKind::Internal,
            REGEX_ERROR_CLASS => MonorubyErrKind::Regex,
            RUNTIME_ERROR_CLASS => MonorubyErrKind::Runtime,
            KEY_ERROR_CLASS => MonorubyErrKind::Key,
            FIBER_ERROR_CLASS => MonorubyErrKind::Fiber,
            STOP_ITERATION_CLASS => MonorubyErrKind::StopIteration,
            SYSTEM_EXIT_ERROR_CLASS => MonorubyErrKind::SystemExit(0),
            _ => MonorubyErrKind::Other(class_id),
        }
    }
}

#[cfg(test)]
mod tests {
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
