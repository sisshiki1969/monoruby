use ruruby_parse::{Loc, ParseErr, ParseErrKind, SourceInfoRef};

use super::*;

///
/// Exception information which is stored in *Executor*.
///
#[derive(Debug, Clone, PartialEq)]
pub struct MonorubyErr {
    pub kind: MonorubyErrKind,
    pub message: String,
    pub trace: Vec<(Option<(Loc, SourceInfoRef)>, Option<FuncId>)>,
}

impl MonorubyErr {
    pub fn new(kind: MonorubyErrKind, message: impl ToString) -> Self {
        MonorubyErr {
            kind,
            message: message.to_string(),
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

    pub fn push_trace(&mut self, loc: Loc, sourceinfo: SourceInfoRef, fid: FuncId) {
        self.trace.push((Some((loc, sourceinfo)), Some(fid)));
    }

    pub fn push_internal_trace(&mut self, fid: FuncId) {
        self.trace.push((None, Some(fid)));
    }

    pub fn take_trace(&mut self) -> Vec<(Option<(Loc, SourceInfoRef)>, Option<FuncId>)> {
        std::mem::take(&mut self.trace)
    }

    pub fn show_error_message_and_all_loc(&self, store: &Store) {
        let mut loc_flag = self.show_error_message_and_loc(store);
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

    pub fn show_error_message_and_loc(&self, store: &Store) -> bool {
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
        };
        loc_flag
    }

    pub fn get_error_message(&self, store: &Store) -> String {
        format!("{} ({})", self.message, self.class_name(store))
    }

    pub fn class_name(&self, store: &Store) -> String {
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
            MonorubyErrKind::Type => "TypeError",
            MonorubyErrKind::Index => "IndexError",
            MonorubyErrKind::Frozen => "FrozenError",
            MonorubyErrKind::Load(_) => "LoadError",
            MonorubyErrKind::Regex => "RegexpError",
            MonorubyErrKind::Runtime => "RuntimeError",
            MonorubyErrKind::IO => "IOError",
            MonorubyErrKind::Key => "KeyError",
            MonorubyErrKind::Fiber => "FiberError",
            MonorubyErrKind::StopIteration => "StopIteration",
            MonorubyErrKind::SystemExit(..) => "SystemExit",
            MonorubyErrKind::Other(class_id) => return class_id.get_name(store),
            MonorubyErrKind::MethodReturn(..) => "MethodReturn",
            MonorubyErrKind::Retry => "Retry",
            MonorubyErrKind::Redo => "Redo",
            MonorubyErrKind::Fatal => "FatalError",
        }
        .to_string()
    }

    pub fn class_id(&self) -> ClassId {
        match &self.kind {
            MonorubyErrKind::Exception => EXCEPTION_CLASS,
            MonorubyErrKind::NotMethod(_) => NO_METHOD_ERROR_CLASS,
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
            MonorubyErrKind::Regex => REGEX_ERROR_CLASS,
            MonorubyErrKind::Runtime => RUNTIME_ERROR_CLASS,
            MonorubyErrKind::IO => IO_ERROR_CLASS,
            MonorubyErrKind::Key => KEY_ERROR_CLASS,
            MonorubyErrKind::Fiber => FIBER_ERROR_CLASS,
            MonorubyErrKind::StopIteration => STOP_ITERATION_CLASS,
            MonorubyErrKind::SystemExit(..) => SYSTEM_EXIT_ERROR_CLASS,
            MonorubyErrKind::Other(class_id) => *class_id,
            MonorubyErrKind::Fatal => FATAL_ERROR_CLASS,
            MonorubyErrKind::MethodReturn(..) | MonorubyErrKind::Retry | MonorubyErrKind::Redo => {
                unreachable!()
            }
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

    pub fn is_no_method_error(&self) -> bool {
        matches!(self.kind, MonorubyErrKind::NotMethod(_))
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

    pub(crate) fn retry() -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Retry, String::new())
    }

    pub(crate) fn redo() -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Redo, String::new())
    }

    pub(crate) fn method_not_found(store: &Store, name: IdentId, obj: Value) -> MonorubyErr {
        MonorubyErr::new(
            MonorubyErrKind::NotMethod(Some(obj.id())),
            format!("undefined method `{name}' for {}", obj.to_s(store)),
        )
    }

    pub(crate) fn method_not_found_for_class(
        store: &Store,
        name: IdentId,
        class: ClassId,
    ) -> MonorubyErr {
        MonorubyErr::new(
            MonorubyErrKind::NotMethod(None),
            format!(
                "undefined method `{name}' for {}",
                store.get_class_name(class)
            ),
        )
    }

    pub(crate) fn private_method_called(store: &Store, name: IdentId, obj: Value) -> MonorubyErr {
        MonorubyErr::new(
            MonorubyErrKind::NotMethod(Some(obj.id())),
            format!(
                "private method `{name}' called for {}:{}",
                obj.to_s(store),
                obj.get_real_class_name(store)
            ),
        )
    }

    pub(crate) fn protected_method_called(store: &Store, name: IdentId, obj: Value) -> MonorubyErr {
        MonorubyErr::new(
            MonorubyErrKind::NotMethod(Some(obj.id())),
            format!(
                "protected method `{name}' called for {}:{}",
                obj.to_s(store),
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
        if range.start() == range.end() {
            Self::wrong_number_of_arg(*range.start(), given)
        } else {
            Self::argumenterr(format!(
                "wrong number of arguments (given {given}, expected {}..{})",
                range.start(),
                range.end()
            ))
        }
    }

    pub(crate) fn wrong_number_of_arg_min(given: usize, min: usize) -> MonorubyErr {
        Self::argumenterr(format!(
            "wrong number of arguments (given {given}, expected {min}+)"
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
            "no block given (yield)".to_string(),
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
    /// Set TypeError with message "*name* is not Symbol".
    ///
    #[allow(dead_code)]
    pub(crate) fn is_not_symbol(store: &Store, val: Value) -> MonorubyErr {
        MonorubyErr::typeerr(format!("{} is not a symbol", val.to_s(store)))
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

    pub fn cant_convert_error_ary(store: &Store, v: Value, result: Value) -> MonorubyErr {
        Self::cant_convert_error(store, v, result, "Array", IdentId::TO_ARY)
    }

    pub fn cant_convert_error_int(store: &Store, v: Value, result: Value) -> MonorubyErr {
        Self::cant_convert_error(store, v, result, "Integer", IdentId::TO_INT)
    }

    pub fn cant_convert_error_f(store: &Store, v: Value, result: Value) -> MonorubyErr {
        Self::cant_convert_error(store, v, result, "Float", IdentId::TO_F)
    }

    pub fn cant_convert_error(
        store: &Store,
        v: Value,
        result: Value,
        target: &str,
        method: IdentId,
    ) -> MonorubyErr {
        let class = v.get_real_class_name(store);
        MonorubyErr::typeerr(format!(
            "can't convert {class} into {target} ({class}#{method} gives {})",
            result.get_real_class_name(store),
        ))
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
        _op: IdentId,
        val: Value,
        msg: &'static str,
    ) -> MonorubyErr {
        MonorubyErr::typeerr(format!(
            "{} can't be coerced into {msg}",
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

    pub(crate) fn exponent_is_too_large() -> MonorubyErr {
        MonorubyErr::argumenterr("exponent is too large")
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
            val.inspect(store),
        ))
    }

    pub(crate) fn loaderr(msg: impl ToString, path: PathBuf) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Load(path), msg)
    }

    pub(crate) fn cant_load(err: Option<std::io::Error>, path: &std::path::Path) -> MonorubyErr {
        let display = path.display();
        MonorubyErr::loaderr(
            match err {
                Some(err) => format!("cannot load such file -- {display} ({err})"),
                None => format!("cannot load such file -- {display}"),
            },
            path.into(),
        )
    }

    pub(crate) fn regexerr(msg: impl ToString) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Regex, msg)
    }

    pub(crate) fn runtimeerr(msg: impl ToString) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Runtime, msg)
    }

    /// Construct a FatalError. Used when a Rust `panic!` is caught at an
    /// `extern "C"` boundary. FatalError propagates up through Ruby's
    /// `rescue` handlers without being caught.
    pub fn fatal(msg: impl ToString) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Fatal, msg)
    }

    /// Convenience: is this error a fatal (panic-caught) one?
    pub fn is_fatal(&self) -> bool {
        self.kind.is_fatal()
    }

    pub(crate) fn ioerr(msg: impl ToString) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::IO, msg)
    }

    /// Convert a `std::io::Error` into the appropriate Ruby Errno exception.
    ///
    /// Looks up the Errno module and the specific error class (e.g. `Errno::ENOTDIR`)
    /// using the OS error number. Falls back to `RuntimeError` if the class is not found.
    pub(crate) fn from_io_err(store: &Store, err: &std::io::Error, msg: String) -> MonorubyErr {
        if let Some(errno) = err.raw_os_error() {
            let errno_name = errno_to_name(errno);
            if let Some(errno_name) = errno_name {
                // Look up `Errno` module under Object
                let errno_module =
                    store.get_constant_noautoload(OBJECT_CLASS, IdentId::get_id("Errno"));
                if let Some(errno_module) = errno_module {
                    let errno_class_id = errno_module.as_class_id();
                    // Look up the specific error class (e.g. `ENOTDIR`) under `Errno`
                    let err_class =
                        store.get_constant_noautoload(errno_class_id, IdentId::get_id(errno_name));
                    if let Some(err_class) = err_class {
                        let class_id = err_class.as_class_id();
                        return MonorubyErr::new(MonorubyErrKind::Other(class_id), msg);
                    }
                }
            }
        }
        // Fallback to RuntimeError
        MonorubyErr::runtimeerr(msg)
    }

    /// Create an Errno exception from a `std::io::Error` with a syscall name and path.
    ///
    /// Formats the message to match CRuby: `"<description> @ <syscall> - <path>"`
    /// For example: `"No such file or directory @ rb_sysopen - /path/to/file"`
    pub(crate) fn errno_with_path(
        store: &Store,
        err: &std::io::Error,
        syscall: &str,
        path: &str,
    ) -> MonorubyErr {
        let desc = errno_description(err);
        let msg = format!("{} @ {} - {}", desc, syscall, path);
        Self::from_io_err(store, err, msg)
    }

    /// Create an Errno exception from a `std::io::Error` with just a path (no syscall name).
    ///
    /// Formats the message to match CRuby: `"<description> - <path>"`
    /// For example: `"No such file or directory - /path/to/file"`
    pub(crate) fn errno_with_msg(store: &Store, err: &std::io::Error, path: &str) -> MonorubyErr {
        let desc = errno_description(err);
        let msg = format!("{} - {}", desc, path);
        Self::from_io_err(store, err, msg)
    }

    pub(crate) fn rangeerr(msg: impl ToString) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Range, msg)
    }

    pub(crate) fn float_out_of_range_of_integer(f: f64) -> MonorubyErr {
        MonorubyErr::rangeerr(format!("float {:?} out of range of integer", f))
    }

    pub fn localjumperr(msg: impl ToString) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::LocalJump, msg)
    }

    pub(crate) fn localjumperr_with_val(msg: impl ToString, _val: Value) -> MonorubyErr {
        // TODO: store val as exit_value on the LocalJumpError exception object
        MonorubyErr::new(MonorubyErrKind::LocalJump, msg)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum MonorubyErrKind {
    Exception,
    NotMethod(Option<u64>),
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
    Regex,
    Runtime,
    IO,
    Key,
    Fiber,
    StopIteration,
    SystemExit(u8),
    Other(ClassId),
    MethodReturn(Value, Lfp),
    Retry,
    Redo,
    /// FatalError — raised when a Rust `panic!` is caught at an `extern "C"`
    /// boundary. Deliberately NOT catchable from Ruby `rescue` (even
    /// `rescue Exception`); propagates up to the top level.
    Fatal,
}

impl MonorubyErrKind {
    /// Returns true if this error must not be caught by any Ruby-level
    /// `rescue` clause. See `MonorubyErrKind::Fatal`.
    pub fn is_fatal(&self) -> bool {
        matches!(self, MonorubyErrKind::Fatal)
    }
}

impl MonorubyErrKind {
    pub fn from_class_id(class_id: ClassId) -> Self {
        match class_id {
            EXCEPTION_CLASS => MonorubyErrKind::Exception,
            NO_METHOD_ERROR_CLASS => MonorubyErrKind::NotMethod(None),
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
            REGEX_ERROR_CLASS => MonorubyErrKind::Regex,
            RUNTIME_ERROR_CLASS => MonorubyErrKind::Runtime,
            IO_ERROR_CLASS => MonorubyErrKind::IO,
            KEY_ERROR_CLASS => MonorubyErrKind::Key,
            FIBER_ERROR_CLASS => MonorubyErrKind::Fiber,
            STOP_ITERATION_CLASS => MonorubyErrKind::StopIteration,
            SYSTEM_EXIT_ERROR_CLASS => MonorubyErrKind::SystemExit(0),
            _ => MonorubyErrKind::Other(class_id),
        }
    }
}

/// Return a human-readable description for an `std::io::Error`, matching CRuby's Errno messages.
pub(crate) fn errno_description(err: &std::io::Error) -> &'static str {
    match err.raw_os_error() {
        Some(1) => "Operation not permitted",
        Some(2) => "No such file or directory",
        Some(5) => "Input/output error",
        Some(9) => "Bad file descriptor",
        Some(12) => "Cannot allocate memory",
        Some(13) => "Permission denied",
        Some(17) => "File exists",
        Some(20) => "Not a directory",
        Some(21) => "Is a directory",
        Some(22) => "Invalid argument",
        Some(28) => "No space left on device",
        Some(30) => "Read-only file system",
        Some(32) => "Broken pipe",
        Some(36) => "File name too long",
        Some(39) => "Directory not empty",
        _ => "Unknown error",
    }
}

/// Map a raw OS errno number to the corresponding Ruby Errno class name.
///
/// Returns `None` for unknown errno values.
fn errno_to_name(errno: i32) -> Option<&'static str> {
    match errno {
        1 => Some("EPERM"),
        2 => Some("ENOENT"),
        3 => Some("ESRCH"),
        4 => Some("EINTR"),
        5 => Some("EIO"),
        6 => Some("ENXIO"),
        7 => Some("E2BIG"),
        8 => Some("ENOEXEC"),
        9 => Some("EBADF"),
        10 => Some("ECHILD"),
        11 => Some("EAGAIN"),
        12 => Some("ENOMEM"),
        13 => Some("EACCES"),
        14 => Some("EFAULT"),
        16 => Some("EBUSY"),
        17 => Some("EEXIST"),
        18 => Some("EXDEV"),
        19 => Some("ENODEV"),
        20 => Some("ENOTDIR"),
        21 => Some("EISDIR"),
        22 => Some("EINVAL"),
        23 => Some("ENFILE"),
        24 => Some("EMFILE"),
        25 => Some("ENOTTY"),
        27 => Some("EFBIG"),
        28 => Some("ENOSPC"),
        29 => Some("ESPIPE"),
        30 => Some("EROFS"),
        31 => Some("EMLINK"),
        32 => Some("EPIPE"),
        33 => Some("EDOM"),
        34 => Some("ERANGE"),
        35 => Some("EDEADLK"),
        36 => Some("ENAMETOOLONG"),
        37 => Some("ENOLCK"),
        38 => Some("ENOSYS"),
        39 => Some("ENOTEMPTY"),
        40 => Some("ELOOP"),
        _ => None,
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

    #[test]
    fn errno_from_dir_operations() {
        // Dir.rmdir on non-existent path triggers Errno::ENOENT
        run_test_no_result_check(
            r#"
            begin
              Dir.rmdir("/nonexistent_dir_xyz_123")
              raise "should have raised"
            rescue Errno::ENOENT
              # expected
            end
            "#,
        );
        // Dir.mkdir on existing directory triggers Errno::EEXIST
        run_test_no_result_check(
            r#"
            begin
              Dir.mkdir("/tmp")
              raise "should have raised"
            rescue Errno::EEXIST
              # expected
            end
            "#,
        );
    }

    #[test]
    fn errno_enotdir() {
        // Dir.rmdir on a non-directory triggers Errno::ENOTDIR
        run_test(
            r#"
            begin
              Dir.rmdir("Cargo.toml")
            rescue Errno::ENOTDIR => e
              e.is_a?(SystemCallError)
            end
            "#,
        );
    }

    #[test]
    fn errno_eexist() {
        // Dir.mkdir on existing directory triggers Errno::EEXIST
        run_test(
            r#"
            begin
              Dir.mkdir("/tmp")
            rescue Errno::EEXIST => e
              e.is_a?(SystemCallError)
            end
            "#,
        );
    }

    #[test]
    fn errno_file_open_enoent() {
        // File.open on a non-existent file raises Errno::ENOENT
        run_test(
            r#"
            begin
              File.open("/nonexistent_file_xyz_123")
            rescue Errno::ENOENT => e
              e.is_a?(SystemCallError)
            end
            "#,
        );
    }

    #[test]
    fn errno_file_read_enoent() {
        // File.read on a non-existent file raises Errno::ENOENT
        run_test(
            r#"
            begin
              File.read("/nonexistent_file_xyz_123")
            rescue Errno::ENOENT => e
              e.is_a?(SystemCallError)
            end
            "#,
        );
    }

    #[test]
    fn errno_file_delete_enoent() {
        // File.delete on a non-existent file raises Errno::ENOENT
        run_test(
            r#"
            begin
              File.delete("/nonexistent_file_xyz_123")
            rescue Errno::ENOENT => e
              e.is_a?(SystemCallError)
            end
            "#,
        );
    }

    #[test]
    fn errno_file_readlines_enoent() {
        // File.readlines on a non-existent file raises Errno::ENOENT
        run_test(
            r#"
            begin
              File.readlines("/nonexistent_file_xyz_123")
            rescue Errno::ENOENT => e
              e.is_a?(SystemCallError)
            end
            "#,
        );
    }

    #[test]
    fn errno_file_size_enoent() {
        // File.size on a non-existent file raises Errno::ENOENT
        run_test(
            r#"
            begin
              File.size("/nonexistent_file_xyz_123")
            rescue Errno::ENOENT => e
              e.is_a?(SystemCallError)
            end
            "#,
        );
    }

    #[test]
    fn errno_file_write_eisdir() {
        // File.write to a directory raises Errno::EISDIR
        run_test(
            r#"
            begin
              File.write("/tmp", "test")
            rescue Errno::EISDIR => e
              e.is_a?(SystemCallError)
            end
            "#,
        );
    }

    #[test]
    fn errno_io_read_enoent() {
        // IO.read on a non-existent file raises Errno::ENOENT
        run_test(
            r#"
            begin
              IO.read("/nonexistent_file_xyz_123")
            rescue Errno::ENOENT => e
              e.is_a?(SystemCallError)
            end
            "#,
        );
    }

    #[test]
    fn errno_io_sysopen_enoent() {
        // IO.sysopen on a non-existent file raises Errno::ENOENT
        run_test(
            r#"
            begin
              IO.sysopen("/nonexistent_file_xyz_123")
            rescue Errno::ENOENT => e
              e.is_a?(SystemCallError)
            end
            "#,
        );
    }

    #[test]
    fn errno_dir_chdir_enoent() {
        // Dir.chdir to a non-existent directory raises Errno::ENOENT
        run_test(
            r#"
            begin
              Dir.chdir("/nonexistent_dir_xyz_123")
            rescue Errno::ENOENT => e
              e.is_a?(SystemCallError)
            end
            "#,
        );
    }

    #[test]
    fn errno_dir_entries_enoent() {
        // Dir.entries on a non-existent directory raises Errno::ENOENT
        run_test(
            r#"
            begin
              Dir.entries("/nonexistent_dir_xyz_123")
            rescue Errno::ENOENT => e
              e.is_a?(SystemCallError)
            end
            "#,
        );
    }
}
