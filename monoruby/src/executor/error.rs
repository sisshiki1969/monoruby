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

#[derive(Debug, Clone, PartialEq)]
pub enum MonorubyErrKind {
    MethodReturn(Value, LFP),
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
    Load,
    Internal,
    Regex,
    Runtime,
    Key,
    Fiber,
    StopIteration,
    Exception,
}

impl MonorubyErr {
    pub fn new(kind: MonorubyErrKind, msg: String) -> Self {
        MonorubyErr {
            kind,
            msg,
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

    pub fn show_error_message_and_all_loc(&self) {
        eprintln!("{}", self.get_error_message());
        self.show_all_loc();
    }

    pub fn show_error_message_and_loc(&self) {
        eprintln!("{}", self.get_error_message());
        self.show_loc();
    }

    pub fn get_error_message(&self) -> String {
        format!("{} ({})", self.msg, self.get_class_name())
    }

    pub fn get_class_name(&self) -> &str {
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
            MonorubyErrKind::Load => "LoadError",
            MonorubyErrKind::Internal => "InternalError",
            MonorubyErrKind::Regex => "RegexError",
            MonorubyErrKind::Runtime => "RuntimeError",
            MonorubyErrKind::Key => "KeyError",
            MonorubyErrKind::Fiber => "FiberError",
            MonorubyErrKind::StopIteration => "StopIteration",
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

    pub(crate) fn unsupported_block_param(lhs: &Node, sourceinfo: SourceInfoRef) -> MonorubyErr {
        let msg = format!("unsupported block parameter type {:?}", lhs.kind);
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
    pub(crate) fn method_return(val: Value, target_lfp: LFP) -> MonorubyErr {
        MonorubyErr::new(
            MonorubyErrKind::MethodReturn(val, target_lfp),
            String::new(),
        )
    }

    pub(crate) fn method_not_found(globals: &Globals, name: IdentId, obj: Value) -> MonorubyErr {
        MonorubyErr::new(
            MonorubyErrKind::NotMethod,
            format!(
                "undefined method `{name}' for {}:{}",
                globals.to_s(obj),
                obj.get_real_class_name(globals)
            ),
        )
    }

    pub(crate) fn method_not_found_for_class(
        globals: &Globals,
        name: IdentId,
        class: ClassId,
    ) -> MonorubyErr {
        MonorubyErr::new(
            MonorubyErrKind::NotMethod,
            format!("undefined method `{name}' for {}", class.get_name(globals)),
        )
    }

    pub(crate) fn private_method_called(
        globals: &Globals,
        name: IdentId,
        obj: Value,
    ) -> MonorubyErr {
        MonorubyErr::new(
            MonorubyErrKind::NotMethod,
            format!(
                "private method `{name}' called for {}:{}",
                globals.to_s(obj),
                obj.get_real_class_name(globals)
            ),
        )
    }

    fn wrong_number_of_arg(expected: usize, given: usize) -> MonorubyErr {
        Self::argumenterr(format!(
            "wrong number of arguments (given {given}, expected {expected})"
        ))
    }

    fn wrong_number_of_arg_range(
        given: usize,
        range: std::ops::RangeInclusive<usize>,
    ) -> MonorubyErr {
        Self::argumenterr(format!(
            "wrong number of arguments (given {given}, expeted {range:?})"
        ))
    }

    fn wrong_number_of_arg_min(given: usize, min: usize) -> MonorubyErr {
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
            format!("{} out of char range", globals.to_s(val)),
        )
    }

    pub(crate) fn uninitialized_constant(name: IdentId) -> MonorubyErr {
        MonorubyErr::new(
            MonorubyErrKind::Name,
            format!("uninitialized constant {name}"),
        )
    }

    pub(crate) fn identifier_must_be_constant(name: &str) -> MonorubyErr {
        MonorubyErr::new(
            MonorubyErrKind::Name,
            format!("identifier {name} needs to be constant"),
        )
    }

    pub(crate) fn typeerr(msg: String) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Type, msg)
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
    pub(crate) fn is_not_symbol_nor_string(globals: &Globals, val: Value) -> MonorubyErr {
        MonorubyErr::typeerr(format!(
            "{} is not a symbol nor a string",
            globals.to_s(val)
        ))
    }

    ///
    /// Set TypeError with message "*name* is not Regexp nor String".
    ///
    pub(crate) fn is_not_regexp_nor_string(globals: &Globals, val: Value) -> MonorubyErr {
        MonorubyErr::typeerr(format!(
            "{} is not a regexp nor a string",
            globals.to_s(val)
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
        MonorubyErr::new(MonorubyErrKind::Arguments, msg)
    }

    pub(crate) fn zero_width_padding() -> MonorubyErr {
        MonorubyErr::argumenterr("zero width padding".to_string())
    }

    pub(crate) fn negative_argument() -> MonorubyErr {
        MonorubyErr::argumenterr("negativeargument".to_string())
    }

    pub(crate) fn negative_array_size() -> MonorubyErr {
        MonorubyErr::argumenterr("negative array size".to_string())
    }

    pub(crate) fn create_proc_no_block() -> MonorubyErr {
        MonorubyErr::argumenterr("tried to create Proc object without a block".to_string())
    }

    pub(crate) fn check_number_of_arguments(given: usize, expect: usize) -> Result<()> {
        if given == expect {
            Ok(())
        } else {
            Err(MonorubyErr::wrong_number_of_arg(expect, given))
        }
    }

    pub(crate) fn check_number_of_arguments_range(
        given: usize,
        range: std::ops::RangeInclusive<usize>,
    ) -> Result<()> {
        if range.contains(&given) {
            Ok(())
        } else {
            let err = if range.start() == range.end() {
                MonorubyErr::wrong_number_of_arg(*range.start(), given)
            } else {
                MonorubyErr::wrong_number_of_arg_range(given, range)
            };
            Err(err)
        }
    }

    pub(crate) fn check_min_number_of_arguments(given: usize, min: usize) -> Result<()> {
        if given >= min {
            return Ok(());
        }
        Err(MonorubyErr::wrong_number_of_arg_min(given, min))
    }

    pub(crate) fn indexerr(msg: String) -> MonorubyErr {
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

    pub(crate) fn frozenerr(msg: String) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Frozen, msg)
    }

    pub(crate) fn cant_modify_frozen(globals: &Globals, val: Value) -> MonorubyErr {
        MonorubyErr::frozenerr(format!(
            "can't modify frozen {}: {}",
            val.get_real_class_name(globals),
            globals.to_s(val),
        ))
    }

    pub(crate) fn loaderr(msg: String) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Load, msg)
    }

    pub(crate) fn cant_load(err: Option<std::io::Error>, path: &std::path::Path) -> MonorubyErr {
        MonorubyErr::loaderr(match err {
            Some(err) => format!("can't load {path:?}. {err}"),
            None => format!("can't load {path:?}"),
        })
    }

    pub(crate) fn internalerr(msg: String) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Internal, msg)
    }

    pub(crate) fn regexerr(msg: String) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Regex, msg)
    }

    pub(crate) fn runtimeerr(msg: String) -> MonorubyErr {
        MonorubyErr::new(MonorubyErrKind::Runtime, msg)
    }
}

//
// error handlers
//
impl Executor {
    pub fn set_error(&mut self, err: MonorubyErr) {
        self.exception = Some(err);
    }

    pub(crate) fn exception(&self) -> Option<&MonorubyErr> {
        self.exception.as_ref()
    }

    pub(crate) fn take_error(&mut self) -> MonorubyErr {
        std::mem::take(&mut self.exception).unwrap()
    }

    pub(crate) fn take_ex_obj(&mut self, globals: &Globals) -> Value {
        let err = self.take_error();
        self.exception_to_val(globals, err)
    }

    pub(crate) fn err_divide_by_zero(&mut self) {
        self.set_error(MonorubyErr::divide_by_zero());
    }

    pub(crate) fn err_no_block_given(&mut self) {
        self.set_error(MonorubyErr::no_block_given());
    }

    pub(crate) fn err_wrong_number_of_arg_range(
        &mut self,
        given: usize,
        range: std::ops::RangeInclusive<usize>,
    ) {
        self.set_error(MonorubyErr::wrong_number_of_arg_range(given, range))
    }

    ///
    /// Set FrozenError with message "can't modify frozen Integer: 5".
    ///
    pub(crate) fn err_cant_modify_frozen(&mut self, globals: &Globals, val: Value) {
        self.set_error(MonorubyErr::cant_modify_frozen(globals, val));
    }

    pub(crate) fn push_error_location(&mut self, loc: Loc, sourceinfo: SourceInfoRef) {
        match &mut self.exception {
            Some(err) => {
                err.trace.push((loc, sourceinfo));
            }
            None => unreachable!(),
        };
    }

    pub fn exception_to_val(&self, globals: &Globals, err: MonorubyErr) -> Value {
        let class_id = globals.get_error_class(&err);
        Value::new_exception_from_err(err, class_id)
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
