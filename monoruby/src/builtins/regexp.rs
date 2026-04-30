use super::*;

//
// Regexp class
//

pub(crate) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Regexp", REGEXP_CLASS, ObjTy::REGEXP);
    globals.define_builtin_class_funcs_with(
        REGEXP_CLASS,
        "new",
        &["compile"],
        regexp_new,
        1,
        2,
        false,
    );
    globals.define_builtin_class_funcs(REGEXP_CLASS, "escape", &["quote"], regexp_escape, 1);
    globals.define_builtin_class_func_rest(REGEXP_CLASS, "union", regexp_union);
    globals.define_builtin_class_func_with(
        REGEXP_CLASS,
        "last_match",
        regexp_last_match,
        0,
        1,
        false,
    );
    globals.define_builtin_func(REGEXP_CLASS, "=~", regexp_match, 1);
    globals.define_builtin_func(REGEXP_CLASS, "===", teq, 1);
    globals.define_builtin_funcs(REGEXP_CLASS, "==", &["eql?"], regexp_eq, 1);
    globals.define_builtin_func(REGEXP_CLASS, "hash", regexp_hash, 0);
    globals.define_builtin_func(REGEXP_CLASS, "source", source, 0);
    globals.define_builtin_func(REGEXP_CLASS, "options", options, 0);
    globals.define_builtin_func_with(REGEXP_CLASS, "match?", match_, 1, 2, false);
    globals.define_builtin_func_with(REGEXP_CLASS, "match", rmatch, 1, 2, false);
    globals.define_builtin_func(REGEXP_CLASS, "names", names, 0);
    globals.store[REGEXP_CLASS].set_alloc_func(regexp_alloc_func);
}

///
/// ### Regexp#names
///
/// - names -> [String]
///
/// Returns the names of named captures declared in the pattern.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/i/names.html]
#[monoruby_builtin]
fn names(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let re = self_.as_regexp_inner();
    let raw = re.capture_names().unwrap_or_default();
    let mut unique: Vec<String> = Vec::with_capacity(raw.len());
    for n in raw {
        if !unique.iter().any(|u| *u == n) {
            unique.push(n);
        }
    }
    Ok(Value::array_from_iter(
        unique.iter().map(|n| Value::string_from_str(n)),
    ))
}

// Class methods

/// Allocator for `Regexp` and its subclasses. An empty pattern with no
/// options cannot fail to compile, so unwrap is safe.
pub(crate) extern "C" fn regexp_alloc_func(class_id: ClassId, _: &mut Globals) -> Value {
    let regexp = RegexpInner::with_option("", 0).expect("empty regexp compile cannot fail");
    Value::regexp_with_class(regexp, class_id)
}

///
/// ### Regexp.new
/// - new(string, option=nil, [NOT SUPPORTED] code=nil) -> Regexp
/// - compile(string, option=nil, [NOT SUPPORTED] code=nil) -> Regexp
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/s/compile.html]
#[monoruby_builtin]
fn regexp_new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let arg0 = lfp.arg(0);
    // When given an existing Regexp, carry over both source and
    // options so `Regexp.new(/abc/i) == /abc/i`. CRuby's `rb_reg_init`
    // copies the source verbatim and inherits the options unless the
    // caller passes an explicit second argument.
    let (string, default_option) = if let Some(re) = arg0.is_regex() {
        (re.as_str().to_string(), Some(re.raw_option()))
    } else {
        (arg0.coerce_to_string(vm, globals)?, None)
    };
    let option = if let Some(option) = lfp.try_arg(1) {
        if option.is_nil() {
            default_option.unwrap_or(onigmo_regex::ONIG_OPTION_NONE)
        } else if let Some(option) = option.try_fixnum() {
            option as i32 as u32
        } else if let Some(s) = option.is_str() {
            // Ruby accepts a string of flag characters as the option
            // argument: each char selects a flag, unknown chars
            // raise ArgumentError. Encoding-letter flags (n/u/e/s)
            // are accepted but currently ignored.
            parse_option_string(s)?
        } else if option == Value::bool(false) {
            onigmo_regex::ONIG_OPTION_NONE
        } else if option == Value::bool(true) {
            onigmo_regex::ONIG_OPTION_IGNORECASE
        } else {
            // CRuby warns "expected true or false as ignorecase: <obj>"
            // and treats the argument as truthy → IGNORECASE.
            warn_unexpected_regexp_option(vm, globals, option);
            onigmo_regex::ONIG_OPTION_IGNORECASE
        }
    } else {
        default_option.unwrap_or(onigmo_regex::ONIG_OPTION_NONE)
    };
    let encoding = if option & RegexpInner::NOENCODING != 0 {
        onigmo_regex::OnigmoEncoding::ASCII
    } else {
        onigmo_regex::OnigmoEncoding::UTF8
    };
    // Strip Ruby-specific encoding flags before passing to onigmo
    let onigmo_option = option & !(RegexpInner::NOENCODING | RegexpInner::FIXEDENCODING);
    let regexp = RegexpInner::with_option_and_encoding(string, onigmo_option, encoding)?;
    let val = Value::regexp(regexp);
    Ok(val)
}

/// Parse a Ruby flag string like "im" into an Onigmo options bitmap.
/// Only `i`/`m`/`x` are accepted; encoding selectors (`n`/`u`/`e`/
/// `s`) are *not* — they're literal-only flags. Anything else raises
/// `ArgumentError: unknown regexp option: <c>`, matching CRuby.
fn parse_option_string(s: &str) -> Result<u32> {
    let mut opt = onigmo_regex::ONIG_OPTION_NONE;
    for c in s.chars() {
        match c {
            'i' => opt |= onigmo_regex::ONIG_OPTION_IGNORECASE,
            'm' => opt |= onigmo_regex::ONIG_OPTION_MULTILINE,
            'x' => opt |= onigmo_regex::ONIG_OPTION_EXTEND,
            other => {
                return Err(MonorubyErr::argumenterr(format!(
                    "unknown regexp option: {other}"
                )));
            }
        }
    }
    Ok(opt)
}

/// Emit CRuby's "expected true or false as ignorecase" warning to
/// `$stderr`, used when `Regexp.new`'s second argument is a non-
/// Integer / non-String / non-nil / non-bool value. We don't need to
/// invoke `Kernel#warn` here — `complain` matchers in mspec read
/// `$stderr.write` output, which is what the spec checks.
fn warn_unexpected_regexp_option(vm: &mut Executor, globals: &mut Globals, option: Value) {
    let stderr_id = IdentId::get_id("$stderr");
    let stderr = match globals.get_gvar(stderr_id) {
        Some(v) => v,
        None => return,
    };
    let msg = format!(
        "warning: expected true or false as ignorecase: {}\n",
        option.inspect(&globals.store)
    );
    let _ = vm.invoke_method_inner(
        globals,
        IdentId::get_id("write"),
        stderr,
        &[Value::string(msg)],
        None,
        None,
    );
}

///
/// ### Regexp.escape
/// - escape(string) -> String
/// - quote(string) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/s/escape.html]
#[monoruby_builtin]
fn regexp_escape(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let arg0 = lfp.arg(0);
    let string = arg0.coerce_to_str(vm, globals)?;
    let val = Value::string(RegexpInner::escape(&string));
    Ok(val)
}

///
/// ### Regexp.union
/// - union(*pattern) -> Regexp
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/s/union.html]
#[monoruby_builtin]
fn regexp_union(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut rest = lfp.arg(0).as_array();
    // No arguments: produce `/(?!)/`, a regex that never matches —
    // matches CRuby's `Regexp.union` documentation.
    if rest.is_empty() {
        return Ok(Value::regexp(RegexpInner::with_option("(?!)", 0)?));
    }
    // A single argument has special handling.
    if rest.len() == 1 {
        let arg = rest[0];
        if let Some(arr) = arg.try_array_ty() {
            // `Regexp.union([...])` flattens the one Array.
            rest = arr;
        } else {
            // Direct Regexp or `#to_regexp`-coercible argument is
            // returned as-is, matching CRuby's `rb_check_regexp_type`
            // path. Otherwise fall through to the standard escape-
            // and-wrap path.
            if let Some(re) = arg.is_regex() {
                return Ok(re.into());
            }
            if let Some(func_id) = globals.check_method(arg, IdentId::get_id("to_regexp")) {
                let result = vm.invoke_func_inner(globals, func_id, arg, &[], None, None)?;
                if let Some(re) = result.is_regex() {
                    return Ok(re.into());
                }
            }
            let s = format_union_member(vm, globals, arg)?;
            return Ok(Value::regexp(RegexpInner::with_option(s, 0)?));
        }
    }
    let mut v = vec![];
    for arg in rest.iter() {
        v.push(format_union_member(vm, globals, *arg)?);
    }
    let s = v.join("|");

    Ok(Value::regexp(RegexpInner::with_option(s, 0)?))
}

/// Render a single `Regexp.union` argument into its embedded form.
/// Strings and Symbols are passed through `Regexp.escape`; Regexps
/// use their `to_s` group form so flags are preserved. Falls back to
/// `to_regexp` then `to_str` for general objects, matching CRuby.
fn format_union_member(
    vm: &mut Executor,
    globals: &mut Globals,
    arg: Value,
) -> Result<String> {
    if let Some(s) = arg.is_str() {
        return Ok(RegexpInner::escape(s));
    }
    if let Some(re) = arg.is_regex() {
        return Ok(re.tos());
    }
    if let Some(sym) = arg.try_symbol() {
        return Ok(RegexpInner::escape(sym.get_name().as_str()));
    }
    if let Some(func_id) = globals.check_method(arg, IdentId::get_id("to_regexp")) {
        let result = vm.invoke_func_inner(globals, func_id, arg, &[], None, None)?;
        if let Some(re) = result.is_regex() {
            return Ok(re.tos());
        }
    }
    if let Some(func_id) = globals.check_method(arg, IdentId::TO_STR) {
        let result = vm.invoke_func_inner(globals, func_id, arg, &[], None, None)?;
        if let Some(s) = result.is_str() {
            return Ok(RegexpInner::escape(s));
        }
    }
    let class = arg.get_real_class_name(&globals.store);
    Err(MonorubyErr::typeerr(format!(
        "no implicit conversion of {class} into String"
    )))
}

///
/// ### Regexp.last_match
/// - last_match -> MatchData (not supported)
/// - last_match(nth) -> String | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/s/last_match.html]
#[monoruby_builtin]
fn regexp_last_match(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    if let Some(arg0) = lfp.try_arg(0) {
        let nth = arg0.coerce_to_int_i64(vm, globals)?;
        Ok(vm.get_special_matches(nth).unwrap_or_default())
    } else {
        Ok(vm.get_last_matchdata())
    }
}

/// Mask of Onigmo options that participate in `Regexp#==` / `#eql?` /
/// `#hash`. Encoding flags (NOENCODING / FIXEDENCODING) are
/// intentionally excluded so that `// == //n`, `/abc/ix == /abc/ixn`,
/// etc. — matching CRuby and `equal_value_spec`.
const REGEXP_EQ_OPTION_MASK: u32 = onigmo_regex::ONIG_OPTION_MULTILINE
    | onigmo_regex::ONIG_OPTION_IGNORECASE
    | onigmo_regex::ONIG_OPTION_EXTEND;

///
/// ### Regexp#==, Regexp#eql?
/// - self == other -> bool
/// - self.eql?(other) -> bool
///
/// True when both are regexps with the same source pattern and the
/// same `m`/`i`/`x` options. Encoding flags (`n`, `u`, `e`, `s`) and
/// FIXEDENCODING do not participate.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/i/=3d=3d.html]
#[monoruby_builtin]
fn regexp_eq(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let lhs = self_.as_regexp_inner();
    let rhs = match lfp.arg(0).is_regex() {
        Some(r) => r,
        None => return Ok(Value::bool(false)),
    };
    let same_source = lhs.as_str() == rhs.as_str();
    let same_options =
        (lhs.raw_option() & REGEXP_EQ_OPTION_MASK) == (rhs.raw_option() & REGEXP_EQ_OPTION_MASK);
    Ok(Value::bool(same_source && same_options))
}

/// ### Regexp#hash
/// Returns a hash code based on the source pattern and `m`/`i`/`x`
/// options. Two regexps that are `==` produce the same hash.
#[monoruby_builtin]
fn regexp_hash(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    use std::hash::{Hash, Hasher};
    let self_ = lfp.self_val();
    let re = self_.as_regexp_inner();
    let mut h = std::collections::hash_map::DefaultHasher::new();
    re.as_str().hash(&mut h);
    (re.raw_option() & REGEXP_EQ_OPTION_MASK).hash(&mut h);
    Ok(Value::integer(h.finish() as i64))
}

/// ### Regexp#===
/// - self === string -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/i/=3d=3d=3d.html]
#[monoruby_builtin]
fn teq(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let regex = self_.is_regex().unwrap();
    let given = match lfp.arg(0).expect_symbol_or_string(globals) {
        Ok(s) => s.to_string(),
        Err(_) => return Ok(Value::bool(false)),
    };
    let res = Value::bool(regex.find_one(vm, &given)?.is_some());
    Ok(res)
}

///
/// ### Regexp#=~
/// - self =~ string -> Integer | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/i/=3d=7e.html]
#[monoruby_builtin]
fn regexp_match(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    if lfp.arg(0).is_nil() {
        vm.clear_capture_special_variables();
        return Ok(Value::nil());
    }
    let self_ = lfp.self_val();
    let regex = self_.is_regex().unwrap();
    let given = lfp.arg(0).expect_symbol_or_string(globals)?.to_string();
    let res = match regex.find_one(vm, &given)? {
        Some(mat) => Value::integer(mat.start as i64),
        None => Value::nil(),
    };
    Ok(res)
}

///
/// Convert `i` to the position of the char in the string with `len` chars.
///
/// Return None if `i` is out of range.
///
fn conv_index(i: i64, len: usize) -> Option<usize> {
    if i >= 0 {
        if i <= len as i64 {
            Some(i as usize)
        } else {
            None
        }
    } else {
        match len as i64 + i {
            n if n < 0 => None,
            n => Some(n as usize),
        }
    }
}

///
/// ### Regexp#source
/// - source -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/i/source.html]
#[monoruby_builtin]
fn source(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    Ok(Value::string_from_str(self_.is_regex().unwrap().as_str()))
}

///
/// ### Regexp#options
/// - options -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/i/options.html]
#[monoruby_builtin]
fn options(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let regexp = self_.is_regex().unwrap();
    Ok(Value::integer(regexp.option() as i64))
}

///
/// ### Regexp#match?
/// - match?(str, pos = 0) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/i/match=3f.html]
#[monoruby_builtin]
fn match_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let regex = self_.is_regex().unwrap();
    let given = lfp.arg(0).expect_symbol_or_string(globals)?.to_string();
    let char_pos = if let Some(pos) = lfp.try_arg(1) {
        match conv_index(pos.coerce_to_int_i64(vm, globals)?, given.chars().count()) {
            Some(pos) => pos,
            None => return Ok(Value::bool(false)),
        }
    } else {
        0
    };
    let byte_pos = match given.char_indices().nth(char_pos) {
        Some((pos, _)) => pos,
        None => 0, //return Ok(Value::bool(false)),
    };
    Ok(Value::bool(
        regex.captures_from_pos(&given, byte_pos, vm)?.is_some(),
    ))
}

///
/// ### Regexp#match
///
/// - match(str, pos = 0) -> MatchData | nil
/// - [NOT SUPPORTED] match(str, pos = 0) {|m| ... } -> object | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/i/match=3f.html]
#[monoruby_builtin]
fn rmatch(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let regex = lfp.self_val().as_regexp();
    let heystack = lfp.arg(0).expect_symbol_or_string(globals)?.to_string();
    let char_pos = if let Some(pos) = lfp.try_arg(1) {
        match conv_index(
            pos.coerce_to_int_i64(vm, globals)?,
            heystack.chars().count(),
        ) {
            Some(pos) => pos,
            None => return Ok(Value::bool(false)),
        }
    } else {
        0
    };
    let byte_pos = match heystack.char_indices().nth(char_pos) {
        Some((pos, _)) => pos,
        None => 0, //return Ok(Value::bool(false)),
    };
    Ok(
        if let Some(captures) = regex.captures_from_pos(&heystack, byte_pos, vm)? {
            Value::new_matchdata(captures, &heystack, regex)
        } else {
            Value::nil()
        },
    )
}

#[cfg(test)]
mod tests {
    use crate::tests::*;
    #[test]
    fn regex() {
        run_test(r##"/abcde/xmi.to_s"##);
        run_test(r##"/abcde/xmi.inspect"##);
        run_test(r##"/abcde/.to_s"##);
        run_test(r##"/abcde/.inspect"##);
    }

    #[test]
    fn regexp_last_match() {
        run_test(
            r#"
        "ab".match(/(.)(.)/) 
        [Regexp.last_match[0], Regexp.last_match[1], Regexp.last_match[2], Regexp.last_match[3]]
        "#,
        );
        run_test(
            r#"
        "ab".match(/(.)(.)/) 
        [Regexp.last_match(0), Regexp.last_match(1), Regexp.last_match(2), Regexp.last_match(3), Regexp.last_match(-1), Regexp.last_match(-2), Regexp.last_match(-10)]
        "#,
        );
    }

    #[test]
    fn last_match1() {
        run_test(
            r#"
          /(.)(.)/ =~ "abcde"
          [Regexp.last_match(0), Regexp.last_match(1), Regexp.last_match(2), Regexp.last_match(3)]
            "#,
        );
    }

    #[test]
    fn last_match_nil_when_no_match() {
        // Regexp.last_match returns nil when the last match failed
        run_test(
            r#"
          /NOMATCH/ =~ "abc"
          Regexp.last_match
            "#,
        );
        // Regexp.last_match returns nil when no match has occurred yet
        run_test(
            r#"
          /DONTMATCH/.match("")
          Regexp.last_match
            "#,
        );
    }

    #[test]
    fn dollar_tilde_read_write() {
        // $~ reads match data after a regex match
        run_test(
            r#"
          "abc" =~ /(a)(b)/
          $~[0]
            "#,
        );
        // $~ = nil clears match data
        run_test(
            r#"
          "abc" =~ /(a)/
          $~ = nil
          $~
            "#,
        );
        // $~ is nil initially after a failed match
        run_test(
            r#"
          /NOMATCH/ =~ "abc"
          $~
            "#,
        );
    }

    #[test]
    fn last_match2() {
        run_test(
            r#"
          /(.)(.)/ =~ "abcde"
          #assert $', Regexp.post_match
          [$', $&, $1, $2, $3]
            "#,
        );
        run_test(
            r#"
          /(.)(.)/ =~ :abcde
          [$', $&, $1, $2, $3]
            "#,
        );
        run_test(
            r#"
          /(.)(.)/ =~ nil
          [$', $&, $1, $2, $3]
            "#,
        );
    }

    #[test]
    fn union() {
        run_test(r##"Regexp.union(/g/i, "a(b)[c]d", /bbbb/x, /cccc/m).to_s"##);
    }

    #[test]
    fn regexp_options() {
        run_test(r##"/foo/.options"##);
        run_test(r##"/foo/i.options"##);
        run_test(r##"/foo/m.options"##);
        run_test(r##"/foo/x.options"##);
        run_test(r##"/foo/mix.options"##);
    }

    #[test]
    fn regexp1() {
        run_test(r#""abcdefg".gsub(/def/, "!!")"#);
        run_test(r#""2.5".gsub(".", ",")"#);
        run_test(r#""xbbgz-xbbbvzbbc".gsub(/(b+.z)(..)/) { $2 + $1.upcase }"#);
    }

    #[test]
    fn regexp_teq() {
        run_test(
            r#"
            res = /(aa).*(bb)/ === "andaadefbbje"
            [res, $&, $1, $2]
        "#,
        );
        run_test(
            r#"
            res = /(aa).*(bb)/ === :andaadefbbje
            [res, $&, $1, $2]
        "#,
        );
        run_test(
            r#"
            a = "HELLO"
            case a
            when /\A[a-z]*\z/
                "Lower case"
            when /\A[A-Z]*\z/
                "Upper case"
            else
                "Mixed case"
            end
        "#,
        );
    }

    #[test]
    fn regexp2() {
        run_test(r#""aaazzz" =~ /\172+/"#);
        run_test(r#"/foo/ =~ "foo""#);
        run_test(r#"/foo/ =~ "afoo""#);
        run_test(r#"/foo/ =~ "bar""#);
        run_test(
            r#"
            i = 123
            /ab#{i}cd/ =~ "ab123cd"
        "#,
        );
    }

    #[test]
    fn regexp3() {
        run_test(
            r#"
        a = /Ruby\Z/
        ["Ruby" =~ /Ruby\Z/, "Rubys" =~ /Ruby\Z/]
        "#,
        );
    }

    #[test]
    fn regexp_error1() {
        run_test_error(r#"/+/"#);
    }

    #[test]
    fn regexp_error2() {
        run_test_error(r#"Regexp.new("+")"#);
    }

    #[test]
    fn match_() {
        run_test(
            r#"
        res = [/R.../.match?("-Ruby")]
        for i in -6...6
            res << /R.../.match?("-Ruby", 0)
        end
        res
        "#,
        );
        run_test(
            r#"
        SEPARATOR_PAT = /#{Regexp.quote File::SEPARATOR}/
        /\A#{SEPARATOR_PAT}?\z/.match?("")
        "#,
        );
    }

    #[test]
    fn r#match() {
        run_test(r##"/(.).(.)/.match("foobar", 3).captures"##);
        run_test(r##"/(.).(.)/.match("foobar", -3).captures"##);
    }

    #[test]
    fn regexp_names() {
        // No named captures → empty array
        run_test(r##"/(foo)(bar)/.names"##);
        // Single named capture
        run_test(r##"/(?<a>foo)/.names"##);
        // Multiple named captures preserve declaration order
        run_test(r##"/(?<a>foo)(?<b>bar)/.names"##);
        // Mixed named + unnamed
        run_test(r##"/(?<a>foo)(bar)(?<c>baz)/.names"##);
        // Duplicate names appear once each in declaration order
        run_test(r##"/(?<a>foo)(?<a>bar)/.names"##);
    }

    #[test]
    fn regexp_eq_eql_hash() {
        // Same source + same options.
        run_test(r#"/abc/ == /abc/"#);
        run_test(r#"/abc/.eql?(/abc/)"#);
        run_test(r#"/abc/i == /abc/i"#);
        run_test(r#"/abc/i == /abc/"#);
        run_test(r#"/abc/.hash == /abc/.hash"#);
        run_test(r#"/abc/i.hash == /abc/i.hash"#);
        // Different source.
        run_test(r#"/abc/ == /abd/"#);
        // /n flag (NOENCODING) doesn't change == for ASCII source.
        run_test(r#"// == //n"#);
        run_test(r#"//.hash == //n.hash"#);
        // Non-Regexp argument is never equal.
        run_test(r#"/abc/ == "abc""#);
    }

    #[test]
    fn regexp_union_empty_and_special() {
        // Empty args → /(?!)/ (never matches).
        run_test(r#"Regexp.union == /(?!)/"#);
        // Single Regexp arg returned as-is (preserves identity via ==).
        run_test(r#"Regexp.union(/foo/) == /foo/"#);
        // Symbol accepted.
        run_test(r#"Regexp.union(:foo) == /foo/"#);
        // Strings escaped.
        run_test(r#"Regexp.union("n", ".") == /n|\./"#);
        // Single Array arg flattened.
        run_test(r#"Regexp.union(["+", "-"]) == /\+|\-/"#);
        run_test(r#"Regexp.union(["skiing", "sledding"]) == /skiing|sledding/"#);
    }

    #[test]
    fn regexp_new_from_regexp_preserves_options() {
        // `Regexp.new(/abc/i)` keeps both source and options.
        run_test(r#"Regexp.new(/abc/i) == /abc/i"#);
        run_test(r#"Regexp.new(/^hi{2,3}fo.o$/) == /^hi{2,3}fo.o$/"#);
    }

    #[test]
    fn regexp_new_string_flags() {
        // `i`, `m`, `x` accepted as a flag string.
        run_test(r#"Regexp.new("Hi", "i") == /Hi/i"#);
        run_test(r#"Regexp.new("Hi", "im") == /Hi/im"#);
        run_test(r#"(Regexp.new("Hi", "im").options & Regexp::IGNORECASE) != 0"#);
    }

    #[test]
    fn regexp_new_string_flag_x() {
        // The `x` (EXTENDED) flag in the string form sets EXTENDED
        // and ignores whitespace / `#`-comments inside the pattern.
        run_test(r#"Regexp.new("Hi", "x") == /Hi/x"#);
        run_test(r#"(Regexp.new("Hi", "x").options & Regexp::EXTENDED) != 0"#);
        // EXTENDED skips inline whitespace, so a pattern with spaces
        // still matches a no-space subject (returns the match position).
        run_test(r#"Regexp.new("a b c", "x") =~ "abc""#);
        // Combined with other flags.
        run_test(r#"Regexp.new("Hi", "ix") == /Hi/ix"#);
        run_test(r#"Regexp.new("Hi", "mix") == /Hi/mix"#);
    }

    #[test]
    fn regexp_new_invalid_flag_string_raises() {
        // `e` is *not* accepted in the string-form flag arg.
        run_test_error(r#"Regexp.new("Hi", "e")"#);
        run_test_error(r#"Regexp.new("Hi", "z")"#);
    }

    #[test]
    fn regexp_new_unexpected_option_warns_and_treats_as_truthy() {
        // A non-Integer / non-String / non-bool / non-nil second
        // argument writes a `warning: expected true or false as
        // ignorecase: <inspect>` line to `$stderr` and treats the
        // argument as truthy → IGNORECASE.
        //
        // The assertion compares the resulting Regexp to `/Hi/i` so
        // the truthy-coercion is observable; the warning text itself
        // ends up on the test's stderr stream and is verified
        // indirectly by the spec battery.
        run_test(r#"Regexp.new("Hi", Object.new) == /Hi/i"#);
        run_test(r#"Regexp.new("Hi", []) == /Hi/i"#);
        run_test(r#"Regexp.new("Hi", :sym) == /Hi/i"#);
        // Even an Object that masquerades as falsey via `to_s` still
        // triggers the warning + IGNORECASE path.
        run_test(
            r#"
              o = Object.new
              def o.inspect; "fake"; end
              Regexp.new("Hi", o) == /Hi/i
            "#,
        );
    }

    #[test]
    fn regexp_inspect_escapes_slashes_and_n_flag() {
        // `/` not already escaped is escaped in inspect.
        run_test(r#"Regexp.new("/foo/bar").inspect"#);
        run_test(r#"Regexp.new("//").inspect"#);
        // Already-escaped `\/` is not double-escaped.
        run_test(r#"/\/foo\/bar/.inspect"#);
        // `n` flag (NOENCODING) appears in inspect output.
        run_test(r#"//n.inspect"#);
        run_test(r#"//nixm.inspect"#);
    }
}
