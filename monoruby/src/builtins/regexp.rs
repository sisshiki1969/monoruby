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
    globals.define_builtin_func(REGEXP_CLASS, "source", source, 0);
    globals.define_builtin_func_with(REGEXP_CLASS, "match?", match_, 1, 2, false);
}

// Class methods

///
/// ### Regexp.new
/// - new(string, [NOT SUPPORTED] option=nil, [NOT SUPPORTED] code=nil) -> Regexp
/// - compile(string, [NOT SUPPORTED] option=nil, [NOT SUPPORTED] code=nil) -> Regexp
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/s/compile.html]
#[monoruby_builtin]
fn regexp_new(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let arg0 = lfp.arg(0);
    let string = arg0.expect_string()?;
    let option = if let Some(option) = lfp.try_arg(1) {
        if let Some(option) = option.try_fixnum() {
            option as i32 as u32
        } else if option.as_bool() {
            onigmo_regex::ONIG_OPTION_IGNORECASE
        } else {
            onigmo_regex::ONIG_OPTION_NONE
        }
    } else {
        onigmo_regex::ONIG_OPTION_NONE
    };
    let regexp = RegexpInner::with_option(string, option)?;
    let val = Value::regexp(regexp);
    Ok(val)
}

///
/// ### Regexp.escape
/// - escape(string) -> String
/// - quote(string) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/s/escape.html]
#[monoruby_builtin]
fn regexp_escape(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let arg0 = lfp.arg(0);
    let string = arg0.expect_str()?;
    let val = Value::string(RegexpInner::escape(string));
    Ok(val)
}

///
/// ### Regexp.union
/// - union(*pattern) -> Regexp
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/s/union.html]
#[monoruby_builtin]
fn regexp_union(_vm: &mut Executor, _: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut rest = lfp.arg(0).as_array();
    let mut v = vec![];
    if rest.len() == 1
        && let Some(arg) = rest[0].try_array_ty()
    {
        rest = arg;
    }
    for arg in rest.iter() {
        if let Some(s) = arg.is_str() {
            v.push(RegexpInner::escape(s));
        } else if let Some(re) = arg.is_regex() {
            v.push(re.tos());
        } else {
            return Err(MonorubyErr::no_implicit_conversion(*arg, STRING_CLASS));
        }
    }
    let s = v.join("|");

    Ok(Value::regexp(RegexpInner::with_option(s, 0)?))
}

///
/// ### Regexp.last_match
/// - last_match -> MatchData (not supported)
/// - last_match(nth) -> String | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/s/last_match.html]
#[monoruby_builtin]
fn regexp_last_match(vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    if let Some(arg0) = lfp.try_arg(0) {
        let nth = arg0.coerce_to_i64()?;
        Ok(vm.get_special_matches(nth))
    } else {
        Ok(vm.get_last_matchdata())
    }
}

/// ### Regexp#===
/// - self === string -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/i/=3d=3d=3d.html]
#[monoruby_builtin]
fn teq(vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_ = lfp.self_val();
    let regex = self_.is_regex().unwrap();
    let given = match lfp.arg(0).expect_symbol_or_string() {
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
fn regexp_match(vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    if lfp.arg(0).is_nil() {
        vm.clear_capture_special_variables();
        return Ok(Value::nil());
    }
    let self_ = lfp.self_val();
    let regex = self_.is_regex().unwrap();
    let given = lfp.arg(0).expect_symbol_or_string()?.to_string();
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
fn source(_: &mut Executor, _: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_ = lfp.self_val();
    Ok(Value::string_from_str(self_.is_regex().unwrap().as_str()))
}

///
/// ### Regexp#match?
/// - match?(str, pos = 0) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/i/match=3f.html]
#[monoruby_builtin]
fn match_(vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let self_ = lfp.self_val();
    let regex = self_.is_regex().unwrap();
    let given_val = lfp.arg(0);
    let given = given_val.expect_str()?;
    let char_pos = if let Some(pos) = lfp.try_arg(1) {
        match conv_index(pos.expect_integer()?, given.chars().count()) {
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
        regex.captures_from_pos(given, byte_pos, vm)?.is_some(),
    ))
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
}
