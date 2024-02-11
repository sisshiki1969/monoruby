use super::*;

//
// Regexp class
//

pub(crate) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Regexp", REGEXP_CLASS);
    globals.define_builtin_class_func(REGEXP_CLASS, "new", regexp_new, 1);
    globals.define_builtin_class_func(REGEXP_CLASS, "compile", regexp_new, 1);
    globals.define_builtin_class_func(REGEXP_CLASS, "escape", regexp_escape, 1);
    globals.define_builtin_class_func(REGEXP_CLASS, "quote", regexp_escape, 1);
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
}

// Class methods

///
/// ### Regexp.new
/// - new(string, [NOT SUPPORTED] option=nil, [NOT SUPPORTED] code=nil) -> Regexp
/// - compile(string, [NOT SUPPORTED] option=nil, [NOT SUPPORTED] code=nil) -> Regexp
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/s/compile.html]
#[monoruby_builtin]
fn regexp_new(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    lfp.check_number_of_arguments(1)?;
    let arg0 = lfp.arg(0);
    let string = arg0.expect_string()?;
    let regexp = RegexpInner::from_string(globals, string)?;
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
fn regexp_escape(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    let arg0 = lfp.arg(0);
    let string = arg0.expect_string()?;
    let val = Value::string(regex::escape(&string));
    Ok(val)
}

///
/// ### Regexp.last_match
/// - last_match -> MatchData (not supported)
/// - last_match(nth) -> String | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/s/last_match.html]
#[monoruby_builtin]
fn regexp_last_match(vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    let len = lfp.arg_len();
    if len == 0 {
        Ok(vm.get_last_matchdata())
    } else {
        let nth = lfp.arg(0).coerce_to_i64()?;
        Ok(vm.get_special_matches(nth))
    }
}

/// ### Regexp#===
/// - self === string -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/i/=3d=3d=3d.html]
#[monoruby_builtin]
fn teq(vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    let self_ = lfp.self_val();
    let regex = self_.is_regex().unwrap();
    let given = match lfp.arg(0).expect_symbol_or_string() {
        Ok(s) => s.to_string(),
        Err(_) => return Ok(Value::bool(false)),
    };
    let res = Value::bool(RegexpInner::find_one(vm, regex, &given)?.is_some());
    Ok(res)
}

///
/// ### Regexp#=~
/// - self =~ string -> Integer | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/i/=3d=7e.html]
#[monoruby_builtin]
fn regexp_match(vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _: Arg) -> Result<Value> {
    if lfp.arg(0).is_nil() {
        return Ok(Value::nil());
    }
    let self_ = lfp.self_val();
    let regex = self_.is_regex().unwrap();
    let given = lfp.arg(0).expect_symbol_or_string()?.to_string();
    let res = match RegexpInner::find_one(vm, regex, &given)? {
        Some(mat) => Value::integer(mat.start() as i64),
        None => Value::nil(),
    };
    Ok(res)
}

#[cfg(test)]
mod test {
    use crate::tests::*;
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
}
