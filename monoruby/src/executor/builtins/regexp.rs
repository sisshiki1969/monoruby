use crate::*;

//
// Regexp class
//

pub(crate) fn init(globals: &mut Globals) {
    globals.define_builtin_class_func(REGEXP_CLASS, "new", regexp_new, 1);
    globals.define_builtin_class_func(REGEXP_CLASS, "compile", regexp_new, 1);
    globals.define_builtin_class_func(REGEXP_CLASS, "escape", regexp_escape, 1);
    globals.define_builtin_class_func(REGEXP_CLASS, "quote", regexp_escape, 1);
    globals.define_builtin_class_func(REGEXP_CLASS, "last_match", regexp_last_match, -1);
    globals.define_builtin_func(REGEXP_CLASS, "=~", regexp_match, 1);
}

// Class methods

/// ### Regexp.new
/// - new(string, option=nil, code=nil) -> Regexp
/// - compile(string, option=nil, code=nil) -> Regexp
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/s/compile.html]
#[monoruby_builtin]
fn regexp_new(
    _vm: &mut Executor,
    globals: &mut Globals,
    _lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Result<Value> {
    let arg0 = arg[0];
    let string = arg0.expect_string(globals)?;
    let regexp = RegexpInner::from_string(globals, string)?;
    let val = Value::new_regexp(regexp);
    Ok(val)
}

/// ### Regexp.new
/// - escape(string) -> String
/// - quote(string) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/s/escape.html]
#[monoruby_builtin]
fn regexp_escape(
    _vm: &mut Executor,
    globals: &mut Globals,
    _lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Result<Value> {
    let arg0 = arg[0];
    let string = arg0.expect_string(globals)?;
    let val = Value::new_string(regex::escape(&string));
    Ok(val)
}

/// ### Regexp.last_match
/// - last_match -> MatchData (not supported)
/// - last_match(nth) -> String | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/s/last_match.html]
#[monoruby_builtin]
fn regexp_last_match(
    vm: &mut Executor,
    globals: &mut Globals,
    _lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    Executor::check_number_of_arguments(len, 0..=1)?;
    if len == 0 {
        Ok(vm.get_last_matchdata())
    } else {
        let nth = arg[0].coerce_to_fixnum(globals)?;
        Ok(vm.get_special_matches(nth))
    }
}

/// ### Regexp#=~
/// - self =~ string -> Integer | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/i/=3d=7e.html]
#[monoruby_builtin]
fn regexp_match(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Result<Value> {
    let self_ = lfp.self_val();
    let regex = self_.is_regex().unwrap();
    let given = arg[0].expect_string(globals)?;
    let res = match RegexpInner::find_one(vm, regex, &given)? {
        Some(mat) => Value::new_integer(mat.start() as i64),
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
    }

    #[test]
    fn regexp2() {
        //run_test(r#""aaazzz" =~ /\172+/"#);
        run_test(r#"/foo/ =~ "foo""#);
        run_test(r#"/foo/ =~ "afoo""#);
        run_test(r#"/foo/ =~ "bar""#);
    }

    /*
        #[test]
        fn regexp1() {
            let program = r#"
          assert "abc!!g", "abcdefg".gsub(/def/, "!!")
          assert "2.5".gsub(".", ","), "2,5"
          assert true, /(aa).*(bb)/ === "andaadefbbje"
          assert "aadefbb", $&
          assert "aa", $1
          assert "bb", $2
          assert 4, "The cat sat in the hat" =~ /[csh](..) [csh]\1 in/
          assert "x-xBBGZbbBBBVZc", "xbbgz-xbbbvzbbc".gsub(/(b+.z)(..)/) { $2 + $1.upcase }
      "#;
            assert_script(program);
        }


        #[test]
        fn regexp3() {
            let program = r#"
          a = /Ruby\Z/
          assert 0, "Ruby" =~ /Ruby\Z/
          assert nil, "Rubys" =~ /Ruby\Z/
          "#;
            assert_script(program);
        }
    */
    #[test]
    fn regexp_error1() {
        run_test_error(r#"/+/"#);
    }

    #[test]
    fn regexp_error2() {
        run_test_error(r#"Regexp.new("+")"#);
    }
}