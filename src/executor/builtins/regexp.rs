use crate::{value::rvalue::regexp::RegexpInfo, *};

//
// Regexp class
//

pub(crate) fn init(globals: &mut Globals) {
    globals.define_builtin_singleton_func(REGEXP_CLASS, "new", regexp_new, 1);
    globals.define_builtin_singleton_func(REGEXP_CLASS, "compile", regexp_new, 1);
    globals.define_builtin_singleton_func(REGEXP_CLASS, "escape", regexp_escape, 1);
    globals.define_builtin_singleton_func(REGEXP_CLASS, "quote", regexp_escape, 1);
    globals.define_builtin_singleton_func(REGEXP_CLASS, "last_match", regexp_last_match, -1);
    // globals.define_builtin_func(REGEXP_CLASS, "=~", regexp_match, 1);
}

// Class methods

/// ### Regexp.new
/// - new(string, option=nil, code=nil) -> Regexp
/// - compile(string, option=nil, code=nil) -> Regexp
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/s/compile.html]
extern "C" fn regexp_new(
    _vm: &mut Executor,
    globals: &mut Globals,
    _self_val: Value,
    arg: Arg,
    _len: usize,
    _block: Option<BlockHandler>,
) -> Option<Value> {
    let arg0 = arg[0];
    let string = arg0.expect_string(globals)?;
    let regexp = RegexpInfo::from_string(globals, string)?;
    let val = Value::new_regexp(regexp);
    Some(val)
}

/// ### Regexp.new
/// - escape(string) -> String
/// - quote(string) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/s/escape.html]
extern "C" fn regexp_escape(
    _vm: &mut Executor,
    globals: &mut Globals,
    _self_val: Value,
    arg: Arg,
    _len: usize,
    _block: Option<BlockHandler>,
) -> Option<Value> {
    let arg0 = arg[0];
    let string = arg0.expect_string(globals)?;
    let val = Value::new_string(regex::escape(&string));
    Some(val)
}

/// ### Regexp.last_match
/// - last_match -> MatchData (not supported)
/// - last_match(nth) -> String | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Regexp/s/last_match.html]
extern "C" fn regexp_last_match(
    vm: &mut Executor,
    globals: &mut Globals,
    _self_val: Value,
    arg: Arg,
    len: usize,
    _block: Option<BlockHandler>,
) -> Option<Value> {
    globals.check_number_of_arguments(len, 0..=1)?;
    if len == 0 {
        Some(vm.get_last_match())
    } else {
        let nth = arg[0].coerce_to_fixnum(globals)?;
        if nth >= 0 {
            Some(vm.get_special_matches(nth as usize))
        } else {
            None
        }
    }
}

/*
// Instance methods
fn regexp_match(vm: &mut VM, self_val: Value, args: &Args2) -> VMResult {
    args.check_args_num(1)?;
    let mut args0 = vm[0];
    let regex = self_val.as_regexp().unwrap();
    let given = args0.expect_string("1st Arg")?;
    let res = match RegexpInfo::find_one(vm, &regex, given).unwrap() {
        Some(mat) => Value::integer(mat.start() as i64),
        None => Value::nil(),
    };
    return Ok(res);
}
*/

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
        [Regexp.last_match(0), Regexp.last_match(1), Regexp.last_match(2), Regexp.last_match(3)]
        "#,
        );
    }
    /*     #[test]
        fn last_match() {
            let program = r#"
          /(.)(.)/ =~ "abcde"
          #Regexp.last_match      # => #<MatchData:0x4599e58>
          assert "ab", Regexp.last_match(0)  # => "ab"
          assert "cde", $'
          assert "a", Regexp.last_match(1)   # => "a"
          assert "b", Regexp.last_match(2)   # => "b"
          assert nil, Regexp.last_match(3)   # => nil
          #assert $', Regexp.post_match
          assert $&, Regexp.last_match(0)
          assert $1, Regexp.last_match(1)
          assert $2, Regexp.last_match(2)
          assert $3, Regexp.last_match(3)
      "#;
            assert_script(program);
        }

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
        fn regexp2() {
            let program = r#"
          assert 3, "aaazzz" =~ /\172+/
          assert 0, /foo/ =~ "foo"  # => 0
          assert 1, /foo/ =~ "afoo" # => 1
          assert nil, /foo/ =~ "bar"  # => nil
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
