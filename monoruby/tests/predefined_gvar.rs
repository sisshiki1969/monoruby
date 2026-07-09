extern crate monoruby;
use monoruby::tests::*;

#[test]
fn read_only_gvar_message() {
    run_test_once(
        r#"
        res = []
        [proc { $: = [] }, proc { $" = [] }, proc { $-I = [] },
         proc { $? = 1 }, proc { $FILENAME = "x" }, proc { $< = 1 },
         proc { $-a = 1 }, proc { $-l = 1 }, proc { $-p = 1 }].each do |blk|
          begin
            blk.call
            res << "no error"
          rescue NameError => e
            res << e.message
          end
        end
        res
        "#,
    );
}

#[test]
fn record_separator_validation() {
    run_test_once(
        r#"
        res = []
        begin; $/ = 1; rescue TypeError => e; res << e.message; end
        begin; $, = :sym; rescue TypeError => e; res << e.message; end
        begin; $; = 1.0; rescue TypeError => e; res << e.message; end
        $; = /,/
        res << $;.class
        $; = nil
        begin; $\ = 42; rescue TypeError => e; res << e.message; end
        $/ = "abc"
        res << $/.frozen? << $/ << $-0
        $-0 = "xyz"
        res << $/
        $/ = "\n"
        str = Class.new(String).new("sub")
        $/ = str
        res << $/.instance_of?(String) << ($/ == "sub")
        $/ = "\n"
        s = +"zz"
        $\ = s
        res << $\.equal?(s)
        $\ = nil
        res
        "#,
    );
}

#[test]
fn last_line_number_coerces_with_to_int() {
    run_test_once(
        r#"
        res = [$.]
        $. = 123.5
        res << $.
        obj = Object.new
        def obj.to_int = 321
        $. = obj
        res << $.
        begin; $. = Object.new; rescue TypeError; res << "TypeError"; end
        $. = 0
        res
        "#,
    );
}

#[test]
fn stdout_must_have_write_method() {
    run_test_once(
        r#"
        res = []
        begin; $stdout = nil; rescue TypeError => e; res << e.message; end
        begin; $stderr = 42; rescue TypeError => e; res << e.message; end
        res
        "#,
    );
}

#[test]
fn verbose_normalizes_truthy_to_true() {
    run_test_once(
        r#"
        saved = $VERBOSE
        $VERBOSE = 1
        res = [$VERBOSE]
        $VERBOSE = nil
        res << $VERBOSE
        $VERBOSE = false
        res << $VERBOSE
        $VERBOSE = saved
        res
        "#,
    );
}

#[test]
fn debug_flag_alias() {
    run_test_once(
        r#"
        res = [$DEBUG, $-d]
        $DEBUG = true
        res << $-d
        $DEBUG = false
        res
        "#,
    );
}

#[test]
fn match_data_assignment() {
    run_test_once(
        r#"
        res = []
        begin; $~ = Object.new; rescue TypeError => e; res << e.message; end
        begin; $~ = 1; rescue TypeError => e; res << e.message; end
        "foo" =~ /(f)oo/
        m = $~
        "bar" =~ /(b)ar/
        res << $1
        $~ = m
        res << $1 << $& << $~.equal?(m)
        $~ = nil
        res << $~ << $1
        res
        "#,
    );
}

#[test]
fn errinfo_backtrace_gvar() {
    run_test_once(
        r#"
        res = [defined?($@), $@]
        begin
          $@ = ["x"]
        rescue ArgumentError => e
          res << e.message
        end
        begin
          raise "oops"
        rescue => err
          res << $@.instance_of?(Array)
          $@ = ["custom"]
          res << err.backtrace
          $@ = "single"
          res << err.backtrace
          begin; $@ = :sym; rescue TypeError; res << "TypeError"; end
          begin; $@ = [nil]; rescue TypeError; res << "TypeError2"; end
          begin; $@ = [["nested"]]; rescue TypeError; res << "TypeError3"; end
          $@ = nil
          res << err.backtrace
        end
        res
        "#,
    );
}

#[test]
fn load_path_aliases_share_identity() {
    run_test_once(
        r#"
        [$:.equal?($LOAD_PATH), $:.equal?($-I)]
        "#,
    );
}
