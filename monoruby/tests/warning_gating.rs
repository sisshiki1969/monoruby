extern crate monoruby;
use monoruby::tests::*;

// Onigmo's compile-time diagnostics ("character class has duplicated
// range", "nested repeat operator") go through CRuby's `rb_warning`, so
// they print only when `$VERBOSE` is `true` (`-w`); a plain run and
// `-W0` stay silent. They are flushed through the current `$stderr`.

#[test]
fn onigmo_warnings_are_verbose_only() {
    run_test_once(
        r#"
        require "stringio"
        def cap; $stderr = StringIO.new; yield; s = $stderr.string; $stderr = STDERR; s; end
        r = []
        r << cap { Regexp.new("[aa]") }
        r << cap { v = $VERBOSE; $VERBOSE = nil; Regexp.new("[bb]"); $VERBOSE = v }
        r << cap { v = $VERBOSE; $VERBOSE = false; Regexp.new("[cc]"); $VERBOSE = v }
        r << cap { v = $VERBOSE; $VERBOSE = true; Regexp.new("[dd]"); $VERBOSE = v }.sub(/\A.*?warning/, "warning")
        r << cap { v = $VERBOSE; $VERBOSE = true; Regexp.new("a**"); $VERBOSE = v }.sub(/\A.*?warning/, "warning")
        r << cap { v = $VERBOSE; $VERBOSE = true; eval('/[ee]/'); $VERBOSE = v }.sub(/\A.*?warning/, "warning")
        r << cap { v = $VERBOSE; $VERBOSE = true; Regexp.new("[a-z]"); $VERBOSE = v }
        r
        "#,
    );
}

// `rb_warn`-level warnings ("already initialized constant", a duplicated
// hash key) print unless `$VERBOSE` is `nil`: rubocop-rails redefines a
// rubocop constant inside `$VERBOSE = nil` to do it silently.

#[test]
fn constant_reinitialization_warning_is_silent_under_verbose_nil() {
    run_test_once(
        r#"
        require "stringio"
        def cap; $stderr = StringIO.new; yield; s = $stderr.string; $stderr = STDERR; s; end
        class A; X = 1; end
        r = []
        r << cap { v = $VERBOSE; $VERBOSE = nil; A.const_set(:X, 2); A::X = 3; Struct.new("X"); $VERBOSE = v }
        r << cap { v = $VERBOSE; $VERBOSE = false; A.const_set(:X, 4); $VERBOSE = v }.sub(/\A.*?warning/, "warning").lines.first
        r << cap { v = $VERBOSE; $VERBOSE = true; A.const_set(:X, 5); $VERBOSE = v }.sub(/\A.*?warning/, "warning").lines.first
        r << cap { v = $VERBOSE; $VERBOSE = nil; eval("h = {a: 1, a: 2}"); $VERBOSE = v }
        r << A::X
        r
        "#,
    );
}
