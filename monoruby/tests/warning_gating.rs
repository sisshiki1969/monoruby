extern crate monoruby;
use monoruby::tests::*;

// Onigmo's compile-time diagnostics ("character class has duplicated
// range", "without escape") are gated on `RTEST(ruby_verbose)` inside
// Onigmo, so they print only when `$VERBOSE` is `true` (`-w`); a plain
// run and `-W0` stay silent. They are flushed through the current
// `$stderr`.

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

// The nested-repeat diagnostics are not gated in Onigmo: CRuby's
// `rb_warn` prints them unless `$VERBOSE` is nil (ruby/spec
// language/regexp/repetition_spec.rb).

#[test]
fn onigmo_nested_repeat_warning_prints_by_default() {
    run_test_once(
        r#"
        require "stringio"
        def cap; $stderr = StringIO.new; yield; s = $stderr.string; $stderr = STDERR; s; end
        r = []
        r << cap { v = $VERBOSE; $VERBOSE = false; Regexp.new("foo(A{0,1}+)Abar"); $VERBOSE = v }.sub(/\A.*?warning/, "warning")
        r << cap { v = $VERBOSE; $VERBOSE = false; Regexp.new("b**"); $VERBOSE = v }.sub(/\A.*?warning/, "warning")
        r << cap { v = $VERBOSE; $VERBOSE = nil; Regexp.new("c**"); $VERBOSE = v }
        r
        "#,
    );
}

// Onigmo's diagnostics carry CRuby's `file:line: ` prefix (#1803): the
// literal's own line, or the line of the Ruby code that called
// `Regexp.new`. Locations are taken relative to the first line, since
// the harness and CRuby run the code under different file names.

#[test]
fn onigmo_warnings_carry_the_location() {
    run_test_once(
        r#"
        require "stringio"
        base = __LINE__
        rel = ->(s) { s.gsub(/#{Regexp.escape(__FILE__)}:(\d+)/) { "L#{$1.to_i - base}" } }
        def cap; $stderr = StringIO.new; yield; s = $stderr.string; $stderr = STDERR; s; end
        def make(s) = Regexp.new(s)
        class SubRe < Regexp; end
        class SubReInit < Regexp; def initialize(s) = super(s); end
        r = []
        r << rel.(cap { Regexp.new("a**") })
        r << rel.(cap { Regexp.compile("b**") })
        r << rel.(cap { make("c**") })
        r << rel.(cap { SubRe.new("d**") })
        r << rel.(cap { SubReInit.new("e**") })
        r << rel.(cap { 2.times { |i| /#{i}**/ } })
        r << rel.(cap { eval("/f**/") })
        r << rel.(cap { eval("x = 1\n\n/g\#{x}**/") })
        r << cap { eval("/h**/", nil, "lit.rb", 10) }
        r << rel.(cap { v = $VERBOSE; $VERBOSE = true; Regexp.new("[ii]"); $VERBOSE = v })
        r
        "#,
    );
}

// Reassigning a constant prints CRuby's pair (#1804): the assignment's
// location, then "previous definition of X was here" at the old
// constant's recorded location (none for a natively defined one).
// `Struct.new("Name")` says "redefining constant Struct::Name" instead.

#[test]
fn constant_reinitialization_warning_carries_the_locations() {
    run_test_once(
        r#"
        require "stringio"
        base = __LINE__
        rel = ->(s) { s.gsub(/#{Regexp.escape(__FILE__)}:(\d+)/) { "L#{$1.to_i - base}" } }
        def cap; $stderr = StringIO.new; yield; s = $stderr.string; $stderr = STDERR; s; end
        module WarnM
          Y = 1
        end
        class WarnK; end
        def set_x(v) = Object.const_set(:WarnX, v)
        r = []
        r << rel.(cap { WarnM::Y = 2 })
        r << rel.(cap { WarnM.const_set(:Y, 3) })
        r << rel.(cap { set_x(1); set_x(2) })
        r << rel.(cap { WarnK = 1 })
        r << rel.(cap { Float.const_set(:INFINITY, 1) })
        r << rel.(cap { Struct.new("WarnP"); Struct.new("WarnP") })
        r << rel.(cap { eval("WarnM::Y = 4", nil, "c.rb", 7) })
        r
        "#,
    );
}
