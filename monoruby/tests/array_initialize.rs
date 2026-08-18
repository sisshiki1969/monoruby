extern crate monoruby;
use monoruby::tests::*;

// In-process coverage of the Ruby Array#initialize and its native legs
// (__init_fill / __init_from / __size_to_int, Kernel#__warn_caller).
// The CLI parity matrix runs out of process; these run under the
// instrumented harness.

#[test]
fn array_new_forms() {
    run_test(
        r#"
        o = Object.new
        def o.to_ary = [4, 5]
        # to_ary returning nil falls through to the size path (#to_int).
        n = Object.new
        def n.to_ary = nil
        def n.to_int = 2
        [
          Array.new,
          Array.new(3),
          Array.new(3, :x),
          Array.new(3) { |i| i * 2 },
          Array.new([1, 2, 3]),
          Array.new(o),
          Array.new(n),
          Array.new(2.7),
        ]
        "#,
    );
}

#[test]
fn array_new_error_paths() {
    run_test(
        r#"
        r = []
        begin; Array.new(-1); rescue ArgumentError => e; r << e.message; end
        begin; Array.new(-1) { |i| i }; rescue ArgumentError => e; r << e.message; end
        begin; Array.new("x"); rescue TypeError => e; r << e.message; end
        bad = Object.new
        def bad.to_ary = 42
        begin; Array.new(bad); rescue TypeError => e; r << e.class; end
        r
        "#,
    );
}

#[test]
fn array_new_subclass_and_integer_to_ary_gate() {
    // CRuby's !FIXNUM_P gate: Integer#to_ary never hijacks a sized
    // construction; a subclass allocates through the same trampoline.
    run_test(
        r#"
        class AIMyArr < Array; end
        class Integer
          def to_ary = [:hijacked]
        end
        a = AIMyArr.new(2, 7)
        [Array.new(5), Array.new(2, :v), a.class, a]
        "#,
    );
}

#[test]
fn init_fill_guards_against_send_bypass() {
    // `send` bypasses privacy, so __init_fill's own size guards are
    // load-bearing even though the Ruby initialize checks first: a raw
    // negative or absurd size must raise, not allocate.
    run_test_error("[].send(:__init_fill, -1, nil)");
    run_test_error("[].send(:__init_fill, 1 << 40, nil)");
}

#[test]
fn array_new_warning_levels() {
    // "given block not used" is rb_warning (verbose-only); "block
    // supersedes default value argument" is rb_warn (default level,
    // silent under -W0). The location prefix is stripped: monoruby and
    // the CRuby oracle run this code under different file names.
    run_test(
        r#"
        require 'stringio'
        def capw
          $stderr = StringIO.new
          yield
          s = $stderr.string
          $stderr = STDERR
          s.lines.map { |l| l[/warning: .*/] }
        end
        with_verbose = lambda do |v, &blk|
          old = $VERBOSE
          $VERBOSE = v
          begin
            capw(&blk)
          ensure
            $VERBOSE = old
          end
        end
        [
          with_verbose.call(false) { Array.new { 1 } },
          with_verbose.call(true) { Array.new { 1 } },
          with_verbose.call(false) { Array.new(2, :v) { 3 } },
          with_verbose.call(true) { Array.new(2, :v) { 3 } },
          with_verbose.call(nil) { Array.new(2, :v) { 3 } },
          with_verbose.call(false) { Array.new([1]) { 2 } },
        ]
        "#,
    );
}
