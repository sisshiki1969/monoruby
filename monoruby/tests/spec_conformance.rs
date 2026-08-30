//! Conformance details ruby/spec's `core` suite caught: keywords
//! documented as booleans that were read as truthy, a `try_convert` that
//! swallowed every error, and two index conversions that raised the wrong
//! class.

extern crate monoruby;
use monoruby::tests::*;

/// `exception:` and `highlight:` take `true` or `false` and nothing else
/// — `rb_bool_expected`. A truthiness test quietly accepted `0` and
/// `"false"`, which CRuby rejects.
#[test]
fn boolean_keywords_are_strict() {
    run_test(
        r##"
        res = []
        [0, "false", nil, :sym].each do |bad|
          [-> { Integer(1, exception: bad) },
           -> { Float(1, exception: bad) },
           -> { Rational(1, exception: bad) },
           -> { Complex(1, exception: bad) },
           -> { StandardError.new("x").detailed_message(highlight: bad) },
           -> { StandardError.new("x").full_message(highlight: bad) }].each do |probe|
            begin
              probe.call
              res << :no_error
            rescue ArgumentError => e
              res << e.message
            end
          end
        end
        res
        "##,
    );
    // The boolean values themselves still work, and so does leaving the
    // keyword out.
    run_test(
        r##"
        [Integer("1", exception: true), Integer("z", exception: false),
         Float("1.5", exception: true), Float("z", exception: false),
         Rational("1", exception: true), Rational("z", exception: false),
         Complex("1", exception: true), Complex("z", exception: false),
         Integer("1"), Float("1.5"),
         StandardError.new("x").detailed_message(highlight: false),
         StandardError.new("x").detailed_message(highlight: true),
         StandardError.new("x").detailed_message]
        "##,
    );
    // `system` is the exception to the rule: there, `nil` is the default.
    run_test(
        r##"
        res = []
        [1, "true"].each do |bad|
          begin
            system("true", exception: bad)
            res << :no_error
          rescue ArgumentError => e
            res << e.message
          end
        end
        res
        "##,
    );
}

/// `Warning.warn` writes nothing for a category that is switched off, and
/// still writes for one that is on — `rb_warning_s_warn` checks the
/// category before writing anything.
#[test]
fn warning_categories_gate_the_default_warn() {
    run_test_once(
        r##"
        require "stringio"
        def capture
          old = $stderr
          $stderr = StringIO.new
          yield
          $stderr.string
        ensure
          $stderr = old
        end
        res = []
        Warning[:deprecated] = false
        res << capture { Warning.warn("off\n", category: :deprecated) }
        Warning[:deprecated] = true
        res << capture { Warning.warn("on\n", category: :deprecated) }
        Warning[:deprecated] = false
        # No category at all always writes.
        res << capture { Warning.warn("plain\n") }
        # An unknown category, and a non-Symbol one, are errors.
        [:no_such_category, "deprecated"].each do |bad|
          begin
            Warning.warn("x", category: bad)
            res << :no_error
          rescue => e
            res << [e.class.to_s, e.message]
          end
        end
        res
        "##,
    );
}

/// `String.try_convert` answers `nil` only for an object with no
/// `#to_str`. A `#to_str` that returns a non-String is a TypeError, and
/// one that raises propagates — both used to come back as `nil`.
#[test]
fn string_try_convert_reports_a_bad_to_str() {
    run_test(
        r##"
        no_to_str = Object.new
        bad = Object.new
        def bad.to_str = 42
        raiser = Object.new
        def raiser.to_str = raise("boom")
        good = Object.new
        def good.to_str = "converted"
        [String.try_convert(no_to_str),
         String.try_convert("already"),
         String.try_convert(good),
         (begin; String.try_convert(bad); rescue TypeError => e; e.message; end),
         (begin; String.try_convert(raiser); rescue RuntimeError => e; e.message; end)]
        "##,
    );
}

/// An index too large for a machine word is a RangeError, whatever
/// container it indexes — `String#[]` reported it as a conversion error
/// about the class instead.
#[test]
fn out_of_range_indices_raise_range_error() {
    run_test(
        r##"
        big = 2 ** 64
        probes = [-> { "hello"[big] }, -> { "hello"[big, 1] }, -> { "hello"[-big, 1] },
                  -> { "hello".slice(big) }, -> { [1, 2][big] }]
        probes.map do |p|
          begin
            p.call
          rescue RangeError => e
            [e.class.to_s, e.message]
          end
        end
        "##,
    );
    // The ordinary conversion error for a non-Integer index is unchanged.
    run_test_error(r#""hello"[Object.new]"#);
}

/// `String#bytesplice` distinguishes its two argument shapes: a Range
/// boundary out of range is a RangeError naming the range, while the
/// (index, length) form keeps IndexError.
#[test]
fn bytesplice_range_boundaries() {
    run_test(
        r##"
        probes = [-> { "hello".bytesplice(-6...-6, "xxx") },
                  -> { "hello".bytesplice(0..1, "HELLO", -6...-6) },
                  -> { "hello".bytesplice(-6, 1, "xxx") }]
        errs = probes.map do |p|
          begin
            p.call
          rescue => e
            [e.class.to_s, e.message]
          end
        end
        # The shapes that are in range still splice.
        [errs, "hello".bytesplice(0..1, "xx"), "hello".bytesplice(1..2, "HELLO", 0..1),
         "hello".bytesplice(1..2, "HELLO", -5...-5), "hello".bytesplice(0...10, "x")]
        "##,
    );
}
