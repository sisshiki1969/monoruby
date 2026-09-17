extern crate monoruby;
use monoruby::tests::*;

// How a TypeError / ArgumentError names the offending value (#1379).
//
// CRuby has three conventions, and monoruby's messages follow each:
//
// * `rb_builtin_class_name` — "no implicit conversion of X into Y",
//   "can't convert X into Y", "wrong argument type X (expected Y)": nil,
//   true and false by keyword, everything else by class.
// * `coerce_failed` / `rb_cmperr` — "X can't be coerced into Y",
//   "comparison of R with X failed": a special constant (nil, true,
//   false, a Symbol, a small Integer, a Float) by `inspect`, so `:a`
//   and `1` appear literally; anything else by class.
// * `rb_check_id` — "X is not a symbol nor a string": always `inspect`.
//
// A few CRuby sites name the class regardless (`rb_obj_class`), and
// those are pinned here too so the helpers are not applied blindly.

/// "no implicit conversion of X into Y" / "can't convert X into Y" /
/// "wrong argument type X (expected Y)" name nil / true / false by
/// keyword — the issue's `"abc" + nil` and friends — and everything else
/// by class.
#[test]
fn implicit_conversion_names_nil_true_false_by_keyword() {
    run_test_once(
        r##"
        t = ->(&b) { begin; b.call; rescue => e; [e.class, e.message]; end }
        res = []
        res << t.() { "abc" + nil }
        res << t.() { "abc".concat(nil) }
        res << t.() { "abc".include?(nil) }
        res << t.() { "abc".start_with?(nil) }
        res << t.() { :abc.end_with?(nil) }
        res << t.() { "abc" + true }
        res << t.() { "abc" + false }
        res << t.() { "abc" + :s }
        res << t.() { "abc" + 1 }
        res << t.() { "abc" + Object.new }
        res << t.() { "abc" << true }
        res << t.() { "a".ljust(2, nil) }
        res << t.() { "abc".tr(nil, "x") }
        res << t.() { "abc".delete(false) }
        res << t.() { "abc".squeeze(true) }
        res << t.() { "abc".prepend(nil) }
        res << t.() { "abc".replace(true) }
        res << t.() { "abc".encode(nil) }
        res << t.() { "abc".force_encoding(true) }
        res << t.() { "abc".unpack(false) }
        res << t.() { "abc".byteindex(true) }
        res << t.() { [1].pack(nil) }
        res << t.() { [1].join(true) }
        res << t.() { File.join(nil) }
        res << t.() { File.exist?(true) }
        res << t.() { Dir.exist?(nil) }
        res << t.() { File.expand_path(false) }
        res << t.() { Object.const_get(nil) }
        res << t.() { Module.new.const_defined?(true) }
        res << t.() { [1] + nil }
        res << t.() { [1].concat(true) }
        res << t.() { [[1], nil].transpose }
        res << t.() { {}.merge(nil) }
        res << t.() { {}.update(true) }
        res << t.() { IO.select([nil]) }
        res << t.() { IO.select([true]) }
        res << t.() { "a".insert(true, "x") }
        res << t.() { Integer.sqrt(nil) }
        res << t.() { Integer.sqrt(true) }
        res << t.() { Kernel.Integer(true) }
        res << t.() { Kernel.Float(false) }
        res << t.() { Kernel.Complex(true) }
        res << t.() { Kernel.Rational(nil) }
        res << t.() { Math.sqrt(true) }
        res << t.() { 1.coerce(nil) }
        res << t.() { 1.coerce(true) }
        res << t.() { 1.coerce(:a) }
        res << t.() { 1.coerce(Object.new) }
        res << t.() { 1.5.coerce(nil) }
        res << t.() { 1.5.coerce(:a) }
        res << t.() { Object.include(nil) }
        res << t.() { Object.include(true) }
        res << t.() { Object.include(:a) }
        res << t.() { Object.prepend(nil) }
        res << t.() { Object.extend(false) }
        res << t.() { $~ = true }
        res << t.() { /(?<a>.)/.match("x").deconstruct_keys([nil]) }
        res << t.() { IO::Buffer.new(8) <=> nil }
        res << t.() { Warning[nil] }
        res << t.() { Math.ldexp(1.0, :a) }
        res << t.() { Math.ldexp(1.0, true) }
        res << t.() { Regexp.union(nil) }
        res << t.() { Regexp.union("a", true) }
        res << t.() { Regexp.escape(true) }
        m = /(?<a>.)/.match("x")
        res << t.() { m[true] }
        res << t.() { m[true, 1] }
        res << t.() { m[0, true] }
        res << t.() { m.match(true) }
        res << t.() { m.begin(true) }
        res << t.() { m.offset(Object.new) }
        require "socket"
        res << t.() { Addrinfo.tcp(true, 1) }
        res << t.() { Socket.new(:INET, :STREAM).connect(true) }
        res << t.() { Socket.new(:INET, :STREAM).bind(nil) }
        res << t.() { UNIXServer.new(true) }
        # A conversion method answering the wrong type is the mismatch
        # form, which names the class.
        class BadToStr
          def to_str; 1; end
        end
        class BadToA
          def to_a; 1; end
        end
        res << t.() { Module.new.const_get(BadToStr.new) }
        res << t.() { Array(BadToA.new) }
        res << t.() { Module.new.send(:refine, nil) {} }
        res << t.() { Module.new.send(:refine, true) {} }
        # CRuby names the class at these (`rb_obj_class`), keyword or not.
        res << t.() { Kernel.Hash(true) }
        res << t.() { Time.now.deconstruct_keys(true) }
        res << t.() { Struct.new(:a).new(1).deconstruct_keys(true) }
        res << t.() { [1].zip(nil) }
        res << t.() { (1..3).overlap?(nil) }
        res << t.() { Complex(1).coerce(nil) }
        res << t.() { Rational(1).coerce(nil) }
        res
        "##,
    );
}

/// "X can't be coerced into Y" and "comparison of R with X failed" name
/// the operand by `inspect` when it is a special constant (`nil`, `true`,
/// `:a`, `1`, `1.5`) and by class otherwise; the receiver of a
/// comparison is always its class.
#[test]
fn coerce_and_comparison_failures_inspect_special_constants() {
    run_test_once(
        r##"
        t = ->(&b) { begin; b.call; rescue => e; [e.class, e.message]; end }
        class Cmp
          include Comparable
          def <=>(o); nil; end
        end
        res = []
        res << t.() { 1 + nil }
        res << t.() { 1 + true }
        res << t.() { 1 + :a }
        res << t.() { 1 + "s" }
        res << t.() { 1 - Object.new }
        res << t.() { 1.5 + nil }
        res << t.() { 1.5 * false }
        res << t.() { 2 ** nil }
        res << t.() { 3 & nil }
        res << t.() { 1 / true }
        res << t.() { 1 << :a }
        res << t.() { 1 >> nil }
        res << t.() { (2 ** 70) << "s" }
        res << t.() { (2 ** 70) + nil }
        res << t.() { (2 ** 70) * :x }
        res << t.() { Rational(1, 2) + nil }
        res << t.() { Rational(1) - :a }
        res << t.() { Rational(1) * true }
        res << t.() { Rational(1) / "s" }
        res << t.() { Rational(1) ** nil }
        res << t.() { Rational(1).div(nil) }
        res << t.() { Rational(1).divmod(true) }
        res << t.() { Complex(1, 2) + nil }
        res << t.() { Complex(1) + :a }
        res << t.() { Complex(1) - Object.new }
        res << t.() { Complex(1) ** nil }
        res << t.() { Complex(1) ** :a }
        res << t.() { 1.fdiv(:a) }
        res << t.() { 1.fdiv(nil) }
        res << t.() { 1.quo(nil) }
        res << t.() { 1.5.quo(nil) }
        res << t.() { 1.pow(nil) }
        res << t.() { Complex(1, 2).to_f }
        res << t.() { Complex(1, 2).to_r }
        res << t.() { Complex(1, 2.0).to_i }
        res << t.() { 1 < nil }
        res << t.() { 1 > true }
        res << t.() { 1.0 < :a }
        res << t.() { 1.0 >= "s" }
        res << t.() { (2 ** 70) < :x }
        res << t.() { Rational(1) < nil }
        res << t.() { "a" < 1 }
        res << t.() { "a" < nil }
        res << t.() { "a" > Object.new }
        res << t.() { 3.clamp(:a, :b) }
        res << t.() { 3.clamp(:a, 5) }
        res << t.() { Cmp.new < nil }
        res << t.() { Cmp.new > 1 }
        res << t.() { Cmp.new <= :a }
        res << t.() { Cmp.new >= 1.5 }
        res << t.() { Cmp.new < "s" }
        res << t.() { Cmp.new.clamp(1, 2) }
        res << t.() { Cmp.new.between?(nil, 1) }
        res << t.() { Cmp.new.clamp(1..2) }
        # A `coerce` answering something other than a pair.
        class BadCoerce
          def coerce(other); 1; end
        end
        res << t.() { Rational(1) + BadCoerce.new }
        res << t.() { Rational(1) - BadCoerce.new }
        res << t.() { Rational(1) * BadCoerce.new }
        res << t.() { Rational(1) / BadCoerce.new }
        res << t.() { Rational(1) ** BadCoerce.new }
        res << t.() { Complex(1) ** BadCoerce.new }
        res
        "##,
    );
}

/// "X is not a symbol nor a string" (a method, constant or variable name
/// that is neither) names the value by `inspect`: `nil`, `true`, `1`.
#[test]
fn name_arguments_inspect_the_value() {
    run_test_once(
        r##"
        t = ->(&b) { begin; b.call; rescue => e; [e.class, e.message]; end }
        res = []
        res << t.() { Object.send(nil) }
        res << t.() { Object.new.public_send(true) }
        res << t.() { Object.new.method(1) }
        res << t.() { Object.new.respond_to?(nil) }
        res << t.() { Object.define_method(nil) {} }
        res << t.() { Object.attr_accessor(nil) }
        res << t.() { Object.alias_method(nil, :a) }
        res << t.() { Object.remove_method(false) }
        res << t.() { Object.new.instance_variable_get(nil) }
        res << t.() { Object.new.instance_variable_set(1, 1) }
        res << t.() { Object.const_set(nil, 1) }
        res << t.() { Module.new.class_variable_get(nil) }
        res << t.() { Module.new.class_variable_defined?(1) }
        res << t.() { Fiber[nil] }
        res << t.() { Fiber[1] }
        res << t.() { Thread.current[nil] }
        res << t.() { Data.define(nil) }
        res << t.() { Data.define(true) }
        res << t.() { Kernel.autoload?(nil) }
        res << t.() { Kernel.autoload(nil, "x") }
        res
        "##,
    );
}

/// Messages monoruby words differently from CRuby at the moment (so the
/// oracle cannot pin them), checked for the naming rule alone:
/// `IO.copy_stream` on a non-IO (CRuby: NoMethodError on `read`) and
/// `String#index` with a non-pattern (CRuby: the String conversion).
#[test]
fn naming_where_the_wording_still_differs() {
    run_test_no_result_check(
        r##"
        t = ->(&b) { begin; b.call; rescue => e; [e.class, e.message]; end }
        got = [
          t.() { IO.copy_stream(nil, $stdout) },
          t.() { IO.copy_stream(true, $stdout) },
          t.() { IO.copy_stream($stdin, nil) },
          t.() { IO.copy_stream($stdin, false) },
          t.() { "abc".index(nil) },
          t.() { "abc".rindex(true) },
          t.() { "abc".match?(:a) },
        ]
        expected = [
          [TypeError, "no implicit conversion of nil into IO"],
          [TypeError, "no implicit conversion of true into IO"],
          [TypeError, "no implicit conversion of nil into IO"],
          [TypeError, "no implicit conversion of false into IO"],
          [TypeError, "nil is not a regexp nor a string"],
          [TypeError, "true is not a regexp nor a string"],
          [TypeError, ":a is not a regexp nor a string"],
        ]
        got.zip(expected).each_with_index do |(g, e), i|
          raise "case #{i}: #{g.inspect} != #{e.inspect}" unless g == e
        end
        true
        "##,
    );
}
