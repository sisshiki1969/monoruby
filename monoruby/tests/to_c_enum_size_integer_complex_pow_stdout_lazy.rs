//! `Rational#to_c`, `Enumerator.new`'s size, `Integer()` and `Array#join`
//! conversions, `Complex ** x` / `Complex * Complex` / `Complex.polar`,
//! `$stdout` as any object with a `write`, `Lazy.new`'s inspect
//! (#2061, #2062, #2066, #2065, #2060, #2048).
//!
//! - **`Rational#to_c`** (#2061) is `Numeric#to_c`: the Rational stays
//!   the real part.
//! - **`Enumerator.new(size)`** (#2062). As `convert_to_feasible_size_value`:
//!   nil, a callable and `Float::INFINITY` are kept, anything else goes
//!   through `to_int`.
//! - **`Integer()`** (#2066). As `rb_convert_to_integer`: `to_int` is
//!   tried under `rb_protect`, `to_str` through `rb_check_string_type`,
//!   `to_i` through `rb_check_funcall`; `exception: false` turns into nil
//!   only what CRuby does. `Array#join` converts with
//!   `rb_check_string_type` / `rb_check_array_type`.
//! - **`Complex`** (#2065). A real exponent that is not a fixnum goes
//!   through `f_complex_polar_real`, which gives exact axes for the
//!   angles pi, pi/2 and 3pi/2; a bignum exponent first tries
//!   `complex_pow_for_special_angle`; a product with an infinite part
//!   times a zero is taken for its sign (`safe_mul`).
//! - **`$stdout`** (#2060). `print` / `puts` / `p` write through
//!   `$stdout.write` when `$stdout` has no `print` / `puts` of its own,
//!   `p` flushes only an IO, and `$>` is the same variable as `$stdout`.
//! - **`Enumerator::Lazy.new`** (#2048) inspects with `:each`.

use monoruby::tests::*;

#[test]
fn rational_to_c_keeps_the_rational() {
    run_test_once(r##"[(1r/3).to_c, 3r.to_c, (-2r/5).to_c.real.class]"##);
}

#[test]
fn enumerator_new_feasible_size() {
    run_test_once(
        r##"
        o = Object.new; def o.to_int = 6
        c = Object.new; def c.call = 9
        [nil, 3, 2.9, -> { 4 }, c, Float::INFINITY, 2**70, o, 3r, "3", Object.new,
         -Float::INFINITY, Float::NAN].map do |s|
          e = Enumerator.new(s) { |y| y << 1 }
          s.is_a?(Proc) || s.equal?(c) ? e.size : [e.size.class, e.size]
        rescue => ex
          [ex.class, ex.message]
        end
        "##,
    );
}

#[test]
fn integer_conversion_protocol() {
    run_test_once(
        r##"
        def t; yield; rescue Exception => e; [e.class, e.message]; end
        class P; def to_int = raise("boom"); def to_i = 5; end
        class Q; def to_int = raise("boom"); def to_str = "12"; end
        class R; def to_int = raise("boom"); end
        class T; def respond_to?(n, a = false) = n != :to_i; def to_i = 7; end
        class V; def to_int = nil; def to_i = 3; end
        class X; def to_i = "x"; end
        class Y; def to_int = throw(:z); def to_i = 1; end
        class S; def to_str = 5; end
        class S2; def to_str = raise("s"); end
        class M
          def method_missing(n, *a) = n == :to_i ? 77 : super
          def respond_to_missing?(n, p = false) = n == :to_i
        end
        o12 = Object.new; def o12.to_str = "12"
        [t { Integer(P.new) }, t { Integer(Q.new) }, t { Integer(R.new) }, t { Integer(T.new) },
         t { Integer(P.new, exception: false) }, t { Integer(R.new, exception: false) },
         t { Integer(V.new) }, t { Integer(X.new) }, t { Integer(X.new, exception: false) },
         t { catch(:z) { Integer(Y.new) } }, t { Integer(M.new) },
         t { Integer(S.new) }, t { Integer(S.new, exception: false) },
         t { Integer(S2.new, exception: false) },
         t { Integer("12".encode("UTF-16LE"), exception: false) },
         t { Integer("z", 99, exception: false) }, t { Integer(Object.new, 2, exception: false) },
         t { Integer(o12, 3, exception: false) }, t { Integer(o12, 3) },
         t { Integer(Float::NAN, exception: false) }, t { Integer(nil, exception: false) },
         t { Integer("1_", exception: false) }, t { Integer(Object.new, exception: false) }]
        "##,
    );
}

#[test]
fn array_join_conversions() {
    run_test_once(
        r##"
        def t; yield; rescue Exception => e; [e.class, e.message]; end
        class U; def to_ary = [7]; def to_str = 42; end
        class U4; def to_ary = 5; end
        class U5; def to_str = nil; def to_s = "five"; end
        class U6; def to_ary = nil; def to_s = "S"; end
        class U7; def to_str = "T"; def to_ary = [1]; end
        class U8; def to_ary = [1, [2]]; end
        [t { [U.new].join }, t { [1, [U.new]].join("-") }, t { [U4.new].join }, t { [U5.new].join },
         t { [U6.new].join }, t { [U7.new].join }, t { [U8.new].join(",") },
         t { [1, [2, [3]], "a", :b, nil].join(",") }]
        "##,
    );
}

#[test]
fn complex_pow_polar_and_bignum() {
    run_test_once(
        r##"
        require "stringio"
        def t; yield; rescue Exception => e; [e.class, e.message]; end
        $stderr = StringIO.new
        r = [t { Complex(-1r/2, 0) ** 0.5 }, t { Complex(-1, 0) ** 0.5 }, t { Complex(0, 2) ** 0.5 },
         t { Complex(-4, 0) ** (1r/2) }, t { Complex(1r/3, 0) ** (2**70) }, t { Complex(1, 1) ** (2**70) },
         t { Complex(1.5, 0) ** (2**70) }, t { Complex(3, 4) ** 1.5 }, t { Complex(2, 0) ** 0.5 },
         t { Complex(0, -2) ** 0.5 }, t { Complex(2, 0) ** (2**70) }, t { Complex(0, 0) ** (2**70) },
         t { Complex(1, 0) ** (2**70) }, t { Complex(-1, 0) ** (2**70) }, t { Complex(0.5, 0) ** (2**70) },
         t { Complex(-1.5, 0) ** (2**70) }, t { Complex(1, 1) ** -(2**70) },
         t { Complex(1r/2, -1r/2) ** (2**70) }, t { Complex(-1, -1) ** (2**70 + 1) },
         t { Complex(0, 1) ** (2**70 + 3) }, t { Complex(0, -1) ** (2**70 + 1) },
         t { Complex(-1, 0) ** -(2**70 + 1) }, t { Complex(1.0, 1.0) ** (2**64) },
         t { Complex(1, 2) ** (2**70) }, t { Complex(1.0, 2) ** -(2**70) },
         t { Complex(0, 0) ** 0.5 }, t { Complex(0, 0) ** -0.5 }, t { Complex(3, 4) ** -1.5 },
         t { Complex(1r/2, 1r/3) ** 0.5 }, t { Complex(1r/2, 1r/3) ** (1r/2) },
         t { Complex(1, 2) ** Complex(0, 1) }, t { Complex(-1, 0) ** Complex(0.5, 0.0) },
         t { Complex(3, 4) ** Float::NAN }]
        w = $stderr.string
        $stderr = STDERR
        r.map(&:inspect) << w.lines.size
        "##,
    );
}

#[test]
fn complex_polar_exact_axes() {
    run_test_once(
        r##"
        [Complex.polar(2, Math::PI), Complex.polar(2, Math::PI / 2), Complex.polar(2, 3 * Math::PI / 2),
         Complex.polar(2r, 1.0), Complex.polar(2, 1), Complex.polar(2.0, 1r), Complex.polar(2),
         Complex.polar(2, 0), Complex.polar(1r/2, Math::PI), Complex.polar(2, 0.0), Complex.polar(0, 1.0),
         Complex.polar(2r, 1r), Complex.polar(1.5, 2), Complex.polar(Float::INFINITY, 1.0),
         Complex.polar(Complex(2, 0), Complex(1.0, 0)), Complex.polar(-2, Math::PI / 2),
         Complex.polar(2.5, -Math::PI), Complex.polar(1, Float::NAN)].map(&:inspect)
        "##,
    );
}

#[test]
fn complex_mul_infinite_times_zero() {
    run_test(
        r##"
        [Float::INFINITY * Complex(0, 1), Complex(Float::INFINITY, 0) * Complex(0, 1),
         Complex(0.0, 1) * Complex(Float::INFINITY, 0), Complex(1, 2) * Complex(3, 4),
         Complex(Float::NAN, 1) * Complex(1, 0), Complex(0, Float::INFINITY) * Complex(0, 0),
         Complex(-Float::INFINITY, 0.0) * Complex(0.0, -2), (2**70) * Complex(Float::INFINITY, 0),
         Complex(1, 1) * Complex(Float::INFINITY, Float::INFINITY), Complex(0, 1) * Float::INFINITY,
         Complex(1.5, 2) * Complex(0, 1)].map(&:inspect)
        "##,
    );
}

#[test]
fn stdout_any_object_with_write() {
    run_test_once(
        r##"
        require "stringio"
        def cap
          o = $stdout; $stdout = s = StringIO.new
          begin; yield; ensure; $stdout = o; end
          s.string
        end
        class W1; def initialize; @b = []; end; def write(a) = (@b << a; 1); attr_reader :b; end
        class W2; def initialize; @b = []; end; def write(*a) = (@b << a; 1); attr_reader :b; end
        def wcap(k)
          o = $stdout; $stdout = w = k.new
          r = begin; yield; :ok; rescue Exception => e; [e.class, e.message[0, 50]]; end
          $stdout = o
          [r, w.b]
        end
        r = [cap { pp({a: 1}) }, cap { 1.display }, cap { print 1, :a, nil, 2.5 },
         cap { puts [1, [2, [3]]], nil, "x\n", "" }, cap { p 1, "s" }, cap { p },
         wcap(W1) { print 1, nil }, wcap(W2) { print 1, nil }, wcap(W1) { p :q },
         wcap(W2) { p :q, 3 }, wcap(W1) { puts 1, [2] }, wcap(W2) { puts },
         wcap(W2) { putc 65 }, wcap(W2) { $stdout.puts 9 }, wcap(W2) { 5.display },
         wcap(W2) { $>.write(1) }, wcap(W2) { pp 7 }, wcap(W2) { puts 1, "", "a\n", [nil] },
         wcap(W1) { puts "", "a\n" }, wcap(W2) { printf("%d", 5) }]
        $\ = "!"
        r << cap { print 1 }
        $\ = nil
        r << cap { $_ = "line"; print } << cap { $_ = nil; print }
        r << (begin; $> = 1; rescue => e; [e.class, e.message]; end)
        r << $>.equal?($stdout)
        o = $stdout; $> = s = StringIO.new; r << $stdout.equal?(s); $stdout = o
        # A `write` that takes one argument is warned about under -w.
        dep, verbose = Warning[:deprecated], $VERBOSE
        Warning[:deprecated] = true; $VERBOSE = true; e = $stderr; $stderr = StringIO.new
        r << wcap(W1) { p 1; puts 2 }
        w = $stderr.string
        $stderr = e; Warning[:deprecated] = dep; $VERBOSE = verbose
        r << w.scan("W1#write is outdated interface which accepts just one argument").size
        r
        "##,
    );
}

#[test]
fn lazy_new_inspects_each() {
    run_test_once(
        r##"
        [Enumerator::Lazy.new([1, 2]) { |y, v| y << v },
         Enumerator::Lazy.new([1, 2], 2) { |y, v| y << v }.map { _1 },
         Class.new(Enumerator::Lazy).new([1]) { }.inspect.sub(/0x\h+/, "0x"),
         [1, 2].lazy, [1, 2].each.lazy, [1, 2].lazy.lazy, (1..3).lazy.map { _1 }, {a: 1}.lazy,
         Enumerator::Lazy.new([1, 2]) { |y, v| y << v }.to_a].map(&:inspect)
        "##,
    );
}
