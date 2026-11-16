//! The inline lowerings of the builtins that take and return a `double`
//! (`Float#**`, `Float#%`, `Math.sqrt` and friends) with an operand that
//! is a compile-time constant other than a Fixnum or a Float.
//!
//! A slot of unknown type is loaded into an FP register behind a
//! "Fixnum or Float" guard that deopts on anything else. A constant has
//! no guard to fail, so the lowering declines and the ordinary builtin
//! call converts the operand (a Bignum) or raises `TypeError` (issue
//! #1997: it used to hit `unreachable!` in the compiler).

extern crate monoruby;
use monoruby::tests::*;

#[test]
fn bignum_constant_operand() {
    run_test(
        r#"
        def a(x) = x ** 2352354645657876868978696835652452546462456245646
        def b(x) = x % 2352354645657876868978696835652452546462456245646
        def c(x) = x.modulo(2352354645657876868978696835652452546462456245646)
        def d(x) = Math.sqrt(2352354645657876868978696835652452546462456245646) + x
        def e(x) = Math.exp(-2352354645657876868978696835652452546462456245646) + x
        res = []
        30.times { |i| res << [a(1.5), a(0.5), b(1.5), c(-1.5), d(i.to_f), e(0.5)] }
        res.uniq
        "#,
    );
}

#[test]
fn non_numeric_constant_operand() {
    run_test(
        r#"
        def a(x) = x ** nil
        def b(x) = x ** :a
        def c(x) = x % true
        def d(x) = Math.sqrt(nil)
        def e(x) = Math.sin(true)
        def f(x) = Math.cos(:a)
        res = []
        30.times do
          res << %i[a b c d e f].map do |m|
            begin
              send(m, 1.5)
            rescue => ex
              [ex.class, ex.message]
            end
          end
        end
        res.uniq
        "#,
    );
}

#[test]
fn constant_operand_in_a_loop() {
    // The loop JIT, whose back-edge analysis walks the body first.
    run_test(
        r#"
        res = []
        i = 0
        x = 1.5
        while i < 40
          res << [x ** 2352354645657876868978696835652452546462456245646,
                  x % 2352354645657876868978696835652452546462456245646,
                  Math.sqrt(2352354645657876868978696835652452546462456245646),
                  (x ** nil rescue $!.class)]
          i += 1
        end
        res.uniq
        "#,
    );
}
