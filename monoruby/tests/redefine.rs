extern crate monoruby;
use monoruby::tests::*;

#[test]
fn redefine_test1() {
    run_test_once(
        r##"
        a = [100 * 100]
        class Integer
          def *(other)
            42
          end
        end
        a << 100 * 100
        a
        "##,
    );
}

#[test]
fn redefine_test2() {
    run_test_once(
        r##"
        res = []
        50.times do |x|
          res << 100 * 100
          if x == 25
            class Integer
              def *(other)
                42
              end
            end
          end
        end
        res
        "##,
    );
}

#[test]
fn redefine_test3() {
    run_test_once(
        r##"
        a = 0
        20.times do |x|
          20.times do |y|
            20.times do |z|
              s = if x == 18 && y == 18 && z == 18
                "def *(other); 42; end;"
              else
                ""
              end
              Integer.class_eval(s)
              a += 100 * 100
            end
            a += 100 * 100
          end
          a += 100 * 100
        end
        a
        "##,
    );
}

#[test]
fn redefine_test4() {
    run_test_once(
        r##"
        a = 0
        for x in 0..20
          for y in 0..20
            for z in 0..20
              s = if x == 18 && y == 18 && z == 18
                "def *(other); 42; end;"
              else
                ""
              end
              Integer.class_eval(s)
              a += 100 * 100
            end
            a += 100 * 100
          end
          a += 100 * 100
        end
        a
        "##,
    );
}

// Regression for #730: a `class`/`module`/`class << obj` definition used
// in *expression* position (its value feeds a surrounding operation)
// must not clobber the accumulator that holds a live operand. Here `a`
// (a freshly built Array) is the receiver of `<<` while the argument is a
// class definition; the JIT used to leave `a` in the accumulator across
// the class-body call, so the `<<` read a clobbered register and tripped
// a non-Array assertion. `run_test` exercises the JIT (≥ warmup runs).
#[test]
fn class_def_in_expression_position_730() {
    run_test(
        r##"
        out = nil
        50.times do
          a = []
          a << (class C730A; 42; end)
          a << (class C730B; def m; 7; end; end; "z")
          obj = Object.new
          a << (class << obj; 99; end)
          out = a
        end
        out
        "##,
    );
    // module form, and the value flowing into arithmetic
    run_test(
        r##"
        r = nil
        50.times { r = 1 + (module M730; 41; end) }
        r
        "##,
    );
}
