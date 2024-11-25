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
        30.times do |x|
          30.times do |y|
            30.times do |z|
              s = if x == 28 && y == 28 && z == 28
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
        for x in 0..30
          for y in 0..30
            for z in 0..30
              s = if x == 28 && y == 28 && z == 28
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
