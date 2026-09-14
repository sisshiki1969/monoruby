extern crate monoruby;
use monoruby::tests::*;

#[test]
fn bang_default() {
    run_test(
        r##"
        [!true, !false, !nil, !0, !"", ![], !:sym]
        "##,
    );
}

#[test]
fn bang_override() {
    run_test(
        r##"
        class Foo
          def !
            42
          end
        end
        !Foo.new
        "##,
    );
}

#[test]
fn bang_override_preserves_default() {
    run_test(
        r##"
        class Baz
          def !
            :custom
          end
        end
        [!Baz.new, !true, !false, !nil, !42]
        "##,
    );
}

#[test]
fn bang_override_on_specific_class() {
    run_test(
        r##"
        class MyObj
          def !
            "not_myobj"
          end
        end
        a = MyObj.new
        b = Object.new
        [!a, !b]
        "##,
    );
}

#[test]
fn bang_method_respond_to() {
    run_test(
        r##"
        class Qux
          def !
            99
          end
        end
        [Qux.new.respond_to?(:!), Object.new.respond_to?(:!)]
        "##,
    );
}

#[test]
fn bang_send() {
    run_test(
        r##"
        class Foo
          def !
            42
          end
        end
        [Foo.new.send(:!), true.send(:!), false.send(:!)]
        "##,
    );
}

#[test]
fn bang_float_compare_in_loop() {
    run_test(
        r##"
        def bang_float_cmp(n)
          x = 0.0
          c = 0
          i = 0
          while i < n
            x = x + 0.5
            c += 1 if !(x > 10.0)
            i += 1
          end
          c
        end
        bang_float_cmp(100)
        "##,
    );
}

#[test]
fn bang_bool_guard_rejects_other_receivers() {
    run_test(
        r##"
        class NotBool
          def !
            :custom
          end
        end
        def bang_of(v) = !v
        r = []
        i = 0
        while i < 30
          r << bang_of(i.even?)
          i += 1
        end
        r << bang_of(NotBool.new)
        r << bang_of(nil)
        r << bang_of(0)
        r
        "##,
    );
}
