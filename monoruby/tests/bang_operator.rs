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
