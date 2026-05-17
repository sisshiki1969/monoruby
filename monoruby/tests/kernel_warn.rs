extern crate monoruby;
use monoruby::tests::*;

#[test]
fn verbose_default_is_false() {
    // `$-v` / `$-w` special-global syntax is only lexed by the Prism
    // backend; the legacy ruruby-parse parser rejects it.
    if parser_is_ruruby() {
        run_test("$VERBOSE");
        return;
    }
    run_test("[$VERBOSE, $-v, $-w]");
}

#[test]
fn kernel_warn_flatten_and_newline() {
    run_test(
        r#"
        require 'stringio'
        def cap; $stderr = StringIO.new; yield; s = $stderr.string; $stderr = STDERR; s; end
        [
          cap { warn(["line 1", "line 2"]) },
          cap { warn("a", "b") },
          cap { warn("ends\n") },
          cap { warn("noend") },
          cap { warn([1, 2], 3) },
          cap { warn(nil) },
          cap { warn() },
        ]
        "#,
    );
}

#[test]
fn kernel_warn_verbose_gating() {
    run_test(
        r#"
        require 'stringio'
        def cap; $stderr = StringIO.new; yield; s = $stderr.string; $stderr = STDERR; s; end
        a = (v = $VERBOSE; $VERBOSE = nil; r = cap { warn("silent") }; $VERBOSE = v; r)
        b = (v = $VERBOSE; $VERBOSE = false; r = cap { warn("shown") }; $VERBOSE = v; r)
        [a, b]
        "#,
    );
}

#[test]
fn kernel_warn_category_gating_delegation() {
    // Stateful (prepends a module, mutates Warning[]/$c) so it must
    // run once rather than the default 25x-in-one-process loop.
    run_test_once(
        r#"
        $c = []
        Warning.singleton_class.prepend(Module.new do
          def warn(m, category: nil); $c << [m, category]; super; end
        end)
        require 'stringio'
        $stderr = StringIO.new
        v = $VERBOSE; $VERBOSE = false
        Warning[:deprecated] = false
        Kernel.warn("a", category: :deprecated)         # gated off
        Warning[:deprecated] = true
        Kernel.warn("b", category: "deprecated")        # delegated, symbolized
        Kernel.warn("c")                                # category nil
        $VERBOSE = v
        out = $stderr.string
        $stderr = STDERR
        [$c, out]
        "#,
    );
}

#[test]
fn kernel_warn_category_type_error() {
    run_test_error(r#"warn("m", category: Object.new)"#);
}

#[test]
fn kernel_warn_uplevel_negative_raises() {
    run_test_error(r#"warn("m", uplevel: -1)"#);
}
