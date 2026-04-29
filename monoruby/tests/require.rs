extern crate monoruby;
use monoruby::tests::*;

#[test]
fn require() {
    run_test(
        r#"
        require File.expand_path("a")
        C::D
        "#,
    );
}

#[test]
fn require_relative() {
    run_test(
        r#"
        require File.expand_path("./b")
        C::D
        "#,
    );
}

#[test]
fn load() {
    run_test(
        r#"
        load "b.rb"
        load "b.rb"
        load "b.rb"
        [$count, C::D]
        "#,
    );
}

#[test]
fn load_priv() {
    run_test_once(
        r#"
        class C
          D = nil
        end
        load "a.rb", true
        load "a.rb", true
        load "a.rb", true
        [$count, C::D]
        "#,
    );
}

#[test]
fn module_autoload() {
    run_test(
        r#"
        class C
          autoload :D, File.expand_path("j")
          autoload :D, File.expand_path("a")
        end
        C::D
        "#,
    );
}

// `Module#const_defined?` reports an autoload-registered constant as
// defined without firing the `require`. Pointing the registration at
// a non-existent path proves that the load isn't triggered: if it
// were, we'd see a LoadError.
#[test]
fn const_defined_does_not_trigger_autoload() {
    run_test(
        r#"
        m = Module.new
        m.autoload :Foo, "/no/such/path/at/all"
        [m.const_defined?(:Foo), m.autoload?(:Foo)]
        "#,
    );
}

// Same idea via `defined?(Foo::Bar)`: the leaf segment must not
// trigger autoload. The intermediate qualifier `M` is loaded already
// (it's just the receiver), so only the leaf probe matters.
#[test]
fn defined_does_not_trigger_autoload() {
    run_test(
        r#"
        module AutoloadProbe
          autoload :Bar, "/nonexistent/probe/file"
        end
        defined?(AutoloadProbe::Bar)
        "#,
    );
}

// `Module#autoload?` continues to return the registered path until
// the autoload is actually consumed. Probing via `const_defined?`
// must not consume it.
#[test]
fn const_defined_preserves_autoload_query() {
    run_test(
        r#"
        m = Module.new
        m.autoload :Lazy, "/nope"
        a = m.autoload?(:Lazy)
        b = m.const_defined?(:Lazy)
        c = m.autoload?(:Lazy)
        [a, b, c]
        "#,
    );
}

// Once the autoload-registered constant is *actually* referenced,
// the `require` runs and the constant binds to the value the file
// defined. After that, `autoload?` reports nil and `const_defined?`
// is still true.
#[test]
fn autoload_triggers_on_reference() {
    run_test(
        r#"
        class AutoLazy
          autoload :D, File.expand_path("a")
        end
        before = AutoLazy.autoload?(:D)
        # Touching the constant fires the require. The fixture
        # `a.rb` defines `class C; D = __FILE__; end`; we don't
        # bind it on `AutoLazy` (the require runs at top level),
        # so we just verify the registration was consumed.
        AutoLazy.const_defined?(:D)
        # Touching for real:
        begin
          AutoLazy::D
        rescue NameError
          # expected — `a.rb` defines C::D, not AutoLazy::D, so
          # after the require the autoload entry is dropped.
        end
        after_query = AutoLazy.autoload?(:D)
        # `before` is the path string; `after_query` is nil.
        [before.is_a?(String), after_query]
        "#,
    );
}

// LoadError during the autoload's `require` keeps the registration
// alive so a future reference (after fixing the path) can retry.
#[test]
fn autoload_loaderror_keeps_registration() {
    run_test(
        r#"
        class AutoRetry
          autoload :Foo, "/definitely/not/a/file"
        end
        first = begin
          AutoRetry::Foo
        rescue LoadError
          :load_error
        end
        # Registration must still be there — `autoload?` returns
        # the (still-registered) path.
        [first, AutoRetry.autoload?(:Foo)]
        "#,
    );
}
