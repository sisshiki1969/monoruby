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
//
// Uses `run_test_once` because the operation is stateful: re-running
// `autoload :D, path` after `a.rb` is already in `$LOADED_FEATURES`
// is a no-op in CRuby (matches `rb_autoload_str`'s `rb_feature_provided`
// short-circuit), so iteration 1 returns `[true, nil]` while iterations
// 2+ return `[false, nil]`.
#[test]
fn autoload_triggers_on_reference() {
    run_test_once(
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

// `Module#const_source_location(:X)` for an autoload-registered
// constant returns the location of the `autoload` call itself, until
// the load actually fires and overwrites the entry. Use a fresh
// Module each iteration so 25 reruns don't accumulate.
#[test]
fn autoload_const_source_location() {
    run_test(
        r#"
        m = Module.new
        m.autoload :Lazy, "/no/such/file"
        loc = m.const_source_location(:Lazy)
        # The location's line number depends on this script's layout
        # in the test harness; just assert the shape and that the
        # path entry is a String. (CRuby reports `(eval at …)` for
        # the path here, so we only check structural properties.)
        [loc.is_a?(Array), loc[0].is_a?(String), loc[1].is_a?(Integer)]
        "#,
    );
}

// `private_constant` registered on an autoload entry must carry
// through the load: after the file assigns the constant, qualified
// access from outside still raises `NameError: private constant
// referenced`.
#[test]
fn autoload_private_constant_carries_through_load() {
    run_test_once(
        r#"
        # `c.rb` defines `Foo::Bar = 20`. Register it as an autoload
        # under `Foo` and immediately mark it private. After the
        # implicit load triggered below, Bar must still be private.
        module Foo
          autoload :Bar, File.expand_path("c")
          private_constant :Bar
        end
        # Inside `Foo`'s lexical scope, `Bar` is reachable (private
        # constants are reachable from their own class). Outside,
        # qualified access raises NameError.
        outer = begin
          Foo::Bar
          :reachable
        rescue NameError => e
          e.message =~ /private/ ? :private : :other_name
        end
        # Sanity check: confirm the autoload was consumed (Bar is now
        # an Integer 20) and that visibility carried through.
        [outer, Foo.const_defined?(:Bar), Foo.autoload?(:Bar)]
        "#,
    );
}

// In `$VERBOSE = true` mode, an autoload whose `require` finished
// without defining the expected constant must emit the CRuby warning
// `Expected <file> to define <Owner::Const> but it didn't` to
// `$stderr.write`. Captures via an IOStub-style replacement so we can
// assert against the message contents (CRuby prefixes the line with
// `<file>:<lineno>:`, monoruby does not — both append the warning text
// itself, so we substring-check rather than equality-check).
//
// `run_test_once` because the assertion is order-sensitive: after the
// first reference, `b.rb` is in `$LOADED_FEATURES`, so re-running the
// same code in iteration 2 hits the autoload's `rb_feature_p`
// short-circuit and the warning never fires.
#[test]
fn autoload_verbose_warning_on_missing_define() {
    run_test_once(
        r#"
        # IOStub: capture writes through `$stderr.write(s)`.
        class IOStub
          def initialize; @buf = String.new; end
          def write(s); @buf << s.to_s; nil; end
          def to_s; @buf; end
        end
        err = IOStub.new
        saved_err = $stderr
        saved_verbose = $VERBOSE
        $stderr = err
        $VERBOSE = true

        # `b.rb` is the standard fixture from this directory; it does
        # not define VerboseAutoload::Bogus, so triggering the autoload
        # causes the verbose-mode "Expected … but it didn't" warning.
        module VerboseAutoload
          autoload :Bogus, File.expand_path("b")
        end
        begin
          VerboseAutoload::Bogus
        rescue NameError
          # expected: file failed to define the constant
        end

        $VERBOSE = saved_verbose
        $stderr = saved_err
        captured = err.to_s
        [captured.include?("Expected"),
         captured.include?("VerboseAutoload::Bogus"),
         captured.include?("but it didn't")]
        "#,
    );
}

// Failed `require`: when the file body raises, the canonicalised path
// must be removed from `$LOADED_FEATURES` so a subsequent `require`
// of the same file re-runs the body (matches CRuby; this is exactly
// the path `Globals::remove_loaded_feature` was introduced for in this
// PR).
//
// `run_test` (default 25 iters) is fine here: each iteration ends with
// `$LOADED_FEATURES` *not* containing the failing path (because the
// failed-require cleanup removed it), so iteration N+1 starts from the
// same state as iteration 1.
#[test]
fn require_removes_loaded_feature_on_failure() {
    run_test(
        r#"
        require "tmpdir"
        # Use a per-process unique path so parallel test runs don't
        # collide on the same file. Both monoruby and CRuby see
        # different absolute paths, but the test only inspects
        # *whether* the path is present in $LOADED_FEATURES, not its
        # exact value.
        path = File.join(Dir.tmpdir, "monoruby_require_failure_#{Process.pid}.rb")
        File.write(path, "raise 'boom'\n")

        first = begin
          require path
          :no_error
        rescue RuntimeError => e
          e.message
        end

        # `remove_loaded_feature` must have run: a substring scan of
        # $LOADED_FEATURES turns up nothing matching the file we just
        # tried to load.
        in_loaded = $LOADED_FEATURES.any? { |p| p.include?("monoruby_require_failure") }

        # The require is retriable because the feature was removed —
        # this should re-execute the file (and re-raise).
        second = begin
          require path
          :no_error
        rescue RuntimeError => e
          e.message
        end

        File.unlink(path) rescue nil
        [first, in_loaded, second]
        "#,
    );
}
