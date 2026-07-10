extern crate monoruby;
use monoruby::tests::*;

#[test]
fn bare_regexp_in_conditional_matches_lastline() {
    // A regexp literal in a conditional matches `$_` implicitly
    // (prism's MatchLastLineNode), setting `$~` — including the
    // interpolated form.
    run_test_once(
        r#"
        $_ = "hello"
        res = []
        res << (/ell/ ? :match : :nomatch)
        res << (/xyz/ ? :match : :nomatch)
        x = "ll"
        res << (/he#{x}/ ? :match : :nomatch)
        res << $~[0]
        res
        "#,
    );
}

#[test]
fn splat_calls_to_a_on_basic_object() {
    // The splat coercion falls back to a raw `to_a` lookup when the
    // object has no `respond_to?` (a bare BasicObject) — CRuby's
    // rb_check_funcall still calls it. Objects without `to_a` wrap.
    run_test_once(
        r#"
        splat = BasicObject.new
        def splat.to_a; [2, 3, 4]; end
        a = [1, *splat]
        o = Object.new
        [a.size, a[1], a[3], [9, *o].size, [*nil]]
        "#,
    );
}

#[test]
fn sclass_return_exits_enclosing_method() {
    // `return` in a singleton-class body returns from the method
    // executing the sclass expression (CRuby), including through a
    // block, and still runs the sclass path when no return triggers.
    run_test_once(
        r#"
        def m1
          obj = Object.new
          class << obj
            return :inner
          end
          :outer
        end
        def m2(flag)
          obj = Object.new
          class << obj
            return :early if flag
          end
          :late
        end
        [m1, m2(true), m2(false)]
        "#,
    );
}

#[test]
fn redo_runs_pending_ensures() {
    // `redo` exits the begin/ensure regions it is written inside —
    // their ensure bodies run before the body restarts, both in a
    // block (no local loop) and in a `while` loop.
    run_test_once(
        r#"
        r = []
        c = {}
        [1, 2, 3].each do |i|
          begin
            r << i * 10
            redo if c[i].nil? && i == 1 && (c[i] = true)
          ensure
            r << i * 100
          end
        end
        w = []
        done = false
        i = 0
        while i < 2
          i += 1
          begin
            w << i
            unless done
              done = true
              redo
            end
          ensure
            w << -i
          end
        end
        [r, w]
        "#,
    );
}
