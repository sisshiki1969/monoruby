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
        def m2
          obj = Object.new
          # A class body opens a fresh local scope, so gate on a global.
          class << obj
            return :early if $sclass_flag
          end
          :late
        end
        $sclass_flag = true
        a = m2
        $sclass_flag = false
        b = m2
        [m1, a, b]
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

#[test]
fn yield_reaches_lexical_home_through_escaped_frames() {
    // `yield` / `block_given?` in a define_method body (and in a Proc
    // whose defining method has returned) refer to the block of the
    // lexical home method — the call-site block of the defined method
    // is ignored, and the home's block survives frame escape via
    // handler materialization.
    run_test_once(
        r#"
        res = []
        class Y
          def self.define_deep(&blk)
            define_method('deep') do |v|
              yield v
            end
          end
          define_deep { |v| v * 2 }
        end
        res << Y.new.deep(2)
        class Z
          define_method(:bg) { block_given? }
        end
        res << (Z.new.bg { :ignored })
        def esc
          proc { yield }
        end
        res << esc { :from_block }.call
        def escl
          lambda { yield }
        end
        res << escl { :lam }.call
        def make_proc
          Proc.new { yield }
        end
        def with_block
          make_proc { 99 }
        end
        res << with_block.call
        res
        "#,
    );
}

#[test]
fn return_in_ensure_supersedes_and_swallows() {
    // A `return` in an ensure body supersedes the in-flight return
    // (running the *outer* ensure bodies, whose returns supersede
    // again), and swallows an in-flight exception — restoring `$!` to
    // its value at region entry.
    run_test_once(
        r#"
        $pad = []
        def f
          begin
            begin
              $pad << :inner_begin
              return :inner_begin
            ensure
              $pad << :inner_ensure
              return :inner_ensure
            end
          ensure
            $pad << :outer_ensure
            return :outer_ensure
          end
        end
        r1 = f
        def g
          begin
            raise "x"
          ensure
            $pad << :before_return
            return :swallowed
          end
        end
        r2 = g
        [r1, r2, $!, $pad]
        "#,
    );
}

#[test]
fn eigenclass_tower() {
    // MRI's full eigenclass tower: a meta-metaclass is classed by the
    // meta-metaclass of Class, methods on an ancestor's meta-metaclass
    // dispatch through `singleton_class` (which, like MRI's
    // rb_singleton_class, ensures the exposed eigenclass belongs to
    // its own eigenclass), and `class << obj` re-linking still works.
    run_test_once(
        r#"
        module MM
          class C
            class << self
              class << self
                def ham; 'iberico'; end
              end
            end
          end
          class D < C; end
          class A; end
        end
        res = [MM::D.singleton_class.ham]
        res << MM::A.singleton_class.singleton_class.is_a?(Class.singleton_class.singleton_class)
        class << Class
          def self.ex_cm; :cm; end
        end
        res << MM::A.singleton_class.singleton_class.respond_to?(:ex_cm)
        def metaclass_of(obj)
          class << obj; self; end
        end
        class BB; def self.cheese; 'stilton'; end; end
        metaclass_of(metaclass_of(BB)).send(:define_method, :cheese) { 'gouda' }
        res << metaclass_of(BB).cheese
        res << BB.cheese
        res
        "#,
    );
}
