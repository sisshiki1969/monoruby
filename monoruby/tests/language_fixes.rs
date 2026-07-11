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

#[test]
fn interpolation_and_eval_source_encoding() {
    // Interpolation seeds its result from the source encoding and
    // negotiates each piece like `<<` (raising CompatibilityError on
    // an incompatible mix); an eval'd string with no magic comment
    // contributes its own encoding as the source encoding.
    run_test_once(
        r##"
        res = []
        res << eval('"a#{"b"}c"'.dup.force_encoding("us-ascii")).encoding.to_s
        res << eval("__ENCODING__".dup.force_encoding("us-ascii")).to_s
        res << eval("# encoding: utf-8\n__ENCODING__".dup.force_encoding("us-ascii")).to_s
        a = "あ"
        b = "\xff".dup.force_encoding "binary"
        begin
          "#{a} #{b}"
        rescue Encoding::CompatibilityError => e
          res << e.message
        end
        res << "x#{"あ"}".encoding.to_s
        res
        "##,
    );
}

#[test]
fn sclass_constants_resolve_per_object() {
    // A `def` inside `class << obj` shares its iseq across executions
    // while each execution owns a distinct singleton class. Constant
    // lookup must use the *runtime* cref (the receiver's singleton
    // class / the per-execution nested class), not the last-stamped
    // static one — including through the inline constant cache and
    // the JIT (run_test warms both).
    run_test(
        r##"
        $r = []
        # Constants stored directly in the singleton class.
        2.times do |i|
          $i = i
          obj = Object.new
          class << obj
            CONST = ($i + 1)
            def foo
              CONST
            end
          end
          10.times { $r << obj.foo }
        end
        # A class nested inside `class << obj` is distinct per
        # execution; its methods must see their own class's constants.
        $classes = []
        2.times do |i|
          $i = i
          obj = Object.new
          class << obj
            class X
              $classes << self
              CONST2 = ($i + 1)
              def foo
                CONST2
              end
            end
            10.times { $r << X.new.foo }
          end
        end
        $r << ($classes[0] != $classes[1])
        # A singleton def (`def a.meth`) still resolves through its
        # lexical scope, not the receiver's ancestry.
        module CSPO_M
          CONST3 = :lexical
          class CSPO_A
            CONST3 = :receiver
          end
          a = CSPO_A.new
          def a.bar; CONST3; end
          CSPO_OBJ = a
        end
        10.times { $r << CSPO_M::CSPO_OBJ.bar }
        $r
        "##,
    );
}

#[test]
fn binary_source_bytes_survive_load_and_eval() {
    // The source pipeline carries raw bytes: a `# encoding: big5`
    // source whose literals hold non-UTF-8 bytes round-trips through
    // `load` and `eval` without mangling, and the literal takes the
    // declared encoding.
    run_test_once(
        r##"
        require "tmpdir"
        path = File.join(Dir.tmpdir, "mrb_bytes_magic_#{Process.pid}.rb")
        src = (+"# encoding: big5\n").force_encoding("binary") +
              "$bytes_result = '\xA7A\xA6n'.bytes\n".b +
              "$enc_result = '\xA7A'.encoding.to_s\n".b
        File.binwrite(path, src)
        load path
        r1 = [$bytes_result, $enc_result]
        code = File.read(path, encoding: "utf-8")
        $bytes_result = nil
        eval(code)
        r2 = $bytes_result
        File.delete(path)
        [r1, r2]
        "##,
    );
}
