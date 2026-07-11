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
fn symbol_literal_takes_source_encoding() {
    // A non-ASCII symbol literal in a binary-encoded source reports
    // ASCII-8BIT and is a distinct symbol from the same bytes in a
    // UTF-8 source (per-(bytes, encoding) identity — CRuby).
    run_test_once(
        r##"
        require "tmpdir"
        path = File.join(Dir.tmpdir, "mrb_binsym_#{Process.pid}.rb")
        File.binwrite(path, (+"# encoding: binary\n").force_encoding("binary") +
          "$binsym = :il_était\n".b +
          "$binsym_frozen = [:il_était].first\n".b)
        load path
        File.delete(path)
        utf8 = :il_était
        [$binsym.encoding.name, $binsym.to_s.bytes,
         $binsym_frozen.encoding.name,
         utf8.encoding.name, ($binsym == utf8)]
        "##,
    );
}

#[test]
fn eval_cref_semantics() {
    // Class-variable and constant crefs of receiver-anchored string
    // evals: class_eval sees the module's cvars; instance_eval skips
    // its singleton cref for cvars (caller's scope wins) but checks
    // the receiver's class for constants between the singleton class
    // and the caller's lexical scopes; a `class << obj` body at the
    // toplevel has no cvar scope at all.
    run_test_once(
        r##"
        $r = []
        m = Module.new
        $r << m.class_eval("@@cv1 = 39; @@cv1")
        class ECS_Recv; FOO9 = :receiver_class; end
        module ECS_CS
          @@cv2 = :caller
          FOO9 = :caller_class
          class C
            def get_const(obj) = obj.instance_eval("FOO9")
            def get_cvar(obj) = obj.instance_eval("@@cv2") rescue :err
          end
        end
        $r << ECS_CS::C.new.get_const(ECS_Recv.new)
        # The defined? probe takes the same blended route.
        recv = ECS_Recv.new
        $r << recv.instance_eval("defined?(FOO9)")
        $r << recv.instance_eval("defined?(NOT_DEFINED_ANYWHERE9)").inspect
        $r << recv.instance_eval("FOO9") rescue $r << :e1
        obj2 = Object.new
        begin
          class << obj2
            @@cv3 = 1
          end
          $r << :sclass_ok
        rescue RuntimeError => e
          $r << e.message
        end
        $r
        "##,
    );
}

#[test]
fn pattern_matching_desugar() {
    // case/in, `expr => pat`, and `expr in pat` are desugared in the
    // prism lowerer to ===/deconstruct/deconstruct_keys calls, local
    // bindings, and NoMatchingPatternError raises. run_test warms the
    // JIT, so the generated code is exercised in both tiers.
    run_test(
        r##"
        $r = []
        # array / find / hash / alternation / pin / capture / guard
        [[0, 1, 2], {name: "Alice", age: 30}, 5, [1, 2, 3, 4, 5], "s"].each do |v|
          case v
          in [0, *rest]
            $r << [:arr, rest]
          in {name: String => n, age: Integer => a}
            $r << [:hash, n, a]
          in Integer | Float if v > 4
            $r << :num
          in [*pre, 3, *post]
            $r << [:find, pre, post]
          in String => s
            $r << [:str, s]
          end
        end
        # one-line forms
        $r << (1 in Integer)
        $r << ({a: 1} in {a: 0.. => x})
        {b: 7} => {b:}
        $r << b
        # pin expression, deconstruct_keys arguments, **rest
        obj = Object.new
        def obj.deconstruct_keys(keys)
          $r << keys
          {a: 1, b: 2, c: 3}
        end
        case obj
        in {a: ^(2 - 1), **rest}
          $r << rest
        end
        # exhaustion raises with the subject's inspect in the message
        begin
          case [9, 9]
          in [0]
          end
        rescue NoMatchingPatternError => e
          $r << e.message.include?("[9, 9]")
        end
        # deconstruct is cached across branches of one case, but the
        # cache resets between executions of the enclosing loop
        $decon_calls = 0
        arr = Object.new
        def arr.deconstruct
          $decon_calls += 1
          [1]
        end
        2.times do
          case arr
          in [0]
            $r << :zero
          in [1, *]
            $r << :one
          end
        end
        $r << $decon_calls
        # nested patterns and variable scope after the case
        case {status: [:ok, 200]}
        in {status: [Symbol => s, Integer => code]} if code < 300
          $r << [s, code]
        end
        $r
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
fn sclass_constants_cover_lookup_paths() {
    // Exercises every runtime-cref recovery path: `defined?` (the
    // probe/check-constant route), the ancestor-chain fallback when
    // the constant lives on the receiver's class rather than the
    // lexical scope, `def self.f` inside a class nested under
    // `class << obj` (owner is the metaclass, cref is the attached
    // class), and three receivers interleaved so the JIT compiles
    // against a constant cache filled for a different self class.
    run_test(
        r##"
        $r = []
        TOPC9 = :top
        # defined?(CONST) resolves through the runtime cref too. The
        # calls happen AFTER the loop so the earlier receivers see a
        # stale static stamp. The TOPC9 probe misses the singleton
        # class and walks out to the enclosing lexical scope.
        dobjs = []
        2.times do |i|
          $i = i
          obj = Object.new
          class << obj
            CONST_D = $i + 10
            def dcheck
              [defined?(CONST_D), CONST_D, defined?(TOPC9)]
            end
          end
          dobjs << obj
        end
        10.times { dobjs.each { |o| $r << o.dcheck } }
        # Lexical miss falls back to the runtime singleton class's
        # ancestors: the constant lives on each receiver's class. A
        # body-level read takes the same route without any `def`.
        class CCLP_A; CONST_F = :a; end
        class CCLP_B; CONST_F = :b; end
        fobjs = [CCLP_A.new, CCLP_B.new]
        fobjs.each do |obj|
          class << obj
            $r << CONST_F
            def fb; CONST_F; end
          end
        end
        10.times { fobjs.each { |o| $r << o.fb } }
        # `def self.f` in a class nested under `class << obj`: the
        # method's owner is the metaclass, but constants resolve in
        # the attached per-execution class.
        $classes2 = []
        2.times do |i|
          $i = i
          obj = Object.new
          class << obj
            CONST_OUTER = :o
            class Y
              $classes2 << self
              CONST_S = $i + 100
              def self.sfoo
                # The probe misses Y and hits the enclosing singleton
                # class, one lexical level out.
                [CONST_S, defined?(CONST_OUTER)]
              end
            end
          end
        end
        10.times { $classes2.each { |c| $r << c.sfoo } }
        $r << ($classes2[0] != $classes2[1])
        # `||=` on a constant in the body takes the definedness-check
        # route; re-opening the same singleton class hits its cache.
        qobj = Object.new
        3.times do
          class << qobj
            CONST_Q ||= :q
            $r << CONST_Q
          end
        end
        # Three receivers interleaved: the constant cache flips
        # between self classes while each method body gets JIT
        # compiled, forcing the cache-key mismatch recompile path.
        objs = []
        3.times do |i|
          $i = i
          o = Object.new
          class << o
            CONST_J = $i
            def jfoo; CONST_J; end
          end
          objs << o
        end
        30.times { objs.each_with_index { |o, i| $r << (o.jfoo == i) } }
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
