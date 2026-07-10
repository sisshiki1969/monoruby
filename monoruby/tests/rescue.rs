extern crate monoruby;
use monoruby::tests::*;

#[test]
fn begin_use() {
    run_tests(&[
        r#"
            # Use
            begin
              100
            end
        "#,
        r#"
            # NoUse
            begin
              100
            end
            nil
        "#,
    ]);
}

#[test]
fn begin_ret() {
    run_test_once(
        r#"
            # Ret
            begin
              100
            end
        "#,
    );
}

#[test]
fn rescue_ret() {
    run_test_once(
        r#"
            #Ret
            begin
              100
            rescue
              200
            end
        "#,
    );
}

#[test]
fn rescue_use() {
    run_tests(&[
        r#"
            begin
              100
            rescue
              200
            end
        "#,
        r#"
            begin
              100
            rescue
            else
              200
            end
        "#,
        r#"
            begin
              100
            rescue
            else
              200
            ensure
              300
            end
        "#,
    ]);
}

#[test]
fn rescue8() {
    run_test(
        r#"
            $x = []
            begin
                begin
                    $x << 50
                    1/0
                    $x << 100
                rescue => c
                    $x << 150
                else
                    $x << 200
                ensure
                    $x << 250
                    1/0
                    $x << 300
                end
            rescue => d
                $x << d.to_s
            end
            $x
        "#,
    );
}

#[test]
fn rescue_write_back1() {
    run_tests(&[
        r#"
        res = []
        for i in 0..10
            begin
                x = 50.0 + i
                1/0
                x = 100.0
            rescue => c
                res << x
                x = 150.0
            else
                x = 200.0
            ensure
                x = 300.0
            end
        end
        res << x
        res
        "#,
        r#"
        res = []
        begin
            for i in 0..10
                x = 50.0 + 50/(9-i)
                res << x
            end
        rescue => c
            res << x
            x = 150.0
        else
            x = 200.0
        ensure
            x = 300.0
        end
        res << x
        "#,
    ]);
}

#[test]
fn block_return_ensure() {
    run_test_with_prelude(
        r#"
            $x = []
            [foo, $x]
            "#,
        r#"
            def foo
              2.times do |i|
                2.times  do |j|
                  $x << [i,j]
                  return 3 if i == 1 && j == 0
                ensure
                  $x << ["j",j]
                end
              ensure
                $x << ["i",i]
              end
            ensure
              $x << "foo"
            end
            "#,
    );
}

#[test]
fn eval_return_ensure() {
    run_test_with_prelude(
        r#"
            $x = []
            $x << foo
            $x
            "#,
        r#"
            def foo
              begin
                eval("return 42")
                $x << "after eval"
              ensure
                $x << "ensure"
              end
            end
            "#,
    );
}

#[test]
fn retry1() {
    run_test(
        r#"
            $x = 0
            begin
              $x += 1
              raise "err" if $x < 3
            rescue
              retry
            end
            $x
        "#,
    );
}

#[test]
fn retry2() {
    run_test(
        r#"
            x = 0.0
            $res = []
            begin
              x += 1.0
              $res << x
              raise "err" if x < 3.0
              $res << "done"
            rescue
              $res << "caught"
              retry
            end
            $res
        "#,
    );
}

#[test]
fn retry3() {
    run_test(
        r#"
            $x = 0
            begin
              $x += 1
              raise "err" if $x < 2
              $x
            rescue
              retry
            ensure
              $x += 10
            end
        "#,
    );
}

#[test]
fn return_in_ensure() {
    run_test_with_prelude(
        "foo",
        r#"
            def foo
              1
            ensure
              return 42
            end
        "#,
    );
    run_test_with_prelude(
        "bar",
        r#"
            def bar
              1
            rescue
              2
            ensure
              return 99
            end
        "#,
    );
    run_test_with_prelude(
        "[baz, $x]",
        r#"
            def baz
              $x = []
              $x << :body
              $x << :ensure_start
              return 100 unless $!
            ensure
              $x << :ensure
              return 200
            end
        "#,
    );
}

#[test]
fn retry_in_loop() {
    run_tests(&[
        r#"
            res = []
            50.times do
              x = 0.0
              begin
                x += 1.0
                raise "err" if x < 3.0
              rescue
                i = 0
                while i < 50
                  retry if i == 49
                  i += 1
                end
              end
              res << x
            end
            res
        "#,
        r#"
            x = raise("err") rescue 42
            x
        "#,
        r#"
            x = 10
            x += raise("err") rescue 5
            x
        "#,
        r#"
            x = nil
            x ||= raise("err") rescue 99
            x
        "#,
        r#"
            x = true
            x &&= raise("err") rescue 77
            x
        "#,
        r#"
            a, b = raise("err") rescue [1, 2]
            [a, b]
        "#,
    ]);
}

#[test]
fn rescue_modifier_constant_assign() {
    run_test_once(
        r#"
            X = raise("err") rescue 100
            X
        "#,
    );
}

#[test]
fn rescue_modifier_no_rescue() {
    run_test(
        r#"
            x = 42
            x
        "#,
    );
}

#[test]
fn rescue_modifier_ivar_assign() {
    run_test(
        r#"
            class C
              def initialize
                @x = raise("err") rescue 55
              end
              def x; @x; end
            end
            C.new.x
        "#,
    );
}

#[test]
fn rescue_modifier_accessor_assign() {
    run_test(
        r#"
            class D
              attr_accessor :val
            end
            d = D.new
            d.val = raise("err") rescue 33
            d.val
        "#,
    );
}

#[test]
fn load_error_path() {
    run_test(
        r#"
            begin
              require "nonexistent_file_that_does_not_exist_12345"
            rescue LoadError => e
              [e.class.name, e.path]
            end
        "#,
    );
}

#[test]
fn dollar_bang_cleared_after_rescue() {
    run_test(
        r#"
            begin
              raise "err"
            rescue
            end
            $!
        "#,
    );
}

#[test]
fn dollar_bang_cleared_on_return_from_rescue() {
    run_test_with_prelude(
        "foo",
        r#"
            def foo
              begin
                raise "err"
              rescue
                return $!.message
              end
            end
        "#,
    );
    run_test_with_prelude(
        "[bar, $!.inspect]",
        r#"
            def bar
              begin
                raise "err"
              rescue
                return 42
              end
            end
        "#,
    );
}

#[test]
fn dollar_bang_nil_at_toplevel() {
    run_test(
        r#"
            $!
        "#,
    );
}

#[test]
fn raise_no_args_without_current_exception() {
    run_test_error(
        r#"
            raise
        "#,
    );
}

#[test]
fn rescue_safe_nav_target() {
    // `rescue => recv&.attr`: the caught exception is stored via a
    // safe-navigation setter; a nil receiver skips the store (the
    // exception is still rescued).
    run_test_with_prelude(
        r##"
        res = []
        t = T.new
        begin; raise "boom"; rescue => t&.e; end
        res << t.e.message
        n = nil
        begin; raise "x"; rescue => n&.e; end
        res << n
        # with an explicit exception class, and as a value.
        r = begin; raise TypeError, "z"; rescue TypeError => t&.e; :done; end
        res << [r, t.e.class.name]
        # JIT warm-up.
        i = 0
        while i < 200
          begin; raise "b#{i}"; rescue => t&.e; end
          i += 1
        end
        res << t.e.message
        res
        "##,
        "class T; attr_accessor :e; end",
    );
}

#[test]
fn eval_custom_filename_and_lineno() {
    run_tests(&[
        r#"
            eval("__FILE__", nil, "myfile.rb", 10)
        "#,
        r#"
            eval("__LINE__", nil, "myfile.rb", 10)
        "#,
        r#"
            Object.new.instance_eval("__FILE__", "inst.rb", 3)
        "#,
        r#"
            Object.new.instance_eval("__LINE__", "inst.rb", 3)
        "#,
        r#"
            Integer.class_eval("__FILE__", "cls.rb", 7)
        "#,
        r#"
            Integer.class_eval("__LINE__", "cls.rb", 7)
        "#,
        r#"
            Integer.module_eval("__FILE__", "custom.rb", 5)
        "#,
        r#"
            Integer.module_eval("__LINE__", "custom.rb", 5)
        "#,
    ]);
}

#[test]
fn rescue_clause_requires_class_or_module() {
    run_test(
        r#"
        res = []
        rescuer = 42
        3.times do
          begin
            begin
              raise "error"
            rescue rescuer
              res << "wrong"
            end
          rescue TypeError => e
            res << e.message
          rescue RuntimeError => e
            res << "runtime: #{e.message}"
          end
        end
        res
        "#,
    );
}

#[test]
fn rescue_clause_private_teq() {
    // The rescue-clause `===` dispatch has funcall semantics once the
    // clause passes the Class/Module check.
    run_test(
        r#"
        klass = Class.new(StandardError)
        class << klass
          def ===(e); e.message == "yes"; end
          private :===
        end
        res = []
        [["yes", :caught], ["no", :other]].map do |msg, _|
          begin
            raise StandardError, msg
          rescue klass
            res << "special #{msg}"
          rescue StandardError
            res << "plain #{msg}"
          end
        end
        res
        "#,
    );
}

// Rescue-clause matching in a JIT-hot method: the method is called
// more than 20 times (the production method-JIT threshold used by
// integration-test binaries), so its happy path is JIT-compiled,
// while every exception dispatch side-exits to the interpreter,
// whose opcode-157 handler validates the clause and dispatches `===`
// with funcall semantics. The JIT deliberately has no lowering for
// the rescue-clause match — its BB graph never enters exception
// handlers — and this test pins the tier hand-off behavior.
#[test]
fn rescue_clause_match_jit() {
    run_test(
        r#"
        def rescue_match(exc_msg, clause)
          begin
            begin
              raise RuntimeError, exc_msg
            rescue clause => e
              "caught #{e.message}"
            rescue RuntimeError => e
              "unmatched #{e.message}"
            end
          rescue TypeError => e
            "type: #{e.message}"
          end
        end

        special = Class.new(RuntimeError)
        res = []
        30.times do |i|
          res << rescue_match("m#{i}", RuntimeError)   # clause matches
          res << rescue_match("n#{i}", special)        # clause misses
          res << rescue_match("o#{i}", 42)             # clause invalid -> TypeError
        end
        res
        "#,
    );
}

#[test]
fn rescue_splat_clause_requires_class_or_module() {
    run_test(
        r#"
        res = []
        [[42], [RuntimeError], [TypeError, RuntimeError], []].each do |list|
          begin
            begin
              raise "boom"
            rescue *list
              res << "caught"
            rescue RuntimeError
              res << "unmatched"
            end
          rescue TypeError => e
            res << e.message
          end
        end
        # An empty rescue body plus no exception: the clause list is
        # never evaluated, so an invalid element does not raise.
        begin
          res << :fine
        rescue *[42]
        end
        res
        "#,
    );
}

#[test]
fn singleton_method_backtrace_naming() {
    // CRuby names a plain object's singleton method by the bare
    // method name in backtraces; class/module singletons get the
    // `Owner.name` form. Compare only the method-name part (paths
    // and line numbers of the harness temp files differ).
    run_test(
        r#"
        res = []
        o = Object.new
        def o.sing_meth = raise("x")
        module BTM; def self.mod_meth = raise("y"); end
        class BTC; def self.cls_meth = raise("z"); end
        [proc { o.sing_meth }, proc { BTM.mod_meth }, proc { BTC.cls_meth }].each do |p|
          begin
            p.call
          rescue => e
            res << e.backtrace.first.split(":in ").last
          end
        end
        res
        "#,
    );
}

#[test]
fn caught_throw_restores_errinfo() {
    run_test(
        r#"
        res = []
        catch(:a) do
          begin
            raise "boom"
          rescue
            throw :a
          end
        end
        res << $!.inspect
        # An uncaught tag propagating out of an inner catch leaves the
        # in-flight exception alone until the matching catch.
        begin
          raise "outer"
        rescue
          catch(:m) { catch(:inner) { throw :m } }
          res << $!.message
        end
        res
        "#,
    );
}

#[test]
fn return_from_rescue_restores_errinfo() {
    // A return leaving a rescue clause restores `$!` to each crossed
    // begin region's entry value, innermost first, running ensure
    // bodies against the restored value — so implicit exception-cause
    // chaining keeps working after the method returns.
    run_test(
        r#"
        def restore_check(log)
          outer = StandardError.new "outer"
          inner = StandardError.new "inner"
          begin
            raise outer
          rescue
            log << ($! == outer)
            begin
              raise inner
            rescue
              log << ($! == inner)
              return
            ensure
              log << ($! == outer ? :outer : :wrong)
            end
          end
        end
        log = []
        restore_check(log)
        log << $!.inspect
        begin
          raise "original"
        rescue
          helper = proc do
            begin
              Object.new.missing_thing
            rescue Exception => e
              e
            end
          end
          e1 = helper.call
          log << [e1.class.to_s, e1.cause.class.to_s]
          begin
            no_such()
          rescue NoMethodError => e2
            log << e2.cause.class.to_s
          end
        end
        log
        "#,
    );
}

#[test]
fn exception_inspect_formats() {
    run_test(
        r#"
        res = []
        begin; raise; rescue => e; res << e.inspect << e.message; end
        begin; raise RuntimeError; rescue => e; res << e.inspect; end
        begin; raise RuntimeError, ""; rescue => e; res << e.inspect; end
        begin; raise "msg"; rescue => e; res << e.inspect; end
        res
        "#,
    );
}

#[test]
fn break_next_from_rescue_in_while_restores_errinfo() {
    // break / next leaving a rescue clause inside a `while` loop use
    // the same per-region restore protocol as return: the region's
    // ensure runs against the restored (outer) `$!`.
    run_test(
        r#"
        def brk_while(log)
          outer = StandardError.new "outer"
          begin
            raise outer
          rescue
            i = 0
            while i < 3
              begin
                raise "inner #{i}"
              rescue
                break if i == 2
                i += 1
                next
              ensure
                log << $!&.message
              end
            end
            log << $!.message
          end
        end
        log = []
        brk_while(log)
        log << $!.inspect
        log
        "#,
    );
}
