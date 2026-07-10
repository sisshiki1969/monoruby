extern crate monoruby;
use monoruby::tests::*;

#[test]
fn break_valid_paths() {
    // break escapes the invocation of the call that received the block
    // literal — direct iteration, &b forwarding while the receiving
    // call is active, and Ruby-defined iterators are all valid.
    run_test_once(
        r#"
        res = []
        res << [1,2].each { break :direct }
        def fwd(&b); r = [1,2].each(&b); [:fwd_after, r]; end
        res << (fwd { break :forwarded })
        res << [1,2].map { break :mapped }
        res << ([1,2].to_enum.each { break :enum })
        res << ({x: 1}.each_pair { break :hash })
        res << ([3,1].sort { |a,b| break :sorted })
        res << ([1,2].each_with_object([]) { break :ewo })
        h = [1].each { [2].each { break :nested } }
        res << h
        res
        "#,
    );
}

#[test]
fn break_from_captured_proc_raises() {
    // A block materialized into a Proc and invoked outside its original
    // receiving call raises LocalJumpError, even while the defining
    // scope is still active (CRuby's BREAK catch-table semantics).
    run_test_once(
        r#"
        def cap(&b); b; end
        res = []
        def use_call
          b = cap { break :x }
          b.call
          :no_error
        rescue LocalJumpError => e
          [:lje, e.message]
        end
        res << use_call
        def yielder; yield; end
        def use_reyield
          b = cap { break :y }
          [:got, yielder(&b)]
        rescue LocalJumpError => e
          [:lje2, e.message]
        end
        res << use_reyield
        begin
          proc { break :z }.call
        rescue LocalJumpError => e
          res << [:lje3, e.message]
        end
        # lambda break is local
        res << lambda { break :local }.call
        res
        "#,
    );
}

#[test]
fn break_ensure_interaction() {
    run_test_once(
        r#"
        class BT
          def one; two { yield }; end
          def two
            yield
          ensure
            $sp << :two_ensure
          end
          def three
            begin
              one { break }
              $sp << :three_post
            ensure
              $sp << :three_ensure
            end
          end
        end
        $sp = []
        BT.new.three
        $sp
        "#,
    );
}

#[test]
fn break_in_thread_body_raises() {
    run_test_once(
        r#"
        t = Thread.new do
          begin
            break :break
          rescue LocalJumpError => e
            e
          end
        end
        v = t.value
        [v.class.to_s, v.instance_of?(LocalJumpError)]
        "#,
    );
}

#[test]
fn break_jit_tier() {
    // Hot loop: break through iteration keeps working under the JIT.
    run_test(
        r#"
        def find_first(arr)
          arr.each { |x| break x if x > 2 }
        end
        res = []
        40.times { res << find_first([1, 2, 3, 4]) }
        res.last(2)
        "#,
    );
}
