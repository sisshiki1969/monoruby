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

#[test]
fn break_through_nested_yield_forwarding() {
    // issue #982: `&b` forwarded from inside a block that is itself run
    // through *another* literal block's `yield`. The forwarded block
    // handler used to be re-based on a statically guessed cfp distance,
    // which mis-identified the block's defining frame and turned the
    // `break` into a bogus `LocalJumpError`.
    run_test_once(
        r#"
        def base; yield :s; end
        def relay; base { |s| yield(s) }; end
        def inner; yield; end
        res = []
        def outer(&b); relay { |_s| inner(&b) }; end
        res << (outer { break 3 })
        res << (outer { 7 })
        # class-hierarchy variant (ruby/spec's ServerLoopPortFinder shape)
        class BaseH
          def self.helper
            yield [:sock]
          ensure
            :cleanup
          end
        end
        class SubH < BaseH
          def self.helper
            super() { |s| yield(s) }
          end
        end
        def accept_loop(_s); loop { yield :conn }; end
        def server_loop(&b); SubH.helper { |s| accept_loop(s, &b) }; end
        res << (server_loop { break :a })
        # zsuper forwarding the block from inside a nested block
        class P1
          def m; yield :zs; end
        end
        class P2 < P1
          def m; relay { |_s| super }; end
        end
        res << (P2.new.m { |x| break [:zsuper, x] })
        # `...` forwarding from inside a nested block
        def fwd_target(a, &b); inner(&b); end
        def fwd(...); relay { |_s| fwd_target(...) }; end
        res << (fwd(1) { break :fwd })
        res
        "#,
    );
}

#[test]
fn break_through_nested_yield_forwarding_jit() {
    // Same shape, hot enough to be JIT-compiled.
    run_test(
        r#"
        def base2; yield :s; end
        def relay2; base2 { |s| yield(s) }; end
        def inner2; yield; end
        def outer_jit(&b); relay2 { |_s| inner2(&b) }; end
        res = nil
        50.times { res = outer_jit { break :jit } }
        res
        "#,
    );
}
