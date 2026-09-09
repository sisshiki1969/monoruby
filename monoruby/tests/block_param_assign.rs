extern crate monoruby;
use monoruby::tests::*;

// A `&block` parameter that is assigned somewhere in the method body is
// a real local (materialized from the block at entry), not the lazy
// `BlockArg` read of the frame's block handler: a read after an
// assignment that did not run (`block = 1 if false`, `block ||= ...`)
// must still see the Proc, a nested block may assign it, and a closure
// created before the assignment sees the new value. optparse's
// `accept(t, pat = /.*/m, &block)` (`block = ... unless block`) is the
// shape that lost every no-argument switch's block under monoruby.

#[test]
fn block_param_survives_an_unexecuted_assignment() {
    run_test(
        r#"
        def v1(t, pat = /.*/m, &block)
          unless block
            block = 1 if pat.respond_to?(:convert)
          end
          block.class
        end
        def v2(&block)
          block = 1 if false
          block.class
        end
        def v3(&block)
          block = 1 unless block
          block.class
        end
        def v4(&block)
          x = block.class
          block = 2
          [x, block]
        end
        def v5(&block); block ||= proc { :dflt }; block.call; end
        def v6(&block); block, x = nil, 1; [block, x]; end
        def v7(&block); (block = 1) rescue 0; block; end
        def v8(&block); block &&= :and; block; end
        def v9(&block); block = (block.call + 1 rescue block.class); block; end
        [v1(1) {}, v2 {}, v3 {}, v4 {}, v5, v5 { :given }, v6 {}, v7 {}, v8 {}, v8, v9 {}, v9 { 41 }]
        "#,
    );
}

#[test]
fn block_param_assigned_from_a_nested_block_or_closure() {
    run_test(
        r#"
        def m1(&block); [1].each { block = 1 }; block.class; end
        def m2(&block); [1].each { block = 1 if false }; block.class; end
        def m3(&block); pr = proc { block }; block = 2; [pr.call, block]; end
        def m4(&block); f = ->(x) { block = x }; f.(3); block; end
        def m5(&block); [1].each { [2].each { block = :deep } }; block; end
        def m6(&block); [1].each { |block| block = 9 }; block.class; end
        def m7(&block); pr = proc { block = :from_proc; block }; [pr.call, block]; end
        [m1 {}, m2 {}, m3 {}, m4 {}, m5 {}, m6 {}, m7 {}]
        "#,
    );
}

#[test]
fn reassigned_block_param_as_a_value_and_a_block() {
    run_test(
        r#"
        def call_it(&block); block = proc { :replaced } if block.nil?; block.call; end
        def pass_it(&block); block = proc { |x| x * 2 } unless block; [1, 2].map(&block); end
        def yield_it(&block); block = nil if false; yield 5; end
        def forward(&block); block = block; [3].map(&block); end
        def arity(&block); block = block || proc { |a, b| }; block.arity; end
        def lam(&block); block = 1 if false; [block.lambda?, block.class]; end
        [call_it, call_it { :given }, pass_it, pass_it { |x| x + 1 }, yield_it { |v| v + 1 }, forward { |x| x * 3 }, arity, arity { |a| }, lam(&->(x) {})]
        "#,
    );
}

#[test]
fn block_param_of_a_block_or_define_method_can_be_reassigned() {
    run_test(
        r#"
        r = []
        pr = proc { |&b| b = proc { :dflt } unless b; b.call }
        r << pr.call << pr.call { :given }
        class C
          define_method(:dm) { |&b| b = nil if false; b ? b.call : :none }
        end
        r << C.new.dm << C.new.dm { :yes }
        r << (lambda { |&b| b = 3; b }).call
        r
        "#,
    );
}
