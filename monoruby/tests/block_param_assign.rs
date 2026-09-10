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

// The parameter's slot starts as an unassigned sentinel; a read
// materializes the frame's block into one Proc and caches it in the
// frame, so every read (and a closure's read, and `binding`) answers the
// same object until an assignment replaces it; `yield` keeps the
// original block regardless.

#[test]
fn block_param_reads_answer_one_proc() {
    run_test(
        r#"
        def a(&b); [b.equal?(b), b.object_id == b.object_id, 3.times.map { b.object_id }.uniq.size]; end
        def h(&b); pr = proc { b }; [pr.call.equal?(b), (b = 5; pr.call)]; end
        def q(&b); b; end
        x = q { }
        y = q(&x)
        def c(&b); x = b; b = nil; [x.class, b]; end
        def j(&b); b = proc { :new }; [yield, b.call, [1].map(&b)]; end
        def i(&b); b ? yield : :none; end
        [a {}, h {}, x.equal?(y), c {}, j { :orig }, i { :y }, i]
        "#,
    );
}

#[test]
fn block_param_through_binding_and_eval() {
    run_test(
        r#"
        def d(&b); binding.local_variable_get(:b); end
        def e(&b); bd = binding; bd.local_variable_set(:b, 7); [b, bd.local_variable_get(:b)]; end
        def f(&b); r = b; [1].each { b = 2 }; [r.class, b, binding.local_variable_get(:b)]; end
        def g(&b); eval("b").class; end
        def r(&b); binding.local_variables.include?(:b); end
        def s(&b); [1].each { |b| b = 9 }; b.class; end
        [d {}.class, d.class, e {}, f {}, g {}, r {}, s {}]
        "#,
    );
}

#[test]
fn anonymous_block_forwarding_has_no_slot() {
    run_test(
        r#"
        def k(&b); [1, 2].map(&b); end
        def l(&); k(&); end
        def m(...); k(...); end
        def n(&b); b.nil?; end
        def o(&b); b.lambda?; end
        [k { |x| x * 2 }, l { |x| x + 1 }, m { |x| x - 1 }, n, n {}, o(&->() {}), o {}]
        "#,
    );
}

// `run_test` warms the JIT: a literal assigned to the parameter is
// known to the abstract state, so the JIT folds the read / forwarding
// to it (no slot test); the other cases keep the run-time test.

#[test]
fn jit_folds_a_literal_assigned_to_the_block_param() {
    run_test(
        r#"
        def k(&b); [1, 2].map(&b); end
        def none(&b); b = nil; [b, k(&b).class, block_given?]; end
        def sym(&b); b = :to_s; [b, k(&b)]; end
        def lit(&b); b = 7; b; end
        def sym_handler(&b); [b.class, b.call(3), b.equal?(b)]; end
        def outer_store(&b); r = nil; [1].each { r = b }; [r.class, r.equal?(b)]; end
        def outer_value(&b); [1].map { b }.first.equal?(b); end
        [none { :x }, sym { :x }, lit { :x }, sym_handler(&:succ), outer_store {}, outer_value {}]
        "#,
    );
}
