extern crate monoruby;
use monoruby::tests::*;

// define_method proc-methods (FuncKind::Proc) at JIT call sites: trivial
// bodies fold like plain methods (the ISeqHint set now covers block
// bodies — without unwrapping `return`, which in a block is a non-local
// method return — plus Symbol/Float immediates), and non-trivial bodies
// inline through `specialized_iseq` with the definition-time outer LFP
// baked into the frame and the frame marked as the `return` home
// (lambda-style).

#[test]
fn bmethod_fold_and_inline() {
    run_test(
        r#"
        x = 10
        define_method(:bm_const) { :b }
        define_method(:bm_one) { |arg| arg * 2 }
        define_method(:bm_cap) { x += 1; x }
        class BmK
          define_method(:me) { self }
          define_method(:ivar) { @v = (@v || 0) + 1 }
        end
        k = BmK.new
        r = []
        60.times { r << bm_const << bm_one(21) << bm_cap << (BmK === k.me) << k.ivar }
        [r.uniq.size, r.last(5), x]
        "#,
    );
}

#[test]
fn bmethod_lambda_return_through_inline() {
    // `return` inside a bmethod body returns from the bmethod itself
    // (lambda-style) — including through the specialized inline, where
    // the body frame is the return's home and the value must join the
    // caller's return state.
    run_test(
        r#"
        class BmR
          define_method(:pick) { |n| return :small if n < 10; :big }
          define_method(:always) { return 7; 8 }
        end
        o = BmR.new
        r = []
        50.times { |i| r << o.pick(i % 20) << o.always }
        r.uniq
        "#,
    );
}

#[test]
fn bmethod_redefinition_invalidates_fold() {
    // Folding a ConstReturn bmethod rides on the class-version guard:
    // re-`define_method` must invalidate warmed sites.
    run_test_once(
        r#"
        class BmRedef
          define_method(:tag) { :old }
        end
        o = BmRedef.new
        r = []
        40.times { r << o.tag }
        class BmRedef
          define_method(:tag) { :new }
        end
        40.times { r << o.tag }
        r.uniq
        "#,
    );
}

#[test]
fn block_hint_never_folds_nonlocal_return() {
    // A plain block whose body is `return <const>` must keep unwinding to
    // its home method — the block-body hint extension must not treat the
    // `return` as a trivial const body.
    run_test(
        r#"
        def bh_each2
          yield
          yield
          :not_reached
        end
        def bh_home
          bh_each2 { return 5 }
          :nope
        end
        r = []
        50.times { r << bh_home }
        r.uniq
        "#,
    );
}

#[test]
fn plain_method_symbol_and_float_fold() {
    // The new Symbol/Float immediates in the trivial-body hint apply to
    // plain methods too, guarded by the same class version.
    run_test_once(
        r#"
        class PmF
          def tag = :alpha
          def rate = 1.5
        end
        o = PmF.new
        r = []
        40.times { r << o.tag << o.rate }
        class PmF
          def tag = :beta
          def rate = 2.5
        end
        40.times { r << o.tag << o.rate }
        r.uniq
        "#,
    );
}

#[test]
fn shared_block_as_bmethod_and_proc() {
    // One block used both as a bmethod and as a plain Proc: the hint on
    // the shared iseq must stay correct for both invocation styles.
    run_test(
        r#"
        blk = proc { :shared }
        class BmShared; end
        BmShared.send(:define_method, :m, &blk)
        o = BmShared.new
        r = []
        40.times { r << o.m << blk.call }
        r.uniq
        "#,
    );
}
