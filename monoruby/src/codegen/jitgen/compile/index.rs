use crate::executor::inline::InlineFuncInfo;

use super::*;

impl<'a> JitContext<'a> {
    ///
    /// Attempt guard-free-*prelude* inline emission of `recv_class#op`
    /// (`[]` / `[]=`) through the registered inline generator — the index
    /// counterpart of [`fire_binary_inline`](Self::fire_binary_inline) and
    /// [`fire_unary_inline`](Self::fire_unary_inline).
    ///
    /// Unlike the numeric direct-fire paths this **keeps the receiver class
    /// guard**: the index generators do not all guard their receiver
    /// themselves (`hash_index` loads it unguarded, and `array_index`'s
    /// `load_array_ty` proves the object *type*, not the exact class, so an
    /// `Array` subclass with its own `#[]` would slip through). What it drops
    /// is the class-version guard and the rest of the `compile_method_call`
    /// prelude, because a redefinition of `Array#[]` / `Hash#[]` / `Array#[]=`
    /// is caught by the recorded bop_dep instead — `basic_op_assumable` only
    /// answers `true` for pairs the eviction machinery tracks, which is what
    /// keeps `Hash#[]=` (deliberately absent from `BASIC_OP_DEFS`) on the
    /// ordinary call path.
    ///
    /// Returns `false` when the method doesn't resolve, has no generator, is
    /// blocked by visibility, has lost its basic-op license, or the generator
    /// declined; the caller then takes the ordinary method call, which emits
    /// the visibility deopt / the cached call as before.
    ///
    fn fire_index_inline(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        op: IdentId,
        base: SlotId,
        recv_class: ClassId,
        idx_class: Option<ClassId>,
        bc_pos: BcIndex,
    ) -> bool {
        let Some((fid, visibility)) = self.jit_check_method(recv_class, op) else {
            return false;
        };
        let Some(InlineFuncInfo::InlineGen(f)) = self.store.inline_info.get_inline(fid) else {
            return false;
        };
        if !self.basic_op_assumable(recv_class, op) {
            return false;
        }
        let callid = self.store.get_callsite_id(self.iseq_id(), bc_pos).unwrap();
        debug_assert_eq!(self.store[callid].recv, base);
        // A private `#[]` reached without an explicit `self` receiver, and a
        // callee that could capture the frame, both belong on the ordinary
        // path (which raises / refuses exactly as before).
        if self.jit_visibility_blocks(callid, visibility)
            || self.store[fid].possibly_capture_without_block()
            || self.store[callid].block_fid.is_some()
        {
            return false;
        }
        // The receiver guard and the generator emit as one unit: a generator
        // that declines after the guard was emitted must leave no trace.
        let state_save = state.clone();
        let ir_save = ir.save();
        if state.class(base) != Some(recv_class) {
            let deopt = ir.new_deopt(state);
            state.load(ir, base, GP::Rdi);
            state.guard_class(ir, base, GP::Rdi, recv_class, deopt);
        }
        if self.inline_asm(state, ir, f, callid, recv_class, idx_class) {
            self.record_bop_dep(recv_class, op);
            state.unset_side_effect_guard();
            true
        } else {
            *state = state_save;
            ir.restore(ir_save);
            false
        }
    }

    pub(super) fn index(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        base: SlotId,
        idx: SlotId,
        ic: Option<(ClassId, ClassId)>,
        bc_pos: BcIndex,
    ) -> JitResult<CompileResult> {
        let (base_class, idx_class) = state.binary_class(base, idx, ic);
        let Some(base_class) = base_class else {
            return Ok(CompileResult::Recompile(RecompileReason::NotCached));
        };
        if self.fire_index_inline(
            state,
            ir,
            IdentId::_INDEX,
            base,
            base_class,
            idx_class,
            bc_pos,
        ) {
            return Ok(CompileResult::Continue);
        }
        // Visibility (`#[]` may be private) is enforced in the shared
        // `compile_method_call` choke point (`jit_visibility_blocks`),
        // using the call site's func-call flag: a plain `obj[i]` deopts to
        // the VM (which raises `NoMethodError`) while an explicit-`self`
        // `self[i]` compiles inline.
        self.call_binary_method(
            state,
            ir,
            base,
            idx,
            base_class,
            idx_class,
            IdentId::_INDEX,
            bc_pos,
            false,
        )
    }

    pub(super) fn index_assign(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        base: SlotId,
        idx: SlotId,
        src: SlotId,
        ic: Option<(ClassId, ClassId)>,
        bc_pos: BcIndex,
    ) -> JitResult<CompileResult> {
        let (base_class, idx_class) = state.binary_class(base, idx, ic);
        let Some(base_class) = base_class else {
            return Ok(CompileResult::Recompile(RecompileReason::NotCached));
        };
        if self.fire_index_inline(
            state,
            ir,
            IdentId::_INDEX_ASSIGN,
            base,
            base_class,
            idx_class,
            bc_pos,
        ) {
            return Ok(CompileResult::Continue);
        }
        // Visibility (`#[]=` may be private) is enforced in the shared
        // `compile_method_call` choke point (`jit_visibility_blocks`),
        // using the call site's func-call flag: a plain `obj[i] = v` deopts
        // to the VM (which raises `NoMethodError`) while an explicit-`self`
        // `self[i] = v` compiles inline.
        self.call_ternary_method(
            state,
            ir,
            base,
            idx,
            src,
            base_class,
            idx_class,
            IdentId::_INDEX_ASSIGN,
            bc_pos,
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn index_inline_hot_loops() {
        // Array / Hash reads and writes in a JIT-compiled loop, in the shapes
        // the generators cover (fixnum index, literal index, the slice-assign
        // form) and one they decline (a Range index).
        run_test_with_prelude(
            r##"
            drive
        "##,
            r##"
            def drive
              a = [1, 2, 3, 4, 5, 6, 7, 8]
              h = {a: 1, "b" => 2, 3 => 4}
              res = []
              j = 0
              while j < 30
                res << a[0]
                res << a[j % 8]
                res << a[-1]
                res << a[1, 3]
                a[j % 8] = j
                a[0, 2] = [9, 9]
                res << h[:a]
                res << h["b"]
                res << h[3]
                res << h[:missing]
                h[:a] = j
                j = j + 1
              end
              res << a
              res << h
              res
            end
        "##,
        );
    }

    #[test]
    fn index_redefinition_evicts() {
        // `Array#[]` / `Array#[]=` are tracked basic ops, so a post-warmup
        // redefinition evicts the bodies that inlined them guard-free.
        run_test_once(
            r##"
            def get(a, i) = a[i]
            def set(a, i, v) = (a[i] = v)
            res = []
            40.times { |j| a = [1, 2, 3]; set(a, 0, j); res << get(a, 0) }
            class Array
              def [](i) = :redefined_get
              def []=(i, v)
                :redefined_set
              end
            end
            a = [1, 2, 3]
            res << set(a, 0, 99)
            res << get(a, 0)
            res << a
            res
        "##,
        );
    }

    #[test]
    fn index_hash_assign_redefinition() {
        // `Hash#[]=` is deliberately absent from BASIC_OP_DEFS, so it must
        // NOT be direct-fired: the class-version guard on the ordinary call
        // path is what makes this redefinition visible.
        run_test_once(
            r##"
            def set(h, k, v) = (h[k] = v)
            res = []
            40.times { |j| h = {}; set(h, :k, j); res << h[:k] }
            class Hash
              def []=(k, v)
                :redefined
              end
            end
            h = {}
            res << set(h, :k, 1)
            res << h
            res
        "##,
        );
    }

    #[test]
    fn index_array_subclass_override() {
        // An `Array` subclass that overrides `#[]` resolves to its own method,
        // and a site compiled for plain `Array` must guard the exact class —
        // the array *type* guard alone would let the subclass through.
        run_test_with_prelude(
            r##"
            drive
        "##,
            r##"
            class MyArray < Array
              def [](i) = :sub
            end
            class PlainSub < Array
            end
            def get(a) = a[0]
            def drive
              plain = [1, 2, 3]
              sub = MyArray.new(3, 7)
              plainsub = PlainSub.new(3, 8)
              res = []
              j = 0
              while j < 30
                res << get(plain)
                res << get(plainsub)
                res << get(sub)
                j = j + 1
              end
              res
            end
        "##,
        );
    }

    #[test]
    fn index_private_receiver() {
        // A private `#[]` is callable only through an explicit `self`
        // receiver; the plain form must raise NoMethodError.
        run_test_once(
            r##"
            class Priv
              def initialize = (@a = [1, 2, 3])
              private def [](i) = @a[i]
              def via_self(i) = self[i]
            end
            p1 = Priv.new
            res = []
            40.times { res << p1.via_self(1) }
            begin
              p1[1]
            rescue NoMethodError
              res << :nome
            end
            res
        "##,
        );
    }
}
