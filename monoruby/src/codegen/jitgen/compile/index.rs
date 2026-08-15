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

    ///
    /// The receiver class a polymorphic index site should give its *inlined*
    /// arm: the one the VM observed here that actually has a generator, most
    /// observed first.
    ///
    /// Deliberately **not** the inline cache's class. optcarrot's
    /// `@fetch[addr][addr]` is the motivating shape — `@fetch` holds the RAM
    /// `Array` for `0x0000..0x07ff` and a `Method` for every I/O register, so
    /// the site alternates — and its cache happens to hold `Method`, which
    /// has no generator. Inlining "the cached class" there would leave the
    /// Array arm (every RAM read) on the C call, which is backwards. The
    /// other arm is a C call either way, so preferring the class that can be
    /// inlined is unambiguous.
    ///
    fn index_inline_class(&mut self, callid: CallSiteId) -> Option<(ClassId, FuncId)> {
        let callsite = &self.store[callid];
        let name = IdentId::_INDEX;
        let pmc = &callsite.pmc;
        // Two-arm dispatch only pays off where the site really alternates;
        // one observed class is the monomorphic guard's case.
        if pmc.entries().len() < 2 {
            return None;
        }
        let mut classes: Vec<(ClassId, u32)> =
            pmc.entries().iter().map(|e| (e.recv, e.count)).collect();
        classes.sort_unstable_by_key(|(_, count)| std::cmp::Reverse(*count));
        for (class, _) in classes {
            let Some((fid, visibility)) = self.jit_check_method(class, name) else {
                continue;
            };
            if !matches!(
                self.store.inline_info.get_inline(fid),
                Some(InlineFuncInfo::InlineGen(_))
            ) {
                continue;
            }
            // Same licence the guarded direct-fire path needs: the arm runs
            // without a class-version guard, so a redefinition has to reach
            // it through the recorded bop dependency.
            if !self.basic_op_assumable(class, name)
                || self.jit_visibility_blocks(callid, visibility)
                || self.store[fid].possibly_capture_without_block()
            {
                continue;
            }
            return Some((class, fid));
        }
        None
    }

    ///
    /// Answer a polymorphic `[]` site with a two-arm dispatch:
    ///
    /// ```text
    ///         br_class_ne rdi, C -> slow
    ///         <C#[] inlined>
    ///         br merge
    ///   slow: <runtime::get_index>      (correct for *any* receiver)
    ///   merge:
    /// ```
    ///
    /// The point is the missing third option: there is no deopt. A class
    /// guard has to send every off-class receiver back to the interpreter,
    /// which is what makes optcarrot's `@fetch[addr][addr]` the single
    /// largest deopt source in its hot loop; here the off-class receiver
    /// takes a C call instead, and `runtime::get_index` handles it — it
    /// re-checks the basic-op licence itself and falls back to
    /// `invoke_method(:[])`, so `Method#[]` dispatches correctly.
    ///
    /// The merge needs no state join machinery, unusually: both arms leave
    /// the element as a plain `Value` in `dst`, so the merge state can be
    /// *declared* up front and each arm bridged to it.
    ///
    /// Returns `false` (leaving `state` and `ir` untouched) when the site
    /// does not qualify, so the caller takes the ordinary path.
    ///
    fn index_dispatch(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        base: SlotId,
        idx: SlotId,
        idx_class: Option<ClassId>,
        bc_pos: BcIndex,
    ) -> JitResult<bool> {
        let Some(callid) = self.store.get_callsite_id(self.iseq_id(), bc_pos) else {
            return Ok(false);
        };
        if self.store[callid].block_fid.is_some() {
            return Ok(false);
        }
        let Some((inline_class, fid)) = self.index_inline_class(callid) else {
            return Ok(false);
        };
        let dst = self.store[callid].dst;
        // For an index site `is_func_call` means "the base is literally
        // `self`" (`bypass_visibility` is only ever set on the `__builtin_*`
        // method-call spelling), which `gen_index` keeps at slot 0 so a
        // private `#[]` stays reachable as in CRuby. It is therefore always
        // `false` *here*: the gate below only takes sites whose receiver
        // class the abstract state cannot pin down, and `self`'s class is
        // seeded by `SlotState::new` for every `JitType` and never cleared.
        // Passed through rather than hard-coded so the residual arm's
        // visibility stays correct if that gate is ever widened.
        let is_func_call = self.store[callid].is_func_call();
        let pc = state.pc();
        let state_save = state.clone();
        let ir_save = ir.save();

        // The arms diverge, so both operands must be in their stack homes
        // before the branch: the slow arm reads them from there, and the
        // merge has to describe one placement for both.
        state.write_back_slots(ir, &[base, idx]);
        state.flush_gp(ir);
        let entry = state.clone();

        // The declared merge state. `dst` is an unknown `Value`, and either
        // arm may have run arbitrary Ruby (`Method#call`, a user `#[]`), so
        // none of the cached invariants survive.
        let mut target = entry.clone();
        if let Some(dst) = dst {
            target.def_S(dst);
        }
        target.unset_class_version_guard();
        target.unset_const_version_guard();
        target.unset_side_effect_guard();

        let slow = self.label();
        let merge = self.label();

        // ---- arm 1: the inlinable receiver class.
        let mut fast = entry.clone();
        fast.load(ir, base, GP::Rdi);
        ir.push(AsmInst::BrClassNe(GP::Rdi, inline_class, slow));
        // Reaching the arm *is* the proof, so refine without a second guard.
        fast.guard_class_state(base, inline_class);
        let Some(InlineFuncInfo::InlineGen(f)) = self.store.inline_info.get_inline(fid) else {
            unreachable!("index_inline_class only answers InlineGen targets")
        };
        if !self.inline_asm(&mut fast, ir, f, callid, inline_class, idx_class) {
            ir.restore(ir_save);
            *state = state_save;
            return Ok(false);
        }
        self.record_bop_dep(inline_class, IdentId::_INDEX);
        fast.unset_side_effect_guard();
        fast.gen_bridge(ir, target.slot_state(), pc);
        ir.push(AsmInst::Br(merge));

        // ---- arm 2: every other receiver, through the generic helper.
        ir.push(AsmInst::Label(slow));
        let mut rest = entry.clone();
        let error = ir.new_error(&rest);
        ir.generic_binop(&rest, base, idx, runtime::get_index, is_func_call);
        ir.handle_error(error);
        rest.def_rax2acc(ir, dst);
        rest.unset_class_version_guard();
        rest.unset_const_version_guard();
        rest.unset_side_effect_guard();
        rest.gen_bridge(ir, target.slot_state(), pc);

        ir.push(AsmInst::Label(merge));
        *state = target;
        Ok(true)
    }

    pub(super) fn index(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        base: SlotId,
        idx: SlotId,
        ic: Option<(ClassId, ClassId)>,
        polymorphic: bool,
        bc_pos: BcIndex,
    ) -> JitResult<CompileResult> {
        let (base_class, idx_class) = state.binary_class(base, idx, ic);
        // A site the VM saw indexing more than one receiver class, whose
        // receiver the abstract state cannot pin down: dispatch instead of
        // guarding. (A proven receiver class is monomorphic by construction,
        // whatever the VM saw at other times.)
        if polymorphic
            && state.class(base).is_none()
            && self.index_dispatch(state, ir, base, idx, idx_class, bc_pos)?
        {
            return Ok(CompileResult::Continue);
        }
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

    /// optcarrot's `@fetch[addr][addr]` shape: one `[]` site whose receiver
    /// is an `Array` for most addresses and a `Method` for the rest, so a
    /// class guard would deopt on every alternation. The two-arm dispatch
    /// must answer both — and keep answering after `Array#[]` is redefined,
    /// which the inlined arm's recorded bop dependency has to catch.
    #[test]
    fn index_polymorphic_array_and_method() {
        run_test_once(
            r##"
            class Io
              def initialize(v) = (@v = v)
              def read(addr) = @v + addr
            end
            ram = [0, 1, 2, 3, 4, 5, 6, 7]
            io  = Io.new(100).method(:read)
            fetch = Array.new(16) { |i| i < 8 ? ram : io }
            def loop_fetch(fetch, n)
              s = 0
              i = 0
              while i < n
                s += fetch[i & 15][i & 7]
                i += 1
              end
              s
            end
            res = [loop_fetch(fetch, 2000)]
            class Array
              def [](i) = :redefined
            end
            res << (begin; loop_fetch(fetch, 16); rescue => e; e.class; end)
            res
        "##,
        );
    }

    /// A polymorphic site where *neither* observed receiver has an inline
    /// generator, and one where the receivers are heap objects with their
    /// own `#[]` — the dispatch must decline or dispatch correctly, never
    /// answer with the wrong body.
    #[test]
    fn index_polymorphic_user_classes() {
        run_test_with_prelude(
            r##"
            drive
        "##,
            r##"
            class Ba; def [](i) = [:a, i]; end
            class Bb; def [](i) = [:b, i]; end
            class Bc < Array; end
            def get(o, i) = o[i]
            def drive
              vals = [Ba.new, Bb.new, [10, 20, 30, 40], { 0 => :h0, 1 => :h1 }, Bc.new(4, 9)]
              res = []
              j = 0
              while j < 60
                res << get(vals[j % 5], j % 4)
                j += 1
              end
              res
            end
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
