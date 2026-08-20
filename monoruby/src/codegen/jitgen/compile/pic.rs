//! Polymorphic inline cache: an N-way class dispatch for a send the VM saw
//! reaching several *different* targets.
//!
//! The class-set guard (`GuardClassIn`, emitted by `pmc_same_target_classes`)
//! already covers polymorphic sites whose classes all resolve to **one**
//! method — `Kernel#is_a?`, `Kernel#nil?` and friends. It cannot cover a site
//! whose classes resolve to different bodies: one guard admitting the whole
//! set would then run the wrong one. Those sites keep a single-class guard and
//! deopt on every off-class receiver, for the rest of the run.
//!
//! ```text
//!         load recv -> rdi
//!         guard_class_version                 (one, dominating every arm)
//!         br_class_ne rdi, C0 -> L1
//!         <call C0#name>
//!         br merge
//!   L1:   br_class_ne rdi, C1 -> L2
//!         <call C1#name>
//!         br merge
//!   L2:   guard_class rdi, C2 -> deopt        (last arm; a miss here is a
//!         <call C2#name>                       class the VM never observed)
//!   merge:
//! ```
//!
//! The merge is the shared intra-block one (`compile/dispatch.rs`): declared
//! before the arms, bridged to from each. An earlier version of this file
//! had no such merge and instead demanded that every arm land in an
//! *equivalent* state, backing the whole chain out when they did not. That
//! restriction is what forced the gate down to argument-less sends — and it
//! still let sites through whose arms diverged in practice (a const-folding
//! `def v = 1` against a calling one), which then simply declined. With a
//! declared merge the arms may differ freely, so the gate only has to exclude
//! shapes that are unsound or that would abort the compile.
//!
//! **Scope.** Method-call deopts are a small share of what the JIT actually
//! loses: across optcarrot, rubykon, rubyboy, erubi, etanni and plb2 they are
//! ~15k against ~4.1M for binary operators. This is not where the time is —
//! the same dispatch shape applied to `+` / `<` / `==` is. What this buys is
//! the mechanism, and `compile/dispatch.rs` is deliberately the part that
//! generalizes.
//!
//! The gate is correspondingly quiet in practice: a run of rubykon compiles
//! 12 chains, of which the one that matters is `MCTS::Node#backpropagate`'s
//! `node.root?` (5,101 deopts before, none after). The dominant refusal is
//! not any of the shape rules below but "the PMC holds one class" — a site
//! whose receiver the abstract state cannot pin down, yet which the VM only
//! ever saw take one class, is monomorphic and rightly keeps its single
//! guard.

use super::{
    method_call::{RecvMissMode, PMC_SET_SHARE_DIVISOR},
    *,
};

///
/// Maximum number of dispatch arms.
///
/// The chain is a linear compare sequence, so every arm taxes the ones ahead
/// of it; and the observation source cannot describe more classes than the
/// [`PMC_WAYS`] it keeps.
///
const PIC_WAYS: usize = PMC_WAYS;

///
/// One dispatch arm: the target, and every observed receiver class that
/// resolves to it.
///
/// Arms are keyed by `FuncId`, not by class. Classes that share a target
/// share an arm, so a site like `Kernel#nil?` seen on four classes emits one
/// call sequence behind a four-class membership test rather than four copies
/// of the same sequence. It also means the arm cannot refine the receiver to
/// a single class unless it holds exactly one — see `single_class`.
///
struct PicGroup {
    func_id: FuncId,
    visibility: Visibility,
    classes: Vec<ClassId>,
}

impl PicGroup {
    /// The class every receiver reaching this arm has, when there is only
    /// one. `None` for a multi-class arm, which may not refine the state and
    /// so may only fire class-independent inline generators.
    fn single_class(&self) -> Option<ClassId> {
        match self.classes.as_slice() {
            [class] => Some(*class),
            _ => None,
        }
    }
}

impl<'a> JitContext<'a> {
    ///
    /// The dispatch arms for *callid*, or `None` when the site is not a
    /// polymorphic-inline-cache candidate.
    ///
    /// Every refusal happens here, before a single instruction is emitted.
    /// That is a requirement, not tidiness: once the chain is under way an arm
    /// that bailed would leave the emitted arms ahead of it stranded, and
    /// unlike the class-set guard there is no single-instruction fallback to
    /// rewrite it as.
    ///
    fn pic_groups(&mut self, callid: CallSiteId) -> Option<Vec<PicGroup>> {
        // Temporary diagnosis (logging builds only): name every refusal, so a
        // polymorphic site that stays on the deopting mono guard can be
        // attributed to the exact gate that turned it away.
        macro_rules! refuse {
            ($why:literal) => {{
                #[cfg(feature = "deopt")]
                eprintln!(
                    "### pic refuse [{}] {}",
                    $why,
                    self.store[callid].name.map_or_else(
                        || "?".to_string(),
                        |n| format!("{:?}", n)
                    )
                );
                return None;
            }};
        }
        let callsite = &self.store[callid];
        let Some(name) = callsite.name else {
            refuse!("no-name")
        };
        // A block argument makes the callee able to capture the frame, and
        // `locals_to_S` inside one arm would describe a frame layout the other
        // arms do not share.
        if callsite.block_fid.is_some() {
            refuse!("block-fid")
        }
        let pmc = &callsite.pmc;
        let observations = pmc.observations();
        let mut classes: Vec<(ClassId, u32)> =
            pmc.entries().iter().map(|e| (e.recv, e.count)).collect();
        if classes.len() < 2 {
            refuse!("pmc-mono")
        }
        classes.sort_unstable_by_key(|(_, count)| std::cmp::Reverse(*count));
        let mut groups: Vec<PicGroup> = Vec::with_capacity(classes.len());
        let mut admitted = 0usize;
        for (class, count) in classes {
            if admitted == PIC_WAYS {
                break;
            }
            // A rare tail is not worth an arm — the classes ahead of it pay
            // its compare on every dispatch. Same threshold the class-set
            // guard uses.
            if count.saturating_mul(PMC_SET_SHARE_DIVISOR) < observations {
                continue;
            }
            // A class that cannot have an arm is dropped, not fatal to the
            // site: it falls past the last arm's guard and deopts, which is
            // exactly what the single-class guard did for it anyway. That
            // covers a name this class does not resolve (`jit_check_call`), a
            // resolution this call site's visibility blocks (the VM then
            // raises `NoMethodError`, as before), and the two shapes
            // `compile_method_call` answers with `CompileError` — which must
            // not be allowed to fire mid-chain, where there is nothing to
            // back out to.
            let Some((func_id, visibility)) = self.jit_check_call(class, Some(name)) else {
                #[cfg(feature = "deopt")]
                eprintln!("### pic drop [no-resolve] {:?} class={:?}", name, class);
                continue;
            };
            if self.jit_visibility_blocks(callid, visibility)
                || self.store[func_id].possibly_capture_without_block()
            {
                #[cfg(feature = "deopt")]
                eprintln!("### pic drop [vis/capture] {:?} class={:?}", name, class);
                continue;
            }
            if let Some(iseq) = self.store[func_id].is_iseq()
                && self.store[iseq].has_block_arg()
            {
                #[cfg(feature = "deopt")]
                eprintln!("### pic drop [callee-block-arg] {:?} class={:?}", name, class);
                continue;
            }
            // An attr/Struct accessor target whose callsite shape is
            // non-canonical (extra arguments, keywords, a splat — a block
            // is already excluded site-wide above) answers `Deopt` in
            // `compile_method_call`, which must not fire mid-chain: drop
            // the class like the other ineligible shapes, so it falls past
            // the arms and deopts.
            if !self.accessor_shape_ok(callid, func_id) {
                continue;
            }
            admitted += 1;
            // Fold into the arm for this target if one is already open. The
            // groups stay in first-seen order, which is most-observed-first,
            // so the hottest target is tested first.
            if let Some(g) = groups.iter_mut().find(|g| g.func_id == func_id) {
                g.classes.push(class);
            } else {
                groups.push(PicGroup {
                    func_id,
                    visibility,
                    classes: vec![class],
                });
            }
        }
        if admitted < 2 {
            refuse!("admitted<2")
        }
        // One target for every class is the class-set guard's case, and one
        // membership guard over the set beats an arm that tests the same set
        // and then falls into the only call sequence there is.
        if groups.len() < 2 {
            refuse!("single-target")
        }
        Some(groups)
    }

    ///
    /// Try to compile *callid* as a polymorphic inline cache.
    ///
    /// Answers `true` when the chain was emitted and *state* describes the
    /// merge point.
    ///
    pub(super) fn compile_pic_call(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        callid: CallSiteId,
    ) -> JitResult<bool> {
        let Some(groups) = self.pic_groups(callid) else {
            return Ok(false);
        };
        let CallSiteInfo {
            recv,
            args,
            pos_num,
            dst,
            ..
        } = self.store[callid];

        // One class-version guard dominates the whole chain: the arms are
        // mutually exclusive, so at most one call executes under it.
        self.guard_class_version(state, ir, true);

        // The receiver and every argument are read by each arm's own call
        // sequence, so they have to be at their stack homes before the split.
        let mut operands = Vec::with_capacity(pos_num + 1);
        operands.push(recv);
        operands.extend((0..pos_num).map(|i| args + i));
        let (entry, merge) = self.declare_merge(state, ir, &operands, dst);

        // The chain compares out of rdi, which is also where each arm's call
        // sequence wants the receiver.
        let mut probe = entry.clone();
        probe.load(ir, recv, GP::Rdi);
        let entry = probe;

        // The chain tests each arm's class set in turn and the *last* arm's
        // test is the deopting one, so a receiver is compared against the
        // union exactly once. Hoisting the deopt into a separate union guard
        // ahead of the arms reads better but compares everything twice — it
        // measured ~16% slower.
        let mut miss: Option<JitLabel> = None;
        for (i, group) in groups.iter().enumerate() {
            let last = i + 1 == groups.len();
            if let Some(miss) = miss.take() {
                ir.push(AsmInst::Label(miss));
            }
            let mut arm = entry.clone();
            if last {
                // Falling out of the last arm's set is a class the VM never
                // observed here: deopt, exactly as the monomorphic guard did
                // for every off-class receiver.
                let deopt = ir.new_deopt(&arm);
                ir.push(AsmInst::GuardClassIn(
                    GP::Rdi,
                    group.classes.clone().into_boxed_slice(),
                    deopt,
                ));
            } else {
                let next = self.label();
                ir.push(AsmInst::BrClassNotIn(
                    GP::Rdi,
                    group.classes.clone().into_boxed_slice(),
                    next,
                ));
                miss = Some(next);
            }
            // Reaching an arm proves the receiver's class only when the arm
            // holds one. A multi-class arm leaves it unrefined, so
            // `compile_method_call` sees an unproven receiver and restricts
            // itself to class-independent inline generators — the same
            // treatment the class-set guard gets, for the same reason.
            let recv_class = match group.single_class() {
                Some(class) => {
                    // `guard_class_state` refines without emitting a guard,
                    // which is also what lets the arm's inline generators
                    // fire.
                    arm.guard_class_state(recv, class);
                    class
                }
                None => group.classes[0],
            };
            // Specialization is suppressed inside an arm (see
            // `JitContext::in_dispatch_arm`), and every other way out of
            // `compile_method_call` was hoisted into `pic_groups`, so the arm
            // always lands on the merge.
            let outcome = self.with_arm(group.single_class().is_none(), |this| {
                this.compile_method_call(
                    &mut arm,
                    ir,
                    recv_class,
                    None,
                    group.func_id,
                    group.visibility,
                    callid,
                    RecvMissMode::Plain,
                )
            })?;
            debug_assert!(matches!(outcome, CompileResult::Continue));
            self.end_arm(arm, ir, &merge, !last);
        }
        self.bind_merge(state, ir, merge);
        Ok(true)
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    /// A site whose receiver classes resolve to *different* bodies — the case
    /// the class-set guard cannot take. Each class must dispatch its own
    /// method with no deopt, including a class arriving only after warmup (it
    /// misses every arm and deopts) and a redefinition of one arm's target
    /// (caught by the shared class-version guard).
    #[test]
    fn pic_distinct_targets() {
        run_test(
            r#"
            class Aa; def tag = :a; end
            class Bb; def tag = :b; end
            class Cc; def tag = :c; end
            def probe(x) = x.tag
            vals = [Aa.new, Bb.new, Cc.new]
            res = []
            300.times { |i| res << probe(vals[i % 3]) }
            tally = res.tally.sort_by { |k, _| k.to_s }
            class Dd; def tag = :d; end
            class Bb; def tag = :b2; end
            [tally, probe(Aa.new), probe(Bb.new), probe(Cc.new), probe(Dd.new)]
            "#,
        );
    }

    /// Arms that leave *different shapes* behind: one folds to a constant
    /// (`ISeqHint::ConstReturn`), one returns self, one really calls. The
    /// declared merge has to absorb all three — the earlier equivalence-based
    /// version refused this site outright.
    #[test]
    fn pic_divergent_arm_shapes() {
        run_test(
            r#"
            class Ka; def v = 1; end
            class Kb; def v = 2; end
            class Kc; def initialize = (@n = 0); def v = (@n += 1); end
            def probe(x) = x.v
            vals = [Ka.new, Kb.new, Kc.new]
            n = 0
            300.times { |i| n += probe(vals[i % 3]) }
            [n, probe(Ka.new), probe(Kb.new)]
            "#,
        );
    }

    /// Arms with arguments, which the equivalence-based version could not
    /// take at all. Covers a differing arity per arm and an argument that is
    /// a compile-time constant at the call site.
    #[test]
    fn pic_arms_with_arguments() {
        run_test(
            r#"
            class Wa; def f(a, b) = a + b; end
            class Wb; def f(a, b) = a * b; end
            class Wc; def f(a, b) = a - b; end
            def probe(x, i) = x.f(i, 3)
            vals = [Wa.new, Wb.new, Wc.new]
            acc = 0
            300.times { |i| acc += probe(vals[i % 3], i % 7) }
            [acc, probe(Wa.new, 10), probe(Wb.new, 10), probe(Wc.new, 10)]
            "#,
        );
    }

    /// Mixed method kinds behind one site: an attr_reader, a plain method and
    /// a builtin, plus a receiver whose `size` resolves per class.
    #[test]
    fn pic_mixed_kinds() {
        run_test(
            r#"
            class Ra
              attr_reader :tag
              def initialize = (@tag = :attr)
            end
            class Rb
              def tag = :plain
            end
            def probe(x) = x.tag
            def sizer(x) = x.size
            vals = [Ra.new, Rb.new]
            sized = [[1, 2, 3], "abcd", { a: 1 }]
            res = []
            300.times do |i|
              res << probe(vals[i % 2])
              res << sizer(sized[i % 3])
            end
            res.tally.sort_by { |k, _| k.to_s }
            "#,
        );
    }

    /// A site that goes polymorphic only *after* the JIT compiled it
    /// monomorphic: the first hot phase sees one class, so the compile
    /// guards that class alone; the second phase alternates two. The
    /// receiver guard's `Learn` exit (see `RecvMissMode`) must recompile the
    /// body — whose fresh compile sees both classes in the PMC and builds
    /// the dispatch chain — instead of deopting on every `Lb` forever.
    #[test]
    fn pic_late_polymorphism_heals() {
        run_test(
            r#"
            class La; def tag = :a; end
            class Lb; def tag = :b; end
            def probe(x) = x.tag
            res = []
            50.times { res << probe(La.new) }
            vals = [La.new, Lb.new]
            100.times { |i| res << probe(vals[i % 2]) }
            res.tally.sort_by { |k, _| k.to_s }
            "#,
        );
    }

    /// The same late-variance shape inside a *specialized* (inlined) body:
    /// `inner` is inlined into `outer`, so the receiver guard that misses
    /// lives in a specialized compile — the heal must go through
    /// `RecompileTarget::Specialized` (re-pointing the caller's patch point),
    /// not the whole-method route.
    #[test]
    fn pic_late_polymorphism_heals_specialized() {
        run_test(
            r#"
            class Sa; def tag = :sa; end
            class Sb; def tag = :sb; end
            def inner(x) = x.tag
            def outer(x) = inner(x)
            res = []
            50.times { res << outer(Sa.new) }
            vals = [Sa.new, Sb.new]
            100.times { |i| res << outer(vals[i % 2]) }
            res.tally.sort_by { |k, _| k.to_s }
            "#,
        );
    }

    /// A private target on one of the arms must not be reachable without an
    /// explicit `self` receiver: the site is refused outright, and a plain
    /// `x.hidden` raises NoMethodError as CRuby does.
    #[test]
    fn pic_private_arm() {
        run_test_once(
            r#"
            class Pa; def hidden = :a; end
            class Pb; private def hidden = :b; end
            def probe(x) = x.hidden
            res = []
            300.times { res << probe(Pa.new) }
            begin
              probe(Pb.new)
            rescue NoMethodError
              res << :nome
            end
            [res.tally.sort_by { |k, _| k.to_s }]
            "#,
        );
    }
}
