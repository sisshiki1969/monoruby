use super::*;
use crate::codegen::jitgen::lir::ConstMiss;

impl Codegen {
    pub(super) fn store_constant(&mut self, id: ConstSiteId, using_fpr: UsingFpr) {
        let const_version = self.const_version_label();
        self.fpr_save(using_fpr);
        monoasm!( &mut self.jit,
          movq rdx, (id.0);  // name: ConstSiteId
          movq rcx, rax;  // val: Value
          movq rdi, rbx;  // &mut Interp
          movq rsi, r12;  // &mut Globals
          addq [rip + const_version], 1;
          movq rax, (runtime::set_constant);
          call rax;
        );
        self.fpr_restore(using_fpr);
    }

    ///
    /// Guard for constant version.
    ///
    /// The snapshot side of the compare is the unit's shared patchable word
    /// (`Codegen::unit_const_version`), so a successful const salvage can
    /// re-validate every guard in the unit with one store.
    ///
    /// A miss always ends at `deopt`; `miss` says what it tries on the way.
    /// `Recompile(position)` calls the salvaging recompile entry (whole
    /// method / the loop at `position`) — the class-version guard's shape.
    /// `Salvage` calls the salvage-only entry, which re-validates the unit's
    /// folds and re-stamps its version word without touching any frame.
    ///
    /// ### destroy
    /// - rax
    ///
    pub(super) fn guard_const_version(&mut self, miss: ConstMiss, deopt: &DestLabel) {
        let cached_const_version = self
            .unit_const_version
            .clone()
            .expect("const guard emitted outside a constant-folding unit");
        let global_const_version = self.const_version_label();
        assert_eq!(0, self.jit.get_page());
        let fail = self.jit.label();
        monoasm! { &mut self.jit,
            movq rax, [rip + global_const_version];
            cmpq rax, [rip + cached_const_version];
            jne  fail;
        }
        self.jit.select_page(1);
        match miss {
            ConstMiss::Recompile(position) => self.gen_recompile(
                position,
                fail,
                RecompileReason::ConstVersionGuardFailed,
                None,
            ),
            ConstMiss::Salvage => self.gen_salvage_const(fail),
        }
        self.version_guard_fail(deopt);
        self.jit.select_page(0);
    }

    ///
    /// Constant version guard for a specialized (inlined-frame) body.
    ///
    /// Check the cached constant version; if it moved, recompile this
    /// specialized entry (re-pointing the caller's patch point at the fresh
    /// body, which folds the constants at the new version), then deopt.
    /// Mirrors `guard_class_version_specialized`.
    ///
    /// ### destroy
    /// - rax
    ///
    pub(super) fn guard_const_version_specialized(
        &mut self,
        _cached_version: usize,
        idx: usize,
        deopt: &DestLabel,
    ) {
        assert_eq!(0, self.jit.get_page());
        let fail = self.jit.label();
        let cached_const_version = self
            .unit_const_version
            .clone()
            .expect("const guard emitted outside a constant-folding unit");
        let global_const_version = self.const_version_label();
        monoasm! { &mut self.jit,
            movq rax, [rip + global_const_version];
            cmpq rax, [rip + cached_const_version];
            jne  fail;
        }
        self.jit.select_page(1);
        self.gen_recompile_specialized(idx, fail, RecompileReason::ConstVersionGuardFailed);
        self.version_guard_fail(deopt);
        self.jit.select_page(0);
    }
}
