use super::*;

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
    /// ### destroy
    /// - rax
    ///
    pub(super) fn guard_const_version(&mut self, cached_version: usize, deopt: &DestLabel) {
        let cached_const_version = self.jit.data_i64(cached_version as _);
        let global_const_version = self.const_version_label();
        monoasm! { &mut self.jit,
            movq rax, [rip + global_const_version];
            cmpq rax, [rip + cached_const_version];
            jne  deopt;
        }
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
        cached_version: usize,
        idx: usize,
        deopt: &DestLabel,
    ) {
        assert_eq!(0, self.jit.get_page());
        let fail = self.jit.label();
        let cached_const_version = self.jit.data_i64(cached_version as _);
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
