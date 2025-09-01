use super::*;

impl Codegen {
    pub(super) fn store_constant(&mut self, id: ConstSiteId, using_xmm: UsingXmm) {
        let const_version = self.const_version_label();
        self.xmm_save(using_xmm);
        monoasm!( &mut self.jit,
          movq rdx, (id.0);  // name: ConstSiteId
          movq rcx, rax;  // val: Value
          movq rdi, rbx;  // &mut Interp
          movq rsi, r12;  // &mut Globals
          addq [rip + const_version], 1;
          movq rax, (runtime::set_constant);
          call rax;
        );
        self.xmm_restore(using_xmm);
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
}
