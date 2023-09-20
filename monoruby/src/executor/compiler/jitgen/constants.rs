use super::*;

impl Codegen {
    ///
    /// Store constant.
    ///
    /// #### in
    /// - rax: src: Value
    ///
    pub(super) fn jit_store_constant(&mut self, ctx: &mut BBContext, id: IdentId, src: SlotId) {
        let const_version = self.const_version;
        let xmm_using = ctx.get_xmm_using();
        self.fetch_to_rax(ctx, src);
        self.xmm_save(&xmm_using);
        monoasm!( &mut self.jit,
          movq rdx, (id.get());  // name: IdentId
          movq rcx, rax;  // val: Value
          movq rdi, rbx;  // &mut Interp
          movq rsi, r12;  // &mut Globals
          addq [rip + const_version], 1;
          movq rax, (runtime::set_constant);
          call rax;
        );
        self.xmm_restore(&xmm_using);
    }

    pub(super) fn load_generic_constant(
        &mut self,
        ctx: &mut BBContext,
        dst: SlotId,
        pc: BcPc,
        v: Value,
        version: usize,
    ) {
        let cached_const_version = self.jit.const_i64(version as _);
        let global_const_version = self.const_version;
        let deopt = self.gen_side_deopt(pc, ctx);

        monoasm!( &mut self.jit,
            movq rax, [rip + global_const_version];
            cmpq rax, [rip + cached_const_version];
            jne  deopt;
            movq rax, (v.id());
        );
        self.store_rax(dst);
    }

    pub(super) fn load_float_constant(
        &mut self,
        ctx: &mut BBContext,
        dst: SlotId,
        fdst: Xmm,
        pc: BcPc,
        f: f64,
        version: usize,
    ) {
        let cached_const_version = self.jit.const_i64(version as _);
        let global_const_version = self.const_version;

        let cached_float = self.jit.const_f64(f);
        let deopt = self.gen_side_deopt(pc, ctx);

        monoasm!( &mut self.jit,
            movq rax, [rip + global_const_version];
            cmpq rax, [rip + cached_const_version];
            jne  deopt;
            movq xmm(fdst.enc()), [rip + cached_float];
            movq rax, (Value::float(f).id());
        );
        self.store_rax(dst);
    }
}
