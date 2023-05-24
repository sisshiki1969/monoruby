use super::*;

impl Codegen {
    pub(super) fn jit_store_constant(&mut self, ctx: &BBContext, id: IdentId, src: SlotId) {
        let const_version = self.const_version;
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(&xmm_using);
        monoasm!( &mut self.jit,
          movq rdx, (id.get());  // name: IdentId
          movq rcx, [r14 - (conv(src))];  // val: Value
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
        ctx: &BBContext,
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
            movq rax, (v.get());
        );
        self.store_rax(dst);
    }

    pub(super) fn load_float_constant(
        &mut self,
        ctx: &BBContext,
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
            movq rax, (Value::new_float(f).get());
        );
        self.store_rax(dst);
    }

    /*fn get_constant(&mut self, ctx: &BBContext, id: ConstSiteId, pc: BcPc) {
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(&xmm_using);
        monoasm!( &mut self.jit,
            movq rdx, (id.0);  // name: ConstSiteId
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (runtime::get_constant);
            call rax;
        );
        self.xmm_restore(&xmm_using);
        self.jit_handle_error(ctx, pc);
    }*/
}
