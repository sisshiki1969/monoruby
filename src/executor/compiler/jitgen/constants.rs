use super::*;

impl Codegen {
    pub(super) fn jit_store_constant(&mut self, ctx: &BBContext, id: IdentId, src: SlotId) {
        let const_version = self.const_version;
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(&xmm_using);
        monoasm!(self.jit,
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
        id: ConstSiteId,
        pc: BcPc,
    ) {
        let cached_value = self.jit.const_i64(0);
        let cached_const_version = self.jit.const_i64(-1);
        let global_const_version = self.const_version;
        let slow_path = self.jit.label();
        let exit = self.jit.label();

        self.jit.select_page(1);
        self.jit.bind_label(slow_path);
        self.get_constant(ctx, id, pc);
        monoasm!(self.jit,
            movq [rip + cached_value], rax;
            movq rdi, [rip + global_const_version];
            movq [rip + cached_const_version], rdi;
            jmp  exit;
        );
        self.jit.select_page(0);

        monoasm!(self.jit,
            movq rax, [rip + global_const_version];
            cmpq rax, [rip + cached_const_version];
            jne  slow_path;
            movq rax, [rip + cached_value];
        exit:
        );
        self.store_rax(dst);
    }

    pub(super) fn load_float_constant(
        &mut self,
        ctx: &BBContext,
        dst: SlotId,
        fdst: Xmm,
        id: ConstSiteId,
        pc: BcPc,
    ) {
        let cached_value = self.jit.const_i64(0);
        let cached_const_version = self.jit.const_i64(-1);
        let global_const_version = self.const_version;
        let slow_path = self.jit.label();
        let exit = self.jit.label();

        let cached_float = self.jit.const_f64(0.0);
        let side_exit = self.gen_side_deopt(pc, ctx);

        self.jit.select_page(1);
        self.jit.bind_label(slow_path);
        self.get_constant(ctx, id, pc);
        monoasm!(self.jit,
            movq [rip + cached_value], rax;
            movq rdi, rax;
        );
        self.unbox_float(0, side_exit);
        monoasm!(self.jit,
            movq [rip + cached_float], xmm0;
            movq rax, [rip + global_const_version];
            movq [rip + cached_const_version], rax;
            jmp  exit;
        );
        self.jit.select_page(0);

        monoasm!(self.jit,
            movq rax, [rip + global_const_version];
            cmpq rax, [rip + cached_const_version];
            jne  slow_path;
        exit:
            movq xmm(fdst.enc()), [rip + cached_float];
            movq rax, [rip + cached_value];
        );
        self.store_rax(dst);
    }

    fn get_constant(&mut self, ctx: &BBContext, id: ConstSiteId, pc: BcPc) {
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(&xmm_using);
        monoasm!(self.jit,
            movq rdx, (id.0);  // name: ConstSiteId
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (runtime::get_constant);
            call rax;
        );
        self.xmm_restore(&xmm_using);
        self.jit_handle_error(ctx, pc);
    }
}
