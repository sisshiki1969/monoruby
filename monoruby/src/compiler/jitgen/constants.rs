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

    ///
    /// Load constant to rax.
    ///
    /// #### out
    /// - rax: Value
    ///
    pub(super) fn load_generic_constant(
        &mut self,
        ctx: &mut BBContext,
        deopt: DestLabel,
        cached_val: Value,
        version: usize,
        base_class: Option<Value>,
        base_slot: Option<SlotId>,
    ) {
        self.guard_base_class(ctx, base_class, base_slot, deopt);
        self.guard_const_version(version, deopt);

        monoasm!( &mut self.jit,
            movq rax, (cached_val.id());
        );
    }

    ///
    /// Load Float constant to rax.
    ///
    /// #### out
    /// - rax: Value
    /// - xmm(fdst): Float
    ///
    pub(super) fn load_float_constant(
        &mut self,
        ctx: &mut BBContext,
        fdst: Xmm,
        deopt: DestLabel,
        cached_f: f64,
        version: usize,
        base_class: Option<Value>,
        base_slot: Option<SlotId>,
    ) {
        self.guard_base_class(ctx, base_class, base_slot, deopt);
        self.guard_const_version(version, deopt);

        let cached_float = self.jit.const_f64(cached_f);
        monoasm! { &mut self.jit,
            movq xmm(fdst.enc()), [rip + cached_float];
            movq rax, (Value::float(cached_f).id());
        }
    }

    fn guard_base_class(
        &mut self,
        ctx: &mut BBContext,
        base_class: Option<Value>,
        base_slot: Option<SlotId>,
        deopt: DestLabel,
    ) {
        let cached_base_class = self.jit.const_i64(base_class.map_or(0, |c| c.id()) as _);
        if let Some(slot) = base_slot {
            self.fetch_to_rax(ctx, slot);
            monoasm! { &mut self.jit,
                cmpq rax, [rip + cached_base_class];  // rax: base_class
                jne  deopt;
            }
        }
    }

    fn guard_const_version(&mut self, cached_version: usize, deopt: DestLabel) {
        let cached_const_version = self.jit.const_i64(cached_version as _);
        let global_const_version = self.const_version;
        monoasm! { &mut self.jit,
            movq rax, [rip + global_const_version];
            cmpq rax, [rip + cached_const_version];
            jne  deopt;
        }
    }
}
