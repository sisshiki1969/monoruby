use super::*;

impl Codegen {
    ///
    /// Load constant to rax.
    ///
    /// #### out
    /// - rax: Value
    ///
    pub(super) fn load_generic_constant(
        &mut self,
        deopt: DestLabel,
        cached_val: Value,
        version: usize,
    ) {
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
        fdst: Xmm,
        deopt: DestLabel,
        cached_f: f64,
        version: usize,
    ) {
        self.guard_const_version(version, deopt);

        let cached_float = self.jit.const_f64(cached_f);
        monoasm! { &mut self.jit,
            movq xmm(fdst.enc()), [rip + cached_float];
            movq rax, (Value::float(cached_f).id());
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
