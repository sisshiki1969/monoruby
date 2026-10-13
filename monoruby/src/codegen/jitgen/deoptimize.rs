use super::*;

impl Codegen {
    pub(super) fn recompile_and_deopt(
        &mut self,
        position: Option<BytecodePtr>,
        deopt: &DestLabel,
        reason: RecompileReason,
        counter: DestLabel,
    ) {
        let recompile = self.jit.label();
        let dec = self.jit.label();

        self.dec_shared_counter(&dec, &recompile, deopt, &counter);

        assert_eq!(0, self.jit.get_page());
        self.jit.select_page(1);
        self.gen_recompile(position, recompile, reason, None);
        monoasm!( &mut self.jit,
            xorq rdi, rdi;
            jmp dec;
        );
        self.jit.select_page(0);
        #[cfg(feature = "jit-debug")]
        eprintln!(" => deopt");
    }

    ///
    /// A specialized body's recompile point. Its recompile rebuilds the
    /// whole root unit (`Codegen::recompile_specialized`), so it draws on the
    /// unit's one shared budget like every main-body `RecompileDeopt`
    /// (`Codegen::unit_recompile_counter`, see `unit_recompile_budget`)
    /// rather than on a counter of its own.
    ///
    pub(super) fn recompile_and_deopt_specialized(
        &mut self,
        deopt: &DestLabel,
        idx: usize,
        reason: RecompileReason,
    ) {
        let recompile = self.jit.label();
        let dec = self.jit.label();
        let counter = self
            .unit_recompile_counter
            .clone()
            .expect("RecompileDeoptSpecialized lowered outside jit_compile");

        self.dec_shared_counter(&dec, &recompile, deopt, &counter);

        assert_eq!(0, self.jit.get_page());
        self.jit.select_page(1);
        self.gen_recompile_specialized(idx, recompile, reason);
        monoasm! { &mut self.jit,
            xorq rdi, rdi;
            jmp dec;
        }
        self.jit.select_page(0);
        #[cfg(feature = "jit-debug")]
        eprintln!(" => deopt_specialized");
    }

    fn dec_shared_counter(
        &mut self,
        dec: &DestLabel,
        recompile: &DestLabel,
        deopt: &DestLabel,
        counter: &DestLabel,
    ) {
        let counter = counter.clone();
        monoasm!( &mut self.jit,
            xorq rdi, rdi;
            cmpl [rip + counter], 0;
            jlt deopt;
            jeq recompile;
        dec:
            subl [rip + counter], 1;
            jmp deopt;
        );
    }
}
