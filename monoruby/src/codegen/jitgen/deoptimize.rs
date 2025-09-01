use super::*;

impl Codegen {
    pub(super) fn recompile_and_deopt(
        &mut self,
        position: Option<BytecodePtr>,
        deopt: &DestLabel,
        reason: RecompileReason,
    ) {
        let recompile = self.jit.label();
        let dec = self.jit.label();

        self.dec_counter(&dec, &recompile, deopt, COUNT_DEOPT_RECOMPILE);

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

    pub(super) fn recompile_and_deopt_specialized(
        &mut self,
        deopt: &DestLabel,
        idx: usize,
        reason: RecompileReason,
    ) {
        let recompile = self.jit.label();
        let dec = self.jit.label();

        self.dec_counter(&dec, &recompile, deopt, COUNT_DEOPT_RECOMPILE_SPECIALIZED);

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

    fn dec_counter(
        &mut self,
        dec: &DestLabel,
        recompile: &DestLabel,
        deopt: &DestLabel,
        count: i32,
    ) {
        let counter = self.jit.data_i32(count);
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
