use super::*;

impl Globals {
    pub(crate) unsafe fn dump_frame_info(&self, lfp: Lfp) {
        let meta = lfp.meta();
        let outer = lfp.outer();
        let func_id = meta.func_id();
        let block = lfp.block();
        eprintln!(
            "    <{}> block:{} outer:{} {:?}",
            self.store.func_description(func_id),
            match block {
                Some(block) => {
                    match block.try_proxy() {
                        Some((func_id, idx)) => {
                            format!("BlockArgProxy {{ {:?}, {} }}", func_id, idx)
                        }
                        _ => block.get().debug(&self.store),
                    }
                }
                None => "None".to_string(),
            },
            match outer {
                None => "None".to_string(),
                Some(outer) => format!("{:?}", outer),
            },
            meta,
        );
        eprint!("    ");
        for r in SlotId(0)..SlotId(0) + meta.reg_num() as usize {
            eprint!(
                "{:?}:[{}] ",
                r,
                if let Some(v) = lfp.register(r) {
                    if let Some(s) = v.debug_check(&self.store) {
                        s
                    } else {
                        "INVALID".to_string()
                    }
                } else {
                    "None".to_string()
                }
            );
        }
        eprintln!();
    }

    pub(crate) unsafe fn check_frame_info(&self, lfp: Lfp) -> bool {
        let meta = lfp.meta();
        let mut invalid = false;
        for r in SlotId(0)..SlotId(0) + meta.reg_num() as usize {
            if let Some(v) = lfp.register(r) {
                if v.debug_check(&self.store).is_none() {
                    invalid = true;
                }
            }
        }
        invalid
    }

    #[cfg(feature = "emit-bc")]
    pub fn dump_bc(&mut self) {
        let dumped_bc = self.dumped_bc;
        if CODEGEN.with(|codegen| codegen.borrow().startup_flag) {
            self.store.functions()[dumped_bc..]
                .iter()
                .for_each(|info| match &info.kind {
                    FuncKind::ISeq(iseq) => self.store.dump_iseq(*iseq),
                    _ => {}
                });
        }
        self.dumped_bc = self.store.func_len();
    }

    #[cfg(any(feature = "profile", feature = "jit-log"))]
    pub(crate) fn show_stats(&self) {
        #[cfg(feature = "profile")]
        {
            eprintln!();
            eprintln!("deoptimization stats (top 20)");
            eprintln!(
                " FuncId  {:60} [{:05}]     {:7}",
                "func name", "index", "count"
            );
            eprintln!("------------------------------------------------------------------------------------------------------------------------------------------------------");
            let mut v: Vec<_> = self.deopt_stats.iter().collect();
            v.sort_unstable_by(|(_, a), (_, b)| b.cmp(a));
            for ((func_id, bc_pos), count) in v.into_iter().take(20) {
                let func = self.store.iseq(*func_id);
                let fmt = if let Some(fmt) = func.trace_ir(&self.store, *bc_pos).format(&self.store)
                {
                    fmt
                } else {
                    "<INVALID>".to_string()
                };
                let name = self.store.func_description(*func_id);
                eprintln!(
                    "({:6}) {:60} [{:05}]  {:10}   {fmt}",
                    func_id.get(),
                    name,
                    bc_pos,
                    count
                );
            }
            eprintln!();
            self.store.show_stats();
            eprintln!();
            eprintln!("jit class guard failed stats (top 20)");
            eprintln!(" FuncId  {:40} {:30} {:10}", "func name", "class", "count");
            eprintln!(
                "--------------------------------------------------------------------------------------------"
            );
            let mut v: Vec<_> = self.jit_class_unmatched_stats.iter().collect();
            v.sort_unstable_by(|(_, a), (_, b)| b.cmp(a));
            for ((func_id, class_id), count) in v.into_iter().take(20) {
                eprintln!(
                    "({:6}) {:40} {:30} {:10}",
                    func_id.get(),
                    self.store.func_description(*func_id),
                    self.store.debug_class_name(*class_id),
                    count
                );
            }
            eprintln!();
            eprintln!("jit recompile stats (top 20)");
            eprintln!(
                " FuncId  {:40} {:30} {:30} {:10}",
                "func name", "class", "reason", "count"
            );
            eprintln!(
                "--------------------------------------------------------------------------------------------"
            );
            let mut v: Vec<_> = self.jit_recompile_count.iter().collect();
            v.sort_unstable_by(|(_, a), (_, b)| b.cmp(a));
            for ((func_id, class_id, reason), count) in v.into_iter().take(20) {
                eprintln!(
                    "({:6}) {:40} {:30} {:30?} {:10}",
                    func_id.get(),
                    self.store.func_description(*func_id),
                    self.store.debug_class_name(*class_id),
                    reason,
                    count
                );
            }
        }
        #[cfg(feature = "jit-log")]
        {
            eprintln!();
            eprintln!(
                "elapsed JIT compile time: {:?}",
                CODEGEN.with(|codegen| codegen.borrow().jit_compile_time)
            );
        }
    }
}

#[cfg(any(feature = "deopt", feature = "profile"))]
pub(crate) extern "C" fn log_deoptimize(
    vm: &mut Executor,
    globals: &mut Globals,
    pc: BytecodePtr,
    #[cfg(feature = "deopt")] reason: Option<Value>,
) {
    use crate::jitgen::trace_ir::*;
    let func_id = vm.cfp().lfp().func_id();
    let func = globals.store.iseq(func_id);
    let bc_pos = func.get_pc_index(Some(pc));
    let trace_ir = func.trace_ir(&globals.store, bc_pos);

    if let TraceIr::LoopEnd = trace_ir {
        // normal exit from jit'ed loop
        #[cfg(feature = "deopt")]
        {
            let name = globals.store.func_description(func_id);
            let fmt = trace_ir.format(&globals.store).unwrap_or_default();
            eprint!("<-- exited from JIT code in <{}> {:?}.", name, func_id);
            eprintln!("    [{:05}] {fmt}", bc_pos);
        }
    } else {
        #[cfg(feature = "profile")]
        {
            match globals.deopt_stats.get_mut(&(func_id, bc_pos)) {
                Some(c) => *c = *c + 1,
                None => {
                    globals.deopt_stats.insert((func_id, bc_pos), 1);
                }
            }
        }
        #[cfg(feature = "deopt")]
        {
            let name = globals.store.func_description(func_id);
            let fmt = trace_ir.format(&globals.store).unwrap_or_default();
            match trace_ir {
                TraceIr::LoadConst(..)          // inline constant cache miss
                | TraceIr::ClassDef { .. }      // error in class def (illegal superclass etc.)
                | TraceIr::LoadIvar(..)         // inline ivar cache miss
                | TraceIr::StoreIvar(..) => {
                    eprint!("<-- deopt occurs in <{}> {:?}.", name, func_id);
                    eprintln!("    [{:05}] {fmt}", bc_pos);
                },
                _ => if let Some(v) = reason {
                    eprint!("<-- deopt occurs in <{}> {:?}.", name, func_id);
                    eprintln!("    [{:05}] {fmt} caused by {}", bc_pos, v.debug(&globals.store));
                } else {
                    eprint!("<-- non-traced branch in <{}> {:?}.", name, func_id);
                    eprintln!("    [{:05}] {fmt}", bc_pos);
                },
            }
        }
    }
}
