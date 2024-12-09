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
        for r in 0..meta.reg_num() as usize {
            eprint!(
                "%{}{}:[{}] ",
                r,
                if r == 0 { "(self)" } else { "" },
                if let Some(v) = lfp.register(r) {
                    v.debug(&self.store)
                } else {
                    "None".to_string()
                }
            );
        }
        eprintln!();
    }

    #[cfg(feature = "emit-bc")]
    pub fn dump_bc(&mut self) {
        let dumped_bc = self.dumped_bc;
        if self.startup_flag {
            self.store.functions()[dumped_bc..]
                .iter()
                .for_each(|info| match &info.kind {
                    FuncKind::ISeq(iseq) => self.dump_iseq(*iseq),
                    _ => {}
                });
        }
        self.dumped_bc = self.store.func_len();
    }

    #[cfg(feature = "emit-bc")]
    fn dump_iseq(&self, iseq: ISeqId) {
        use bytecodegen::BcIndex;

        let func = &self.store[iseq];
        eprintln!("------------------------------------");
        let loc = func.loc;
        let line = func.sourceinfo.get_line(&loc);
        let file_name = func.sourceinfo.file_name();
        eprintln!(
            "<{}> {file_name}:{line}",
            self.store.func_description(func.func_id()),
        );
        eprintln!(
            "{:?} local_vars:{} temp:{}",
            self[func.func_id()].meta(),
            func.local_num(),
            func.temp_num
        );
        eprintln!("{:?}", func.args);
        eprintln!("{:?}", func.get_exception_map());
        for i in 0..func.bytecode().len() {
            let bc_pos = BcIndex::from(i);
            if let Some(bbid) = func.bb_info.is_bb_head(bc_pos) {
                eprintln!("{:?}", bbid);
            };
            let trace_ir = func.trace_ir(&self.store, bc_pos);
            if let Some(fmt) = trace_ir.format(&self.store) {
                eprintln!("{bc_pos} [{:02}] {fmt}", func.sp[i].0);
            };
        }
        eprintln!("------------------------------------");
    }

    #[cfg(feature = "emit-asm")]
    pub(crate) fn dump_disas(
        &mut self,
        sourcemap: Vec<(bytecodegen::BcIndex, usize)>,
        func_id: FuncId,
    ) {
        let (start, code_end, end) = self.codegen.jit.code_block.last().unwrap();
        eprintln!(
            "offset:{:?} code: {} bytes  data: {} bytes",
            start,
            *code_end - *start,
            *end - *code_end
        );
        self.codegen.jit.select_page(0);
        let dump = self.codegen.jit.dump_code().unwrap();
        let dump: Vec<(usize, String)> = dump
            .split('\n')
            .filter(|s| s.len() >= 29)
            .map(|x| {
                let i = x.find(':').unwrap();
                (
                    match usize::from_str_radix(&x[0..i].trim(), 16) {
                        Ok(i) => i,
                        _ => {
                            panic!("{}", &x[0..i].trim());
                        }
                    },
                    x[i + 24..].to_string(),
                )
            })
            .collect();
        let iseq = self.store.iseq(func_id);
        for (i, text) in dump {
            sourcemap
                .iter()
                .filter_map(
                    |(bc_pos, code_pos)| {
                        if *code_pos == i {
                            Some(*bc_pos)
                        } else {
                            None
                        }
                    },
                )
                .for_each(|bc_pos| {
                    if iseq.bb_info.is_bb_head(bc_pos).is_some() {
                        eprintln!("{:?}", iseq.bb_info.get_bb_id(bc_pos));
                    }
                    eprintln!(
                        "{bc_pos} {}",
                        match iseq.trace_ir(&self.store, bc_pos).format(&self.store) {
                            Some(s) => s,
                            None => "".to_string(),
                        }
                    );
                });

            eprintln!("  {:05x}: {}", i, text);
        }
    }

    #[cfg(any(feature = "profile", feature = "jit-log"))]
    pub(crate) fn show_stats(&self) {
        #[cfg(feature = "profile")]
        {
            eprintln!();
            eprintln!("deoptimization stats (top 20)");
            eprintln!(
                "{:60} FuncId [{:05}]     {:7}",
                "func name", "index", "count"
            );
            eprintln!("--------------------------------------------------------------------------------------------------------------------------------------------");
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
                    "{:60}  {:5} [{:05}]  {:10}   {fmt}",
                    name,
                    func_id.get(),
                    bc_pos,
                    count
                );
            }
            eprintln!();
            self.store.show_stats();
            eprintln!();
            eprintln!("jit class guard failed stats (top 20)");
            eprintln!("{:40} {:30} {:10}", "func name", "class", "count");
            eprintln!("------------------------------------------------------------------------");
            let mut v: Vec<_> = self.jit_class_unmatched_stats.iter().collect();
            v.sort_unstable_by(|(_, a), (_, b)| b.cmp(a));
            for ((func_id, class_id), count) in v.into_iter().take(20) {
                eprintln!(
                    "{:40} {:30} {:10}",
                    self.store.func_description(*func_id),
                    self.store.debug_class_name(*class_id),
                    count
                );
            }
        }
        #[cfg(feature = "jit-log")]
        {
            eprintln!();
            eprintln!(
                "elapsed JIT compile time: {:?}",
                self.codegen.jit_compile_time
            );
        }
    }
}

#[cfg(any(feature = "deopt", feature = "profile"))]
pub(crate) extern "C" fn log_deoptimize(
    vm: &mut Executor,
    globals: &mut Globals,
    pc: BytecodePtr,
    #[cfg(feature = "deopt")] v: Option<Value>,
) {
    use crate::jitgen::trace_ir::*;
    let func_id = vm.cfp().lfp().meta().func_id();
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
                _ => if let Some(v) = v {
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
