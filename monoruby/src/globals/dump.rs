use super::*;

impl Globals {
    pub(crate) unsafe fn dump_frame_info(&self, lfp: Lfp) {
        let meta = lfp.meta();
        let outer = lfp.outer();
        let func_id = meta.func_id();
        let block = lfp.block();
        eprintln!(
            "    <{}> block:{} outer:{} {:?}",
            self.func_description(func_id),
            match block {
                Some(block) => {
                    match block.try_proxy() {
                        Some((func_id, idx)) => {
                            format!("BlockArgProxy {{ {:?}, {} }}", func_id, idx)
                        }
                        _ => self.inspect2(block.get()),
                    }
                }
                None => "None".to_string(),
            },
            match outer {
                None => "None".to_string(),
                Some(outer) => format!("{:?}", outer.lfp()),
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
                    self.inspect2(v)
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
                    FuncKind::ISeq(_) => info.dump_bc(self),
                    _ => {}
                });
        }
        self.dumped_bc = self.store.func_len();
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
        let func = self[func_id].as_ruby_func();
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
                    let pc = BytecodePtr::from_bc(&func.bytecode()[bc_pos.to_usize()]);
                    eprintln!(
                        "{bc_pos} {}",
                        match pc
                            .trace_ir(&self.store)
                            .format(&self.store, bc_pos.to_usize())
                        {
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
                "{:40} FuncId [{:05}]     {:7}",
                "func name", "index", "count"
            );
            eprintln!("-------------------------------------------------------------------------------------------------------------------");
            let mut v: Vec<_> = self.deopt_stats.iter().collect();
            v.sort_unstable_by(|(_, a), (_, b)| b.cmp(a));
            for ((func_id, index), count) in v.into_iter().take(20) {
                let bc =
                    BytecodePtr::from_bc(&self.store[*func_id].as_ruby_func().bytecode()[*index]);
                let fmt = if let Some(fmt) = bc.trace_ir(&self.store).format(&self.store, *index) {
                    fmt
                } else {
                    "<INVALID>".to_string()
                };
                let name = self.func_description(*func_id);
                eprintln!(
                    "{:40}  {:5} [{:05}]  {:10}   {fmt}",
                    name,
                    func_id.get(),
                    index,
                    count
                );
            }
            eprintln!();
            eprintln!("global method cache stats (top 20)");
            eprintln!("{:30} {:30} {:10}", "func name", "class", "count");
            eprintln!("------------------------------------------------------------------------");
            let mut v: Vec<_> = self.global_method_cache_stats.iter().collect();
            v.sort_unstable_by(|(_, a), (_, b)| b.cmp(a));
            for ((class_id, name), count) in v.into_iter().take(20) {
                eprintln!(
                    "{:30} {:30} {:10}",
                    name.to_string(),
                    self.get_class_name(*class_id),
                    count
                );
            }
            eprintln!();
            eprintln!("full method exploration stats (top 20)");
            eprintln!("{:30} {:30} {:10}", "func name", "class", "count");
            eprintln!("------------------------------------------------------------------------");
            let mut v: Vec<_> = self.method_exploration_stats.iter().collect();
            v.sort_unstable_by(|(_, a), (_, b)| b.cmp(a));
            for ((class_id, name), count) in v.into_iter().take(20) {
                eprintln!(
                    "{:30} {:30} {:10}",
                    name.to_string(),
                    self.get_class_name(*class_id),
                    count
                );
            }
            eprintln!();
            eprintln!("jit class guard failed stats (top 20)");
            eprintln!("{:40} {:30} {:10}", "func name", "class", "count");
            eprintln!("------------------------------------------------------------------------");
            let mut v: Vec<_> = self.jit_class_unmatched_stats.iter().collect();
            v.sort_unstable_by(|(_, a), (_, b)| b.cmp(a));
            for ((func_id, class_id), count) in v.into_iter().take(20) {
                eprintln!(
                    "{:40} {:30} {:10}",
                    self.func_description(*func_id),
                    self.get_class_name(*class_id),
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
    let bc_begin = globals[func_id].as_ruby_func().get_top_pc();
    let index = pc - bc_begin;
    let trace_ir = pc.trace_ir(&globals.store);

    if let TraceIr::LoopEnd = trace_ir {
        // normal exit from jit'ed loop
        #[cfg(feature = "deopt")]
        {
            let name = globals.func_description(func_id);
            let fmt = trace_ir.format(&globals.store, index).unwrap_or_default();
            eprint!("<-- exited from JIT code in <{}> {:?}.", name, func_id);
            eprintln!("    [{:05}] {fmt}", index);
        }
    } else {
        #[cfg(feature = "profile")]
        {
            match globals.deopt_stats.get_mut(&(func_id, index)) {
                Some(c) => *c = *c + 1,
                None => {
                    globals.deopt_stats.insert((func_id, index), 1);
                }
            }
        }
        #[cfg(feature = "deopt")]
        {
            let trace_ir = pc.trace_ir(&globals.store);
            let name = globals.func_description(func_id);
            let fmt = trace_ir.format(&globals.store, index).unwrap_or_default();
            match trace_ir {
                TraceIr::LoadConst(..)          // inline constant cache miss
                | TraceIr::ClassDef { .. }      // error in class def (illegal superclass etc.)
                | TraceIr::LoadIvar(..)         // inline ivar cache miss
                | TraceIr::StoreIvar(..) => {
                    eprint!("<-- deopt occurs in <{}> {:?}.", name, func_id);
                    eprintln!("    [{:05}] {fmt}", index);
                },
                _ => if let Some(v) = v {
                    eprint!("<-- deopt occurs in <{}> {:?}.", name, func_id);
                    eprintln!("    [{:05}] {fmt} caused by {}", index, globals.inspect2(v));
                } else {
                    eprint!("<-- non-traced branch in <{}> {:?}.", name, func_id);
                    eprintln!("    [{:05}] {fmt}", index);
                },
            }
        }
    }
}
