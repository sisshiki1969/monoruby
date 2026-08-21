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
        let iseq = self.store[func_id].is_iseq();
        let names = if let Some(iseq) = iseq {
            let mut names = vec![None; self.store[iseq].local_num()];
            for (name, i) in &self.store[iseq].locals {
                names[i.0 as usize] = Some(*name);
            }
            names
        } else {
            vec![]
        };
        for r in meta.regs() {
            eprint!(
                "{:?}{}:[{}] ",
                r,
                if let Some(iseq) = iseq {
                    if r.0 == 0 || r.0 as usize > self.store[iseq].local_num() {
                        "".to_string()
                    } else if let Some(name) = names[r.0 as usize - 1] {
                        format!("({name})")
                    } else {
                        "".to_string()
                    }
                } else {
                    "".to_string()
                },
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
        for r in meta.regs() {
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
                .enumerate()
                .for_each(|(_id, info)| match &info.kind {
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
            eprintln!(
                "------------------------------------------------------------------------------------------------------------------------------------------------------"
            );
            let mut v: Vec<_> = self.deopt_stats.iter().collect();
            v.sort_unstable_by(|(_, a), (_, b)| b.cmp(a));
            for ((func_id, bc_pos), count) in v.into_iter().take(20) {
                let iseq_id = self.store[*func_id].as_iseq();
                let pc = self.store[iseq_id].get_pc(*bc_pos);
                let fmt = if let Some(fmt) =
                    jitgen::trace_ir::TraceIr::format(&self.store, iseq_id, pc)
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
            eprintln!(" FuncId  {:35} {:45} {:10}", "func name", "class", "count");
            eprintln!(
                "------------------------------------------------------------------------------------------------------"
            );
            let mut v: Vec<_> = self.jit_class_unmatched_stats.iter().collect();
            v.sort_unstable_by(|(_, a), (_, b)| b.cmp(a));
            for ((func_id, class_id), count) in v.into_iter().take(20) {
                eprintln!(
                    "({:6}) {:35} {:45} {:10}",
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
                "--------------------------------------------------------------------------------------------------------------------------------"
            );
            let mut v: Vec<_> = self.jit_recompile_count.iter().collect();
            v.sort_unstable_by(|(_, a), (_, b)| b.cmp(a));
            for ((func_id, class_id, reason), count) in v.into_iter().take(20) {
                eprintln!(
                    "({:6}) {:40} {:30} {:30} {:10}",
                    func_id.get(),
                    self.store.func_description(*func_id),
                    self.store.debug_class_name(*class_id),
                    format!("{:?}", reason),
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
            crate::codegen::jit_stats::dump();
        }
    }
}

#[cfg(any(feature = "deopt", feature = "profile"))]
pub(crate) extern "C" fn log_deoptimize(
    vm: &mut Executor,
    globals: &mut Globals,
    pc: BytecodePtr,
    // `exit_id`: registry id of the handler that is running, baked into the
    // call as an immediate — so the reported exit kind cannot disagree with
    // the code that produced it. (The branch *into* the handler is
    // identified separately, by the trampoline record this reads from `vm`.)
    #[cfg(feature = "deopt")] exit_id: u32,
) {
    use crate::jitgen::trace_ir::*;
    let func_id = vm.cfp().lfp().func_id();
    let iseq_id = globals.store[func_id].as_iseq();
    let bc_pos = globals.store[iseq_id].get_pc_index(Some(pc));
    let trace_ir = TraceIr::from_pc(pc, &globals.store);

    if let TraceIr::LoopEnd = trace_ir {
        // normal exit from jit'ed loop
        #[cfg(feature = "deopt")]
        {
            let name = globals.store.func_description(func_id);
            let fmt = TraceIr::format(&globals.store, iseq_id, pc).unwrap_or_default();
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
            use crate::codegen::jitgen::deopt_log;
            let name = globals.store.func_description(func_id);
            let fmt = TraceIr::format(&globals.store, iseq_id, pc).unwrap_or_default();
            let exit = deopt_log::exit(exit_id);
            let site = vm.take_deopt_site();

            eprintln!("<-- deopt occurs in <{name}> {func_id:?}.");
            eprintln!(
                "      [{bc_pos:05}] {fmt}   exit: {}",
                match &exit {
                    Some(e) => e.to_string(),
                    None => "unknown".to_string(),
                }
            );
            match site.and_then(|(id, bits)| deopt_log::site(id).map(|s| (s, bits))) {
                Some((s, bits)) => {
                    eprintln!("      guard: {}", s.lowered_at);
                    if let Some(created) = s.created_at {
                        eprintln!("      exit emitted by: {created}");
                    }
                    eprintln!("      cause: {}", render_cause(globals, s.cause, bits));
                }
                // Reached without a trampoline — an evict resumed through a
                // patched return address, say. Report that honestly instead
                // of attributing a stale record to this deopt.
                None => eprintln!("      guard: unknown (handler entered without a trampoline)"),
            }
        }
    }
}

///
/// Render the operand a guard recorded, given what its lowering site
/// declared it to be.
///
/// A `Value` cause is only decoded once its bits look like one; a guard
/// that fires on corrupt input must not turn the log into a crash.
///
#[cfg(feature = "deopt")]
fn render_cause(
    globals: &Globals,
    cause: crate::codegen::jitgen::deopt_log::DeoptCause,
    bits: u64,
) -> String {
    use crate::codegen::jitgen::deopt_log::DeoptCause;
    let decode = |bits: u64| -> String {
        match std::num::NonZeroU64::new(bits) {
            None => format!("<null> (bits={bits:#x})"),
            Some(_) => {
                let v = unsafe { std::mem::transmute::<u64, Value>(bits) };
                match v.debug_check(&globals.store) {
                    Some(s) => format!("{s} (bits={bits:#x})"),
                    None => format!("<not a Value> (bits={bits:#x})"),
                }
            }
        }
    };
    match cause {
        DeoptCause::Value(r) => format!("{r:?} = {}", decode(bits)),
        DeoptCause::ClassGuard(r, expected) => {
            // Name the class the value actually has, not just the value:
            // "expected FFI::MemoryPointer, got FFI::Pointer" is the whole
            // diagnosis for a monomorphic guard that a sibling class walks
            // into, and reading it off the rendered object is guesswork.
            let actual = match std::num::NonZeroU64::new(bits) {
                None => "<null>".to_string(),
                Some(_) => {
                    let v = unsafe { std::mem::transmute::<u64, Value>(bits) };
                    match v.debug_check(&globals.store) {
                        Some(_) => globals.store.debug_class_name(v.class()),
                        None => "<not a Value>".to_string(),
                    }
                }
            };
            format!(
                "{r:?} = {}, class {} but guard expected {}",
                decode(bits),
                actual,
                globals.store.debug_class_name(expected)
            )
        }
        DeoptCause::ValueVsBaked(r, expected) => {
            let mut s = format!(
                "{r:?} = {}, expected {} (bits={:#x})",
                decode(bits),
                expected.debug(&globals.store),
                expected.id()
            );
            // A guard that missed on bits equal to what it compares against
            // is a contradiction — the kind that cost four wrong hypotheses
            // before this log could state it. Say so loudly rather than
            // printing two identical values and leaving it to the reader.
            if bits == expected.id() {
                s.push_str("  !!! guard reported a miss on equal bits");
            }
            s
        }
        DeoptCause::Raw(r) => format!("{r:?} = {bits:#x} (raw)"),
        DeoptCause::Static(what) => what.to_string(),
    }
}
