use super::*;

impl Globals {
    pub(crate) unsafe fn dump_frame_info(&mut self, lfp: LFP) {
        let meta = lfp.meta();
        let outer = lfp.outer();
        let func_id = meta.func_id();
        let block = lfp.block();
        eprintln!(
            "    {} block:{} outer:{} {:?}",
            self.store.func_description(func_id),
            match block {
                Some(block) => {
                    match block.try_proxy() {
                        Some((func_id, idx)) => {
                            format!("BlockArgProxy {{ {:?}, {} }}", func_id, idx)
                        }
                        _ => self.inspect(block.0),
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
                    self.inspect(v)
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
                    let pc = BcPc::from(&func.bytecode()[bc_pos.to_usize()]);
                    eprintln!(
                        "{bc_pos} {}",
                        match self.format(pc, bc_pos.to_usize()) {
                            Some(s) => s,
                            None => "".to_string(),
                        }
                    );
                });

            eprintln!("  {:05x}: {}", i, text);
        }
    }

    #[cfg(feature = "profile")]
    pub(crate) fn show_stats(&self) {
        eprintln!();
        eprintln!("deoptimization stats (top 20)");
        eprintln!(
            "{:30} FuncId [{:05}]     {:7}",
            "func name", "index", "count"
        );
        eprintln!("-------------------------------------------------------------------------------------------------------------------");
        let mut v: Vec<_> = self.deopt_stats.iter().collect();
        v.sort_unstable_by(|(_, a), (_, b)| b.cmp(a));
        for ((func_id, index), count) in v.into_iter().take(20) {
            let bc = BcPc::from(&self.store[*func_id].as_ruby_func().bytecode()[*index]);
            let fmt = if let Some(fmt) = self.format(bc, *index) {
                fmt
            } else {
                "<INVALID>".to_string()
            };
            let name = self.store.func_description(*func_id);
            eprintln!(
                "{:30}  {:5} [{:05}]  {:10}   {fmt}",
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
                class_id.get_name(self),
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
                class_id.get_name(self),
                count
            );
        }
    }

    #[cfg(feature = "dump-bc")]
    pub(super) fn format(&self, pc: BcPc, i: usize) -> Option<String> {
        use crate::jitgen::trace_ir::*;

        fn optstr(opt: bool) -> &'static str {
            if opt {
                "_"
            } else {
                ""
            }
        }

        fn ret_str(slot: Option<SlotId>) -> String {
            match slot {
                None => "_".to_string(),
                Some(ret) => format!("{:?}", ret),
            }
        }

        let s = match pc.trace_ir() {
            TraceIr::InitMethod(info) => {
                format!("init_method {info:?}")
            }
            TraceIr::CheckLocal(local, disp) => {
                format!("check_local({:?}) =>:{:05}", local, i as i32 + 1 + disp)
            }
            TraceIr::Br(disp) => {
                format!("br =>:{:05}", i as i32 + 1 + disp)
            }
            TraceIr::CondBr(reg, disp, opt, kind) => {
                format!(
                    "cond{}br {}{:?} =>:{:05}",
                    kind.to_s(),
                    optstr(opt),
                    reg,
                    i as i32 + 1 + disp
                )
            }
            TraceIr::OptCase { cond: dst, optid } => {
                format!("opt_case {:?}->({:?})", dst, self.store[optid])
            }
            TraceIr::Integer(reg, num) => format!("{:?} = {}: i32", reg, num),
            TraceIr::Symbol(reg, id) => format!("{:?} = :{id}", reg),
            TraceIr::Range {
                dst,
                start,
                end,
                exclude_end,
            } => format!(
                "{:?} = {:?} {} {:?}",
                dst,
                start,
                if exclude_end { "..." } else { ".." },
                end
            ),
            TraceIr::Literal(reg, val) => {
                format!("{:?} = literal[{}]", reg, self.inspect(val))
            }
            TraceIr::Array { dst, callid } => {
                let CallSiteInfo { args, pos_num, .. } = self.store[callid];
                format!("{:?} = array[{:?}; {}]", dst, args, pos_num)
            }
            TraceIr::Hash { dst, args, len } => {
                format!("{:?} = hash[{:?}; {}]", dst, args, len)
            }
            TraceIr::Index { dst, base, idx } => {
                let op1 = format!("{:?} = {:?}.[{:?}]", dst, base, idx);
                format!(
                    "{:36} [{}][{}]",
                    op1,
                    pc.classid1().get_name(self),
                    pc.classid2().get_name(self)
                )
            }
            TraceIr::IndexAssign { src, base, idx } => {
                format!("{:?}:.[{:?}:] = {:?}", base, idx, src,)
            }
            TraceIr::LoadConst(reg, id) => {
                let ConstSiteInfo {
                    name,
                    base,
                    prefix,
                    toplevel,
                    ..
                } = &self.store[id];
                let mut const_name = if *toplevel { "::" } else { "" }.to_string();
                for c in prefix {
                    c.append_to(&mut const_name);
                    const_name += "::";
                }
                name.append_to(&mut const_name);
                let op1 = format!(
                    "{:?} = {}const[{}]",
                    reg,
                    if let Some(base) = base {
                        format!("{:?}::", base)
                    } else {
                        "".to_string()
                    },
                    const_name
                );
                format!(
                    "{:36} [{}]",
                    op1,
                    match pc.value() {
                        None => "<INVALID>".to_string(),
                        Some(val) => self.inspect(val),
                    }
                )
            }
            TraceIr::StoreConst(src, id) => {
                format!("const[{id}] = {:?}", src)
            }
            TraceIr::BlockArgProxy(dst, outer) => {
                format!("{:?} = block_proxy({outer})", dst)
            }
            TraceIr::BlockArg(dst, outer) => {
                format!("{:?} = block_arg({outer})", dst)
            }
            TraceIr::LoadDynVar(dst, src) => {
                format!("{:?} = {:?}", dst, src)
            }
            TraceIr::StoreDynVar(dst, src) => {
                format!("{:?} = {:?}", dst, src)
            }
            TraceIr::LoadIvar(reg, id, class_id, ivar_id) => {
                format!(
                    "{:?} = {id}: {}",
                    reg,
                    if let Some(id) = class_id {
                        format!("{}[{:?}]", id.get_name(self), ivar_id)
                    } else {
                        format!("-")
                    }
                )
            }
            TraceIr::StoreIvar(reg, id, class_id, ivar_id) => {
                format!(
                    "{id}: {} = {:?}",
                    if let Some(id) = class_id {
                        format!("{}[{:?}]", id.get_name(self), ivar_id)
                    } else {
                        format!("-")
                    },
                    reg
                )
            }
            TraceIr::LoadGvar { dst: ret, name } => {
                format!("{:?} = {name}", ret)
            }
            TraceIr::StoreGvar { src, name } => {
                format!("{name} = {:?}", src)
            }
            TraceIr::LoadSvar { dst: ret, id } => {
                // 0 => $&
                // 1 => $'
                // 100 + n => $n
                format!(
                    "{:?} = ${}",
                    ret,
                    match id {
                        0 => "&".to_string(),
                        1 => "'".to_string(),
                        n => (n - 100).to_string(),
                    }
                )
            }
            TraceIr::Nil(reg) => format!("{:?} = nil", reg),
            TraceIr::BitNot { dst, src } => {
                let op1 = format!("{:?} = ~{:?}", dst, src);
                format!("{:36} [{}]", op1, pc.classid1().get_name(self),)
            }
            TraceIr::UnOp { kind, dst, src } => {
                let op1 = format!("{:?} = {}{:?}", dst, kind, src);
                format!("{:36} [{}]", op1, pc.classid1().get_name(self),)
            }
            TraceIr::Not { dst, src } => {
                let op1 = format!("{:?} = !{:?}", dst, src);
                format!("{:36} [{}]", op1, pc.classid1().get_name(self),)
            }
            TraceIr::BinOp {
                kind,
                dst,
                mode: OpMode::RR(lhs, rhs),
            }
            | TraceIr::IBinOp {
                kind,
                dst,
                mode: OpMode::RR(lhs, rhs),
            }
            | TraceIr::FBinOp {
                kind,
                dst,
                mode: OpMode::RR(lhs, rhs),
            } => {
                let op1 = format!("{} = {:?} {} {:?}", ret_str(dst), lhs, kind, rhs);
                format!(
                    "{:36} [{}][{}]",
                    op1,
                    pc.classid1().get_name(self),
                    pc.classid2().get_name(self)
                )
            }
            TraceIr::BinOp {
                kind,
                dst,
                mode: OpMode::RI(lhs, rhs),
            }
            | TraceIr::IBinOp {
                kind,
                dst,
                mode: OpMode::RI(lhs, rhs),
            }
            | TraceIr::FBinOp {
                kind,
                dst,
                mode: OpMode::RI(lhs, rhs),
            } => {
                let op1 = format!("{} = {:?} {} {}: i16", ret_str(dst), lhs, kind, rhs,);
                format!(
                    "{:36} [{}][{}]",
                    op1,
                    pc.classid1().get_name(self),
                    pc.classid2().get_name(self)
                )
            }
            TraceIr::BinOp {
                kind,
                dst,
                mode: OpMode::IR(lhs, rhs),
            }
            | TraceIr::IBinOp {
                kind,
                dst,
                mode: OpMode::IR(lhs, rhs),
            }
            | TraceIr::FBinOp {
                kind,
                dst,
                mode: OpMode::IR(lhs, rhs),
            } => {
                let op1 = format!("{} = {}: i16 {} {:?}", ret_str(dst), lhs, kind, rhs,);
                format!(
                    "{:36} [{}][{}]",
                    op1,
                    pc.classid1().get_name(self),
                    pc.classid2().get_name(self)
                )
            }
            TraceIr::Cmp(kind, dst, mode, opt) => {
                let op1 = match mode {
                    OpMode::RR(lhs, rhs) => {
                        format!(
                            "{}{} = {:?} {:?} {:?}",
                            optstr(opt),
                            ret_str(dst),
                            lhs,
                            kind,
                            rhs,
                        )
                    }
                    OpMode::RI(lhs, rhs) => format!(
                        "{}{} = {:?} {:?} {}: i16",
                        optstr(opt),
                        ret_str(dst),
                        lhs,
                        kind,
                        rhs,
                    ),
                    _ => unreachable!(),
                };
                format!(
                    "{:36} [{}][{}]",
                    op1,
                    pc.classid1().get_name(self),
                    pc.classid2().get_name(self)
                )
            }

            TraceIr::Ret(reg) => format!("ret {:?}", reg),
            TraceIr::MethodRet(reg) => format!("method_ret {:?}", reg),
            TraceIr::Break(reg) => format!("break {:?}", reg),
            TraceIr::Raise(reg) => format!("raise {:?}", reg),
            TraceIr::EnsureEnd => format!("ensure_end"),
            TraceIr::Mov(dst, src) => format!("{:?} = {:?}", dst, src),
            TraceIr::MethodCall { callid, .. } | TraceIr::MethodCallBlock { callid, .. } => {
                let callsite = &self.store[callid];
                let class = pc.cached_class1();
                let name = if let Some(name) = callsite.name {
                    name.to_string()
                } else {
                    "super".to_string()
                };
                let CallSiteInfo {
                    recv,
                    args,
                    pos_num,
                    dst: ret,
                    block_fid,
                    block_arg,
                    ..
                } = *callsite;
                let has_splat = callsite.has_splat();
                let kw_len = callsite.kw_num();
                let op1 = format!(
                    "{} = {:?}.{name}({}{}{}){}",
                    ret_str(ret),
                    recv,
                    if pos_num == 0 {
                        "".to_string()
                    } else {
                        format!("{:?};{}{}", args, pos_num, if has_splat { "*" } else { "" })
                    },
                    if kw_len == 0 {
                        "".to_string()
                    } else {
                        format!(" kw:{:?};{}", args + pos_num as u16, kw_len)
                    },
                    if let Some(block_arg) = block_arg {
                        format!(" &{:?}", block_arg)
                    } else {
                        "".to_string()
                    },
                    if let Some(block_fid) = block_fid {
                        format!(" {{ {:?} }}", block_fid)
                    } else {
                        "".to_string()
                    },
                );
                format!(
                    "{:36} [{}]",
                    op1,
                    match class {
                        Some(class) => class.get_name(self),
                        None => "-".to_string(),
                    }
                )
            }
            TraceIr::InlineCall {
                inline_id, callid, ..
            } => {
                let CallSiteInfo {
                    recv,
                    args,
                    pos_num,
                    dst: ret,
                    ..
                } = self.store[callid];
                let class = pc.cached_class1().unwrap();
                let name = &self.store.get_inline_info(inline_id).2;
                let op1 = if pos_num == 0 {
                    format!("{} = {:?}.inline {name}()", ret_str(ret), recv,)
                } else {
                    format!(
                        "{} = {:?}.inline {name}({:?}; {})",
                        ret_str(ret),
                        recv,
                        args,
                        pos_num,
                    )
                };
                format!("{:36} [{}]", op1, class.get_name(self))
            }
            TraceIr::Yield { callid } => {
                let CallSiteInfo {
                    args,
                    pos_num,
                    dst: ret,
                    ..
                } = self.store[callid];
                if pos_num == 0 {
                    format!("{} = yield", ret_str(ret))
                } else {
                    format!("{} = yield({:?}; {})", ret_str(ret), args, pos_num)
                }
            }
            TraceIr::InlineCache => return None,
            TraceIr::MethodDef { name, func_id } => {
                format!("method_def {name}: {:?}", func_id)
            }
            TraceIr::SingletonMethodDef { obj, name, func_id } => {
                format!("singleton_method_def {:?}.{name}: {:?}", obj, func_id)
            }
            TraceIr::ClassDef {
                dst: ret,
                superclass,
                name,
                func_id,
            } => {
                format!(
                    "{} = class_def {name} < {:?}: {:?}",
                    ret_str(ret),
                    superclass,
                    func_id
                )
            }
            TraceIr::ModuleDef {
                dst: ret,
                name,
                func_id,
            } => {
                format!("{} = module_def {name}: {:?}", ret_str(ret), func_id)
            }
            TraceIr::SingletonClassDef {
                dst: ret,
                base,
                func_id,
            } => {
                format!(
                    "{} = singleton_class_def << {:?}: {:?}",
                    ret_str(ret),
                    base,
                    func_id
                )
            }
            TraceIr::ConcatStr(ret, args, len) => {
                format!("{} = concat({:?}; {})", ret_str(ret), args, len)
            }
            TraceIr::ConcatRegexp(ret, args, len) => {
                format!("{} = concat_regexp({:?}; {})", ret_str(ret), args, len)
            }
            TraceIr::ExpandArray(src, dst, len) => {
                format!("{:?}; {} = expand({:?})", dst, len, src)
            }
            TraceIr::AliasMethod { new, old } => {
                format!("alias_method({:?}<-{:?})", new, old)
            }
            TraceIr::DefinedYield { dst: ret } => format!("{:?} = defined?(yield)", ret),
            TraceIr::DefinedConst { dst: ret, siteid } => {
                let ConstSiteInfo {
                    name,
                    prefix,
                    toplevel,
                    ..
                } = &self.store[siteid];
                let mut const_name = if *toplevel { "::" } else { "" }.to_string();
                for c in prefix {
                    c.append_to(&mut const_name);
                    const_name += "::";
                }
                name.append_to(&mut const_name);
                format!("{:?} = defined?(constant) {const_name}", ret)
            }
            TraceIr::DefinedMethod {
                dst: ret,
                recv,
                name,
            } => {
                format!("{:?} = defined?(method) {:?}.{}", ret, recv, name)
            }
            TraceIr::DefinedGvar { dst: ret, name } => {
                format!("{:?} = defined?(gvar) {}", ret, name)
            }
            TraceIr::DefinedIvar { dst: ret, name } => {
                format!("{:?} = defined?(ivar) {}", ret, name)
            }
            TraceIr::LoopStart(count) => format!(
                "loop_start counter={} jit-addr={:016x}",
                count,
                pc.into_jit_addr()
            ),
            TraceIr::LoopEnd => "loop_end".to_string(),
        };
        Some(s)
    }
}

#[cfg(any(feature = "log-jit", feature = "profile"))]
pub(crate) extern "C" fn log_deoptimize(
    vm: &mut Executor,
    globals: &mut Globals,
    pc: BcPc,
    #[cfg(feature = "log-jit")] v: Option<Value>,
) {
    use crate::jitgen::trace_ir::*;
    let func_id = vm.cfp().lfp().meta().func_id();
    let bc_begin = globals[func_id].as_ruby_func().get_top_pc();
    let index = pc - bc_begin;

    if let TraceIr::LoopEnd = pc.trace_ir() {
        // normal exit from jit'ed loop
        #[cfg(feature = "log-jit")]
        {
            let name = globals.store.func_description(func_id);
            let fmt = globals.format(pc, index).unwrap_or_default();
            eprint!("<-- exited from JIT code in {} {:?}.", name, func_id);
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
        #[cfg(feature = "log-jit")]
        {
            let trace_ir = pc.trace_ir();
            let name = globals.store.func_description(func_id);
            let fmt = globals.format(pc, index).unwrap_or_default();
            match trace_ir {
                TraceIr::LoadConst(..)          // inline constant cache miss
                | TraceIr::ClassDef { .. }      // error in class def (illegal superclass etc.)
                | TraceIr::LoadIvar(..)         // inline ivar cache miss
                | TraceIr::StoreIvar(..) => {
                    eprint!("<-- deopt occurs in {} {:?}.", name, func_id);
                    eprintln!("    [{:05}] {fmt}", index);
                },
                _ => if let Some(v) = v {
                    eprint!("<-- deopt occurs in {} {:?}.", name, func_id);
                    eprintln!("    [{:05}] {fmt} caused by {}", index, globals.to_s(v));
                } else {
                    eprint!("<-- non-optimized branch in {} {:?}.", name, func_id);
                    eprintln!("    [{:05}] {fmt}", index);
                },
            }
        }
    }
}
