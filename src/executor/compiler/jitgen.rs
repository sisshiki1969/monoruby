use monoasm_macro::monoasm;
use paste::paste;

pub(crate) use self::basic_block::BasicBlockInfo;
pub(self) use self::basic_block::{BasciBlockInfoEntry, BasicBlockId};

use super::*;
use analysis::{ExitType, SlotInfo};

mod analysis;
mod basic_block;
mod binary_op;
mod compile;
mod constants;
mod guard;
mod init_method;
mod merge;
mod method_call;
mod read_slot;

//
// Just-in-time compiler module.
//

///
/// Context for JIT compilation.
///
struct JitContext {
    ///
    /// Destination labels for jump instructions.
    ///
    labels: HashMap<BcIndex, DestLabel>,
    ///
    /// Basic block information.
    ///
    bb_scan: Vec<(ExitType, SlotInfo)>,
    loop_backedges: HashMap<BasicBlockId, SlotInfo>,
    ///
    /// Loop
    ///
    /// ### key
    /// start basic block.
    ///
    /// ### value
    /// (end basic block, slot_info)
    ///
    loop_exit: HashMap<BasicBlockId, (BasicBlockId, SlotInfo)>,
    ///
    /// The start bytecode position of the current basic block.
    ///
    cur_pos: BcIndex,
    ///
    /// Nested loop count.
    ///
    loop_count: usize,
    ///
    /// true for a loop, false for a method.
    ///
    is_loop: bool,
    ///
    /// A map for bytecode position and branches.
    ///
    branch_map: HashMap<BcIndex, Vec<BranchEntry>>,
    ///
    /// Target context (BBContext) for an each instruction.
    ///
    target_ctx: HashMap<BcIndex, BBContext>,
    ///
    /// A map for backward branches.
    ///
    backedge_map: HashMap<BcIndex, (DestLabel, BBContext, Vec<SlotId>)>,
    ///
    /// The start offset of a machine code corresponding to thhe current basic block.
    ///
    #[cfg(feature = "emit-asm")]
    start_codepos: usize,
    ///
    /// the number of slots.
    ///
    total_reg_num: usize,
    ///
    /// the number of local variables.
    ///
    local_num: usize,
    ///
    /// *self* for this loop/method.
    ///
    self_value: Value,
    ///
    /// source map.
    ///
    sourcemap: Vec<(BcIndex, usize)>,
}

#[derive(Clone)]
pub(crate) struct Incoming(Vec<Vec<BcIndex>>);

impl std::ops::Deref for Incoming {
    type Target = Vec<Vec<BcIndex>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::Index<BcIndex> for Incoming {
    type Output = Vec<BcIndex>;
    fn index(&self, index: BcIndex) -> &Self::Output {
        &self.0[index.0 as usize]
    }
}

impl JitContext {
    ///
    /// Create new JitContext.
    ///
    fn new(
        func: &ISeqInfo,
        codegen: &mut Codegen,
        start_pos: BcIndex,
        is_loop: bool,
        self_value: Value,
    ) -> Self {
        let mut labels = HashMap::default();
        for i in 0..func.bytecode_len() {
            let idx = BcIndex::from(i);
            if func.bb_info.is_bb_head(idx) {
                labels.insert(idx, codegen.jit.label());
            }
        }
        let bb_scan = func.bb_info.init_bb_scan(func);

        #[cfg(feature = "emit-asm")]
        let start_codepos = codegen.jit.get_current();

        let total_reg_num = func.total_reg_num();
        let local_num = func.local_num();
        Self {
            labels,
            bb_scan,
            loop_backedges: HashMap::default(),
            loop_exit: HashMap::default(),
            cur_pos: start_pos,
            loop_count: 0,
            is_loop,
            branch_map: HashMap::default(),
            target_ctx: HashMap::default(),
            backedge_map: HashMap::default(),
            #[cfg(feature = "emit-asm")]
            start_codepos,
            total_reg_num,
            local_num,
            self_value,
            sourcemap: vec![],
        }
    }

    fn new_branch(&mut self, src_idx: BcIndex, dest: BcIndex, bbctx: BBContext, entry: DestLabel) {
        self.branch_map.entry(dest).or_default().push(BranchEntry {
            src_idx,
            bbctx,
            entry,
            cont: false,
        })
    }

    fn new_continue(
        &mut self,
        src_idx: BcIndex,
        dest: BcIndex,
        bbctx: BBContext,
        entry: DestLabel,
    ) {
        self.branch_map.entry(dest).or_default().push(BranchEntry {
            src_idx,
            bbctx,
            entry,
            cont: true,
        })
    }

    fn new_backedge(
        &mut self,
        bbctx: &BBContext,
        bb_pos: BcIndex,
        dest_label: DestLabel,
        unused: Vec<SlotId>,
    ) {
        self.backedge_map
            .insert(bb_pos, (dest_label, bbctx.clone(), unused));
    }
}

#[derive(Debug)]
struct BranchEntry {
    src_idx: BcIndex,
    bbctx: BBContext,
    entry: DestLabel,
    cont: bool,
}

fn conv(reg: SlotId) -> i64 {
    reg.0 as i64 * 8 + LBP_SELF
}

struct WriteBack {
    xmm: Vec<(Xmm, Vec<SlotId>)>,
    fixnum: Vec<(Value, SlotId)>,
}

impl WriteBack {
    fn new(xmm: Vec<(Xmm, Vec<SlotId>)>, fixnum: Vec<(Value, SlotId)>) -> Self {
        Self { xmm, fixnum }
    }

    fn is_empty(&self) -> bool {
        self.xmm.is_empty() && self.fixnum.is_empty()
    }
}

///
/// Context of the current Basic block.
///
#[derive(Debug, Clone, PartialEq)]
struct BBContext {
    /// Information for stack slots.
    slot_state: StackSlotInfo,
    /// Information for xmm registers.
    xmm: XmmInfo,
    self_value: Value,
    local_num: usize,
    recompile_flag: bool,
}

impl BBContext {
    fn new(cc: &JitContext) -> Self {
        let xmm = XmmInfo::new();
        Self {
            slot_state: StackSlotInfo(vec![LinkMode::Stack; cc.total_reg_num]),
            xmm,
            self_value: cc.self_value,
            local_num: cc.local_num,
            recompile_flag: false,
        }
    }

    fn remove_unused(&mut self, unused: &[SlotId]) {
        unused.iter().for_each(|reg| self.dealloc_xmm(*reg));
    }

    ///
    /// Allocate a new xmm register.
    ///
    fn alloc_xmm(&mut self) -> Xmm {
        for (flhs, xmm) in self.xmm.0.iter_mut().enumerate() {
            if xmm.is_empty() {
                return Xmm(flhs as u16);
            }
        }
        unreachable!("no xmm reg is vacant.")
    }

    fn link_xmm(&mut self, reg: SlotId, freg: Xmm) {
        self.dealloc_xmm(reg);
        self.slot_state[reg] = LinkMode::Xmm(freg);
        self.xmm[freg].push(reg);
    }

    fn link_both(&mut self, reg: SlotId, freg: Xmm) {
        self.dealloc_xmm(reg);
        self.slot_state[reg] = LinkMode::Both(freg);
        self.xmm[freg].push(reg);
    }

    fn link_const(&mut self, reg: SlotId, v: Value) {
        self.dealloc_xmm(reg);
        self.slot_state[reg] = LinkMode::Const(v);
    }

    ///
    /// Deallocate an xmm register corresponding to the stack slot *reg*.
    ///
    fn dealloc_xmm(&mut self, reg: SlotId) {
        match self.slot_state[reg] {
            LinkMode::Both(freg) | LinkMode::Xmm(freg) => {
                assert!(self.xmm[freg].contains(&reg));
                self.xmm[freg].retain(|e| *e != reg);
                self.slot_state[reg] = LinkMode::Stack;
            }
            LinkMode::Const(_) => {
                self.slot_state[reg] = LinkMode::Stack;
            }
            LinkMode::Stack => {}
        }
    }

    fn dealloc_locals(&mut self) {
        for reg in 1..1 + self.local_num as u16 {
            self.dealloc_xmm(SlotId(reg));
        }
    }

    fn xmm_swap(&mut self, l: Xmm, r: Xmm) {
        self.xmm.0.swap(l.0 as usize, r.0 as usize);
        self.slot_state.0.iter_mut().for_each(|mode| match mode {
            LinkMode::Both(x) | LinkMode::Xmm(x) => {
                if *x == l {
                    *x = r;
                } else if *x == r {
                    *x = l;
                }
            }
            LinkMode::Stack | LinkMode::Const(_) => {}
        });
    }

    ///
    /// Allocate new xmm register to the given stack slot for read/write f64.
    ///
    fn xmm_write(&mut self, reg: SlotId) -> Xmm {
        match self.slot_state[reg] {
            LinkMode::Xmm(freg) if self.xmm[freg].len() == 1 => {
                assert_eq!(reg, self.xmm[freg][0]);
                freg
            }
            LinkMode::Xmm(_) | LinkMode::Both(_) | LinkMode::Stack | LinkMode::Const(_) => {
                self.dealloc_xmm(reg);
                let freg = self.alloc_xmm();
                self.link_xmm(reg, freg);
                freg
            }
        }
    }

    fn merge(&mut self, other: &BBContext) {
        for i in 0..self.slot_state.0.len() {
            let i = SlotId(i as u16);
            match (&self.slot_state[i], &other.slot_state[i]) {
                (LinkMode::Both(l), LinkMode::Both(_) | LinkMode::Xmm(_))
                | (LinkMode::Xmm(l), LinkMode::Both(_)) => self.link_both(i, *l),
                (LinkMode::Both(l), LinkMode::Const(r)) if r.class() == FLOAT_CLASS => {
                    self.link_both(i, *l)
                }
                (LinkMode::Const(l), LinkMode::Both(_)) if l.class() == FLOAT_CLASS => {
                    let x = self.alloc_xmm();
                    self.link_both(i, x);
                }
                (LinkMode::Xmm(l), LinkMode::Xmm(_)) => self.link_xmm(i, *l),
                (LinkMode::Xmm(l), LinkMode::Const(r)) if r.class() == FLOAT_CLASS => {
                    self.link_xmm(i, *l)
                }
                (LinkMode::Const(l), LinkMode::Xmm(_)) if l.class() == FLOAT_CLASS => {
                    let x = self.alloc_xmm();
                    self.link_xmm(i, x);
                }
                (LinkMode::Const(l), LinkMode::Const(r)) if l == r => self.link_const(i, *l),
                _ => self.dealloc_xmm(i),
            };
        }
    }

    fn merge_entries(entries: &[BranchEntry]) -> Self {
        let mut merge_ctx = entries.last().unwrap().bbctx.clone();
        for BranchEntry {
            src_idx: _src_idx,
            bbctx,
            entry: _,
            ..
        } in entries.iter()
        {
            #[cfg(feature = "emit-tir")]
            eprintln!("  <-{:?}: {:?}", _src_idx, bbctx.slot_state);
            merge_ctx.merge(bbctx);
        }
        #[cfg(feature = "emit-tir")]
        eprintln!("  merged_entries: {:?}", &merge_ctx.slot_state);
        merge_ctx
    }

    fn get_write_back(&self) -> WriteBack {
        let xmm = self
            .xmm
            .0
            .iter()
            .enumerate()
            .filter_map(|(i, v)| {
                if v.is_empty() {
                    None
                } else {
                    let v: Vec<_> = self.xmm.0[i]
                        .iter()
                        .filter(|reg| matches!(self.slot_state[**reg], LinkMode::Xmm(_)))
                        .cloned()
                        .collect();
                    if v.is_empty() {
                        None
                    } else {
                        Some((Xmm::new(i as u16), v))
                    }
                }
            })
            .collect();
        let fixnum = self
            .slot_state
            .0
            .iter()
            .enumerate()
            .filter_map(|(idx, mode)| match mode {
                LinkMode::Const(v) => Some((*v, SlotId(idx as u16))),
                _ => None,
            })
            .collect();
        WriteBack::new(xmm, fixnum)
    }

    fn get_locals_write_back(&self) -> WriteBack {
        let local_num = self.local_num;
        let xmm = self
            .xmm
            .0
            .iter()
            .enumerate()
            .filter_map(|(i, v)| {
                if v.is_empty() {
                    None
                } else {
                    let v: Vec<_> = self.xmm.0[i]
                        .iter()
                        .filter(|reg| {
                            reg.0 as usize <= local_num
                                && matches!(self.slot_state[**reg], LinkMode::Xmm(_))
                        })
                        .cloned()
                        .collect();
                    if v.is_empty() {
                        None
                    } else {
                        Some((Xmm::new(i as u16), v))
                    }
                }
            })
            .collect();
        let fixnum = self
            .slot_state
            .0
            .iter()
            .enumerate()
            .filter_map(|(idx, mode)| match mode {
                LinkMode::Const(v) if idx <= local_num => Some((*v, SlotId(idx as u16))),
                _ => None,
            })
            .collect();
        WriteBack::new(xmm, fixnum)
    }

    fn get_xmm_using(&self) -> Vec<Xmm> {
        self.xmm
            .0
            .iter()
            .enumerate()
            .filter_map(|(i, v)| {
                if v.is_empty() {
                    None
                } else {
                    Some(Xmm::new(i as u16))
                }
            })
            .collect()
    }
}

#[derive(Debug)]
struct InlineCached {
    class_id: ClassId,
    version: u32,
}

impl InlineCached {
    fn new(pc: BcPc) -> Self {
        let (class_id, version) = (pc - 1).class_version();
        InlineCached { class_id, version }
    }
}

#[derive(Debug, Clone, PartialEq)]
#[repr(transparent)]
struct XmmInfo([Vec<SlotId>; 14]);

impl XmmInfo {
    fn new() -> Self {
        let v: Vec<Vec<SlotId>> = (0..14).map(|_| vec![]).collect();
        Self(v.try_into().unwrap())
    }
}

impl std::ops::Index<Xmm> for XmmInfo {
    type Output = Vec<SlotId>;
    fn index(&self, index: Xmm) -> &Self::Output {
        &self.0[index.0 as usize]
    }
}

impl std::ops::IndexMut<Xmm> for XmmInfo {
    fn index_mut(&mut self, index: Xmm) -> &mut Self::Output {
        &mut self.0[index.0 as usize]
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(transparent)]
struct Xmm(u16);

impl Xmm {
    fn new(id: u16) -> Self {
        Self(id)
    }

    fn enc(&self) -> u64 {
        self.0 as u64 + 2
    }
}

///
/// Mode of linkage between stack slot and xmm registers.
///
#[derive(Debug, Clone, Copy, PartialEq)]
enum LinkMode {
    ///
    /// Linked to an xmm register and we can read and write.
    ///
    /// mutation of the corresponding xmm register (lazily) affects the stack slot.
    ///
    Xmm(Xmm),
    ///
    /// Linked to an xmm register but we can only read.
    ///
    Both(Xmm),
    ///
    /// No linkage with any xmm regiter.
    ///
    Stack,
    ///
    /// Constant.
    ///
    Const(Value),
}

#[derive(Clone, PartialEq)]
struct StackSlotInfo(Vec<LinkMode>);

impl std::fmt::Debug for StackSlotInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s: String = self
            .0
            .iter()
            .enumerate()
            .flat_map(|(i, mode)| match mode {
                LinkMode::Stack => None,
                LinkMode::Const(v) => Some(format!("%{i}:Const({:?}) ", v)),
                LinkMode::Both(x) => Some(format!("%{i}:Both({x:?}) ")),
                LinkMode::Xmm(x) => Some(format!("%{i}:Xmm({x:?}) ")),
            })
            .collect();
        write!(f, "[{s}]")
    }
}

impl std::ops::Index<SlotId> for StackSlotInfo {
    type Output = LinkMode;
    fn index(&self, i: SlotId) -> &Self::Output {
        &self.0[i.0 as usize]
    }
}

impl std::ops::IndexMut<SlotId> for StackSlotInfo {
    fn index_mut(&mut self, i: SlotId) -> &mut Self::Output {
        &mut self.0[i.0 as usize]
    }
}

#[cfg(feature = "log-jit")]
extern "C" fn log_deoptimize(
    _vm: &mut Executor,
    globals: &mut Globals,
    func_id: FuncId,
    pc: BcPc,
    v: Option<Value>,
) {
    let name = globals[func_id].as_ruby_func().name();
    let bc_begin = globals[func_id].as_ruby_func().get_top_pc();
    let index = pc - bc_begin;
    let fmt = pc.format(globals, index).unwrap_or_default();
    if let TraceIr::LoopEnd = pc.get_ir() {
        eprint!("<-- exited from JIT code in {} {:?}.", name, func_id);
        eprintln!("    [{:05}] {fmt}", index);
    } else if let TraceIr::ClassDef { .. } = pc.get_ir() {
        eprint!("<-- deopt occurs in {} {:?}.", name, func_id);
        eprintln!("    [{:05}] {fmt}", index);
    } else {
        match globals.deopt_stats.get_mut(&(func_id, index)) {
            Some(c) => *c = *c + 1,
            None => {
                globals.deopt_stats.insert((func_id, index), 1);
            }
        };
        if let Some(v) = v {
            eprint!("<-- deopt occurs in {} {:?}.", name, func_id);
            eprintln!("    [{:05}] {fmt} caused by {}", index, globals.tos(v));
        } else {
            eprint!("<-- non-optimized branch in {} {:?}.", name, func_id);
            eprintln!("    [{:05}] {fmt}", index);
        }
    }
}

impl Codegen {
    pub(super) fn compile(
        &mut self,
        fnstore: &Store,
        func_id: FuncId,
        self_value: Value,
        position: Option<BcPc>,
    ) -> (DestLabel, Vec<(BcIndex, usize)>) {
        let func = fnstore[func_id].as_ruby_func();
        let start_pos = func.get_pc_index(position);

        #[cfg(any(feature = "emit-asm", feature = "log-jit"))]
        let now = std::time::Instant::now();

        let entry = self.jit.label();
        self.jit.bind_label(entry);

        let mut ctx = JitContext::new(func, self, start_pos, position.is_some(), self_value);
        for (loop_start, loop_end) in &func.bb_info.loops {
            let (backedge, exit) = ctx.analyse_loop(func, *loop_start, *loop_end);
            ctx.loop_backedges.insert(*loop_start, backedge);
            ctx.loop_exit.insert(*loop_start, (*loop_end, exit));
        }

        if position.is_none() {
            // generate prologue and class guard of *self* for a method
            let pc = func.get_top_pc();
            self.prologue(pc);
            let side_exit = self.gen_side_deopt_without_writeback(pc + 1);
            monoasm!( &mut self.jit,
                movq rdi, [r14 - (LBP_SELF)];
            );
            self.guard_class(self_value.class(), side_exit);
        }

        ctx.branch_map.insert(
            start_pos,
            vec![BranchEntry {
                src_idx: BcIndex(0),
                bbctx: BBContext::new(&ctx),
                entry: self.jit.label(),
                cont: true,
            }],
        );
        for i in func.bb_info.get_bb_pos(start_pos) {
            ctx.cur_pos = i;
            if self.compile_bb(fnstore, func, &mut ctx, position) {
                break;
            };
        }

        self.gen_backedge_branches(&mut ctx, func);

        self.jit.finalize();

        #[cfg(any(feature = "emit-asm", feature = "log-jit"))]
        {
            let elapsed = now.elapsed();
            eprintln!("<== finished compile. elapsed:{:?}", elapsed);
        }
        #[cfg(feature = "emit-tir")]
        eprintln!("<== finished compile.");
        (entry, ctx.sourcemap)
    }
}

macro_rules! load_store {
    ($reg: ident) => {
        paste! {
            ///
            /// store $reg to *reg*
            ///
            #[allow(dead_code)]
            fn [<store_ $reg>](&mut self, reg: SlotId) {
                monoasm!( &mut self.jit,
                    movq [r14 - (conv(reg))], $reg;
                );
            }

            ///
            /// load *reg* to $reg
            ///
            #[allow(dead_code)]
            fn [<load_ $reg>](&mut self, reg: SlotId) {
                monoasm!( &mut self.jit,
                    movq $reg, [r14 - (conv(reg))];
                );
            }
        }
    };
}

impl Codegen {
    load_store!(rax);
    load_store!(rdi);
    load_store!(rsi);
    load_store!(rcx);

    ///
    /// move xmm(*src*) to xmm(*dst*).
    ///
    fn xmm_mov(&mut self, src: Xmm, dst: Xmm) {
        if src != dst {
            monoasm!( &mut self.jit,
                movq  xmm(dst.enc()), xmm(src.enc());
            );
        }
    }

    ///
    /// Confirm a value in the slot is Float.
    ///
    /// side-exit if not Float.
    ///
    /// ### registers destroyed
    ///
    /// - rdi
    ///
    fn slot_guard_float(&mut self, reg: SlotId, side_exit: DestLabel) {
        self.load_rdi(reg);
        self.guard_float(side_exit);
    }

    ///
    /// Copy *src* to *dst*.
    ///
    fn copy_slot(&mut self, ctx: &mut BBContext, src: SlotId, dst: SlotId) {
        match ctx.slot_state[src] {
            LinkMode::Xmm(freg) | LinkMode::Both(freg) => {
                ctx.link_xmm(dst, freg);
            }
            LinkMode::Stack => {
                ctx.dealloc_xmm(dst);
                self.load_rax(src);
                self.store_rax(dst);
            }
            LinkMode::Const(v) => {
                ctx.link_const(dst, v);
            }
        }
    }

    ///
    /// Write back a corresponding xmm register to the stack slot *reg*.
    ///
    /// the xmm will be deallocated.
    ///
    fn write_back_slot(&mut self, ctx: &mut BBContext, reg: SlotId) {
        match ctx.slot_state[reg] {
            LinkMode::Xmm(freg) => {
                let f64_to_val = self.f64_to_val;
                monoasm!( &mut self.jit,
                    movq xmm0, xmm(freg.enc());
                    call f64_to_val;
                );
                self.store_rax(reg);
                ctx.slot_state[reg] = LinkMode::Both(freg);
            }
            LinkMode::Const(v) => {
                self.write_back_val(reg, v);
                ctx.slot_state[reg] = LinkMode::Stack;
            }
            LinkMode::Both(_) | LinkMode::Stack => {}
        }
    }

    fn load_slot_to_rax(&mut self, ctx: &mut BBContext, reg: SlotId) {
        match ctx.slot_state[reg] {
            LinkMode::Xmm(freg) => {
                let f64_to_val = self.f64_to_val;
                monoasm!( &mut self.jit,
                    movq xmm0, xmm(freg.enc());
                    call f64_to_val;
                );
                ctx.slot_state[reg] = LinkMode::Both(freg);
            }
            LinkMode::Const(v) => {
                monoasm!(&mut self.jit,
                    movq rax, (v.get());
                );
                ctx.slot_state[reg] = LinkMode::Stack;
            }
            LinkMode::Both(_) | LinkMode::Stack => {
                monoasm!(&mut self.jit,
                    movq rax, [r14 - (conv(reg))];
                );
            }
        }
    }

    fn write_back_range(&mut self, ctx: &mut BBContext, args: SlotId, len: u16) {
        for reg in args.0..args.0 + len {
            self.write_back_slot(ctx, SlotId::new(reg))
        }
    }

    fn write_back_args(
        &mut self,
        ctx: &mut BBContext,
        args: SlotId,
        len: u16,
        callsite: &CallSiteInfo,
    ) {
        let pos_kw_len = len as usize + callsite.kw_args.len();
        self.write_back_range(ctx, args, pos_kw_len as u16);
        callsite
            .hash_splat_pos
            .iter()
            .for_each(|r| self.write_back_slot(ctx, *r));
    }

    fn compile_bb(
        &mut self,
        fnstore: &Store,
        func: &ISeqInfo,
        cc: &mut JitContext,
        position: Option<BcPc>,
    ) -> bool {
        self.jit.bind_label(cc.labels[&cc.cur_pos]);
        let mut ctx = if let Some(ctx) = cc.target_ctx.remove(&cc.cur_pos) {
            ctx
        } else {
            if let Some(ctx) = self.gen_merging_branches(func, cc) {
                ctx
            } else {
                return false;
            }
        };
        for (ofs, pc) in func.bytecode()[cc.cur_pos.to_usize()..].iter().enumerate() {
            let pc = BcPc::from(pc);
            #[cfg(feature = "emit-asm")]
            cc.sourcemap
                .push((cc.cur_pos + ofs, self.jit.get_current() - cc.start_codepos));
            match pc.get_ir() {
                TraceIr::InitMethod { .. } => {}
                TraceIr::LoopStart(_) => {
                    cc.loop_count += 1;
                }
                TraceIr::LoopEnd => {
                    assert_ne!(0, cc.loop_count);
                    cc.loop_count -= 1;
                    if cc.is_loop && cc.loop_count == 0 {
                        #[cfg(any(feature = "emit-asm", feature = "log-jit"))]
                        eprintln!("<-- compile finished. end:[{:05}]", cc.cur_pos + ofs);
                        self.go_deopt(&ctx, pc);
                        break;
                    }
                }
                TraceIr::Integer(ret, i) => {
                    ctx.link_const(ret, Value::int32(i));
                }
                TraceIr::Symbol(ret, id) => {
                    ctx.link_const(ret, Value::new_symbol(id));
                }
                TraceIr::Literal(ret, val) => {
                    ctx.dealloc_xmm(ret);
                    if val.class() == FLOAT_CLASS {
                        ctx.link_const(ret, val);
                        //let freg = ctx.alloc_xmm();
                        //ctx.link_both(ret, freg);
                        //let imm = self.jit.const_f64(f);
                        //monoasm!( &mut self.jit,
                        //    movq xmm(freg.enc()), [rip + imm];
                        //    movq rax, (Value::new_float(f).get());
                        //);
                        //self.store_rax(ret);
                    } else {
                        if val.is_packed_value() {
                            monoasm!( &mut self.jit,
                                movq rax, (val.get());
                            );
                        } else {
                            let xmm_using = ctx.get_xmm_using();
                            self.xmm_save(&xmm_using);
                            monoasm!( &mut self.jit,
                              movq rdi, (val.get());
                              movq rax, (Value::value_deep_copy);
                              call rax;
                            );
                            self.xmm_restore(&xmm_using);
                        }
                        self.store_rax(ret);
                    }
                }
                TraceIr::Array { ret, args, len } => {
                    self.write_back_range(&mut ctx, args, len);
                    ctx.dealloc_xmm(ret);
                    monoasm!( &mut self.jit,
                        lea  rdi, [r14 - (conv(args))];
                        movq rsi, (len);
                        movq rax, (runtime::gen_array);
                        call rax;
                    );
                    self.store_rax(ret);
                }
                TraceIr::Hash { ret, args, len } => {
                    self.write_back_range(&mut ctx, args, len * 2);
                    ctx.dealloc_xmm(ret);
                    monoasm!( &mut self.jit,
                        lea  rdi, [r14 - (conv(args))];
                        movq rsi, (len);
                        movq rax, (runtime::gen_hash);
                        call rax;
                    );
                    self.store_rax(ret);
                }
                TraceIr::Range {
                    ret,
                    start,
                    end,
                    exclude_end,
                } => {
                    self.write_back_slot(&mut ctx, start);
                    self.write_back_slot(&mut ctx, end);
                    let xmm_using = ctx.get_xmm_using();
                    self.xmm_save(&xmm_using);
                    self.load_rdi(start);
                    self.load_rsi(end);
                    monoasm! { &mut self.jit,
                        movq rdx, rbx; // &mut Executor
                        movq rcx, r12; // &mut Globals
                        movl r8, (if exclude_end {1} else {0});
                        movq rax, (runtime::gen_range);
                        call rax;
                    };
                    self.xmm_restore(&xmm_using);
                    self.jit_handle_error(&ctx, pc);
                    self.store_rax(ret);
                }
                TraceIr::Index { ret, base, idx } => {
                    self.write_back_slot(&mut ctx, base);
                    self.write_back_slot(&mut ctx, idx);
                    ctx.dealloc_xmm(ret);
                    self.jit_get_index(&ctx, ret, base, idx, pc);
                }
                TraceIr::IndexAssign { src, base, idx } => {
                    self.write_back_slot(&mut ctx, base);
                    self.write_back_slot(&mut ctx, idx);
                    self.write_back_slot(&mut ctx, src);
                    self.jit_index_assign(&ctx, src, base, idx, pc);
                }
                TraceIr::LoadConst(dst, id) => {
                    ctx.dealloc_xmm(dst);
                    if pc.value().map(|v| v.class()) == Some(FLOAT_CLASS) {
                        let fdst = ctx.alloc_xmm();
                        ctx.link_both(dst, fdst);
                        self.load_float_constant(&ctx, dst, fdst, id, pc);
                    } else {
                        self.load_generic_constant(&ctx, dst, id, pc);
                    }
                }
                TraceIr::StoreConst(src, id) => {
                    self.write_back_slot(&mut ctx, src);
                    self.jit_store_constant(&ctx, id, src);
                }
                TraceIr::BlockArgProxy(dst, outer) => {
                    ctx.dealloc_xmm(dst);
                    let panic = self.entry_panic;
                    if outer == 0 {
                        monoasm! { &mut self.jit,
                            movq rax, r14;
                        };
                    } else {
                        monoasm!( &mut self.jit,
                            movq rax, [r14 - (LBP_OUTER)];
                        );
                        for _ in 0..outer - 1 {
                            monoasm!( &mut self.jit,
                                movq rax, [rax];
                            );
                        }
                        monoasm!( &mut self.jit,
                            lea rax, [rax + (LBP_OUTER)];
                        );
                    }
                    monoasm! { &mut self.jit,
                        movq rax, [rax - (LBP_BLOCK)];
                        testq rax, 0b1;
                        jeq panic;
                        addq rax, 0b10;
                    };
                    self.store_rax(dst);
                }
                TraceIr::LoadIvar(ret, id, cached_class, cached_ivarid) => {
                    ctx.dealloc_xmm(ret);
                    self.jit_load_ivar(&ctx, id, ret, cached_class, cached_ivarid);
                }
                TraceIr::StoreIvar(src, id, cached_class, cached_ivarid) => {
                    self.write_back_slot(&mut ctx, src);
                    self.jit_store_ivar(&ctx, id, src, pc, cached_class, cached_ivarid);
                }
                TraceIr::LoadGvar { dst: ret, name } => {
                    ctx.dealloc_xmm(ret);
                    let xmm_using = ctx.get_xmm_using();
                    self.xmm_save(&xmm_using);
                    monoasm! { &mut self.jit,
                        movq rdi, r12;
                        movl rsi, (name.get());
                        movq rax, (runtime::get_global_var);
                        call rax;
                    };
                    self.store_rax(ret);
                    self.xmm_restore(&xmm_using);
                }
                TraceIr::StoreGvar { src: val, name } => {
                    self.write_back_slot(&mut ctx, val);
                    let xmm_using = ctx.get_xmm_using();
                    self.xmm_save(&xmm_using);
                    monoasm! { &mut self.jit,
                        movq rdi, r12;
                        movl rsi, (name.get());
                        movq rdx, [r14 - (conv(val))];
                        movq rax, (runtime::set_global_var);
                        call rax;
                    };
                    self.xmm_restore(&xmm_using);
                }
                TraceIr::LoadSvar { dst: ret, id } => {
                    ctx.dealloc_xmm(ret);
                    let xmm_using = ctx.get_xmm_using();
                    self.xmm_save(&xmm_using);
                    monoasm! { &mut self.jit,
                        movq rdi, rbx;
                        movl rsi, r12;
                        movl rdx, (id);
                        movq rax, (runtime::get_special_var);
                        call rax;
                    };
                    self.store_rax(ret);
                    self.xmm_restore(&xmm_using);
                }
                TraceIr::LoadDynVar(ret, src) => {
                    ctx.dealloc_xmm(ret);
                    monoasm!( &mut self.jit,
                        movq rax, [r14 - (LBP_OUTER)];
                    );
                    for _ in 0..src.outer - 1 {
                        monoasm!( &mut self.jit,
                            movq rax, [rax];
                        );
                    }
                    let offset = conv(src.reg) - LBP_OUTER;
                    monoasm!( &mut self.jit,
                        movq rax, [rax - (offset)];
                    );
                    if ret.0 != 0 {
                        self.store_rax(ret);
                    }
                }
                TraceIr::StoreDynVar(dst, src) => {
                    self.write_back_slot(&mut ctx, src);
                    monoasm!( &mut self.jit,
                        movq rax, [r14 - (LBP_OUTER)];
                    );
                    for _ in 0..dst.outer - 1 {
                        monoasm!( &mut self.jit,
                            movq rax, [rax];
                        );
                    }
                    let offset = conv(dst.reg) - LBP_OUTER;
                    self.load_rdi(src);
                    monoasm!( &mut self.jit,
                        movq [rax - (offset)], rdi;
                    );
                }
                TraceIr::Nil(ret) => {
                    ctx.dealloc_xmm(ret);
                    monoasm!( &mut self.jit,
                        movq [r14 - (conv(ret))], (NIL_VALUE);
                    );
                }
                TraceIr::BitNot { ret, src } => {
                    self.write_back_slot(&mut ctx, src);
                    ctx.dealloc_xmm(ret);
                    if pc.classid1().0 == 0 {
                        self.recompile_and_deopt(&mut ctx, position, pc);
                        return false;
                    } else {
                        let xmm_using = ctx.get_xmm_using();
                        self.xmm_save(&xmm_using);
                        self.load_rdi(src);
                        self.call_unop(bitnot_value as _);
                        self.xmm_restore(&xmm_using);
                        self.jit_handle_error(&ctx, pc);
                        self.store_rax(ret);
                    }
                }
                TraceIr::Not { ret, src } => {
                    self.write_back_slot(&mut ctx, src);
                    ctx.dealloc_xmm(ret);
                    monoasm!( &mut self.jit,
                        movq rdi, [r14 - (conv(src))];
                    );
                    self.not_rdi_to_rax();
                    monoasm!( &mut self.jit,
                        movq [r14 - (conv(ret))], rax;
                    );
                }
                TraceIr::Neg { ret, src } => {
                    if pc.is_float1() {
                        let fsrc = self.xmm_read_assume_float(&mut ctx, src, pc);
                        let fdst = ctx.xmm_write(ret);
                        let imm = self.jit.const_i64(0x8000_0000_0000_0000u64 as i64);
                        self.xmm_mov(fsrc, fdst);
                        monoasm!( &mut self.jit,
                            xorps xmm(fdst.enc()), [rip + imm];
                        );
                    } else {
                        self.write_back_slot(&mut ctx, src);
                        ctx.dealloc_xmm(ret);
                        if pc.classid1().0 == 0 {
                            self.recompile_and_deopt(&mut ctx, position, pc);
                            return false;
                        } else {
                            let xmm_using = ctx.get_xmm_using();
                            self.xmm_save(&xmm_using);
                            self.load_rdi(src);
                            self.call_unop(neg_value as _);
                            self.xmm_restore(&xmm_using);
                            self.jit_handle_error(&ctx, pc);
                            self.store_rax(ret);
                        }
                    }
                }
                TraceIr::Pos { ret, src } => {
                    if pc.is_float1() {
                        let fsrc = self.xmm_read_assume_float(&mut ctx, src, pc);
                        let fdst = ctx.xmm_write(ret);
                        self.xmm_mov(fsrc, fdst);
                    } else {
                        self.write_back_slot(&mut ctx, src);
                        ctx.dealloc_xmm(ret);
                        if pc.classid1().0 == 0 {
                            self.recompile_and_deopt(&mut ctx, position, pc);
                            return false;
                        } else {
                            let xmm_using = ctx.get_xmm_using();
                            self.xmm_save(&xmm_using);
                            self.load_rdi(src);
                            self.call_unop(pos_value as _);
                            self.xmm_restore(&xmm_using);
                            self.jit_handle_error(&ctx, pc);
                            self.store_rax(ret);
                        }
                    }
                }
                TraceIr::IBinOp {
                    kind, ret, mode, ..
                } => {
                    self.writeback_binary(&mut ctx, &mode);
                    ctx.dealloc_xmm(ret);
                    self.gen_binop_integer(pc, kind, ret, mode, &ctx);
                }
                TraceIr::FBinOp {
                    kind, ret, mode, ..
                } => {
                    match mode {
                        OpMode::RR(lhs, rhs) => {
                            let (flhs, frhs) = self.xmm_read_binary(&mut ctx, lhs, rhs, pc);
                            let fret = ctx.xmm_write(ret);
                            self.gen_binop_float_rr(kind, &ctx, fret, flhs, frhs);
                        }
                        OpMode::RI(lhs, rhs) => {
                            let flhs = self.xmm_read_assume_float(&mut ctx, lhs, pc);
                            let fret = ctx.xmm_write(ret);
                            self.gen_binop_float_ri(kind, &ctx, fret, flhs, rhs);
                        }
                        OpMode::IR(lhs, rhs) => {
                            let frhs = self.xmm_read_assume_float(&mut ctx, rhs, pc);
                            let fret = ctx.xmm_write(ret);
                            self.gen_binop_float_ir(kind, &ctx, fret, lhs, frhs);
                        }
                    };
                }
                TraceIr::BinOp {
                    kind, ret, mode, ..
                } => {
                    self.writeback_binary(&mut ctx, &mode);
                    ctx.dealloc_xmm(ret);
                    if pc.classid1().0 == 0 || pc.classid2().0 == 0 {
                        self.recompile_and_deopt(&mut ctx, position, pc);
                        return false;
                    } else {
                        self.load_binary_args_with_mode(&mode);
                        self.gen_generic_binop(&ctx, pc, kind, ret);
                    }
                }
                TraceIr::Cmp(kind, ret, mode, false) => {
                    if mode.is_float_op(&pc) {
                        match mode {
                            OpMode::RR(lhs, rhs) => {
                                let (flhs, frhs) = self.xmm_read_binary(&mut ctx, lhs, rhs, pc);
                                ctx.dealloc_xmm(ret);
                                monoasm! { &mut self.jit,
                                    xorq rax, rax;
                                    ucomisd xmm(flhs.enc()), xmm(frhs.enc());
                                };
                            }
                            OpMode::RI(lhs, rhs) => {
                                let rhs_label = self.jit.const_f64(rhs as f64);
                                let flhs = self.xmm_read_assume_float(&mut ctx, lhs, pc);
                                ctx.dealloc_xmm(ret);
                                monoasm! { &mut self.jit,
                                    xorq rax, rax;
                                    ucomisd xmm(flhs.enc()), [rip + rhs_label];
                                };
                            }
                            _ => unreachable!(),
                        }
                        self.setflag_float(kind);
                        self.store_rax(ret);
                    } else if mode.is_integer_op(&pc) {
                        self.writeback_binary(&mut ctx, &mode);
                        ctx.dealloc_xmm(ret);
                        let deopt = self.gen_side_deopt(pc, &ctx);
                        self.load_and_guard_binary_fixnum_with_mode(deopt, &mode);
                        self.integer_cmp(kind);
                        self.jit_handle_error(&ctx, pc);
                        self.store_rax(ret);
                    } else {
                        self.writeback_binary(&mut ctx, &mode);
                        ctx.dealloc_xmm(ret);
                        if pc.classid1().0 == 0 || pc.classid2().0 == 0 {
                            self.recompile_and_deopt(&mut ctx, position, pc);
                            return false;
                        } else {
                            self.load_binary_args_with_mode(&mode);
                            self.generic_cmp(kind, &ctx);
                            self.jit_handle_error(&ctx, pc);
                            self.store_rax(ret);
                        }
                    }
                }

                TraceIr::Cmp(kind, ret, mode, true) => {
                    let index = cc.cur_pos + ofs + 1;
                    self.gen_cmp_opt(&mut ctx, cc, mode, kind, ret, pc, index);
                }
                TraceIr::Mov(dst, src) => {
                    self.copy_slot(&mut ctx, src, dst);
                }
                TraceIr::ConcatStr(ret, arg, len) => {
                    self.write_back_range(&mut ctx, arg, len);
                    ctx.dealloc_xmm(ret);
                    let xmm_using = ctx.get_xmm_using();
                    self.xmm_save(&xmm_using);
                    monoasm!( &mut self.jit,
                        movq rdi, r12;
                        lea rsi, [r14 - (conv(arg))];
                        movq rdx, (len);
                        movq rax, (runtime::concatenate_string);
                        call rax;
                    );
                    self.xmm_restore(&xmm_using);
                    if ret.0 != 0 {
                        self.store_rax(ret);
                    }
                }
                TraceIr::ExpandArray(src, dst, len) => {
                    self.write_back_slot(&mut ctx, src);
                    for reg in dst.0..dst.0 + len {
                        ctx.dealloc_xmm(SlotId(reg));
                    }
                    let xmm_using = ctx.get_xmm_using();
                    self.xmm_save(&xmm_using);
                    self.load_rdi(src);
                    monoasm!( &mut self.jit,
                        lea rsi, [r14 - (conv(dst))];
                        movq rdx, (len);
                        movq rax, (runtime::expand_array);
                        call rax;
                    );
                    self.xmm_restore(&xmm_using);
                }
                TraceIr::AliasMethod { new, old } => {
                    self.write_back_slot(&mut ctx, new);
                    self.write_back_slot(&mut ctx, old);
                    let xmm_using = ctx.get_xmm_using();
                    self.xmm_save(&xmm_using);
                    monoasm!( &mut self.jit,
                        movq rdi, rbx;
                        movq rsi, r12;
                        movq rdx, [r14 - (LBP_SELF)];
                        movq rcx, [r14 - (conv(new))];
                        movq r8, [r14 - (conv(old))];
                        movq r9, [r14 - (LBP_META)];
                        movq rax, (runtime::alias_method);
                        call rax;
                    );
                    self.xmm_restore(&xmm_using);
                    self.jit_handle_error(&ctx, pc);
                }
                TraceIr::MethodCall {
                    ret,
                    callid,
                    has_splat,
                    info,
                    ..
                } => {
                    let MethodInfo {
                        args, len, recv, ..
                    } = info;
                    self.write_back_slot(&mut ctx, recv);
                    self.write_back_args(&mut ctx, args, len, &fnstore[callid]);
                    ctx.dealloc_xmm(ret);
                    // We must write back and unlink all local vars if this method is eval.
                    //self.gen_write_back_locals(&mut ctx);
                    if info.func_data.is_none() {
                        self.recompile_and_deopt(&mut ctx, position, pc);
                        return false;
                    } else {
                        self.gen_call(
                            fnstore,
                            &mut ctx,
                            info,
                            callid,
                            None,
                            ret,
                            pc + 1,
                            has_splat,
                        );
                    }
                }
                TraceIr::MethodCallBlock {
                    ret,
                    callid,
                    has_splat,
                    mut info,
                    ..
                } => {
                    let MethodInfo {
                        args, len, recv, ..
                    } = info;
                    self.write_back_slot(&mut ctx, recv);
                    self.write_back_args(&mut ctx, args, len + 1, &fnstore[callid]);
                    ctx.dealloc_xmm(ret);
                    // We must write back and unlink all local vars since they may be accessed from block.
                    self.gen_write_back_locals(&mut ctx);
                    if info.func_data.is_none() {
                        self.recompile_and_deopt(&mut ctx, position, pc);
                        return false;
                    } else {
                        info.args = args + 1;
                        self.gen_call(
                            fnstore,
                            &mut ctx,
                            info,
                            callid,
                            Some(args),
                            ret,
                            pc + 1,
                            has_splat,
                        );
                    }
                }
                TraceIr::Super {
                    ret, callid, info, ..
                } => {
                    let MethodInfo {
                        args, len, recv, ..
                    } = info;
                    self.write_back_slot(&mut ctx, recv);
                    self.write_back_range(&mut ctx, args, len);
                    ctx.dealloc_xmm(ret);
                    // We must write back and unlink all local vars since they may be accessed by eval.
                    self.gen_write_back_locals(&mut ctx);
                    if info.func_data.is_none() {
                        self.recompile_and_deopt(&mut ctx, position, pc);
                        return false;
                    } else {
                        self.gen_call(fnstore, &mut ctx, info, callid, None, ret, pc + 1, false);
                    }
                }
                TraceIr::InlineCall {
                    ret, method, info, ..
                } => {
                    self.write_back_slot(&mut ctx, info.recv);
                    self.gen_inlinable(&mut ctx, &info, &method, ret, pc);
                }
                TraceIr::Yield {
                    ret,
                    args,
                    len,
                    callid,
                } => {
                    ctx.dealloc_xmm(ret);
                    self.write_back_range(&mut ctx, args, len);
                    self.gen_yield(&ctx, fnstore, args, len, ret, callid, pc);
                }
                TraceIr::MethodArgs(_) => {}
                TraceIr::MethodDef { name, func_id } => {
                    let xmm_using = ctx.get_xmm_using();
                    self.xmm_save(&xmm_using);
                    monoasm!( &mut self.jit,
                        movq rdi, rbx; // &mut Interp
                        movq rsi, r12; // &Globals
                        movq rdx, (u32::from(name)); // IdentId
                        movq rcx, (u32::from(func_id)); // FuncId
                        movq rax, (runtime::define_method);
                        call rax;
                    );
                    self.xmm_restore(&xmm_using);
                }
                TraceIr::SingletonMethodDef { obj, name, func_id } => {
                    let xmm_using = ctx.get_xmm_using();
                    self.xmm_save(&xmm_using);
                    monoasm!( &mut self.jit,
                        movq rdi, rbx; // &mut Interp
                        movq rsi, r12; // &Globals
                        movq rdx, (u32::from(name)); // IdentId
                        movq rcx, (u32::from(func_id)); // FuncId
                        movq r8, [r14 - (conv(obj))];
                        movq rax, (runtime::singleton_define_method);
                        call rax;
                    );
                    self.xmm_restore(&xmm_using);
                }
                TraceIr::ClassDef {
                    ret,
                    superclass,
                    name,
                    func_id,
                } => {
                    self.jit_class_def(&ctx, ret, superclass, name, func_id, false, pc);
                }
                TraceIr::ModuleDef { ret, name, func_id } => {
                    self.jit_class_def(&ctx, ret, SlotId::new(0), name, func_id, true, pc);
                }
                TraceIr::SingletonClassDef { ret, base, func_id } => {
                    self.jit_singleton_class_def(&ctx, ret, base, func_id, pc);
                }
                TraceIr::DefinedYield { ret } => {
                    ctx.dealloc_xmm(ret);
                    monoasm! { &mut self.jit,
                        movq rdi, rbx;  // &mut Interp
                        movq rsi, r12;  // &mut Globals
                        lea  rdx, [r14 - (conv(ret))];
                        movq rax, (runtime::defined_yield);
                        call rax;
                    };
                }
                TraceIr::DefinedConst { ret, siteid } => {
                    ctx.dealloc_xmm(ret);
                    monoasm! { &mut self.jit,
                        movq rdi, rbx;  // &mut Interp
                        movq rsi, r12;  // &mut Globals
                        lea  rdx, [r14 - (conv(ret))];
                        movl rcx, (siteid.0);
                        movq rax, (runtime::defined_const);
                        call rax;
                    };
                }
                TraceIr::DefinedMethod { ret, recv, name } => {
                    ctx.dealloc_xmm(ret);
                    self.write_back_slot(&mut ctx, recv);
                    monoasm! { &mut self.jit,
                        movq rdi, rbx;  // &mut Interp
                        movq rsi, r12;  // &mut Globals
                        lea  rdx, [r14 - (conv(ret))];
                        movq rcx, [r14 - (conv(recv))];
                        movl r8, (name.get());
                        movq rax, (runtime::defined_method);
                        call rax;
                    };
                }
                TraceIr::DefinedGvar { ret, name } => {
                    ctx.dealloc_xmm(ret);
                    monoasm! { &mut self.jit,
                        movq rdi, rbx;  // &mut Interp
                        movq rsi, r12;  // &mut Globals
                        lea  rdx, [r14 - (conv(ret))];
                        movl rcx, (name.get());
                        movq rax, (runtime::defined_gvar);
                        call rax;
                    };
                }
                TraceIr::DefinedIvar { ret, name } => {
                    ctx.dealloc_xmm(ret);
                    monoasm! { &mut self.jit,
                        movq rdi, rbx;  // &mut Interp
                        movq rsi, r12;  // &mut Globals
                        lea  rdx, [r14 - (conv(ret))];
                        movl rcx, (name.get());
                        movq rax, (runtime::defined_ivar);
                        call rax;
                    };
                }
                TraceIr::Ret(lhs) => {
                    self.gen_write_back_locals(&mut ctx);
                    self.load_slot_to_rax(&mut ctx, lhs);
                    self.epilogue();
                    return false;
                }
                TraceIr::MethodRet(lhs) => {
                    self.gen_write_back_locals(&mut ctx);
                    self.load_slot_to_rax(&mut ctx, lhs);
                    monoasm! { &mut self.jit,
                        movq r13, ((pc + 1).get_u64());
                    };
                    self.method_return();
                    return false;
                }
                TraceIr::Break(lhs) => {
                    self.gen_write_back_locals(&mut ctx);
                    self.load_slot_to_rax(&mut ctx, lhs);
                    self.block_break();
                    self.epilogue();
                    return false;
                }
                TraceIr::EnsureEnd => {
                    self.gen_write_back_locals(&mut ctx);
                    let raise = self.entry_raise;
                    monoasm! { &mut self.jit,
                        movq rdi, rbx;
                        movq rax, (runtime::check_err);
                        call rax;
                        testq rax, rax;
                        jne  raise;
                    };
                }
                TraceIr::Br(disp) => {
                    let next_idx = cc.cur_pos + ofs + 1;
                    let dest_idx = next_idx + disp;
                    let branch_dest = self.jit.label();
                    cc.new_branch(cc.cur_pos + ofs, dest_idx, ctx, branch_dest);
                    monoasm!( &mut self.jit,
                        jmp branch_dest;
                    );
                    return false;
                }
                TraceIr::CondBr(cond_, disp, false, kind) => {
                    self.write_back_slot(&mut ctx, cond_);
                    let dest_idx = cc.cur_pos + ofs + 1 + disp;
                    let branch_dest = self.jit.label();
                    cc.new_branch(cc.cur_pos + ofs, dest_idx, ctx.clone(), branch_dest);
                    self.load_rax(cond_);
                    monoasm!( &mut self.jit,
                        orq rax, 0x10;
                        cmpq rax, (FALSE_VALUE);
                    );
                    match kind {
                        BrKind::BrIf => monoasm!( &mut self.jit, jne branch_dest;),
                        BrKind::BrIfNot => monoasm!( &mut self.jit, jeq branch_dest;),
                    }
                }
                TraceIr::CondBr(_, _, true, _) => {}
                TraceIr::CheckLocal(local, disp) => {
                    let dest_idx = cc.cur_pos + ofs + 1 + disp;
                    let branch_dest = self.jit.label();
                    cc.new_branch(cc.cur_pos + ofs, dest_idx, ctx.clone(), branch_dest);
                    self.load_rax(local);
                    monoasm!( &mut self.jit,
                        testq rax, rax;
                        jnz  branch_dest;
                    );
                }
            }

            let next_idx = cc.cur_pos + ofs + 1;
            if func.bb_info.is_bb_head(next_idx) {
                let branch_dest = self.jit.label();
                cc.new_continue(cc.cur_pos + ofs, next_idx, ctx, branch_dest);
                cc.cur_pos = next_idx;
                let target_ctx = if let Some(ctx) = self.gen_merging_branches(func, cc) {
                    ctx
                } else {
                    return false;
                };
                assert!(cc.target_ctx.insert(next_idx, target_ctx).is_none());
                return false;
            }
        }
        true
    }

    fn recompile_and_deopt(&mut self, ctx: &mut BBContext, position: Option<BcPc>, pc: BcPc) {
        if ctx.recompile_flag {
            return;
        } else {
            ctx.recompile_flag = true;
        }
        let recompile = self.jit.label();
        let dec = self.jit.label();
        let counter = self.jit.const_i32(5);
        let deopt = self.gen_side_deopt(pc, ctx);
        monoasm!( &mut self.jit,
            cmpl [rip + counter], 0;
            jlt deopt;
            jeq recompile;
        dec:
            subl [rip + counter], 1;
            xorq rdi, rdi;
            jmp deopt;
        );
        self.jit.select_page(1);
        monoasm!( &mut self.jit,
        recompile:
            movq rdi, r12;
            movl rsi, [r14 - (LBP_META_FUNCID)];
            movq rdx, [r14 - (LBP_SELF)];
        );
        if let Some(index) = position {
            monoasm!( &mut self.jit,
                movq rcx, (index.get_u64());
                movq rax, (exec_jit_partial_compile);
                call rax;
            );
        } else {
            monoasm!( &mut self.jit,
                movq rax, (exec_jit_recompile);
                call rax;
            );
        }
        monoasm!( &mut self.jit,
            jmp dec;
        );
        self.jit.select_page(0);
    }
}

impl Codegen {
    fn jit_handle_error(&mut self, ctx: &BBContext, pc: BcPc) {
        let raise = self.entry_raise;
        let wb = ctx.get_write_back();
        if self.jit.get_page() == 0 {
            let error = self.jit.label();
            monoasm!( &mut self.jit,
                testq rax, rax; // Option<Value>
                jeq  error;
            );
            self.jit.select_page(1);
            monoasm!( &mut self.jit,
            error:
            );
            self.gen_write_back(wb);
            monoasm!( &mut self.jit,
                movq r13, ((pc + 1).get_u64());
                jmp  raise;
            );
            self.jit.select_page(0);
        } else {
            let cont = self.jit.label();
            monoasm!( &mut self.jit,
                testq rax, rax; // Option<Value>
                jne  cont;
            );
            self.gen_write_back(wb);
            monoasm!( &mut self.jit,
                movq r13, ((pc + 1).get_u64());
                jmp  raise;
            cont:
            );
        }
    }

    ///
    /// Generate a code which write back all xmm registers to corresponding stack slots.
    ///
    /// xmms are not deallocated.
    ///
    fn gen_write_back(&mut self, wb: WriteBack) {
        for (freg, v) in wb.xmm {
            self.gen_write_back_single(freg, v);
        }
        for (v, slot) in wb.fixnum {
            self.write_back_val(slot, v);
        }
    }

    fn gen_write_back_single(&mut self, freg: Xmm, v: Vec<SlotId>) {
        if v.is_empty() {
            return;
        }
        #[cfg(feature = "emit-tir")]
        eprintln!("      wb: {:?}->{:?}", freg, v);
        let f64_to_val = self.f64_to_val;
        monoasm!( &mut self.jit,
            movq xmm0, xmm(freg.enc());
            call f64_to_val;
        );
        for reg in v {
            self.store_rax(reg);
        }
    }

    fn gen_write_back_locals(&mut self, ctx: &mut BBContext) {
        let wb = ctx.get_locals_write_back();
        self.gen_write_back(wb);
        ctx.dealloc_locals();
    }

    ///
    /// Get *DestLabel* for write-back and fallback to interpreter.
    ///
    fn gen_side_deopt(&mut self, pc: BcPc, ctx: &BBContext) -> DestLabel {
        let entry = self.jit.label();
        self.gen_side_deopt_with_label(pc, Some(ctx), entry);
        entry
    }

    ///
    /// Get *DestLabel* for fallback to interpreter. (without write-back)
    ///
    fn gen_side_deopt_without_writeback(&mut self, pc: BcPc) -> DestLabel {
        let entry = self.jit.label();
        self.gen_side_deopt_with_label(pc, None, entry);
        entry
    }

    fn gen_side_deopt_with_label(&mut self, pc: BcPc, ctx: Option<&BBContext>, entry: DestLabel) {
        assert_eq!(0, self.jit.get_page());
        self.jit.select_page(1);
        self.jit.bind_label(entry);
        if let Some(ctx) = ctx {
            let wb = ctx.get_write_back();
            if !wb.is_empty() {
                #[cfg(feature = "emit-tir")]
                eprintln!("--gen deopt");
                self.gen_write_back(wb);
                #[cfg(feature = "emit-tir")]
                eprintln!("--gen deopt end");
            }
        }
        let fetch = self.vm_fetch;
        monoasm!( &mut self.jit,
            movq r13, (pc.get_u64());
        );
        #[cfg(feature = "log-jit")]
        monoasm!( &mut self.jit,
            movq r8, rdi; // the Value which caused this deopt.
            movq rdi, rbx;
            movq rsi, r12;
            movq rdx, [r14 - (LBP_META)];
            movq rcx, r13;
            movq rax, (log_deoptimize);
            call rax;
        );
        monoasm!( &mut self.jit,
            jmp fetch;
        );
        self.jit.select_page(0);
    }

    ///
    /// Fallback to interpreter after Writing back all linked xmms.
    ///
    fn go_deopt(&mut self, ctx: &BBContext, pc: BcPc) {
        let fallback = self.gen_side_deopt(pc, ctx);
        monoasm!( &mut self.jit,
            jmp fallback;
        );
    }

    fn load_binary_args(&mut self, lhs: SlotId, rhs: SlotId) {
        self.load_rdi(lhs);
        self.load_rsi(rhs);
    }

    fn write_back_val(&mut self, reg: SlotId, v: Value) {
        let i = v.get() as i64;
        #[cfg(feature = "emit-tir")]
        eprintln!("      wb: Const({:?})->{:?}", v, reg);
        if i32::try_from(i).is_ok() {
            monoasm! { &mut self.jit,
                movq [r14 - (conv(reg))], (v.get());
            }
        } else {
            monoasm! { &mut self.jit,
                movq rax, (v.get());
                movq [r14 - (conv(reg))], rax;
            }
        }
    }

    fn xmm_save(&mut self, xmm_using: &[Xmm]) {
        let len = xmm_using.len();
        if len == 0 {
            return;
        }
        let sp_offset = (len + len % 2) * 8;
        monoasm!( &mut self.jit,
            subq rsp, (sp_offset);
        );
        for (i, freg) in xmm_using.iter().enumerate() {
            monoasm!( &mut self.jit,
                movq [rsp + (8 * i)], xmm(freg.enc());
            );
        }
    }

    fn xmm_restore(&mut self, xmm_using: &[Xmm]) {
        let len = xmm_using.len();
        if len == 0 {
            return;
        }
        let sp_offset = (len + len % 2) * 8;
        for (i, freg) in xmm_using.iter().enumerate() {
            monoasm!( &mut self.jit,
                movq xmm(freg.enc()), [rsp + (8 * i)];
            );
        }
        monoasm!( &mut self.jit,
            addq rsp, (sp_offset);
        );
    }
}
