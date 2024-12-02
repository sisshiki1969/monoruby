use monoasm_macro::monoasm;
use paste::paste;
use ruruby_parse::CmpKind;

use crate::bytecodegen::{BcIndex, UnOpK};

pub(crate) use self::basic_block::{BasciBlockInfoEntry, BasicBlockId, BasicBlockInfo};
use self::builtins::object_send_splat;
use self::slot::Guarded;

use super::*;
//use analysis::{ExitType, SlotInfo};
use asmir::*;
use slot::{Liveness, SlotContext};
use trace_ir::*;

//pub mod analysis;
pub mod asmir;
mod basic_block;
mod binary_op;
mod compile;
mod definition;
mod guard;
mod index;
mod init_method;
mod merge;
mod method_call;
mod read_slot;
mod slot;
pub mod trace_ir;
mod variables;

//
// Just-in-time compiler module.
//

///
/// Context for JIT compilation.
///
struct JitContext {
    ///
    /// Destination labels for each BasicBlock.
    ///
    basic_block_labels: HashMap<BasicBlockId, JitLabel>,
    ///
    /// Loop information.
    ///
    /// ### key
    /// the entry basic block of the loop.
    ///
    /// ### value
    /// (the last basic block, liveness info)
    ///
    loop_info: HashMap<BasicBlockId, Liveness>,
    ///
    /// Nested loop count.
    ///
    loop_count: usize,
    ///
    /// Flag whether this context is a loop.
    ///
    is_loop: bool,
    ///
    /// Map for bytecode position and branches.
    ///
    branch_map: HashMap<BasicBlockId, Vec<BranchEntry>>,
    ///
    /// Target `BBContext` for an each instruction.
    ///
    target_ctx: HashMap<BasicBlockId, BBContext>,
    ///
    /// Map for backward branches.
    ///
    backedge_map: HashMap<BasicBlockId, BackedgeInfo>,
    ///
    /// Number of slots.
    ///
    total_reg_num: usize,
    ///
    /// Number of local variables.
    ///
    local_num: usize,
    ///
    /// *self* for this loop/method.
    ///
    self_value: Value,
    ///
    /// Source map.
    ///
    sourcemap: Vec<(BcIndex, usize)>,
    ///
    /// Information for bridges.
    ///
    bridges: Vec<(AsmIr, JitLabel, BasicBlockId)>,
    ///
    /// Information for continuation bridge.
    ///
    continuation_bridge: Option<(Option<ContinuationInfo>, JitLabel)>,
    labels: Vec<Option<DestLabel>>,
    ///
    /// Class version at compile time.
    ///
    #[allow(dead_code)]
    class_version: u32,
    ///
    /// BOP redefinition flag at compile time.
    ///
    #[allow(dead_code)]
    bop_redefine_flags: u32,
    ///
    /// Start offset of a machine code corresponding to the current basic block.
    ///
    #[cfg(feature = "emit-asm")]
    start_codepos: usize,
}

impl JitContext {
    ///
    /// Create new JitContext.
    ///
    fn new(func: &ISeqInfo, codegen: &mut Codegen, is_loop: bool, self_value: Value) -> Self {
        let mut basic_block_labels = HashMap::default();
        let mut labels = vec![];
        for i in 0..func.bb_info.len() {
            let idx = BasicBlockId(i);
            basic_block_labels.insert(idx, JitLabel(labels.len()));
            labels.push(Some(codegen.jit.label()));
        }

        let total_reg_num = func.total_reg_num();
        let local_num = func.local_num();
        Self {
            basic_block_labels,
            loop_info: HashMap::default(),
            loop_count: 0,
            is_loop,
            branch_map: HashMap::default(),
            target_ctx: HashMap::default(),
            backedge_map: HashMap::default(),
            total_reg_num,
            local_num,
            self_value,
            sourcemap: vec![],
            bridges: vec![],
            continuation_bridge: None,
            labels,
            class_version: codegen.class_version(),
            bop_redefine_flags: codegen.bop_redefine_flags(),
            #[cfg(feature = "emit-asm")]
            start_codepos: codegen.jit.get_current(),
        }
    }

    fn from(&self) -> Self {
        let total_reg_num = self.total_reg_num;
        let local_num = self.local_num;
        Self {
            basic_block_labels: HashMap::default(),
            loop_info: HashMap::default(),
            loop_count: 0,
            is_loop: true,
            branch_map: HashMap::default(),
            target_ctx: HashMap::default(),
            backedge_map: HashMap::default(),
            total_reg_num,
            local_num,
            self_value: Value::nil(),
            sourcemap: vec![],
            bridges: vec![],
            continuation_bridge: None,
            labels: vec![],
            class_version: 0,
            bop_redefine_flags: 0,
            #[cfg(feature = "emit-asm")]
            start_codepos: 0,
        }
    }

    ///
    /// Create a new *JitLabel*.
    ///
    fn label(&mut self) -> JitLabel {
        let id = self.labels.len();
        self.labels.push(None);
        JitLabel(id)
    }

    ///
    /// Resolve *JitLabel* and return *DestLabel*.
    ///
    fn resolve_label(&mut self, jit: &mut JitMemory, label: JitLabel) -> DestLabel {
        match self.labels[label.0] {
            Some(l) => l,
            None => {
                let l = jit.label();
                self.labels[label.0] = Some(l);
                l
            }
        }
    }

    fn loop_info(&self, entry_bb: BasicBlockId) -> (Vec<(SlotId, bool)>, Vec<SlotId>) {
        match self.loop_info.get(&entry_bb) {
            Some(liveness) => (liveness.get_loop_used_as_float(), liveness.get_unused()),
            None => (vec![], vec![]),
        }
    }

    ///
    /// Add new branch from *src_idx* to *dest* with the context *bbctx*.
    ///
    fn new_branch(
        &mut self,
        func: &ISeqInfo,
        src_idx: BcIndex,
        dest: BasicBlockId,
        mut bbctx: BBContext,
        branch_dest: JitLabel,
    ) {
        bbctx.sp = func.get_sp(src_idx);
        #[cfg(feature = "jit-debug")]
        eprintln!("   new_branch: [{:?}]{src_idx}->{:?}", bbctx.sp, dest);
        self.branch_map.entry(dest).or_default().push(BranchEntry {
            src_idx,
            bbctx,
            branch_dest,
            cont: false,
        });
    }

    ///
    /// Add new continuation branch from *src_idx* to *dest* with the context *bbctx*.
    ///
    fn new_continue(
        &mut self,
        func: &ISeqInfo,
        src_idx: BcIndex,
        dest: BasicBlockId,
        mut bbctx: BBContext,
        branch_dest: JitLabel,
    ) {
        bbctx.sp = func.get_sp(src_idx);
        #[cfg(feature = "jit-debug")]
        eprintln!("   new_continue:[{:?}] {src_idx}->{:?}", bbctx.sp, dest);
        self.branch_map.entry(dest).or_default().push(BranchEntry {
            src_idx,
            bbctx,
            branch_dest,
            cont: true,
        })
    }

    ///
    /// Add new backward branch from *src_idx* to *dest* with the context *bbctx*.
    ///
    fn new_backedge(
        &mut self,
        func: &ISeqInfo,
        bb: &mut BBContext,
        bb_pos: BasicBlockId,
        unused: Vec<SlotId>,
    ) {
        bb.sp = func.get_sp(func.bb_info[bb_pos].begin);
        #[cfg(feature = "jit-debug")]
        eprintln!("   new_backedge:[{:?}] {:?}", bb.sp, bb_pos);
        self.backedge_map.insert(
            bb_pos,
            BackedgeInfo {
                target_ctx: MergeContext::new(bb),
                unused,
            },
        );
    }
}

#[derive(Debug)]
struct BackedgeInfo {
    target_ctx: MergeContext,
    unused: Vec<SlotId>,
}

struct ContinuationInfo {
    from: BBContext,
    to: MergeContext,
    pc: BytecodePtr,
}

impl ContinuationInfo {
    fn new(from: BBContext, to: MergeContext, pc: BytecodePtr) -> Self {
        Self { from, to, pc }
    }
}

///
/// Compile result of the current instruction.
///
enum CompileResult {
    /// continue to the next instruction.
    Continue,
    /// exit from the loop.
    ExitLoop,
    /// jump to another basic block.
    Branch,
    /// leave the current method/block.
    Leave,
    /// deoptimize and fallback to the interpreter.
    Deopt,
}

#[derive(Debug, Clone, Copy)]
struct JitLabel(usize);

///
/// The information for branches.
///
#[derive(Debug)]
struct BranchEntry {
    /// source instruction index of the branch.
    src_idx: BcIndex,
    /// context of the source basic block.
    bbctx: BBContext,
    /// `DestLabel` for the destination basic block.
    branch_dest: JitLabel,
    /// true if the branch is a continuation branch.
    /// 'continuation' means the destination is adjacent to the source basic block on the bytecode.
    cont: bool,
}

pub(crate) fn conv(reg: SlotId) -> i32 {
    reg.0 as i32 * 8 + LFP_SELF
}

///
/// Context of an each basic block.
///
#[derive(Debug, Clone)]
pub(crate) struct BBContext {
    /// state stack slots.
    slot_state: SlotContext,
    /// stack top register.
    sp: SlotId,
    next_sp: SlotId,
    /// *self* value
    self_value: Value,
}

impl std::ops::Deref for BBContext {
    type Target = SlotContext;
    fn deref(&self) -> &Self::Target {
        &self.slot_state
    }
}

impl std::ops::DerefMut for BBContext {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.slot_state
    }
}

impl BBContext {
    fn new(cc: &JitContext) -> Self {
        Self {
            slot_state: SlotContext::from(cc),
            sp: SlotId(cc.local_num as u16),
            next_sp: SlotId(cc.local_num as u16),
            self_value: cc.self_value,
        }
    }

    fn union(entries: &[BranchEntry]) -> MergeContext {
        let mut merge_ctx = MergeContext::new(&entries.last().unwrap().bbctx);
        for BranchEntry {
            src_idx: _src_idx,
            bbctx,
            ..
        } in entries.iter()
        {
            #[cfg(feature = "jit-debug")]
            eprintln!("  <-{:?}:[{:?}] {:?}", _src_idx, bbctx.sp, bbctx.slot_state);
            merge_ctx.merge(bbctx);
        }
        #[cfg(feature = "jit-debug")]
        eprintln!("  union_entries: {:?}", &merge_ctx);
        merge_ctx
    }

    pub(crate) fn get_using_xmm(&self) -> UsingXmm {
        self.slot_state.get_using_xmm(self.sp)
    }

    pub(crate) fn get_write_back(&self) -> WriteBack {
        self.slot_state.get_write_back(self.sp)
    }

    pub(crate) fn rax2acc(&mut self, ir: &mut AsmIr, dst: impl Into<Option<SlotId>>) {
        self.reg2acc(ir, GP::Rax, dst);
    }

    pub(crate) fn reg2acc(&mut self, ir: &mut AsmIr, src: GP, dst: impl Into<Option<SlotId>>) {
        self.reg2acc_guarded(ir, src, dst, slot::Guarded::Value)
    }

    pub(crate) fn reg2acc_fixnum(
        &mut self,
        ir: &mut AsmIr,
        src: GP,
        dst: impl Into<Option<SlotId>>,
    ) {
        self.reg2acc_guarded(ir, src, dst, slot::Guarded::Fixnum)
    }

    pub(crate) fn reg2acc_array_ty(
        &mut self,
        ir: &mut AsmIr,
        src: GP,
        dst: impl Into<Option<SlotId>>,
    ) {
        self.reg2acc_guarded(ir, src, dst, slot::Guarded::ArrayTy)
    }

    pub(crate) fn reg2acc_concrete_value(
        &mut self,
        ir: &mut AsmIr,
        src: GP,
        dst: impl Into<Option<SlotId>>,
        v: Value,
    ) {
        self.reg2acc_guarded(ir, src, dst, Guarded::from_concrete_value(v))
    }

    fn reg2acc_guarded(
        &mut self,
        ir: &mut AsmIr,
        src: GP,
        dst: impl Into<Option<SlotId>>,
        guarded: slot::Guarded,
    ) {
        if let Some(dst) = dst.into() {
            self.clear(ir);
            if let Some(acc) = self.clear_r15(ir)
                && acc < self.sp
                && acc != dst
            {
                ir.acc2stack(acc);
            }
            self.store_r15(ir, dst, guarded);
            ir.push(AsmInst::RegToAcc(src));
        }
    }

    pub(crate) fn writeback_acc(&mut self, ir: &mut AsmIr) {
        if let Some(slot) = self.clear_r15(ir)
            && slot < self.sp
        {
            ir.acc2stack(slot);
        }
    }
}

///
/// The strust holds information for writing back Value's in xmm registers or accumulator to the corresponding stack slots.
///
/// Currently supports `literal`s, `xmm` registers and a `R15` register (as an accumulator).
///
#[derive(Clone)]
pub(crate) struct WriteBack {
    xmm: Vec<(Xmm, Vec<SlotId>)>,
    literal: Vec<(Value, SlotId)>,
    alias: Vec<(SlotId, Vec<SlotId>)>,
    r15: Option<SlotId>,
}

impl std::fmt::Debug for WriteBack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();
        for (xmm, slots) in &self.xmm {
            s.push_str(&format!(" {:?}->", xmm));
            for slot in slots {
                s.push_str(&format!("{:?}", slot));
            }
        }
        for (val, slot) in &self.literal {
            s.push_str(&format!(" {:?}->{:?}", val, slot));
        }
        for (slot, slots) in &self.alias {
            s.push_str(&format!(" {:?}->", slot));
            for slot in slots {
                s.push_str(&format!("{:?}", slot));
            }
        }
        if let Some(slot) = self.r15 {
            s.push_str(&format!(" R15->{:?}", slot));
        }
        write!(f, "WriteBack({})", s)
    }
}

impl WriteBack {
    fn new(
        xmm: Vec<(Xmm, Vec<SlotId>)>,
        literal: Vec<(Value, SlotId)>,
        alias: Vec<(SlotId, Vec<SlotId>)>,
        r15: Option<SlotId>,
    ) -> Self {
        Self {
            xmm,
            literal,
            alias,
            r15,
        }
    }
}

#[derive(Clone, Copy)]
pub(crate) struct UsingXmm {
    inner: bitvec::prelude::BitArr!(for 14, in u16),
}

impl std::fmt::Debug for UsingXmm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();
        for i in 0..14 {
            if self.inner[i] {
                s.push_str(&format!("%{i}"));
            }
        }
        write!(f, "UsingXmm({})", s)
    }
}

impl std::ops::Deref for UsingXmm {
    type Target = bitvec::prelude::BitArr!(for 14, in u16);
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl std::ops::DerefMut for UsingXmm {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl UsingXmm {
    fn new() -> Self {
        Self {
            inner: bitvec::prelude::BitArray::new([0; 1]),
        }
    }
}

///
/// Mode of linkage between stack slot and xmm registers.
///
#[derive(Debug, Clone, Copy, PartialEq, Default)]
enum LinkMode {
    ///
    /// No linkage with xmm regiter.
    ///
    #[default]
    Stack,
    ///
    /// Linked to an xmm register and we can read and write.
    ///
    /// mutation of the corresponding xmm register (lazily) affects the stack slot.
    ///
    Xmm(Xmm),
    ///
    /// Linked to an xmm register but we can only read to keep consistency.
    ///
    Both(Xmm),
    ///
    /// Alias of *SlotId*.
    ///
    Alias(SlotId),
    ///
    /// Concrete value..
    ///
    ConcreteValue(Value),
    ///
    /// On accumulator (r15).
    ///
    Accumulator,
}

#[derive(Debug, Clone)]
struct MergeContext(BBContext);

impl std::ops::Deref for MergeContext {
    type Target = BBContext;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for MergeContext {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl MergeContext {
    fn new(bb: &BBContext) -> Self {
        MergeContext(bb.clone())
    }

    fn get(self) -> BBContext {
        self.0
    }

    fn remove_unused(&mut self, unused: &[SlotId]) {
        let mut ir = AsmIr::new();
        unused.iter().for_each(|reg| self.unlink(&mut ir, *reg));
    }
}

impl Codegen {
    pub(super) fn jit_compile(
        &mut self,
        store: &Store,
        func_id: FuncId,
        self_value: Value,
        position: Option<BytecodePtr>,
        entry_label: DestLabel,
    ) -> Vec<(BcIndex, usize)> {
        #[cfg(feature = "jit-log")]
        let now = std::time::Instant::now();

        self.jit.bind_label(entry_label);

        let func = store[func_id].as_ruby_func();
        let start_pos = func.get_pc_index(position);

        let mut ctx = JitContext::new(func, self, position.is_some(), self_value);
        for (loop_start, loop_end) in func.bb_info.loops() {
            ctx.analyse_loop(store, func, *loop_start, *loop_end);
        }

        let bbctx = BBContext::new(&ctx);

        if let Some(pc) = position {
            // generate class guard of *self* for loop JIT
            // We must pass pc + 1 because pc (= LoopStart) cause an infinite loop.
            let side_exit = self.gen_deopt(pc + 1, &bbctx);
            monoasm!( &mut self.jit,
                movq rdi, [r14 - (LFP_SELF)];
            );
            self.guard_class_rdi(self_value.class(), side_exit);
        } else {
            // for method JIT, class of *self* is already checked in an entry stub.
            self.prologue(func, store);
        }

        #[cfg(feature = "jit-debug")]
        eprintln!("   new_branch_init: {}->{}", BcIndex(0), start_pos);
        let bb_begin = func.bb_info.get_bb_id(start_pos);
        let branch_dest = ctx.label();
        ctx.branch_map.insert(
            bb_begin,
            vec![BranchEntry {
                src_idx: BcIndex(0),
                bbctx,
                branch_dest,
                cont: true,
            }],
        );

        let bb_end = match func.bb_info.get_loop(bb_begin) {
            Some((a, b)) => {
                assert_eq!(a, bb_begin);
                b
            }
            None => BasicBlockId(func.bb_info.len() - 1),
        };
        let bbir: Vec<_> = (bb_begin..=bb_end)
            .map(|bbid| {
                let ir = ctx.compile_basic_block(store, func, position, bbid);
                (bbid, ir)
            })
            .collect();

        ctx.backedge_branches(func);

        #[cfg(feature = "emit-cfg")]
        Self::dump_cfg(func, store, bb_begin, bb_end);

        // generate machine code for a main context
        for (_bbid, ir) in bbir.into_iter() {
            #[cfg(feature = "emit-asm")]
            eprintln!("{:?}", _bbid);
            self.gen_asm(ir, store, &mut ctx, None, None);
        }

        // generate machine code for bridges
        for (ir, entry, exit) in std::mem::take(&mut ctx.bridges) {
            let entry = ctx.resolve_label(&mut self.jit, entry);
            self.gen_asm(ir, store, &mut ctx, Some(entry), Some(exit));
        }
        assert!(ctx.continuation_bridge.is_none());

        self.jit.finalize();

        #[cfg(any(feature = "jit-debug", feature = "jit-log"))]
        {
            self.jit.select_page(0);
            eprintln!("    total bytes(0):{:?}", self.jit.get_current());
            self.jit.select_page(1);
            eprintln!("    total bytes(1):{:?}", self.jit.get_current());
            self.jit.select_page(0);
        }
        #[cfg(feature = "jit-log")]
        {
            let elapsed = now.elapsed();
            eprintln!("<== finished compile. elapsed:{:?}", elapsed);
            self.jit_compile_time += elapsed;
        }
        #[cfg(feature = "emit-asm")]
        eprintln!("<== finished compile.");

        ctx.sourcemap
    }

    #[cfg(feature = "emit-cfg")]
    fn dump_cfg(func: &ISeqInfo, store: &Store, bb_begin: BasicBlockId, bb_end: BasicBlockId) {
        let mut s = format!(
            r###"digraph graph_name {{
  graph [
    charset = "UTF-8";
    label = "{}",
    labelloc = "t",
    labeljust = "c",
    bgcolor = "#343434",
    fontcolor = white,
    fontsize = 20,
    rankdir = TB,
    margin = 0.2,
    splines = spline,
    nodesep = 0.8,
    ranksep = 1.1
  ];

  node [
    colorscheme = "accent8"
    shape = box,
    style = "solid,filled",
    fontsize = 16,
    fontcolor = 5,
    fontname = "Consolas",
    color = 5,
    fillcolor = 4,
  ];

  edge [
    style = solid,
    fontsize = 14,
    fontcolor = white,
    fontname = "Migu 1M",
    color = white,
    labelfloat = true,
    labeldistance = 2.5,
    labelangle = 70
  ];"###,
            func.name()
        );
        s += "\n";
        for bbid in bb_begin..=bb_end {
            s += &format!("  {:?} [\n    shape=record\n    label=\"{{{:?}", bbid, bbid);
            let BasciBlockInfoEntry { begin, end, .. } = func.bb_info[bbid];
            for bc in begin..=end {
                if let Some(inst) = func.trace_ir(store, bc).format(store) {
                    s += "|";
                    let html = html_escape::encode_text(&inst)
                        .replace('|', "\\|")
                        .replace('"', "\\\"");
                    s += &format!("{} {}\\l", bc, html);
                }
            }
            s += "}\"\n  ];\n";
        }

        for bbid in bb_begin..=bb_end {
            let entry = &func.bb_info[bbid];
            for succ in &entry.succ {
                s += &format!("  {:?} -> {:?} [headport = n, tailport = s];\n", bbid, succ);
            }
        }

        s += "}\n";
        std::fs::write(format!("func_id-{}.dot", func.id().get()), s).unwrap();
    }
}

macro_rules! load_store {
    ($reg: ident) => {
        paste! {
            ///
            /// store $reg to *reg*
            ///
            #[allow(dead_code)]
            pub(crate) fn [<store_ $reg>](&mut self, reg: impl Into<Option<SlotId>>) {
                let reg = reg.into();
                if let Some(reg) = reg {
                    monoasm!{ &mut self.jit,
                        movq [r14 - (conv(reg))], $reg;
                    }
                }
            }

            ///
            /// load *reg* to $reg
            ///
            #[allow(dead_code)]
            pub(crate) fn [<load_ $reg>](&mut self, reg: SlotId) {
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
    load_store!(r15);

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
    /// swap xmm(*l*) and xmm(*r*).
    ///
    fn xmm_swap(&mut self, l: Xmm, r: Xmm) {
        monoasm!( &mut self.jit,
            movq  xmm0, xmm(l.enc());
            movq  xmm(l.enc()), xmm(r.enc());
            movq  xmm(r.enc()), xmm0;
        );
    }

    pub(crate) fn xmm_save(&mut self, using_xmm: UsingXmm) {
        if using_xmm.not_any() {
            return;
        }
        let len = using_xmm.count_ones();
        let sp_offset = (len + len % 2) * 8;
        monoasm!( &mut self.jit,
            subq rsp, (sp_offset);
        );
        let mut i = 0;
        for (x, b) in using_xmm.iter().enumerate() {
            if *b {
                monoasm!( &mut self.jit,
                    movq [rsp + (8 * i)], xmm(Xmm::new(x as _).enc());
                );
                i += 1;
            }
        }
    }

    pub(crate) fn xmm_restore(&mut self, using_xmm: UsingXmm) {
        if using_xmm.not_any() {
            return;
        }
        let len = using_xmm.count_ones();
        let sp_offset = (len + len % 2) * 8;
        let mut i = 0;
        for (x, b) in using_xmm.iter().enumerate() {
            if *b {
                monoasm!( &mut self.jit,
                    movq xmm(Xmm::new(x as _).enc()), [rsp + (8 * i)];
                );
                i += 1;
            }
        }
        monoasm!( &mut self.jit,
            addq rsp, (sp_offset);
        );
    }

    fn recompile_and_deopt(&mut self, position: Option<BytecodePtr>, deopt: DestLabel) {
        let recompile = self.jit.label();
        let dec = self.jit.label();
        let counter = self.jit.data_i32(COUNT_DEOPT_RECOMPILE);

        monoasm!( &mut self.jit,
            xorq rdi, rdi;
            cmpl [rip + counter], 0;
            jlt deopt;
            jeq recompile;
        dec:
            subl [rip + counter], 1;
            jmp deopt;
        );

        self.jit.select_page(1);
        monoasm!( &mut self.jit,
        recompile:
            movq rdi, rbx;
            movq rsi, r12;
        );
        if let Some(pc) = position {
            monoasm!( &mut self.jit,
                movq rdx, (pc.as_ptr());
                movq rax, (exec_jit_partial_compile);
                call rax;
            );
        } else {
            monoasm!( &mut self.jit,
                movq rax, (exec_jit_recompile_method);
                call rax;
            );
        }
        monoasm!( &mut self.jit,
            xorq rdi, rdi;
            jmp dec;
        );
        self.jit.select_page(0);
        #[cfg(feature = "jit-debug")]
        eprintln!(" => deopt");
    }
}

impl Codegen {
    fn gen_handle_error(&mut self, pc: BytecodePtr, wb: WriteBack, entry: DestLabel) {
        let raise = self.entry_raise;
        self.jit.select_page(1);
        monoasm!( &mut self.jit,
        entry:
        );
        self.gen_write_back(&wb);
        monoasm!( &mut self.jit,
            movq r13, ((pc + 1).as_ptr());
            jmp  raise;
        );
        self.jit.select_page(0);
    }

    ///
    /// Generate a code which write back all xmm registers to corresponding stack slots.
    ///
    /// xmms are not deallocated.
    ///
    pub(super) fn gen_write_back(&mut self, wb: &WriteBack) {
        for (xmm, v) in &wb.xmm {
            self.xmm_to_stack(*xmm, v);
        }
        for (v, slot) in &wb.literal {
            self.literal_to_stack(*slot, *v);
        }
        if let Some(slot) = wb.r15 {
            self.store_r15(slot);
        }
        for (origin, v) in &wb.alias {
            if !v.is_empty() {
                self.load_rax(*origin);
                for reg in v {
                    self.store_rax(*reg);
                }
            }
        }
    }

    ///
    /// Generate convert code from xmm to stack slots.
    ///
    /// ### out
    /// - rax: Value
    ///
    /// ### destroy
    /// - rcx
    ///
    fn xmm_to_stack(&mut self, xmm: Xmm, v: &[SlotId]) {
        if v.is_empty() {
            return;
        }
        #[cfg(feature = "jit-debug")]
        eprintln!("      wb: {:?}->{:?}", xmm, v);
        let f64_to_val = self.f64_to_val;
        monoasm!( &mut self.jit,
            movq xmm0, xmm(xmm.enc());
            call f64_to_val;
        );
        for reg in v {
            self.store_rax(*reg);
        }
    }

    ///
    /// ### destroy
    /// - rax
    ///
    fn literal_to_stack(&mut self, reg: SlotId, v: Value) {
        let i = v.id() as i64;
        if i32::try_from(i).is_ok() {
            monoasm! { &mut self.jit,
                movq [r14 - (conv(reg))], (v.id());
            }
        } else {
            monoasm! { &mut self.jit,
                movq rax, (v.id());
                movq [r14 - (conv(reg))], rax;
            }
        }
    }

    ///
    /// Get *DestLabel* for write-back and fallback to interpreter.
    ///
    /// ### in
    /// - rdi: deopt-reason:Value
    ///
    pub(crate) fn gen_deopt(&mut self, pc: BytecodePtr, bb: &BBContext) -> DestLabel {
        let entry = self.jit.label();
        let wb = bb.get_write_back();
        self.gen_deopt_with_label(pc, &wb, entry);
        entry
    }

    ///
    /// Get *DestLabel* for fallback to interpreter by deoptimization.
    ///
    /// ### in
    /// - rdi: deopt-reason:Value
    ///
    fn gen_deopt_with_label(&mut self, pc: BytecodePtr, wb: &WriteBack, entry: DestLabel) {
        self.side_exit_with_label(pc, wb, entry, false)
    }

    ///
    /// Get *DestLabel* for fallback to interpreter by immediate eviction.
    ///
    fn gen_evict_with_label(&mut self, pc: BytecodePtr, wb: &WriteBack, entry: DestLabel) {
        self.side_exit_with_label(pc, wb, entry, true)
    }

    ///
    /// Get *DestLabel* for fallback to interpreter.
    ///
    /// ### in
    /// - rdi: deopt-reason:Value
    ///
    fn side_exit_with_label(
        &mut self,
        pc: BytecodePtr,
        wb: &WriteBack,
        entry: DestLabel,
        _is_evict: bool,
    ) {
        assert_eq!(0, self.jit.get_page());
        self.jit.select_page(1);
        self.jit.bind_label(entry);
        self.gen_write_back(wb);
        monoasm!( &mut self.jit,
            movq r13, (pc.as_ptr());
        );

        #[cfg(any(feature = "deopt", feature = "profile"))]
        {
            if _is_evict {
                monoasm!( &mut self.jit,
                    movq rcx, (Value::symbol_from_str("__immediate_evict").id());
                );
            } else {
                monoasm!( &mut self.jit,
                    movq rcx, rdi; // the Value which caused this deopt.
                );
            }
            monoasm!( &mut self.jit,
                movq rdi, rbx;
                movq rsi, r12;
                movq rdx, r13;
                movq rax, (crate::globals::log_deoptimize);
                call rax;
            );
        }
        let fetch = self.vm_fetch;
        monoasm!( &mut self.jit,
            jmp fetch;
        );
        self.jit.select_page(0);
    }
}

#[test]
fn float_test() {
    let gen = Codegen::new(false);

    let from_f64_entry = gen.jit.get_label_address(gen.f64_to_val);
    let from_f64: fn(f64) -> Value = unsafe { std::mem::transmute(from_f64_entry.as_ptr()) };

    for lhs in [
        0.0,
        4.2,
        35354354354.2135365,
        -3535354345111.5696876565435432,
        f64::MAX,
        f64::MAX / 10.0,
        f64::MIN * 10.0,
        f64::NAN,
    ] {
        let v = from_f64(lhs);
        let rhs = match v.unpack() {
            RV::Float(f) => f,
            _ => panic!(),
        };
        if lhs.is_nan() {
            assert!(rhs.is_nan());
        } else {
            assert_eq!(lhs, rhs);
        }
    }
}

#[test]
fn float_test2() {
    let mut gen = Codegen::new(false);

    let panic = gen.entry_panic;
    let assume_int_to_f64 = gen.jit.label();
    let x = Xmm(0);
    monoasm!(&mut gen.jit,
    assume_int_to_f64:
        pushq rbp;
    );
    gen.integer_val_to_f64(GP::Rdi, x, panic);
    monoasm!(&mut gen.jit,
        movq xmm0, xmm(x.enc());
        popq rbp;
        ret;
    );
    gen.jit.finalize();
    let int_to_f64_entry = gen.jit.get_label_address(assume_int_to_f64);

    let int_to_f64: fn(Value) -> f64 = unsafe { std::mem::transmute(int_to_f64_entry.as_ptr()) };
    assert_eq!(143.0, int_to_f64(Value::integer(143)));
    assert_eq!(14354813558.0, int_to_f64(Value::integer(14354813558)));
    assert_eq!(-143.0, int_to_f64(Value::integer(-143)));
}
