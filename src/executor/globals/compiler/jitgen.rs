use monoasm_macro::monoasm;

use super::*;

mod analysis;
mod binary_op;
mod compile;
mod constants;
mod guard;
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
    /// Destinatiol labels for jump instructions.
    labels: HashMap<usize, DestLabel>,
    /// Basic block information.
    /// (bb_id, Vec<src_idx>)
    bb_info: Vec<Option<(usize, Vec<usize>)>>,
    /// Current basic block id.
    bb_pos: usize,
    loop_count: usize,
    /// true for a loop, false for a method.
    is_loop: bool,
    branch_map: HashMap<usize, Vec<BranchEntry>>,
    backedge_map: HashMap<usize, (DestLabel, StackSlotInfo, Vec<SlotId>)>,
    start_codepos: usize,
    /// *self* for this loop/method.
    self_value: Value,
    /// source map.
    sourcemap: Vec<(usize, usize)>,
}

impl JitContext {
    ///
    /// Create new JitContext.
    ///
    fn new(
        func: &ISeqInfo,
        codegen: &mut Codegen,
        start_pos: usize,
        is_loop: bool,
        self_value: Value,
    ) -> Self {
        let bb_info = func.get_bb_info();
        let mut labels = HashMap::default();
        bb_info.into_iter().enumerate().for_each(|(idx, elem)| {
            if elem.is_some() {
                labels.insert(idx, codegen.jit.label());
            }
        });
        Self {
            labels,
            bb_info: func.get_bb_info(),
            bb_pos: start_pos,
            loop_count: 0,
            is_loop,
            branch_map: HashMap::default(),
            backedge_map: HashMap::default(),
            start_codepos: 0,
            self_value,
            sourcemap: vec![],
        }
    }

    fn new_branch(&mut self, src_idx: usize, dest: usize, bbctx: BBContext, dest_label: DestLabel) {
        self.branch_map.entry(dest).or_default().push(BranchEntry {
            src_idx,
            bbctx,
            dest_label,
        })
    }

    fn new_backedge(
        &mut self,
        bb_pos: usize,
        dest_label: DestLabel,
        slot_info: StackSlotInfo,
        unused: Vec<SlotId>,
    ) {
        self.backedge_map
            .insert(bb_pos, (dest_label, slot_info, unused));
    }

    fn get_backedge(&mut self, bb_pos: usize) -> (DestLabel, StackSlotInfo, Vec<SlotId>) {
        self.backedge_map.remove_entry(&bb_pos).unwrap().1
    }
}

#[derive(Debug)]
struct BranchEntry {
    src_idx: usize,
    bbctx: BBContext,
    dest_label: DestLabel,
}

fn conv(reg: SlotId) -> i64 {
    reg.0 as i64 * 8 + OFFSET_SELF
}

type WriteBack = Vec<(Xmm, Vec<SlotId>)>;

///
/// Context of the current Basic block.
///
#[derive(Debug, Clone, PartialEq)]
struct BBContext {
    /// Information for stack slots.
    stack_slot: StackSlotInfo,
    /// Information for xmm registers.
    xmm: XmmInfo,
    self_class: ClassId,
    self_kind: Option<u8>,
    local_num: usize,
}

impl BBContext {
    fn new(reg_num: usize, local_num: usize, self_value: Value) -> Self {
        let xmm = XmmInfo::new();
        Self {
            stack_slot: StackSlotInfo(vec![LinkMode::None; reg_num]),
            xmm,
            self_class: self_value.class_id(),
            self_kind: self_value.kind(),
            local_num,
        }
    }

    fn from(slot_info: &StackSlotInfo, local_num: usize, self_value: Value) -> Self {
        let mut ctx = Self::new(slot_info.0.len(), local_num, self_value);
        for (i, mode) in slot_info.0.iter().enumerate() {
            let reg = SlotId(i as u16);
            match mode {
                LinkMode::None => {}
                LinkMode::XmmR(x) => {
                    ctx.stack_slot[reg] = LinkMode::XmmR(*x);
                    ctx.xmm[*x].push(reg);
                }
                LinkMode::XmmRW(x) => {
                    ctx.stack_slot[reg] = LinkMode::XmmRW(*x);
                    ctx.xmm[*x].push(reg);
                }
            }
        }
        ctx
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
        unreachable!()
    }

    fn link_rw_xmm(&mut self, reg: SlotId, freg: Xmm) {
        self.stack_slot[reg] = LinkMode::XmmRW(freg);
        self.xmm[freg].push(reg);
    }

    fn link_r_xmm(&mut self, reg: SlotId, freg: Xmm) {
        self.stack_slot[reg] = LinkMode::XmmR(freg);
        self.xmm[freg].push(reg);
    }

    ///
    /// Deallocate an xmm register corresponding to the stack slot *reg*.
    ///
    fn dealloc_xmm(&mut self, reg: SlotId) {
        match self.stack_slot[reg] {
            LinkMode::XmmR(freg) | LinkMode::XmmRW(freg) => {
                assert!(self.xmm[freg].contains(&reg));
                self.xmm[freg].retain(|e| *e != reg);
                self.stack_slot[reg] = LinkMode::None;
            }
            LinkMode::None => {}
        }
    }

    fn dealloc_locals(&mut self) {
        for reg in 1..1 + self.local_num as u16 {
            self.dealloc_xmm(SlotId(reg));
        }
    }

    fn xmm_swap(&mut self, l: Xmm, r: Xmm) {
        self.xmm.0.swap(l.0 as usize, r.0 as usize);
        self.stack_slot.0.iter_mut().for_each(|mode| match mode {
            LinkMode::XmmR(x) | LinkMode::XmmRW(x) => {
                if *x == l {
                    *x = r;
                } else if *x == r {
                    *x = l;
                }
            }
            LinkMode::None => {}
        });
    }

    ///
    /// Allocate new xmm register to the given stack slot for read/write f64.
    ///
    fn xmm_write(&mut self, reg: SlotId) -> Xmm {
        match self.stack_slot[reg] {
            LinkMode::XmmRW(freg) if self.xmm[freg].len() == 1 => {
                assert_eq!(reg, self.xmm[freg][0]);
                freg
            }
            _ => {
                self.dealloc_xmm(reg);
                let freg = self.alloc_xmm();
                self.link_rw_xmm(reg, freg);
                freg
            }
        }
    }

    fn get_write_back(&self) -> WriteBack {
        self.xmm
            .0
            .iter()
            .enumerate()
            .filter_map(|(i, v)| {
                if v.is_empty() {
                    None
                } else {
                    let v: Vec<_> = self.xmm.0[i]
                        .iter()
                        .filter(|reg| matches!(self.stack_slot[**reg], LinkMode::XmmRW(_)))
                        .cloned()
                        .collect();
                    Some((Xmm::new(i as u16), v))
                }
            })
            .filter(|(_, v)| !v.is_empty())
            .collect()
    }

    fn get_locals_write_back(&self) -> WriteBack {
        let local_num = self.local_num;
        self.xmm
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
                                && matches!(self.stack_slot[**reg], LinkMode::XmmRW(_))
                        })
                        .cloned()
                        .collect();
                    Some((Xmm::new(i as u16), v))
                }
            })
            .filter(|(_, v)| !v.is_empty())
            .collect()
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
    codeptr: CodePtr,
    meta: Meta,
    class_id: ClassId,
    version: u32,
    pc: BcPc,
}

impl InlineCached {
    fn new(pc: BcPc, codeptr: CodePtr) -> Self {
        let (class_id, version) = (pc - 1).class_version();
        InlineCached {
            codeptr,
            meta: (pc + 1).meta(),
            class_id,
            version,
            pc: (pc + 1).pc(),
        }
    }

    fn func_id(&self) -> FuncId {
        self.meta.func_id()
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
    XmmRW(Xmm),
    ///
    /// Linked to an xmm register but we can only read.
    ///
    XmmR(Xmm),
    ///
    /// No linkage with any xmm regiter.
    ///
    None,
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
                LinkMode::None => None,
                LinkMode::XmmR(x) => Some(format!("%{i}:R({:?}) ", x)),
                LinkMode::XmmRW(x) => Some(format!("%{i}:RW({:?}) ", x)),
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

impl StackSlotInfo {
    fn merge(&mut self, other: &StackSlotInfo) {
        self.0.iter_mut().zip(other.0.iter()).for_each(|(l, r)| {
            *l = match (&l, r) {
                (LinkMode::XmmR(l), LinkMode::XmmR(_) | LinkMode::XmmRW(_))
                | (LinkMode::XmmRW(l), LinkMode::XmmR(_)) => LinkMode::XmmR(*l),
                (LinkMode::XmmRW(l), LinkMode::XmmRW(_)) => LinkMode::XmmRW(*l),
                _ => LinkMode::None,
            };
        });
    }

    fn merge_entries(entries: &[BranchEntry]) -> Self {
        let mut target = entries[0].bbctx.stack_slot.clone();
        #[cfg(feature = "emit-tir")]
        eprintln!("  <-{}: {:?}", entries[0].src_idx, target);
        for BranchEntry {
            src_idx: _src_idx,
            bbctx,
            dest_label: _,
        } in entries.iter().skip(1)
        {
            #[cfg(feature = "emit-tir")]
            eprintln!("  <-{_src_idx}: {:?}", bbctx.stack_slot);
            target.merge(&bbctx.stack_slot);
        }
        target
    }
}

type UsingXmm = Vec<Xmm>;

#[derive(PartialEq)]
enum BinOpMode {
    RR(SlotId, SlotId),
    RI(SlotId, i16),
    IR(i16, SlotId),
}

#[cfg(feature = "log-jit")]
extern "C" fn log_deoptimize(
    _interp: &mut Executor,
    globals: &mut Globals,
    func_id: FuncId,
    pc: BcPc,
    v: Value,
) {
    let name = globals.func[func_id].as_ruby_func().name();
    let bc_begin = globals.func[func_id].as_ruby_func().get_bytecode_address(0);
    let index = pc - bc_begin;
    let fmt = pc.format(globals, index).unwrap_or_default();
    if let TraceIr::LoopEnd = pc.op1() {
        eprint!("<-- exited from JIT code in {} {:?}.", name, func_id);
        eprintln!("    [{:05}] {fmt}", index);
    } else if let TraceIr::ClassDef { .. } = pc.op1() {
        eprint!("<-- deoptimization occurs in {} {:?}.", name, func_id);
        eprintln!("    [{:05}] {fmt}", index);
    } else {
        match globals.deopt_stats.get_mut(&(func_id, index)) {
            Some(c) => *c = *c + 1,
            None => {
                globals.deopt_stats.insert((func_id, index), 0);
            }
        };
        eprint!("<-- deoptimization occurs in {} {:?}.", name, func_id);
        eprintln!("    [{:05}] {fmt} caused by {}", index, v.to_s(globals));
    }
}

impl Codegen {
    pub(super) fn jit_compile_ruby(
        &mut self,
        fnstore: &FnStore,
        func_id: FuncId,
        self_value: Value,
        position: Option<BcPc>,
    ) -> (DestLabel, Vec<(usize, usize)>) {
        let func = fnstore[func_id].as_ruby_func();
        let start_pos = func.get_pc_index(position);

        #[cfg(any(feature = "emit-asm", feature = "log-jit"))]
        let now = std::time::Instant::now();

        let entry = self.jit.label();
        self.jit.bind_label(entry);

        let mut cc = jitgen::JitContext::new(func, self, start_pos, position.is_some(), self_value);
        let bb_start_pos: Vec<_> = cc
            .bb_info
            .iter()
            .enumerate()
            .filter_map(|(idx, v)| match v {
                Some(_) => {
                    if idx >= start_pos {
                        Some(idx)
                    } else {
                        None
                    }
                }
                None => None,
            })
            .collect();
        let reg_num = func.total_reg_num();
        let local_num = func.local_num();
        cc.start_codepos = self.jit.get_current();

        if position.is_none() {
            // generate prologue and class guard of *self* for a method
            let pc = func.get_pc(0);
            self.prologue(pc);
            let side_exit = self.gen_side_deopt_without_writeback(pc + 1);
            monoasm!(self.jit,
                movq rdi, [rbp - (OFFSET_SELF)];
            );
            self.guard_class(self_value.class_id(), side_exit);
        }

        cc.branch_map.insert(
            start_pos,
            vec![BranchEntry {
                src_idx: 0,
                bbctx: BBContext::new(reg_num, local_num, self_value),
                dest_label: self.jit.label(),
            }],
        );
        for i in bb_start_pos {
            cc.bb_pos = i;
            if self.compile_bb(fnstore, func, &mut cc, position) {
                break;
            };
        }

        let keys: Vec<_> = cc.branch_map.keys().cloned().collect();
        for pos in keys.into_iter() {
            self.gen_backedge_branch(&mut cc, func, pos);
        }

        self.jit.finalize();

        #[cfg(any(feature = "emit-asm", feature = "log-jit"))]
        {
            let elapsed = now.elapsed();
            eprintln!("<== finished compile. elapsed:{:?}", elapsed);
        }
        #[cfg(feature = "emit-tir")]
        eprintln!("<== finished compile.");
        (entry, cc.sourcemap)
    }

    ///
    /// Convert f64 to Value.
    ///
    /// ### in
    ///
    /// - xmm0: f64
    ///
    /// ### out
    ///
    /// - rax: Value
    ///
    /// ### registers destroyed
    ///
    /// - rcx, xmm1
    ///
    pub(super) fn generate_f64_to_val(&mut self) -> DestLabel {
        let entry = self.jit.label();
        let normal = self.jit.label();
        let heap_alloc = self.jit.label();
        monoasm!(self.jit,
        entry:
            xorps xmm1, xmm1;
            ucomisd xmm0, xmm1;
            jne normal;
            jp normal;
            movq rax, (Value::new_float(0.0).get());
            ret;
        normal:
            movq rax, xmm0;
            movq rcx, rax;
            shrq rcx, 60;
            addl rcx, 1;
            andl rcx, 6;
            cmpl rcx, 4;
            jne heap_alloc;
            rolq rax, 3;
            andq rax, (-4);
            orq rax, 2;
            ret;
        heap_alloc:
        // we must save rdi for log_optimize.
            subq rsp, 120;
            movq [rsp + 112], rdi;
            movq [rsp + 104], xmm15;
            movq [rsp + 96], xmm14;
            movq [rsp + 88], xmm13;
            movq [rsp + 80], xmm12;
            movq [rsp + 72], xmm11;
            movq [rsp + 64], xmm10;
            movq [rsp + 56], xmm9;
            movq [rsp + 48], xmm8;
            movq [rsp + 40], xmm7;
            movq [rsp + 32], xmm6;
            movq [rsp + 24], xmm5;
            movq [rsp + 16], xmm4;
            movq [rsp + 8], xmm3;
            movq [rsp + 0], xmm2;
            movq rax, (Value::new_float);
            call rax;
            movq xmm2, [rsp + 0];
            movq xmm3, [rsp + 8];
            movq xmm4, [rsp + 16];
            movq xmm5, [rsp + 24];
            movq xmm6, [rsp + 32];
            movq xmm7, [rsp + 40];
            movq xmm8, [rsp + 48];
            movq xmm9, [rsp + 56];
            movq xmm10, [rsp + 64];
            movq xmm11, [rsp + 72];
            movq xmm12, [rsp + 80];
            movq xmm13, [rsp + 88];
            movq xmm14, [rsp + 96];
            movq xmm15, [rsp + 104];
            movq rdi, [rsp + 112];
            addq rsp, 120;
            ret;
        );
        entry
    }
}

impl Codegen {
    ///
    /// store rax to *ret*.
    ///
    fn store_rax(&mut self, ret: SlotId) {
        monoasm!(self.jit,
            movq [rbp - (conv(ret))], rax;
        );
    }

    ///
    /// store rdi to *ret*.
    ///
    fn store_rdi(&mut self, ret: SlotId) {
        monoasm!(self.jit,
            movq [rbp - (conv(ret))], rdi;
        );
    }

    ///
    /// store rsi to *ret*.
    ///
    fn store_rsi(&mut self, ret: SlotId) {
        monoasm!(self.jit,
            // store the result to return reg.
            movq [rbp - (conv(ret))], rsi;
        );
    }

    ///
    /// move xmm(*src*) to xmm(*dst*).
    ///
    fn xmm_mov(&mut self, src: Xmm, dst: Xmm) {
        if src != dst {
            monoasm!(self.jit,
                movq  xmm(dst.enc()), xmm(src.enc());
            );
        }
    }

    ///
    /// Assume the Value is Integer, and convert to f64.
    ///
    /// side-exit if not Integer.
    ///
    /// ### in
    ///
    /// - rdi: Value
    ///
    /// ### out
    ///
    /// - xmm0: f64
    ///
    fn gen_val_to_f64_assume_integer(&mut self, xmm: u64, side_exit: DestLabel) -> DestLabel {
        let entry = self.jit.label();
        monoasm!(&mut self.jit,
        entry:
            testq rdi, 0b01;
            jz side_exit;
            sarq rdi, 1;
            cvtsi2sdq xmm(xmm), rdi;
        );
        entry
    }

    ///
    /// Assume the Value is Float, and convert to f64.
    ///
    /// side-exit if not Float.
    ///
    /// ### in
    ///
    /// - rdi: Value
    ///
    /// ### out
    ///
    /// - xmm(*xmm*): f64
    ///
    /// ### registers destroyed
    ///
    /// - rax, rdi
    ///
    fn gen_val_to_f64_assume_float(&mut self, xmm: u64, side_exit: DestLabel) -> DestLabel {
        let heap_to_f64 = self.heap_to_f64;
        let entry = self.jit.label();
        let heap = self.jit.label();
        let exit = self.jit.label();
        monoasm!(&mut self.jit,
        entry:
            testq rdi, 0b01;
            jnz side_exit;
            testq rdi, 0b10;
            jz heap;
            xorps xmm(xmm), xmm(xmm);
            movq rax, (FLOAT_ZERO);
            cmpq rdi, rax;
            je exit;
            movq rax, rdi;
            sarq rax, 63;
            addq rax, 2;
            andq rdi, (-4);
            orq rdi, rax;
            rolq rdi, 61;
            movq xmm(xmm), rdi;
            jmp exit;
        heap:
            call heap_to_f64;
            testq rax, rax;
            jz   side_exit;
            movq xmm(xmm), xmm0;
        exit:
        );

        entry
    }

    ///
    /// Confirm the Value is Float.
    ///
    /// side-exit if not Float.
    ///
    /// ### registers destroyed
    ///
    /// - rax, rdi
    ///
    fn gen_assume_float(&mut self, reg: SlotId, side_exit: DestLabel) {
        let heap_to_f64 = self.heap_to_f64;
        let heap = self.jit.label();
        let exit = self.jit.label();
        monoasm!(&mut self.jit,
            movq rdi, [rbp - (conv(reg))];
            testq rdi, 0b01;
            jnz side_exit;
            testq rdi, 0b10;
            jnz exit;
        heap:
            call heap_to_f64;
            testq rax, rax;
            jz   side_exit;
        exit:
        );
    }

    ///
    /// Convert the Value to f64.
    ///
    /// side-exit if neither Float nor Integer.
    ///
    /// ### in
    ///
    /// - rdi: Value
    ///
    /// ### out
    ///
    /// - xmm(*xmm*): f64
    ///
    /// ### registers destroyed
    ///
    /// - caller save registers except xmm registers(xmm2-xmm15).
    ///
    fn gen_val_to_f64(&mut self, xmm: u64, side_exit: DestLabel) {
        let heap_to_f64 = self.heap_to_f64;
        let integer = self.jit.label();
        let heap = self.jit.label();
        let exit = self.jit.label();
        monoasm!(&mut self.jit,
            testq rdi, 0b01;
            jnz integer;
            testq rdi, 0b10;
            jz  heap;
            xorps xmm(xmm), xmm(xmm);
            movq rax, (FLOAT_ZERO);
            cmpq rdi, rax;
            je  exit;
            movq rax, rdi;
            sarq rax, 63;
            addq rax, 2;
            andq rdi, (-4);
            orq rdi, rax;
            rolq rdi, 61;
            movq xmm(xmm), rdi;
            jmp exit;
        integer:
            sarq rdi, 1;
            cvtsi2sdq xmm(xmm), rdi;
            jmp exit;
        heap:
            call heap_to_f64;
            testq rax, rax;
            jz   side_exit;
            movq xmm(xmm), xmm0;
        exit:
        );
    }

    ///
    /// Copy *src* to *dst*.
    ///
    fn copy_slot(&mut self, ctx: &mut BBContext, src: SlotId, dst: SlotId) {
        ctx.dealloc_xmm(dst);
        match ctx.stack_slot[src] {
            LinkMode::XmmRW(freg) | LinkMode::XmmR(freg) => {
                ctx.link_rw_xmm(dst, freg);
            }
            LinkMode::None => {
                monoasm!(self.jit,
                  movq rax, [rbp - (conv(src))];
                  movq [rbp - (conv(dst))], rax;
                );
            }
        }
    }

    ///
    /// Write back a corresponding xmm register to the stack slot *reg*.
    ///
    /// the xmm will be deallocated.
    ///
    fn write_back_slot(&mut self, ctx: &mut BBContext, reg: SlotId) {
        if let LinkMode::XmmRW(freg) = ctx.stack_slot[reg] {
            let f64_to_val = self.f64_to_val;
            monoasm!(self.jit,
                movq xmm0, xmm(freg.enc());
                call f64_to_val;
            );
            self.store_rax(reg);
            ctx.stack_slot[reg] = LinkMode::XmmR(freg);
        }
    }

    fn write_back_range(&mut self, ctx: &mut BBContext, arg: SlotId, len: u16) {
        for reg in arg.0..arg.0 + len {
            self.write_back_slot(ctx, SlotId::new(reg))
        }
    }

    fn compile_bb(
        &mut self,
        fnstore: &FnStore,
        func: &ISeqInfo,
        cc: &mut JitContext,
        position: Option<BcPc>,
    ) -> bool {
        let mut skip = false;
        let is_loop = matches!(func.get_pc(cc.bb_pos).op1(), TraceIr::LoopStart(_));
        self.jit.bind_label(cc.labels[&cc.bb_pos]);
        let mut ctx = if is_loop {
            self.gen_merging_branches_loop(func, fnstore, cc, cc.bb_pos)
        } else {
            self.gen_merging_branches(func, cc, cc.bb_pos)
        };
        for (ofs, pc) in func.bytecode()[cc.bb_pos..].iter().enumerate() {
            let pc = BcPc::from(pc);
            if skip {
                skip = false;
                continue;
            }

            #[cfg(feature = "emit-asm")]
            cc.sourcemap
                .push((cc.bb_pos + ofs, self.jit.get_current() - cc.start_codepos));
            match pc.op1() {
                TraceIr::InitMethod { .. } => {}
                TraceIr::InitBlock { .. } => {}
                TraceIr::LoopStart(_) => {
                    cc.loop_count += 1;
                }
                TraceIr::LoopEnd => {
                    assert_ne!(0, cc.loop_count);
                    cc.loop_count -= 1;
                    if cc.is_loop && cc.loop_count == 0 {
                        #[cfg(any(feature = "emit-asm", feature = "log-jit"))]
                        eprintln!("<-- compile finished. end:[{:05}]", cc.bb_pos + ofs);
                        self.go_deopt(&ctx, pc);
                        break;
                    }
                }
                TraceIr::Integer(ret, i) => {
                    ctx.dealloc_xmm(ret);
                    let i = Value::int32(i).get();
                    monoasm!(self.jit,
                      movq [rbp - (conv(ret))], (i);
                    );
                }
                TraceIr::Symbol(ret, id) => {
                    ctx.dealloc_xmm(ret);
                    let sym = Value::new_symbol(id).get();
                    monoasm!(self.jit,
                      movq rax, (sym);
                    );
                    self.store_rax(ret);
                }
                TraceIr::Literal(dst, val) => {
                    ctx.dealloc_xmm(dst);
                    if let RV::Float(f) = val.unpack() {
                        let freg = ctx.alloc_xmm();
                        ctx.link_r_xmm(dst, freg);
                        let imm = self.jit.const_f64(f);
                        monoasm!(self.jit,
                            movq xmm(freg.enc()), [rip + imm];
                            movq rax, (Value::new_float(f).get());
                            movq [rbp - (conv(dst))], rax;
                        );
                    } else {
                        if val.is_packed_value() {
                            monoasm!(self.jit,
                                movq rax, (val.get());
                            );
                        } else {
                            let xmm_using = ctx.get_xmm_using();
                            self.xmm_save(&xmm_using);
                            monoasm!(self.jit,
                              movq rdi, (val.get());
                              movq rax, (Value::deep_copy);
                              call rax;
                            );
                            self.xmm_restore(&xmm_using);
                        }
                        self.store_rax(dst);
                    }
                }
                TraceIr::Array(ret, src, len) => {
                    self.write_back_range(&mut ctx, src, len);
                    ctx.dealloc_xmm(ret);
                    monoasm!(self.jit,
                        lea  rdi, [rbp - (conv(src))];
                        movq rsi, (len);
                        movq rax, (gen_array);
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
                    let xmm_using = ctx.get_xmm_using();
                    self.xmm_save(&xmm_using);
                    monoasm! { self.jit,
                        movq rdi, [rbp - (conv(start))]; // base: Value
                        movq rsi, [rbp - (conv(end))]; // idx: Value
                        movq rdx, r12; // &mut Globals
                        movl rcx, (if exclude_end {1} else {0});
                        movq rax, (gen_range);
                        call rax;
                    };
                    self.xmm_restore(&xmm_using);
                    self.handle_error(pc);
                    self.store_rax(ret);
                }
                TraceIr::Index(ret, base, idx) => {
                    self.write_back_slot(&mut ctx, base);
                    self.write_back_slot(&mut ctx, idx);
                    ctx.dealloc_xmm(ret);
                    self.jit_get_index(ret, base, idx, pc, &ctx);
                }
                TraceIr::IndexAssign(src, base, idx) => {
                    self.write_back_slot(&mut ctx, base);
                    self.write_back_slot(&mut ctx, idx);
                    self.write_back_slot(&mut ctx, src);
                    self.jit_index_assign(src, base, idx, pc, &ctx);
                }
                TraceIr::LoadConst(dst, id) => {
                    ctx.dealloc_xmm(dst);
                    if pc.value().map(|v| v.class_id()) == Some(FLOAT_CLASS) {
                        let fdst = ctx.alloc_xmm();
                        ctx.link_r_xmm(dst, fdst);
                        self.load_float_constant(&ctx, dst, fdst, id, pc);
                    } else {
                        self.load_generic_constant(&ctx, dst, id, pc);
                    }
                }
                TraceIr::StoreConst(src, id) => {
                    self.write_back_slot(&mut ctx, src);
                    self.jit_store_constant(&ctx, id, src);
                }
                TraceIr::BlockArgProxy(dst) => {
                    ctx.dealloc_xmm(dst);
                    let panic = self.entry_panic;
                    monoasm! { self.jit,
                        movq rax, [rbp - (OFFSET_BLOCK)];
                        testq rax, 0b1;
                        jeq panic;
                        addq rax, 0b10;
                        movq [rbp - (conv(dst))], rax;
                    };
                }
                TraceIr::LoadIvar(ret, id, cached_class, cached_ivarid) => {
                    ctx.dealloc_xmm(ret);
                    self.jit_load_ivar(&ctx, id, ret, cached_class, cached_ivarid);
                }
                TraceIr::StoreIvar(src, id, cached_class, cached_ivarid) => {
                    self.write_back_slot(&mut ctx, src);
                    self.jit_store_ivar(&ctx, id, src, pc, cached_class, cached_ivarid);
                }
                TraceIr::LoadGvar { ret, name } => {
                    ctx.dealloc_xmm(ret);
                    let xmm_using = ctx.get_xmm_using();
                    self.xmm_save(&xmm_using);
                    monoasm! { self.jit,
                        movq rdi, r12;
                        movl rsi, (name.get());
                        movq rax, (get_global_var);
                        call rax;
                        movq [rbp - (conv(ret))], rax;
                    };
                    self.xmm_restore(&xmm_using);
                }
                TraceIr::StoreGvar { val, name } => {
                    self.write_back_slot(&mut ctx, val);
                    let xmm_using = ctx.get_xmm_using();
                    self.xmm_save(&xmm_using);
                    monoasm! { self.jit,
                        movq rdi, r12;
                        movl rsi, (name.get());
                        movq rdx, [rbp - (conv(val))];
                        movq rax, (set_global_var);
                        call rax;
                    };
                    self.xmm_restore(&xmm_using);
                }
                TraceIr::LoadDynVar(ret, src) => {
                    ctx.dealloc_xmm(ret);
                    monoasm!(self.jit,
                        movq rax, [rbp - (OFFSET_OUTER)];
                    );
                    for _ in 0..src.outer - 1 {
                        monoasm!(self.jit,
                            movq rax, [rax];
                        );
                    }
                    let offset = conv(src.reg) - OFFSET_OUTER;
                    monoasm!(self.jit,
                        movq rax, [rax - (offset)];
                    );
                    if ret.0 != 0 {
                        self.store_rax(ret);
                    }
                }
                TraceIr::StoreDynVar(dst, src) => {
                    self.write_back_slot(&mut ctx, src);
                    monoasm!(self.jit,
                        movq rax, [rbp - (OFFSET_OUTER)];
                    );
                    for _ in 0..dst.outer - 1 {
                        monoasm!(self.jit,
                            movq rax, [rax];
                        );
                    }
                    let offset = conv(dst.reg) - OFFSET_OUTER;
                    monoasm!(self.jit,
                        movq rdi, [rbp - (conv(src))];
                        movq [rax - (offset)], rdi;
                    );
                }
                TraceIr::Nil(ret) => {
                    ctx.dealloc_xmm(ret);
                    monoasm!(self.jit,
                        movq [rbp - (conv(ret))], (NIL_VALUE);
                    );
                }
                TraceIr::Neg(dst, src) => {
                    if pc.is_float1() {
                        let fsrc = self.xmm_read_assume_float(&mut ctx, src, pc);
                        let fdst = ctx.xmm_write(dst);
                        let imm = self.jit.const_i64(0x8000_0000_0000_0000u64 as i64);
                        self.xmm_mov(fsrc, fdst);
                        monoasm!(self.jit,
                            xorps xmm(fdst.enc()), [rip + imm];
                        );
                    } else {
                        if pc.classid1().0 == 0 {
                            self.recompile_and_deopt(&ctx, position, pc);
                        }
                        self.write_back_slot(&mut ctx, src);
                        ctx.dealloc_xmm(dst);
                        let xmm_using = ctx.get_xmm_using();
                        self.xmm_save(&xmm_using);
                        monoasm!(self.jit,
                            movq rdi, [rbp - (conv(src))];
                        );
                        self.call_unop(neg_value as _);
                        self.xmm_restore(&xmm_using);
                        self.handle_error(pc);
                        self.store_rax(dst);
                    }
                }
                TraceIr::IntegerBinOp {
                    kind,
                    ret,
                    lhs,
                    rhs,
                    ..
                } => {
                    self.write_back_slot(&mut ctx, lhs);
                    self.write_back_slot(&mut ctx, rhs);
                    ctx.dealloc_xmm(ret);
                    self.gen_binop_integer(pc, kind, ret, BinOpMode::RR(lhs, rhs), &ctx);
                }
                TraceIr::FloatBinOp {
                    kind,
                    ret,
                    lhs,
                    rhs,
                    ..
                } => {
                    let (flhs, frhs) = self.xmm_read_binary(&mut ctx, lhs, rhs, pc);
                    let fret = ctx.xmm_write(ret);
                    self.gen_binop_float(kind, &ctx, fret, flhs, frhs);
                }
                TraceIr::BinOp {
                    kind,
                    ret,
                    lhs,
                    rhs,
                    ..
                } => {
                    if pc.classid1().0 == 0 || pc.classid2().0 == 0 {
                        self.recompile_and_deopt(&ctx, position, pc);
                    }
                    self.write_back_slot(&mut ctx, lhs);
                    self.write_back_slot(&mut ctx, rhs);
                    ctx.dealloc_xmm(ret);
                    self.load_binary_args(lhs, rhs);
                    self.gen_generic_binop(&ctx, pc, kind, ret);
                }

                TraceIr::BinOpRi {
                    kind,
                    ret,
                    lhs,
                    rhs,
                } => {
                    if pc.is_integer1() {
                        self.write_back_slot(&mut ctx, lhs);
                        ctx.dealloc_xmm(ret);
                        self.gen_binop_integer(pc, kind, ret, BinOpMode::RI(lhs, rhs), &ctx);
                    } else if pc.is_float1() {
                        let flhs = self.xmm_read_assume_float(&mut ctx, lhs, pc);
                        let fret = ctx.xmm_write(ret);
                        self.gen_binop_float_ri(kind, &ctx, fret, flhs, rhs);
                    } else {
                        if pc.classid1().0 == 0 {
                            self.recompile_and_deopt(&ctx, position, pc);
                        }
                        self.write_back_slot(&mut ctx, lhs);
                        ctx.dealloc_xmm(ret);
                        monoasm!(self.jit,
                            movq rdi, [rbp - (conv(lhs))];
                            movq rsi, (Value::int32(rhs as i32).get());
                        );
                        self.gen_generic_binop(&ctx, pc, kind, ret);
                    }
                }

                TraceIr::BinOpIr {
                    kind,
                    ret,
                    lhs,
                    rhs,
                } => {
                    if pc.is_integer2() {
                        self.write_back_slot(&mut ctx, rhs);
                        ctx.dealloc_xmm(ret);
                        self.gen_binop_integer(pc, kind, ret, BinOpMode::IR(lhs, rhs), &ctx);
                    } else if pc.is_float2() {
                        let frhs = self.xmm_read_assume_float(&mut ctx, rhs, pc);
                        let fret = ctx.xmm_write(ret);
                        self.gen_binop_float_ir(kind, &ctx, fret, lhs, frhs);
                    } else {
                        if pc.classid2().0 == 0 {
                            self.recompile_and_deopt(&ctx, position, pc);
                        }
                        self.write_back_slot(&mut ctx, rhs);
                        ctx.dealloc_xmm(ret);
                        monoasm!(self.jit,
                            movq rdi, (Value::int32(lhs as i32).get());
                            movq rsi, [rbp - (conv(rhs))];
                        );
                        self.gen_generic_binop(&ctx, pc, kind, ret);
                    }
                }

                TraceIr::Cmp(kind, ret, lhs, rhs, false) => {
                    if pc.is_float_binop() {
                        let (flhs, frhs) = self.xmm_read_binary(&mut ctx, lhs, rhs, pc);
                        ctx.dealloc_xmm(ret);
                        monoasm! { self.jit,
                            xorq rax, rax;
                            ucomisd xmm(flhs.enc()), xmm(frhs.enc());
                        };
                        self.setflag_float(kind);
                        self.store_rax(ret);
                    } else if pc.is_integer_binop() {
                        let deopt = self.gen_side_deopt(pc, &ctx);
                        self.write_back_slot(&mut ctx, lhs);
                        self.write_back_slot(&mut ctx, rhs);
                        ctx.dealloc_xmm(ret);
                        self.gen_cmp_prep(lhs, rhs, deopt);
                        self.gen_integer_cmp_kind(kind, ret);
                    } else {
                        if pc.classid1().0 == 0 || pc.classid2().0 == 0 {
                            self.recompile_and_deopt(&ctx, position, pc);
                        }
                        let generic = self.jit.label();
                        self.write_back_slot(&mut ctx, lhs);
                        self.write_back_slot(&mut ctx, rhs);
                        ctx.dealloc_xmm(ret);
                        self.gen_cmp_prep(lhs, rhs, generic);
                        self.gen_cmp_kind(kind, generic, ret, &ctx);
                    }
                }
                TraceIr::Cmp(_, _, _, _, true) => {}
                TraceIr::Cmpri(kind, ret, lhs, rhs, false) => {
                    if pc.is_float1() {
                        let rhs_label = self.jit.const_f64(rhs as f64);
                        let flhs = self.xmm_read_assume_float(&mut ctx, lhs, pc);
                        ctx.dealloc_xmm(ret);
                        monoasm! { self.jit,
                            xorq rax, rax;
                            ucomisd xmm(flhs.enc()), [rip + rhs_label];
                        };
                        self.setflag_float(kind);
                        self.store_rax(ret);
                    } else if pc.is_integer1() {
                        let deopt = self.gen_side_deopt(pc, &ctx);
                        self.write_back_slot(&mut ctx, lhs);
                        ctx.dealloc_xmm(ret);
                        self.gen_cmpri_prep(lhs, rhs, deopt);
                        self.gen_integer_cmp_kind(kind, ret);
                    } else {
                        if pc.classid1().0 == 0 {
                            self.recompile_and_deopt(&ctx, position, pc);
                        }
                        let generic = self.jit.label();
                        self.write_back_slot(&mut ctx, lhs);
                        ctx.dealloc_xmm(ret);
                        self.gen_cmpri_prep(lhs, rhs, generic);
                        self.gen_cmp_kind(kind, generic, ret, &ctx);
                    }
                }
                TraceIr::Cmpri(_, _, _, _, true) => {}
                TraceIr::Mov(dst, src) => {
                    self.copy_slot(&mut ctx, src, dst);
                }
                TraceIr::ConcatStr(ret, arg, len) => {
                    self.write_back_range(&mut ctx, arg, len);
                    ctx.dealloc_xmm(ret);
                    let xmm_using = ctx.get_xmm_using();
                    self.xmm_save(&xmm_using);
                    monoasm!(self.jit,
                        movq rdi, r12;
                        lea rsi, [rbp - (conv(arg))];
                        movq rdx, (len);
                        movq rax, (concatenate_string);
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
                    monoasm!(self.jit,
                        movq rdi, [rbp - (conv(src))];
                        lea rsi, [rbp - (conv(dst))];
                        movq rdx, (len);
                        movq rax, (expand_array);
                        call rax;
                    );
                    self.xmm_restore(&xmm_using);
                }
                TraceIr::MethodCall { ret, name, .. } => {
                    if let TraceIr::MethodArgs(method_info) = (pc + 1).op1() {
                        if method_info.callee_codeptr.is_none() {
                            self.recompile_and_deopt(&ctx, position, pc);
                        }
                        self.gen_method_call(fnstore, &mut ctx, method_info, ret, name, pc);
                    } else {
                        unreachable!()
                    }
                }
                TraceIr::MethodCallBlock { ret, name, .. } => {
                    if let TraceIr::MethodArgs(method_info) = (pc + 1).op1() {
                        if method_info.callee_codeptr.is_none() {
                            self.recompile_and_deopt(&ctx, position, pc);
                        }
                        self.gen_method_call_with_block(
                            fnstore,
                            &mut ctx,
                            method_info,
                            ret,
                            name,
                            pc,
                        );
                    } else {
                        unreachable!()
                    }
                }
                TraceIr::Yield { ret, args, len } => {
                    ctx.dealloc_xmm(ret);
                    self.write_back_range(&mut ctx, args, len);
                    self.gen_yield(&mut ctx, args, len, ret, pc);
                }
                TraceIr::MethodArgs(_) => {
                    skip = true;
                }
                TraceIr::MethodDef(name, func) => {
                    let class_version = self.class_version;
                    let xmm_using = ctx.get_xmm_using();
                    self.xmm_save(&xmm_using);
                    monoasm!(self.jit,
                        movq rdi, rbx; // &mut Interp
                        movq rsi, r12; // &Globals
                        movq rdx, (u32::from(name)); // IdentId
                        movq rcx, (u32::from(func)); // FuncId
                        movq rax, (define_method);
                        call rax;
                        addl [rip + class_version], 1;
                    );
                    self.xmm_restore(&xmm_using);
                }
                TraceIr::ClassDef {
                    ret,
                    superclass,
                    name,
                    func_id,
                } => {
                    self.jit_class_def(&ctx, ret, superclass, name, func_id);
                }
                TraceIr::Ret(lhs) => {
                    self.write_back_slot(&mut ctx, lhs);
                    monoasm!(self.jit,
                        movq rax, [rbp - (conv(lhs))];
                    );
                    self.epilogue();
                    return false;
                }
                TraceIr::Br(disp) => {
                    let next_idx = cc.bb_pos + ofs + 1;
                    let dest_idx = (next_idx as i64 + disp as i64) as usize;
                    let branch_dest = self.jit.label();
                    cc.new_branch(cc.bb_pos + ofs, dest_idx, ctx, branch_dest);
                    monoasm!(self.jit,
                        jmp branch_dest;
                    );
                    return false;
                }
                TraceIr::CondBr(cond_, disp, false, kind) => {
                    let dest_idx = ((cc.bb_pos + ofs + 1) as i32 + disp) as usize;
                    let branch_dest = self.jit.label();
                    cc.new_branch(cc.bb_pos + ofs, dest_idx, ctx.clone(), branch_dest);
                    monoasm!(self.jit,
                        movq rax, [rbp - (conv(cond_))];
                        orq rax, 0x10;
                        cmpq rax, (FALSE_VALUE);
                    );
                    match kind {
                        BrKind::BrIf => monoasm!(self.jit, jne branch_dest;),
                        BrKind::BrIfNot => monoasm!(self.jit, jeq branch_dest;),
                    }
                }
                TraceIr::CondBr(_, disp, true, brkind) => {
                    let dest_idx = ((cc.bb_pos + ofs + 1) as i32 + disp) as usize;
                    let pc = pc - 1;
                    if pc.is_float_binop() {
                        let kind = match pc.op1() {
                            TraceIr::Cmp(kind, _ret, lhs, rhs, true) => {
                                let (flhs, frhs) = self.xmm_read_binary(&mut ctx, lhs, rhs, pc);
                                monoasm! { self.jit,
                                    ucomisd xmm(flhs.enc()), xmm(frhs.enc());
                                };
                                kind
                            }
                            TraceIr::Cmpri(kind, _ret, lhs, rhs, true) => {
                                let rhs_label = self.jit.const_f64(rhs as f64);
                                let flhs = self.xmm_read_assume_float(&mut ctx, lhs, pc);
                                monoasm! { self.jit,
                                    ucomisd xmm(flhs.enc()), [rip + rhs_label];
                                };
                                kind
                            }
                            _ => unreachable!(),
                        };
                        let branch_dest = self.jit.label();
                        cc.new_branch(cc.bb_pos + ofs, dest_idx, ctx.clone(), branch_dest);
                        self.gen_cmp_float_opt(kind, branch_dest, brkind);
                    //} else if pc.classid1().0 == 0 || pc.classid2().0 == 0 {
                    //    self.recompile_and_deopt(&ctx, position, pc);
                    //    let branch_dest = self.jit.label();
                    //    cc.new_branch(cc.bb_pos + ofs, dest_idx, ctx.clone(), branch_dest);
                    } else {
                        let generic = self.jit.label();
                        let kind = match pc.op1() {
                            TraceIr::Cmp(kind, ret, lhs, rhs, true) => {
                                self.write_back_slot(&mut ctx, lhs);
                                self.write_back_slot(&mut ctx, rhs);
                                ctx.dealloc_xmm(ret);
                                self.gen_cmp_prep(lhs, rhs, generic);
                                kind
                            }
                            TraceIr::Cmpri(kind, ret, lhs, rhs, true) => {
                                self.write_back_slot(&mut ctx, lhs);
                                ctx.dealloc_xmm(ret);
                                self.gen_cmpri_prep(lhs, rhs, generic);
                                kind
                            }
                            _ => unreachable!(),
                        };

                        let xmm_using = ctx.get_xmm_using();
                        monoasm! { self.jit,
                            cmpq rdi, rsi;
                        };
                        let branch_dest = self.jit.label();
                        cc.new_branch(cc.bb_pos + ofs, dest_idx, ctx.clone(), branch_dest);
                        self.gen_cmp_int_opt(kind, branch_dest, generic, brkind, xmm_using);
                    }
                }
                TraceIr::CheckLocal(local, disp) => {
                    let dest_idx = ((cc.bb_pos + ofs + 1) as i32 + disp) as usize;
                    let branch_dest = self.jit.label();
                    cc.new_branch(cc.bb_pos + ofs, dest_idx, ctx.clone(), branch_dest);
                    monoasm!(self.jit,
                        movq rax, [rbp - (conv(local))];
                        testq rax, rax;
                        jnz  branch_dest;
                    );
                }
            }

            let next_idx = cc.bb_pos + ofs + 1;
            if cc.bb_info[next_idx].is_some() {
                let branch_dest = self.jit.label();
                cc.new_branch(cc.bb_pos + ofs, next_idx, ctx.clone(), branch_dest);
                monoasm!(self.jit,
                    jmp branch_dest;
                );
                return false;
            }
        }
        true
    }

    fn recompile_and_deopt(&mut self, ctx: &BBContext, position: Option<BcPc>, pc: BcPc) {
        let recompile = self.jit.label();
        let counter = self.jit.const_i32(5);
        let deopt = self.gen_side_deopt(pc, &ctx);
        monoasm!(self.jit,
            subl [rip + counter], 1;
            jeq recompile;
        );
        self.jit.select_page(1);
        monoasm!(self.jit,
        recompile:
            movq rdi, r12;
            movl rsi, [rbp - (OFFSET_FUNCID)];
            movq rdx, [rbp - (OFFSET_SELF)];
        );
        if let Some(index) = position {
            monoasm!(self.jit,
                movq rcx, (index.get_u64());
                movq rax, (Self::exec_jit_partial_compile);
                call rax;
            );
        } else {
            monoasm!(self.jit,
                movq rax, (Self::exec_jit_recompile);
                call rax;
            );
        }
        monoasm!(self.jit,
            movq rdi, (NIL_VALUE);
            jmp deopt;
        );
        self.jit.select_page(0);
    }
}

impl Codegen {
    fn handle_error(&mut self, pc: BcPc) {
        let jit_return = self.vm_return;
        monoasm!(self.jit,
            movq r13, ((pc + 1).get_u64());
            testq rax, rax; // Option<Value>
            jeq  jit_return;
        );
    }

    ///
    /// Generate a code which write back all xmm registers to corresponding stack slots.
    ///
    /// xmms are not deallocated.
    ///
    fn gen_write_back(&mut self, wb: WriteBack) {
        for (freg, v) in wb {
            self.gen_write_back_single(freg, v);
        }
    }

    fn gen_write_back_single(&mut self, freg: Xmm, v: Vec<SlotId>) {
        if v.is_empty() {
            return;
        }
        #[cfg(feature = "emit-tir")]
        eprintln!("      wb: {:?}->{:?}", freg, v);
        let f64_to_val = self.f64_to_val;
        monoasm!(self.jit,
            movq xmm0, xmm(freg.enc());
            call f64_to_val;
        );
        for reg in v {
            self.store_rax(reg);
        }
    }

    ///
    /// Get *DestLabel* for write-back and fallback to interpreter.
    ///
    fn gen_side_deopt(&mut self, pc: BcPc, ctx: &BBContext) -> DestLabel {
        self.gen_side_deopt_main(pc, Some(ctx))
    }

    ///
    /// Get *DestLabel* for fallback to interpreter. (without write-back)
    ///
    pub(super) fn gen_side_deopt_without_writeback(&mut self, pc: BcPc) -> DestLabel {
        self.gen_side_deopt_main(pc, None)
    }

    fn gen_side_deopt_main(&mut self, pc: BcPc, ctx: Option<&BBContext>) -> DestLabel {
        assert_eq!(0, self.jit.get_page());
        self.jit.select_page(1);
        let entry = self.jit.label();
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
        monoasm!(self.jit,
            movq r13, (pc.get_u64());
        );
        #[cfg(feature = "log-jit")]
        monoasm!(self.jit,
            movq r8, rdi; // the Value which caused this deopt.
            movq rdi, rbx;
            movq rsi, r12;
            movq rdx, [rbp - (OFFSET_META)];
            movq rcx, r13;
            movq rax, (log_deoptimize);
            call rax;
        );
        monoasm!(self.jit,
            jmp fetch;
        );
        self.jit.select_page(0);
        entry
    }

    ///
    /// Fallback to interpreter after Writing back all linked xmms.
    ///
    fn go_deopt(&mut self, ctx: &BBContext, pc: BcPc) {
        let fallback = self.gen_side_deopt(pc, ctx);
        monoasm!(self.jit,
            jmp fallback;
        );
    }

    fn prologue(&mut self, pc: BcPc) {
        monoasm!(self.jit,
            pushq rbp;
            movq rbp, rsp;
            // save len in rdx.
            movq rdx, rdi;
        );
        match pc.op1() {
            TraceIr::InitMethod {
                reg_num,
                arg_num,
                pos_num,
                req_num,
                block_pos,
                stack_offset,
            } => {
                self.setup_stack(stack_offset);
                self.init_func(reg_num, arg_num, pos_num, req_num, block_pos, pc, false);
            }
            TraceIr::InitBlock {
                reg_num,
                arg_num,
                pos_num,
                req_num,
                block_pos,
                stack_offset,
            } => {
                self.setup_stack(stack_offset);
                if pos_num >= 2 {
                    self.jit_expand_arg0(req_num);
                }
                self.init_func(reg_num, arg_num, pos_num, req_num, block_pos, pc, true);
            }
            _ => unreachable!(),
        }
    }

    fn setup_stack(&mut self, stack_offset: usize) {
        monoasm!(self.jit,
            subq rsp, (stack_offset * 16);
        );
    }

    fn init_func(
        &mut self,
        reg_num: usize,
        arg_num: usize,
        pos_num: usize,
        req_num: usize,
        _block_pos: usize,
        pc: BcPc,
        is_block: bool,
    ) {
        let err_label = self.jit.label();
        self.jit.select_page(1);
        let err = self.wrong_argument;
        monoasm! { self.jit,
        err_label:
            movq r13, ((pc+1).get_u64());
            jmp  err;
        }
        self.jit.select_page(0);

        // rdx: number of args passed from caller
        let has_rest_param = pos_num != arg_num;

        if pos_num > 0 {
            let set_rest_empty = self.jit.label();
            let fill_req = self.jit.label();
            let fill_opt = self.jit.label();
            let fill_temp = self.jit.label();
            monoasm! { self.jit,
                // if passed_args >= pos_num then goto l1
                cmpl rdx, (pos_num);
                jeq  set_rest_empty;
                jlt  fill_req;
            }
            if has_rest_param {
                monoasm! { self.jit,
                    lea  rdi, [rbp - (pos_num as i32 * 8 + OFFSET_ARG0)];
                    movl rsi, rdx;
                    subl rsi, (pos_num);
                    movq rax, (make_rest_array);
                    call rax;
                    jmp  fill_temp;
                };
            } else {
                if is_block {
                    monoasm! { self.jit, jmp  fill_temp; }
                } else {
                    monoasm! { self.jit, jmp  err_label; }
                }
            }
            monoasm! { self.jit,
            fill_req:
            }
            if req_num > 0 {
                if pos_num != req_num {
                    monoasm! { self.jit,
                        // if passed_args >= req_num then goto l2
                        cmpl rdx, (req_num);
                        jge  fill_opt;
                    }
                }
                if is_block {
                    monoasm! { self.jit,
                        movl rax, (req_num);
                        subl rax, rdx;
                    }
                    self.jit_fill(req_num, NIL_VALUE);
                    monoasm! { self.jit,
                        movl rdx, (req_num);
                    }
                } else {
                    // in method, raise error if passed_args < req_num.
                    monoasm! { self.jit,
                        jmp  err_label;
                    }
                }
            }
            monoasm! { self.jit,
            fill_opt:
            // rax = pos_num - max(passed_args, req_num)
                movl rax, (pos_num);
                subl rax, rdx;
            // fill zero to residual locals.
            }
            self.jit_fill(pos_num, 0);
            monoasm! { self.jit,
            set_rest_empty:
            };
            if has_rest_param {
                monoasm! { self.jit,
                    lea  rdi, [rbp - (pos_num as i32 * 8 + OFFSET_ARG0)];
                    xorq rsi, rsi;
                    movq rax, (make_rest_array);
                    call rax;
                };
            }
            monoasm! { self.jit,
            fill_temp:
            }
        } else {
            if has_rest_param {
                monoasm! { self.jit,
                    lea  rdi, [rbp - (OFFSET_ARG0)];
                    movl rsi, rdx;
                    movq rax, (make_rest_array);
                    call rax;
                    //jmp  fill_temp;
                };
            } else {
                if !is_block {
                    monoasm! { self.jit,
                        cmpl rdx, (0);
                        jne  err_label;
                    }
                }
            }
        }

        // fill nil to temporary registers.
        let clear_len = reg_num - arg_num - 1;
        if clear_len > 2 {
            monoasm!(self.jit,
                movq rax, (NIL_VALUE);
            );
            for i in 0..clear_len {
                monoasm!(self.jit,
                    movq [rbp - ((arg_num + i) as i32 * 8 + OFFSET_ARG0)], rax;
                );
            }
        } else {
            for i in 0..clear_len {
                monoasm!(self.jit,
                    movq [rbp - ((arg_num + i) as i32 * 8 + (OFFSET_ARG0))], (NIL_VALUE);
                );
            }
        }
    }

    fn jit_expand_arg0(&mut self, req_num: usize) {
        let l1 = self.jit.label();
        monoasm! { self.jit,
            cmpl rdx, 1;
            jne  l1;
            movq rdi, [rbp - (OFFSET_ARG0)];
            testq rdi, 0b111;
            jnz  l1;
            cmpl [rdi + 4], (ARRAY_CLASS.0);
            jne  l1;
            movq rdx, (req_num);
            lea  rsi, [rbp - (OFFSET_ARG0)];
            movq rax, (block_expand_array);
            call rax;
            movq rdx, rax;
        l1:
        };
    }

    /// fill *val* to the slots [*end* - rax + 1 .. *end*]
    fn jit_fill(&mut self, ends: usize, val: u64) {
        let l0 = self.jit.label();
        let l1 = self.jit.label();
        monoasm! { self.jit,
            testq rax, rax;
            jz   l1;
            lea  rdi, [rbp - (OFFSET_ARG0 as i32 + ends as i32 * 8)];
        l0:
            movq [rdi + rax * 8], (val);
            subq rax, 1;
            jne  l0;
        l1:
        };
    }

    fn epilogue(&mut self) {
        monoasm!(self.jit,
            leave;
            ret;
        );
    }

    fn load_binary_args(&mut self, lhs: SlotId, rhs: SlotId) {
        monoasm!(self.jit,
            movq rdi, [rbp - (conv(lhs))];
            movq rsi, [rbp - (conv(rhs))];
        );
    }

    fn xmm_save(&mut self, xmm_using: &[Xmm]) {
        let len = xmm_using.len();
        if len == 0 {
            return;
        }
        let sp_offset = (len + len % 2) * 8;
        monoasm!(self.jit,
            subq rsp, (sp_offset);
        );
        for (i, freg) in xmm_using.iter().enumerate() {
            monoasm!(self.jit,
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
            monoasm!(self.jit,
                movq xmm(freg.enc()), [rsp + (8 * i)];
            );
        }
        monoasm!(self.jit,
            addq rsp, (sp_offset);
        );
    }
}
