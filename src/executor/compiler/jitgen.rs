use monoasm_macro::monoasm;

use super::*;

mod analysis;
mod binary_op;
mod compile;
mod constants;
mod method_call;

//
// Just-in-time compiler module.
//

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
    XmmRW(u16),
    ///
    /// Linked to an xmm register but we can only read.
    ///
    XmmR(u16),
    ///
    /// No linkage with any xmm regiter.
    ///
    None,
}

///
/// Context of the current Basic block.
///
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct BBContext {
    /// information for stack slots.
    stack_slot: StackSlotInfo,
    /// information for xmm registers.
    xmm: [Vec<SlotId>; 14],
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
                LinkMode::XmmR(x) => Some(format!("%{i}:R({}) ", x)),
                LinkMode::XmmRW(x) => Some(format!("%{i}:RW({}) ", x)),
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

    fn merge_entries(entries: &Vec<BranchEntry>) -> Self {
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

type WriteBack = Vec<(u16, Vec<SlotId>)>;
type UsingXmm = Vec<usize>;

impl BBContext {
    fn new(reg_num: usize) -> Self {
        let xmm = (0..14)
            .map(|_| vec![])
            .collect::<Vec<Vec<SlotId>>>()
            .try_into()
            .unwrap();
        Self {
            stack_slot: StackSlotInfo(vec![LinkMode::None; reg_num]),
            xmm,
        }
    }

    fn from(slot_info: &StackSlotInfo) -> Self {
        let mut ctx = Self::new(slot_info.0.len());
        for (i, mode) in slot_info.0.iter().enumerate() {
            let reg = SlotId(i as u16);
            match mode {
                LinkMode::None => {}
                LinkMode::XmmR(x) => {
                    ctx.stack_slot[reg] = LinkMode::XmmR(*x);
                    ctx.xmm[*x as usize].push(reg);
                }
                LinkMode::XmmRW(x) => {
                    ctx.stack_slot[reg] = LinkMode::XmmRW(*x);
                    ctx.xmm[*x as usize].push(reg);
                }
            }
        }
        ctx
    }

    fn remove_unused(&mut self, unused: &Vec<SlotId>) {
        unused.iter().for_each(|reg| self.dealloc_xmm(*reg));
    }

    ///
    /// Allocate a new xmm register.
    ///
    fn alloc_xmm(&mut self) -> u16 {
        for (flhs, xmm) in self.xmm.iter_mut().enumerate() {
            if xmm.is_empty() {
                return flhs as u16;
            }
        }
        unreachable!()
    }

    fn link_rw_xmm(&mut self, reg: SlotId, freg: u16) {
        self.stack_slot[reg] = LinkMode::XmmRW(freg as u16);
        self.xmm[freg as usize].push(reg);
    }

    fn link_r_xmm(&mut self, reg: SlotId, freg: u16) {
        self.stack_slot[reg] = LinkMode::XmmR(freg as u16);
        self.xmm[freg as usize].push(reg);
    }

    ///
    /// Deallocate an xmm register corresponding to the stack slot *reg*.
    ///
    fn dealloc_xmm(&mut self, reg: SlotId) {
        match self.stack_slot[reg] {
            LinkMode::XmmR(freg) | LinkMode::XmmRW(freg) => {
                assert!(self.xmm[freg as usize].contains(&reg));
                self.xmm[freg as usize].retain(|e| *e != reg);
                self.stack_slot[reg] = LinkMode::None;
            }
            LinkMode::None => {}
        }
    }

    fn xmm_swap(&mut self, l: u16, r: u16) {
        self.xmm.swap(l as usize, r as usize);
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
    fn xmm_write(&mut self, reg: SlotId) -> u16 {
        if let LinkMode::XmmRW(freg) = self.stack_slot[reg] {
            if self.xmm[freg as usize].len() == 1 {
                assert_eq!(reg, self.xmm[freg as usize][0]);
                return freg;
            }
        };
        self.dealloc_xmm(reg);
        let freg = self.alloc_xmm();
        self.link_rw_xmm(reg, freg);
        freg
    }

    ///
    /// Allocate new xmm register to the given stack slot for read f64.
    ///
    fn alloc_xmm_read(&mut self, reg: SlotId) -> u16 {
        match self.stack_slot[reg] {
            LinkMode::None => {
                let freg = self.alloc_xmm();
                self.link_r_xmm(reg, freg);
                freg
            }
            _ => unreachable!(),
        }
    }

    ///
    /// Copy *src* to *dst*.
    ///
    fn copy_slot(&mut self, codegen: &mut Codegen, src: SlotId, dst: SlotId) {
        self.dealloc_xmm(dst);
        match self.stack_slot[src] {
            LinkMode::XmmRW(freg) | LinkMode::XmmR(freg) => {
                self.link_rw_xmm(dst, freg);
            }
            LinkMode::None => {
                monoasm!(codegen.jit,
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
    fn read_slot(&mut self, codegen: &mut Codegen, reg: SlotId) {
        if let LinkMode::XmmRW(freg) = self.stack_slot[reg] {
            let f64_to_val = codegen.f64_to_val;
            monoasm!(codegen.jit,
                movq xmm0, xmm(freg as u64 + 2);
                call f64_to_val;
            );
            codegen.store_rax(reg);
            self.stack_slot[reg] = LinkMode::XmmR(freg);
        }
    }

    fn write_back_range(&mut self, codegen: &mut Codegen, arg: SlotId, len: u16) {
        for reg in arg.0..arg.0 + len {
            self.read_slot(codegen, SlotId::new(reg))
        }
    }

    fn get_write_back(&self) -> WriteBack {
        self.xmm
            .iter()
            .enumerate()
            .filter_map(|(i, v)| {
                if v.is_empty() {
                    None
                } else {
                    let v: Vec<_> = self.xmm[i]
                        .iter()
                        .filter(|reg| matches!(self.stack_slot[**reg], LinkMode::XmmRW(_)))
                        .cloned()
                        .collect();
                    Some((i as u16, v))
                }
            })
            .filter(|(_, v)| !v.is_empty())
            .collect()
    }

    fn get_xmm_using(&self) -> Vec<usize> {
        self.xmm
            .iter()
            .enumerate()
            .filter_map(|(i, v)| if v.is_empty() { None } else { Some(i) })
            .collect()
    }
}

#[derive(Debug)]
struct BranchEntry {
    src_idx: usize,
    bbctx: BBContext,
    dest_label: DestLabel,
}

struct CompileContext {
    labels: HashMap<usize, DestLabel>,
    /// (bb_id, Vec<src_idx>)
    bb_info: Vec<Option<(usize, Vec<usize>)>>,
    bb_pos: usize,
    loop_count: usize,
    is_loop: bool,
    branch_map: HashMap<usize, Vec<BranchEntry>>,
    backedge_map: HashMap<usize, (DestLabel, StackSlotInfo, Vec<SlotId>)>,
    start_codepos: usize,
    #[cfg(feature = "emit-asm")]
    sourcemap: Vec<(usize, usize)>,
}

impl CompileContext {
    fn new(func: &RubyFuncInfo, codegen: &mut Codegen, start_pos: usize, is_loop: bool) -> Self {
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
            #[cfg(feature = "emit-asm")]
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

#[derive(PartialEq)]
enum BinOpMode {
    RR(SlotId, SlotId),
    RI(SlotId, i16),
    IR(i16, SlotId),
}

#[cfg(feature = "log-jit")]
extern "C" fn log_deoptimize(
    _interp: &mut Interp,
    globals: &mut Globals,
    func_id: FuncId,
    pc: BcPc,
    v: Value,
) {
    let name = match globals.func[func_id].as_ruby_func().name() {
        Some(name) => name.to_string(),
        None => "<unnamed>".to_string(),
    };
    let bc_begin = globals.func[func_id].as_ruby_func().get_bytecode_address(0);
    let index = pc - bc_begin;
    let fmt = pc.format(globals, index).unwrap_or_default();
    if let BcOp::LoopEnd = pc.op1() {
        eprint!("<-- exited from JIT code in {} {:?}.", name, func_id);
        eprintln!("    [{:05}] {}", index, fmt);
    } else if let BcOp::ClassDef { .. } = pc.op1() {
        eprint!("<-- deoptimization occurs in {} {:?}.", name, func_id);
        eprintln!("    [{:05}] {}", index, fmt);
    } else {
        eprint!("<-- deoptimization occurs in {} {:?}.", name, func_id);
        eprintln!("    [{:05}] {} caused by {:?}", index, fmt, v);
    }
}

impl Codegen {
    fn xmm_read_assume(
        &mut self,
        ctx: &mut BBContext,
        rhs: SlotId,
        class: ClassId,
        pc: BcPc,
    ) -> u16 {
        match class {
            INTEGER_CLASS => self.xmm_read_assume_integer(ctx, rhs, pc),
            FLOAT_CLASS => self.xmm_read_assume_float(ctx, rhs, pc),
            _ => unreachable!(),
        }
    }

    fn xmm_read_assume_float(&mut self, ctx: &mut BBContext, reg: SlotId, pc: BcPc) -> u16 {
        match ctx.stack_slot[reg] {
            LinkMode::XmmR(freg) | LinkMode::XmmRW(freg) => freg,
            _ => {
                let freg = ctx.alloc_xmm_read(reg);
                let side_exit = self.gen_side_deopt_dest(pc, ctx);
                monoasm!(self.jit,
                    movq rdi, [rbp - (conv(reg))];
                );
                self.gen_val_to_f64_assume_float(freg as u64 + 2, side_exit);
                freg
            }
        }
    }

    fn xmm_read_assume_integer(&mut self, ctx: &mut BBContext, reg: SlotId, pc: BcPc) -> u16 {
        match ctx.stack_slot[reg] {
            LinkMode::XmmR(freg) | LinkMode::XmmRW(freg) => freg,
            _ => {
                let freg = ctx.alloc_xmm_read(reg);
                let side_exit = self.gen_side_deopt_dest(pc, ctx);
                monoasm!(self.jit,
                    movq rdi, [rbp - (conv(reg))];
                );
                self.gen_val_to_f64_assume_integer(freg as u64 + 2, side_exit);
                freg
            }
        }
    }

    fn xmm_read_binary(
        &mut self,
        ctx: &mut BBContext,
        lhs: SlotId,
        rhs: SlotId,
        pc: BcPc,
    ) -> (u16, u16) {
        if lhs != rhs {
            (
                self.xmm_read_assume(ctx, lhs, pc.classid1(), pc),
                self.xmm_read_assume(ctx, rhs, pc.classid2(), pc),
            )
        } else {
            let lhs = self.xmm_read_assume(ctx, lhs, pc.classid1(), pc);
            (lhs, lhs)
        }
    }
}

impl Codegen {
    ///
    /// Type guard.
    ///
    /// Generate type guard for *class_id*.
    /// If the type was not matched, deoptimize and go to *side_exit*.
    ///
    /// ### in
    ///
    /// - rdi: Value
    ///
    /// ### out
    ///
    /// - rdi: Value
    ///
    /// ### registers destroyed
    ///
    /// - rax
    ///
    fn guard_class(&mut self, class_id: ClassId, side_exit: DestLabel) {
        match class_id {
            INTEGER_CLASS => {
                let exit = self.jit.label();
                monoasm!(self.jit,
                    testq rdi, 0b001;
                    jnz exit;
                );
                self.guard_unpacked_class(class_id, side_exit);
                self.jit.bind_label(exit);
            }
            FLOAT_CLASS => {
                let exit = self.jit.label();
                monoasm!(self.jit,
                    testq rdi, 0b001;
                    jnz side_exit;
                    testq rdi, 0b010;
                    jnz exit;
                );
                self.guard_unpacked_class(class_id, side_exit);
                self.jit.bind_label(exit);
            }
            NIL_CLASS => {
                monoasm!(self.jit,
                    cmpq rdi, (NIL_VALUE);
                    jnz side_exit;
                );
            }
            SYMBOL_CLASS => {
                monoasm!(self.jit,
                    cmpb rdi, (TAG_SYMBOL);
                    jnz side_exit;
                );
            }
            TRUE_CLASS => {
                monoasm!(self.jit,
                    cmpq rdi, (TRUE_VALUE);
                    jnz side_exit;
                );
            }
            FALSE_CLASS => {
                monoasm!(self.jit,
                    cmpq rdi, (FALSE_VALUE);
                    jnz side_exit;
                );
            }
            _ => self.guard_unpacked_class(class_id, side_exit),
        }
    }

    fn guard_unpacked_class(&mut self, class_id: ClassId, side_exit: DestLabel) {
        monoasm!(self.jit,
            testq rdi, 0b111;
            jnz side_exit;
            cmpl [rdi + 4], (class_id.0);
            jne side_exit;
        )
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn guard_class() {
        let mut gen = Codegen::new(false, Value::new_object(OBJECT_CLASS));
        let side_exit = gen.entry_panic;

        for (class, value) in [
            (INTEGER_CLASS, Value::new_integer(-2558)),
            (INTEGER_CLASS, Value::new_integer(i64::MAX)),
            (INTEGER_CLASS, Value::new_integer(i64::MIN)),
            (FLOAT_CLASS, Value::new_float(1.44e-17)),
            (FLOAT_CLASS, Value::new_float(0.0)),
            (FLOAT_CLASS, Value::new_float(f64::MAX)),
            (FLOAT_CLASS, Value::new_float(f64::MIN)),
            (NIL_CLASS, Value::nil()),
        ] {
            let entry_point = gen.jit.get_current_address();
            gen.guard_class(class, side_exit);
            monoasm!(gen.jit,
                xorq rax, rax;
                ret;
            );
            gen.jit.finalize();

            let func: fn(Value) -> u64 = unsafe { std::mem::transmute(entry_point.as_ptr()) };
            assert_eq!(0, func(value));
        }
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

    fn jit_load_ivar(
        &mut self,
        ctx: &BBContext,
        id: IdentId,
        ret: SlotId,
        cached_class: ClassId,
        cached_ivarid: IvarId,
    ) {
        assert!(!cached_class.is_always_frozen());
        let slow_path = self.jit.label();
        let exit = self.jit.label();
        let xmm_using = ctx.get_xmm_using();
        monoasm!(self.jit,
            movq rdi, [rbp - (OFFSET_SELF)];  // base: Value
        );
        self.guard_class(cached_class, slow_path);
        if cached_ivarid.get() < OBJECT_INLINE_IVAR as u32 {
            let no_inline = self.jit.label();
            monoasm!(self.jit,
                xorq rax, rax;
                movw rax, [rdi + 2];    // ObjKind
                cmpq rax, (ObjKind::OBJECT);
                jne  no_inline;
                movq rax, [rdi + (16 + (cached_ivarid.get() as i32) * 8)];
                jmp exit;
            no_inline:
            );
        }
        self.xmm_save(&xmm_using);
        monoasm!(self.jit,
            movl rsi, (cached_ivarid.get());
            movq rax, (RValue::get_ivar);
            call rax;
        );
        self.xmm_restore(&xmm_using);
        monoasm!(self.jit,
            jmp exit;
        );

        self.jit.bind_label(slow_path);
        self.xmm_save(&xmm_using);
        monoasm!(self.jit,
            movq rsi, (id.get());  // id: IdentId
            movq rdx, r12; // &mut Globals
            movq rax, (get_instance_var);
            call rax;
        );
        self.xmm_restore(&xmm_using);
        self.jit.bind_label(exit);
        self.store_rax(ret);
    }

    fn jit_store_ivar(
        &mut self,
        ctx: &BBContext,
        id: IdentId,
        src: SlotId,
        pc: BcPc,
        cached_class: ClassId,
        cached_ivarid: IvarId,
    ) {
        assert!(!cached_class.is_always_frozen());
        let slow_path = self.jit.label();
        let exit = self.jit.label();
        let xmm_using = ctx.get_xmm_using();
        monoasm!(self.jit,
            movq rdi, [rbp - (OFFSET_SELF)];  // base: Value
        );
        self.guard_class(cached_class, slow_path);
        if cached_ivarid.get() < OBJECT_INLINE_IVAR as u32 {
            let no_inline = self.jit.label();
            monoasm!(self.jit,
                xorq rax, rax;
                movw rax, [rdi + 2];
                cmpq rax, (ObjKind::OBJECT);
                jne  no_inline;
                movq rax, [rbp - (conv(src))];   // val: Value
                movq [rdi + (16 + (cached_ivarid.get() as i32) * 8)], rax;
                movq rax, (NIL_VALUE);
                jmp exit;
            no_inline:
            );
        }

        self.xmm_save(&xmm_using);
        monoasm!(self.jit,
            movl rsi, (cached_ivarid.get());
            movq rdx, [rbp - (conv(src))];   // val: Value
            movq rax, (RValue::set_ivar);
            call rax;
            movq rax, (NIL_VALUE);
        );
        self.xmm_restore(&xmm_using);
        monoasm!(self.jit,
            jmp exit;
        );

        self.jit.bind_label(slow_path);
        self.xmm_save(&xmm_using);
        monoasm!(self.jit,
            movq rsi, rdi;  // base: Value
            movq rdx, (id.get());  // id: IdentId
            movq rcx, [rbp - (conv(src))];   // val: Value
            movq rdi, r12; //&mut Globals
            movq rax, (set_instance_var);
            call rax;
        );
        self.xmm_restore(&xmm_using);
        self.jit.bind_label(exit);
        self.handle_error(pc);
    }

    fn jit_get_index(&mut self, ret: SlotId, base: SlotId, idx: SlotId, pc: BcPc, ctx: &BBContext) {
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(&xmm_using);
        monoasm! { self.jit,
            movq rdx, [rbp - (conv(base))]; // base: Value
            movq rcx, [rbp - (conv(idx))]; // idx: Value
            movq rdi, rbx; // &mut Interp
            movq rsi, r12; // &mut Globals
            movq rax, (get_index);
            call rax;
        };
        self.xmm_restore(&xmm_using);
        self.handle_error(pc);
        self.store_rax(ret);
    }

    fn jit_index_assign(
        &mut self,
        src: SlotId,
        base: SlotId,
        idx: SlotId,
        pc: BcPc,
        ctx: &BBContext,
    ) {
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(&xmm_using);
        monoasm! { self.jit,
            movq rdx, [rbp - (conv(base))]; // base: Value
            movq rcx, [rbp - (conv(idx))]; // idx: Value
            movq r8, [rbp - (conv(src))];  // src: Value
            movq rdi, rbx; // &mut Interp
            movq rsi, r12; // &mut Globals
            movq rax, (set_index);
            call rax;
        };
        self.xmm_restore(&xmm_using);
        self.handle_error(pc);
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

    fn gen_write_back_for_target(
        &mut self,
        mut src_ctx: BBContext,
        target_ctx: &BBContext,
        pc: BcPc,
    ) {
        #[cfg(feature = "emit-tir")]
        {
            eprintln!("      src:    {:?}", src_ctx.stack_slot);
            eprintln!("      target: {:?}", target_ctx.stack_slot);
        }
        let len = src_ctx.stack_slot.0.len();

        for i in 0..len {
            let reg = SlotId(i as u16);
            if target_ctx.stack_slot[reg] == LinkMode::None {
                match src_ctx.stack_slot[reg] {
                    LinkMode::XmmRW(freg) => {
                        let v = src_ctx.xmm[freg as usize].clone();
                        for i in &v {
                            src_ctx.stack_slot[*i] = LinkMode::XmmR(freg);
                        }
                        src_ctx.dealloc_xmm(reg);
                        self.gen_write_back_single(freg, v);
                    }
                    LinkMode::XmmR(_) => {
                        src_ctx.dealloc_xmm(reg);
                    }
                    _ => {}
                }
            };
        }

        let mut conv_list = vec![];
        let mut guard_list = vec![];
        for i in 0..len {
            let reg = SlotId(i as u16);
            match (src_ctx.stack_slot[reg], target_ctx.stack_slot[reg]) {
                (LinkMode::XmmRW(l), LinkMode::XmmRW(r)) => {
                    if l == r {
                        src_ctx.stack_slot[reg] = LinkMode::XmmRW(l);
                    } else if src_ctx.xmm[r as usize].is_empty() {
                        monoasm!(self.jit,
                            movq  xmm(r as u64 + 2), xmm(l as u64 + 2);
                        );
                        src_ctx.dealloc_xmm(reg);
                        src_ctx.link_rw_xmm(reg, r);
                    } else {
                        src_ctx.xmm_swap(l, r);
                        monoasm!(self.jit,
                            movq  xmm0, xmm(l as u64 + 2);
                            movq  xmm(l as u64 + 2), xmm(r as u64 + 2);
                            movq  xmm(r as u64 + 2), xmm0;
                        );
                    }
                }
                (LinkMode::XmmR(l), LinkMode::XmmRW(r)) => {
                    if l == r {
                        src_ctx.stack_slot[reg] = LinkMode::XmmRW(l);
                    } else if src_ctx.xmm[r as usize].is_empty() {
                        monoasm!(self.jit,
                            movq  xmm(r as u64 + 2), xmm(l as u64 + 2);
                        );
                        src_ctx.dealloc_xmm(reg);
                        src_ctx.link_rw_xmm(reg, r);
                    } else {
                        src_ctx.xmm_swap(l, r);
                        monoasm!(self.jit,
                            movq  xmm0, xmm(l as u64 + 2);
                            movq  xmm(l as u64 + 2), xmm(r as u64 + 2);
                            movq  xmm(r as u64 + 2), xmm0;
                        );
                    }
                    guard_list.push(reg);
                }
                (_, LinkMode::None) => {}
                (LinkMode::XmmRW(l), LinkMode::XmmR(r)) => {
                    self.gen_write_back_single(l, vec![reg]);
                    if l == r {
                        src_ctx.stack_slot[reg] = LinkMode::XmmR(l);
                    } else if src_ctx.xmm[r as usize].is_empty() {
                        monoasm!(self.jit,
                            movq  xmm(r as u64 + 2), xmm(l as u64 + 2);
                        );
                        src_ctx.dealloc_xmm(reg);
                        src_ctx.link_r_xmm(reg, r);
                    } else {
                        src_ctx.xmm_swap(l, r);
                        monoasm!(self.jit,
                            movq  xmm0, xmm(l as u64 + 2);
                            movq  xmm(l as u64 + 2), xmm(r as u64 + 2);
                            movq  xmm(r as u64 + 2), xmm0;
                        );
                    }
                }
                (LinkMode::XmmR(l), LinkMode::XmmR(r)) => {
                    if l == r {
                        src_ctx.stack_slot[reg] = LinkMode::XmmR(l);
                    } else if src_ctx.xmm[r as usize].is_empty() {
                        monoasm!(self.jit,
                            movq  xmm(r as u64 + 2), xmm(l as u64 + 2);
                        );
                        src_ctx.dealloc_xmm(reg);
                        src_ctx.link_r_xmm(reg, r);
                    } else {
                        src_ctx.xmm_swap(l, r);
                        monoasm!(self.jit,
                            movq  xmm0, xmm(l as u64 + 2);
                            movq  xmm(l as u64 + 2), xmm(r as u64 + 2);
                            movq  xmm(r as u64 + 2), xmm0;
                        );
                    }
                }
                (LinkMode::None, LinkMode::XmmR(r)) => {
                    src_ctx.link_r_xmm(reg, r);
                    conv_list.push((reg, r));
                }
                _ => unreachable!(),
            }
        }
        #[cfg(feature = "emit-tir")]
        eprintln!("      src_end:   {:?}", src_ctx.stack_slot);

        let side_exit = self.gen_side_deopt_dest(pc + 1, &src_ctx);
        for (reg, freg) in conv_list {
            monoasm!(self.jit,
                movq rdi, [rbp - (conv(reg))];
            );
            self.gen_val_to_f64(freg as u64 + 2, side_exit);
            #[cfg(feature = "emit-tir")]
            eprintln!("      conv: {:?}->{:?}", reg, freg);
        }
        for reg in guard_list {
            self.gen_assume_float(reg, side_exit);
        }
    }

    fn gen_write_back_single(&mut self, freg: u16, v: Vec<SlotId>) {
        if v.len() == 0 {
            return;
        }
        #[cfg(feature = "emit-tir")]
        eprintln!("      wb: {:?}->{:?}", freg, v);
        let f64_to_val = self.f64_to_val;
        monoasm!(self.jit,
            movq xmm0, xmm(freg as u64 + 2);
            call f64_to_val;
        );
        for reg in v {
            self.store_rax(reg);
        }
    }

    ///
    /// Get *DestLabel* for fallback to interpreter.
    ///
    fn gen_side_deopt_dest(&mut self, pc: BcPc, ctx: &BBContext) -> DestLabel {
        let wb = ctx.get_write_back();
        let old_p = self.jit.get_page();
        self.jit.select_page(2);
        let entry = self.jit.label();
        self.jit.bind_label(entry);
        if wb.len() != 0 {
            #[cfg(feature = "emit-tir")]
            eprintln!("--gen deopt");
            self.gen_write_back(wb);
            #[cfg(feature = "emit-tir")]
            eprintln!("--gen deopt end");
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
        self.jit.select_page(old_p);
        entry
    }

    ///
    /// Fallback to interpreter after Writing back all linked xmms.
    ///
    fn deopt(&mut self, ctx: &BBContext, pc: BcPc) {
        let fallback = self.gen_side_deopt_dest(pc, ctx);
        monoasm!(self.jit,
            jmp fallback;
        );
    }

    pub(super) fn jit_compile_ruby(
        &mut self,
        globals: &Globals,
        func_id: FuncId,
        position: Option<usize>,
    ) -> DestLabel {
        let func = globals.func[func_id].as_ruby_func();
        let start_pos = position.unwrap_or_default();

        #[cfg(any(feature = "emit-asm", feature = "log-jit", feature = "emit-tir"))]
        {
            eprintln!(
                "--> start {} compile: {} {:?} start:[{:05}] bytecode:{:?}",
                if position.is_some() {
                    "partial"
                } else {
                    "whole"
                },
                match func.name() {
                    Some(name) => name,
                    None => "<unnamed>",
                },
                func.id,
                start_pos,
                func.bytecode().as_ptr(),
            );
        }
        #[cfg(any(feature = "emit-asm", feature = "log-jit"))]
        let now = Instant::now();

        let entry = self.jit.label();
        self.jit.bind_label(entry);

        let mut cc = CompileContext::new(func, self, start_pos, position.is_some());
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
        cc.start_codepos = self.jit.get_current();

        if position.is_none() {
            self.prologue(func.total_reg_num(), func.total_arg_num());
        }

        cc.branch_map.insert(
            start_pos,
            vec![BranchEntry {
                src_idx: 0,
                bbctx: BBContext::new(reg_num),
                dest_label: self.jit.label(),
            }],
        );
        for i in bb_start_pos {
            cc.bb_pos = i;
            if self.compile_bb(globals, func, &mut cc) {
                break;
            };
        }

        let keys: Vec<_> = cc.branch_map.keys().cloned().collect();
        for pos in keys.into_iter() {
            self.gen_backedge_branch(&mut cc, func, pos);
        }

        self.jit.finalize();

        #[cfg(any(feature = "emit-asm", feature = "log-jit"))]
        let elapsed = now.elapsed();
        //#[cfg(feature = "emit-tir")]
        //eprintln!("{:?}", cc.tir);
        #[cfg(any(feature = "emit-asm"))]
        self.dump_disas(globals, &cc, func);
        #[cfg(any(feature = "emit-asm", feature = "log-jit"))]
        eprintln!("    finished compile. elapsed:{:?}", elapsed);
        #[cfg(feature = "emit-tir")]
        eprintln!("    finished compile.");

        entry
    }

    fn prologue(&mut self, regs: usize, args: usize) {
        let offset = (regs * 8 + OFFSET_SELF as usize + 15) & !0xf;
        let clear_len = regs - args;
        monoasm!(self.jit,
            pushq rbp;
            movq rbp, rsp;
            subq rsp, (offset);
        );
        //
        //       +-------------+
        //       | return addr |
        //       +-------------+
        //       |   old rbp   |
        //       +-------------+
        //       |    meta     |
        //       +-------------+
        //       |     %0      |
        //       +-------------+
        //       |     %1      |
        //       +-------------+
        //       |     %2      |
        //       +-------------+
        //       |     %3      |
        //       +-------------+
        // rsp-> |             |
        //       +-------------+
        if clear_len > 2 {
            monoasm!(self.jit,
                movq rax, (NIL_VALUE);
            );
            for i in 0..clear_len {
                monoasm!(self.jit,
                    movq [rbp - ((args + i) as i32 * 8 + (OFFSET_SELF))], rax;
                );
            }
        } else {
            for i in 0..clear_len {
                monoasm!(self.jit,
                    movq [rbp - ((args + i) as i32 * 8 + (OFFSET_SELF))], (NIL_VALUE);
                );
            }
        }
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

    fn xmm_save(&mut self, xmm_using: &Vec<usize>) {
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
                movq [rsp + (8 * i)], xmm(*freg as u64 + 2);
            );
        }
    }

    fn xmm_restore(&mut self, xmm_using: &Vec<usize>) {
        let len = xmm_using.len();
        if len == 0 {
            return;
        }
        let sp_offset = (len + len % 2) * 8;
        for (i, freg) in xmm_using.iter().enumerate() {
            monoasm!(self.jit,
                movq xmm(*freg as u64 + 2), [rsp + (8 * i)];
            );
        }
        monoasm!(self.jit,
            addq rsp, (sp_offset);
        );
    }
}

#[cfg(any(feature = "emit-asm"))]
impl Codegen {
    fn dump_disas(&mut self, globals: &Globals, cc: &CompileContext, func: &RubyFuncInfo) {
        let (start, code_end, end) = self.jit.code_block.last().unwrap();
        eprintln!(
            "offset:{:?} code: {} bytes  data: {} bytes",
            start,
            *code_end - *start,
            *end - *code_end
        );
        self.jit.select_page(0);
        let dump: Vec<(usize, String)> = self
            .jit
            .dump_code()
            .unwrap()
            .split('\n')
            .filter(|s| s.len() >= 29)
            .map(|x| {
                (
                    usize::from_str_radix(&x[0..4].trim(), 16).unwrap(),
                    x[28..].to_string(),
                )
            })
            .collect();
        for (i, text) in dump {
            cc.sourcemap
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
                    let pc = BcPc::from(&func.bytecode()[bc_pos]);
                    eprintln!(
                        ":{:05} {}",
                        bc_pos,
                        match pc.format(globals, bc_pos) {
                            Some(s) => s,
                            None => "".to_string(),
                        }
                    );
                });

            eprintln!("  {:05x}: {}", i, text);
        }
    }
}
