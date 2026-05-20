use super::*;

/// Diagnostics: cumulative count / bytes of heap-frame promotions
/// (`MONORUBY_FRAME_STATS=1` prints them at exit). The buffers are no
/// longer leaked — they are registered with the GC and reclaimed by
/// `Allocator::sweep_heap_frames` once unreachable.
pub(crate) static FRAME_PROMOTIONS: std::sync::atomic::AtomicU64 =
    std::sync::atomic::AtomicU64::new(0);
pub(crate) static FRAME_LEAK_BYTES: std::sync::atomic::AtomicU64 =
    std::sync::atomic::AtomicU64::new(0);

/// Bump the promotion counters and hand the `Box<[u64]>` raw parts to
/// the GC frame registry. Registration uses `try_borrow_mut` so it is
/// a safe no-op (the frame simply stays un-reclaimed, as before) if
/// the allocator is already borrowed — promotion never runs during a
/// GC cycle, so this is purely defensive.
#[inline]
fn register_promoted_frame(lfp_addr: usize, base: *mut u64, len_u64: usize) {
    use std::sync::atomic::Ordering::Relaxed;
    FRAME_PROMOTIONS.fetch_add(1, Relaxed);
    FRAME_LEAK_BYTES.fetch_add((len_u64 * 8) as u64, Relaxed);
    let _ = alloc::ALLOC.try_with(|a| {
        if let Ok(mut g) = a.try_borrow_mut() {
            g.register_heap_frame(lfp_addr, base, len_u64);
        }
    });
}

/// `(count, bytes)` of heap frames promoted so far (diagnostics).
pub fn frame_leak_stats() -> (u64, u64) {
    use std::sync::atomic::Ordering::Relaxed;
    (
        FRAME_PROMOTIONS.load(Relaxed),
        FRAME_LEAK_BYTES.load(Relaxed),
    )
}

///
/// Control frame pointer.
///
/// CFP points to a control frame which corresponds to an each function call.
/// The control frame contains a CFP of the previous control frame, LFP, and a base pointer.
///
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct Cfp(std::ptr::NonNull<Option<Cfp>>);

impl Cfp {
    ///
    /// Create new CFP from a raw pointer.
    ///
    /// ### safety
    /// This function is extremely dangerous. Programmer must ensure that *ptr* is a valid pointer which pointes to a control frame.
    ///
    unsafe fn new(ptr: *mut u8) -> Self {
        Cfp(std::ptr::NonNull::new(ptr as *mut Option<Cfp>).unwrap())
    }

    ///
    /// Get inner raw pointer.
    ///
    fn as_ptr(&self) -> *const Option<Cfp> {
        self.0.as_ptr()
    }

    ///
    /// Get a previous control frame of *self*.
    ///
    pub fn prev(&self) -> Option<Self> {
        unsafe { *self.as_ptr() }
    }

    ///
    /// Get base pointer address of *self*.
    ///
    pub unsafe fn return_addr(&self) -> Option<monoasm::CodePtr> {
        unsafe { *(self.as_ptr() as *const Option<monoasm::CodePtr>).add(1 + BP_CFP as usize / 8) }
    }

    ///
    /// Get LFP.
    ///
    pub(crate) fn lfp(&self) -> Lfp {
        unsafe { *(self.as_ptr().sub(CFP_LFP as usize / 8) as *mut Lfp) }
    }

    ///
    /// Get outermost LFP.
    ///
    pub(crate) fn outermost_lfp(&self) -> Lfp {
        self.lfp().outermost().0
    }

    ///
    /// Get outermost LFP and the depth.
    ///
    pub(crate) fn outermost_lfp_depth(&self) -> (Lfp, usize) {
        self.lfp().outermost()
    }

    pub(crate) fn block_given(&self) -> bool {
        self.outermost_lfp().block().is_some()
    }

    pub(crate) fn caller(&self) -> Option<Cfp> {
        let target_lfp = self.lfp().outer()?;
        let mut cfp = *self;
        loop {
            let cfp_prev = cfp.prev()?;
            if cfp_prev.lfp() == target_lfp {
                return Some(cfp);
            }
            cfp = cfp_prev;
        }
    }

    ///
    /// Set LFP.
    ///
    pub unsafe fn set_lfp(&mut self, lfp: Lfp) {
        unsafe { *(self.as_ptr().sub(CFP_LFP as usize / 8) as *mut Lfp) = lfp };
    }

    ///
    /// Get *FuncId* of a current method / classdef.
    ///
    pub fn method_func_id(&self) -> FuncId {
        self.outermost_lfp().func_id()
    }

    pub fn method_func_id_depth(&self) -> (FuncId, usize) {
        let (lfp, depth) = self.outermost_lfp_depth();
        (lfp.func_id(), depth)
    }

    ///
    /// Get *FuncId* of a current position in the source code.
    ///
    pub fn get_source_pos(&self) -> FuncId {
        let mut cfp = Some(*self);
        while let Some(inner_cfp) = cfp {
            if !inner_cfp.lfp().meta().is_native() {
                return inner_cfp.lfp().func_id();
            }
            cfp = inner_cfp.prev();
        }
        unreachable!("get_source_pos: non-native method not found.")
    }
}

impl Executor {
    ///
    /// Get *BlockHandler* of a current method / classdef.
    ///
    pub fn get_block(&self) -> Option<BlockHandler> {
        let cfp = self.cfp();
        let lfp = cfp.outermost_lfp();
        let bh = lfp.block()?;
        Some(match bh.0.try_fixnum() {
            Some(mut i) => {
                i = Self::traverse_cfp(self, cfp, lfp, i)?;
                BlockHandler::new(Value::integer(i))
            }
            None => bh,
        })
    }

    // TODO: this does not support nested fibers.
    pub fn prev_cfp(vm: &Executor, cfp: Cfp) -> (&Executor, Cfp) {
        match cfp.prev() {
            Some(prev) => (vm, prev),
            None => {
                // SAFETY: parent_fiber is set when a fiber is resumed.
                let vm = unsafe { vm.parent_fiber.unwrap().as_ref() };
                (vm, vm.cfp())
            }
        }
    }

    /// Try to get the previous CFP, returning `None` if there is no previous
    /// frame and no parent fiber (i.e. the Proc's enclosing method has already
    /// returned — "detached context").
    fn try_prev_cfp(vm: &Executor, cfp: Cfp) -> Option<(&Executor, Cfp)> {
        match cfp.prev() {
            Some(prev) => Some((vm, prev)),
            None => {
                // SAFETY: parent_fiber is set when a fiber is resumed.
                let parent = unsafe { vm.parent_fiber?.as_ref() };
                Some((parent, parent.cfp()))
            }
        }
    }

    /// Traverse the CFP chain to find the target LFP.
    /// Returns `None` if the CFP chain is exhausted before finding the target
    /// (detached Proc context — the enclosing method has already returned).
    fn traverse_cfp(mut vm: &Executor, mut cfp: Cfp, lfp: Lfp, mut i: i64) -> Option<i64> {
        loop {
            if cfp.lfp() == lfp {
                return Some(i);
            }
            i += 1;
            (vm, cfp) = Self::try_prev_cfp(vm, cfp)?;
        }
    }
}

///
/// Local frame pointer.
///
/// the LFP points a local frame which contains self value, local variables, given block, and meta data which correspond to an each function call.
/// Most local frames are on the stack, but some local frames are placed on the heap in such situations like closures.
///
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct Lfp(std::ptr::NonNull<u8>);

impl std::ops::Deref for Lfp {
    type Target = std::ptr::NonNull<u8>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::cmp::PartialEq<Cfp> for Lfp {
    fn eq(&self, other: &Cfp) -> bool {
        self.as_ptr() == other.as_ptr() as _
    }
}

impl std::cmp::PartialOrd<Cfp> for Lfp {
    fn partial_cmp(&self, other: &Cfp) -> Option<std::cmp::Ordering> {
        self.as_ptr().partial_cmp(&(other.as_ptr() as _))
    }
}

impl alloc::GC<RValue> for Lfp {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        // A reachable heap frame: record it live this cycle. If it was
        // already marked, the outer chain below it has been walked too
        // (DAG) — stop to keep marking O(n) and cycle-safe.
        if !self.on_stack() && alloc.mark_heap_frame(self.0.as_ptr() as usize) {
            return;
        }
        let meta = self.meta();
        for r in meta.regs() {
            if let Some(v) = self.register(r) {
                v.mark(alloc);
            }
        }
        if let Some(v) = self.block() {
            v.0.mark(alloc)
        };
        // Mark the LFP_SVAR slot if this frame owns one (LEP-style
        // frames lazily allocate a container `Value` here on first
        // `$~ = ...`; zero is the "unset" sentinel). Block-style
        // frames keep zero — they read/write `$~` through their
        // outer-chain LEP, which the recursive `outer.mark` below
        // reaches separately.
        if let Some(v) = self.svar_slot() {
            v.mark(alloc);
        }
        if let Some(outer) = self.outer() {
            outer.mark(alloc);
        }
    }
}

impl Lfp {
    ///
    /// Create LFP from a raw pointer.
    ///
    /// This function is extremely dangerous. Programmer must ensure that *ptr* is a valid pointer which points to a local frame.
    ///
    unsafe fn new(ptr: *mut u8) -> Self {
        Self(std::ptr::NonNull::new(ptr).unwrap())
    }

    ///
    /// Get CFP.
    ///
    /// ### safety
    ///
    /// *self* must be on the stack. otherwise, panic.
    ///
    fn cfp(&self) -> Cfp {
        assert!(self.on_stack());
        unsafe { Cfp::new(self.as_ptr().add(16) as _) }
    }

    ///
    /// Get CFP without checking `on_stack`. Used in the invalidated
    /// stack path of `move_frame_to_heap`, where the LFP pointer is
    /// still a valid stack address (we're within the owner frame's
    /// stack extent) but the meta's on_stack bit has been flipped.
    ///
    /// ### safety
    ///
    /// *self* must point at a valid (possibly invalidated) stack
    /// frame with a well-formed CFP at `self + 16`.
    unsafe fn cfp_unchecked(&self) -> Cfp {
        unsafe { Cfp::new(self.as_ptr().add(16) as _) }
    }

    ///
    /// Get the *FuncId* of the current frame.
    ///
    pub fn func_id(&self) -> FuncId {
        self.meta().func_id()
    }

    ///
    /// Get outer LFP.
    ///
    pub fn outer(self) -> Option<Lfp> {
        unsafe { *(self.0.as_ptr() as *mut Option<Lfp>) }
    }

    ///
    /// Walk the outer chain up to the *Local Environment Pointer* —
    /// the nearest method-introducing frame.
    ///
    /// CRuby's `vm_svar` lives on the LEP, and every frame-local
    /// global (`$~`, `$_`, and the BACK_REF / NTH_REF family derived
    /// from `$~`) reads/writes through it. The walking rule:
    ///
    /// * **Block-style** frames (block literal `{ }` / `do … end`
    ///   and `Proc.new`) `share` their lexical parent's LEP — follow
    ///   `outer` upward.
    /// * **Method-style** frames (`def`, lambda, class/module body,
    ///   toplevel script, `define_method`-installed Proc tagged
    ///   `is_proc_method`) own their LEP — stop.
    ///
    /// The chain is guaranteed to terminate at a method-style frame
    /// (the toplevel script body is method-style with no outer), so
    /// this loop always returns a real LEP.
    pub(crate) fn lep(self) -> Lfp {
        let mut lfp = self;
        loop {
            let meta = lfp.meta();
            if !meta.is_block_style() || meta.is_proc_method() {
                return lfp;
            }
            match lfp.outer() {
                Some(outer) => lfp = outer,
                None => return lfp,
            }
        }
    }

    ///
    /// Read this frame's `LFP_SVAR` slot **without** walking the
    /// outer chain. Returns `None` when the slot still carries the
    /// zero sentinel (no container allocated yet).
    ///
    /// Only the LEP's slot ever holds a meaningful value — block-
    /// style frames keep zero here and resolve `$~` through
    /// `Lfp::svar` / `Lfp::set_svar`. Use this raw accessor only for
    /// the GC mark walker and for the LEP itself.
    fn svar_slot(self) -> Option<Value> {
        let raw = unsafe { *(self.sub(LFP_SVAR as _) as *const u64) };
        if raw == 0 {
            None
        } else {
            Some(Value::from_u64(raw))
        }
    }

    fn set_svar_slot(self, val: Value) {
        unsafe { *(self.sub(LFP_SVAR as _) as *mut u64) = val.id() }
    }

    ///
    /// Read the **LEP's** `LFP_SVAR` slot — the `$~`/`$_` container
    /// (or `None` if no container has been allocated yet).
    ///
    /// Walks the outer chain to the LEP first; calling on a block
    /// frame transparently returns the enclosing method's slot.
    pub(crate) fn svar(self) -> Option<Value> {
        self.lep().svar_slot()
    }

    ///
    /// Write `val` into the **LEP's** `LFP_SVAR` slot.
    ///
    /// Walks the outer chain to the LEP first, so a `$~ = …`
    /// assignment from inside a block lands in the enclosing
    /// method's slot — matching CRuby `vm_svar` semantics.
    pub(crate) fn set_svar(self, val: Value) {
        self.lep().set_svar_slot(val)
    }

    ///
    /// Get the outermost LFP and the depth.
    ///
    /// A frame tagged with `is_proc_method` (the `define_method`-installed
    /// proc body) is its own outermost — even though it has an `outer`
    /// pointer for closure variable access, semantically it's a method
    /// boundary. Stopping there lets `method_func_id` return the actual
    /// method's `FuncId` instead of the enclosing toplevel/class iseq's,
    /// so `super` inside `define_method { ... }` looks up the right name.
    pub(crate) fn outermost(&self) -> (Lfp, usize) {
        let mut lfp = *self;
        let mut depth = 0;
        if lfp.meta().is_proc_method() {
            return (lfp, depth);
        }
        while let Some(outer) = lfp.outer() {
            lfp = outer;
            depth += 1;
            if lfp.meta().is_proc_method() {
                break;
            }
        }
        (lfp, depth)
    }

    pub(crate) fn method_func_id(&self) -> FuncId {
        self.outermost().0.func_id()
    }

    ///
    /// Set outer.
    ///
    pub(crate) fn set_outer(&mut self, outer: Option<Lfp>) {
        unsafe { *(self.0.as_ptr() as *mut Option<Lfp>) = outer }
    }

    fn sub(self, count: i64) -> *mut u8 {
        unsafe { self.as_ptr().sub(count as usize) }
    }

    pub fn on_stack(self) -> bool {
        self.meta().on_stack()
    }

    fn frame_bytes(self) -> usize {
        LFP_SELF as usize + 8 * self.meta().reg_num() as usize
    }

    fn frame_ref(&self) -> &[u8] {
        let len = self.frame_bytes();
        unsafe {
            std::slice::from_raw_parts((self.0.as_ptr() as usize + 8 - len) as *const u8, len)
        }
    }

    ///
    /// Returns an iterator over the arguments of a natve function.
    ///
    /// The iterator yields all values from start to end.
    ///
    /// ### safety
    ///
    /// *self* must be native function. otherwise, panic.
    ///
    fn iter_inner(&self) -> impl DoubleEndedIterator<Item = &Value> {
        let len = self.arg_len();
        unsafe {
            let data = if len == 0 {
                self.0.as_ptr().sub(LFP_ARG0 as usize)
            } else {
                self.0.as_ptr().sub(LFP_ARG0 as usize + len * 8 - 8)
            };
            std::slice::from_raw_parts(data as *const Value, len).iter()
        }
    }

    ///
    /// Move the frame to heap.
    ///
    /// If the frame is already on the heap, do nothing.
    ///
    /// ### args
    /// - *lfp*: the address of the frame to move.
    ///
    /// ### return
    /// - the frame moved to the heap.
    ///
    pub fn move_frame_to_heap(mut self) -> Self {
        if !self.on_stack() {
            return self;
        }
        // Already copied to heap: a caller is holding a stale stack
        // pointer (e.g. `Kernel#loop` cached ProcData.outer before
        // another capture promoted this frame). Forward to the heap
        // copy via `cfp.lfp()`, which was redirected at promotion.
        if self.meta().invalidated() {
            return unsafe { self.cfp_unchecked().lfp() };
        }
        unsafe {
            let mut cfp = self.cfp();
            let len = self.frame_bytes();
            // `frame_bytes` is always a multiple of 8 (LFP_SELF plus
            // 8*reg_num), so copy the frame as a `Box<[u64]>` — uniform
            // with `heap_frame`/`dummy_*` so the GC can reclaim every
            // promoted buffer with one `Box::from_raw(*mut [u64])`.
            let n = len / 8;
            let src = std::slice::from_raw_parts(
                (self.0.as_ptr() as usize + 8 - len) as *const u64,
                n,
            );
            let v: Box<[u64]> = src.to_vec().into_boxed_slice();
            let base = Box::into_raw(v) as *mut u64;
            let lfp_addr = base as usize + len - 8;
            register_promoted_frame(lfp_addr, base, n);
            let mut heap_lfp = Lfp::new(lfp_addr as _);
            heap_lfp.meta_mut().set_on_heap();
            cfp.set_lfp(heap_lfp);
            // Mark the stack slot as a tombstone so any future reader
            // (this function's recursive step, ProcData.outer lookups
            // at block invocation, etc.) forwards to the heap copy
            // instead of re-copying.
            self.meta_mut().set_invalidated();
            if let Some(outer_lfp) = heap_lfp.outer() {
                let outer = outer_lfp.move_frame_to_heap();
                heap_lfp.set_outer(Some(outer));
            }
            assert!(!heap_lfp.on_stack());
            heap_lfp
        }
    }

    pub fn heap_frame(self_val: Value, mut meta: Meta) -> Self {
        let local_len = meta.reg_num() as usize - 1;
        meta.set_on_heap();
        let mut v = vec![Value::nil().id(); local_len];
        // The heap-allocated frame must mirror the stack layout
        // exactly: lfp_addr (= base + len - 8) points at LFP_OUTER,
        // and each LFP_xxx offset reads memory at lfp_addr - LFP_xxx.
        // Push slots in **ascending memory order**, finishing with
        // LFP_OUTER at the highest index = lfp_addr.
        // Order: locals[0..N], self (LFP_SELF), block (LFP_BLOCK),
        // cme (LFP_CME), svar (LFP_SVAR), meta (LFP_META), outer (LFP_OUTER).
        v.push(self_val.id()); // -> LFP_SELF
        v.push(0); //               -> LFP_BLOCK
        v.push(0); //               -> LFP_CME (unused; zero sentinel)
        v.push(0); //               -> LFP_SVAR (unused; zero sentinel)
        v.push(meta.get()); //      -> LFP_META
        v.push(0); //               -> LFP_OUTER (no outer)
        let v = v.into_boxed_slice();
        let n = v.len();
        let len = n * 8;
        unsafe {
            let base = Box::into_raw(v) as *mut u64;
            let lfp_addr = base as usize + len - 8;
            register_promoted_frame(lfp_addr, base, n);
            let heap_lfp = Lfp::new(lfp_addr as _);
            assert!(!heap_lfp.on_stack());
            heap_lfp
        }
    }

    pub fn dummy_heap_frame_with_self(self_val: Value) -> Self {
        // Same slot order as `heap_frame` (locals.., self, block, cme,
        // svar, meta, outer) with zero locals. Eight u64 slots: 1 self
        // + 5 zero header slots (block/cme/svar/meta/outer) padded out
        // to two extra zeros so the LFP layout stays self-consistent
        // with the new (post-SVAR/CME) header size.
        let v: Box<[u64]> = vec![
            0,                  // padding (matches the historical extra slot)
            0,                  // padding
            self_val.id(),      // LFP_SELF
            0,                  // LFP_BLOCK
            0,                  // LFP_CME
            0,                  // LFP_SVAR
            0,                  // LFP_META — set via `set_reg_num` below
            0,                  // LFP_OUTER
        ]
        .into_boxed_slice();
        let n = v.len();
        let len = n * 8;
        unsafe {
            let base = Box::into_raw(v) as *mut u64;
            let lfp_addr = base as usize + len - 8;
            register_promoted_frame(lfp_addr, base, n);
            let mut heap_lfp = Lfp::new(lfp_addr as _);
            heap_lfp.meta_mut().set_on_heap();
            heap_lfp.meta_mut().set_reg_num(1);
            assert!(!heap_lfp.on_stack());
            heap_lfp
        }
    }

    ///
    /// Get Meta.
    ///
    pub(crate) fn meta(&self) -> &Meta {
        unsafe { &*(self.sub(LFP_META as _) as *const Meta) }
    }

    fn meta_mut(&mut self) -> &mut Meta {
        unsafe { &mut *(self.sub(LFP_META as _) as *mut Meta) }
    }

    ///
    /// Get block.
    ///
    pub fn block(&self) -> Option<BlockHandler> {
        let block: Option<BlockHandler> = unsafe { *(self.sub(LFP_BLOCK as _) as *const _) };
        if let Some(BlockHandler(v)) = block
            && v.is_nil()
        {
            return None;
        }
        block
    }

    ///
    /// Set block.
    ///
    pub fn set_block(&self, bh: Option<BlockHandler>) {
        unsafe { *(self.sub(LFP_BLOCK as _) as *mut _) = bh }
    }

    pub fn register_ptr(&self, index: SlotId) -> *mut Option<Value> {
        self.sub(LFP_SELF as i64 + 8 * index.0 as i64) as _
    }

    ///
    /// Get a value of a register slot *index*.
    ///
    pub fn register(&self, index: SlotId) -> Option<Value> {
        unsafe { std::ptr::read(self.register_ptr(index)) }
    }

    pub fn locals(&self, len: usize) -> Vec<Value> {
        let mut v = vec![];
        for i in SlotId(1)..SlotId(1) + len {
            if let Some(val) = self.register(i) {
                v.push(val);
            }
        }
        v
    }

    ///
    /// Set a value to a register *index*.
    ///
    pub(crate) unsafe fn set_register(&mut self, index: SlotId, val: Option<Value>) {
        unsafe { std::ptr::write(self.register_ptr(index), val) };
    }

    pub(crate) unsafe fn args_to_vec(&self, args: SlotId, args_len: usize) -> Vec<Value> {
        let p = self.register_ptr(args) as *const Value;
        let p = if args_len == 0 {
            p
        } else {
            unsafe { p.sub(args_len - 1) }
        };
        let mut v = unsafe { std::slice::from_raw_parts(p, args_len).to_vec() };
        v.reverse();
        v
    }
}

// APIs for native methods.
impl Lfp {
    ///
    /// Get the length of arguments for a native function.
    ///
    /// ### safety
    ///
    /// *self* must be native function. otherwise, panic.
    ///
    pub fn arg_len(&self) -> usize {
        assert!(self.meta().is_native());
        self.meta().reg_num() as usize - 1
    }

    ///
    /// Get *self*.
    ///
    pub fn self_val(&self) -> Value {
        unsafe { *(self.sub(LFP_SELF as _) as *const _) }
    }

    ///
    /// Get the given block.
    ///
    /// If none, return Err.
    ///
    pub fn expect_block(&self) -> Result<BlockHandler> {
        if let Some(block) = self.block() {
            Ok(block)
        } else {
            Err(MonorubyErr::no_block_given())
        }
    }

    ///
    /// Ensure a block was not given.
    ///
    /// If given, return Err.
    ///
    pub fn expect_no_block(&self) -> Result<()> {
        if self.block().is_none() {
            Ok(())
        } else {
            Err(MonorubyErr::runtimeerr(
                "Currently, calling with block is not supported.",
            ))
        }
    }

    pub fn to_vec(&self) -> Vec<Value> {
        self.iter().collect()
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = Value> + '_ {
        self.iter_inner().rev().cloned()
    }

    pub fn rev(&self) -> impl Iterator<Item = Value> + '_ {
        self.iter_inner().cloned()
    }

    /*pub fn slice(&self, start_pos: usize, len: usize) -> impl DoubleEndedIterator<Item = Value> {
        unsafe {
            let ptr = self.register_ptr(start_pos + len);
            std::slice::from_raw_parts(ptr, len)
                .iter()
                .rev()
                .map(|v| v.unwrap())
        }
    }*/

    pub fn arg(&self, i: usize) -> Value {
        self.try_arg(i).unwrap()
    }

    pub fn try_arg(&self, i: usize) -> Option<Value> {
        let v = unsafe { *((self.0.as_ptr().sub(LFP_ARG0 as usize + i * 8)) as *const u64) };
        if v == 0 {
            None
        } else {
            Some(Value::from_u64(v))
        }
    }

    pub fn args_count(&self, max: usize) -> usize {
        for i in 0..max {
            if self.try_arg(i).is_none() {
                return i;
            }
        }
        max
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn heap_frame_reclaim_correctness() {
        // Exercise the promote → register → mark → sweep → free path.
        // `run_test_once` keeps it cheap under the instrumented CI
        // (llvm-cov + gc-stress + nextest); gc-stress GCs on every
        // allocation regardless of run count, so even small loops
        // promote and reclaim many frames. Result must be
        // byte-identical to CRuby (reclamation never frees a
        // still-reachable frame).
        // ruruby-parse has no endless-method-def (`def f(x) = expr`,
        // Ruby 3.0); under `MONORUBY_PARSER=ruruby` use the equivalent
        // `def f(x); expr; end` form (same promoted-frame GC path).
        let mk = if parser_is_ruruby() {
            "def mk(n); ->{ n += 1 }; end"
        } else {
            "def mk(n) = ->{ n += 1 }"
        };
        run_test_once(&format!(
            r#"
            {mk}
            fs = (1..150).map {{ |i| mk(i) }}
            fs.map {{ |f| f.call + f.call }}.sum
            "#
        ));
        run_test_once(
            r#"
            r = []
            60.times do |i|
              x = i
              b = binding
              b.local_variable_set(:y, i * 2)
              r << b.eval("x + y")
            end
            r.sum
            "#,
        );
        let outer = if parser_is_ruruby() {
            "def outer(n); ->{ ->{ n } }; end"
        } else {
            "def outer(n) = ->{ ->{ n } }"
        };
        run_test_once(&format!(
            r#"
            {outer}
            (1..80).map {{ |i| outer(i).call.call }}.sum
            "#
        ));
    }
}
