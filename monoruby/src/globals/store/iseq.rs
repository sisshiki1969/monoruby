use indexmap::IndexMap;

use super::*;
use crate::{
    basic_block::{BasicBlockId, BasicBlockInfo},
    bytecodegen::BcIndex,
};

#[derive(Clone, Debug)]
struct ExceptionMapEntry {
    range: std::ops::Range<BcIndex>, // range of capturing exception
    rescue_pc: Option<BcIndex>,      // rescue destination pc
    ensure_pc: Option<BcIndex>,      // ensure destination pc
    error_slot: Option<SlotId>,      // a slot where an error object is assigned
}

impl ExceptionMapEntry {
    fn new(
        range: std::ops::Range<BcIndex>,
        rescue_pc: Option<BcIndex>,
        ensure_pc: Option<BcIndex>,
        error_slot: Option<SlotId>,
    ) -> Self {
        ExceptionMapEntry {
            range,
            rescue_pc,
            ensure_pc,
            error_slot,
        }
    }
}

///
/// ID of ISEQ.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct ISeqId(usize);

impl From<ISeqId> for usize {
    fn from(id: ISeqId) -> usize {
        id.0
    }
}

impl ISeqId {
    pub const fn new(id: usize) -> Self {
        Self(id)
    }

    pub fn get(&self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone)]
pub struct JitInfo {
    pub entry: DestLabel,
    pub class_version_label: DestLabel,
    pub inline_cache_map: Vec<(ClassId, Option<IdentId>, FuncId)>,
}

///
/// Hint for ISeq optimization.
/// When an ISeq is detected as a trivial method during bytecode compilation,
/// the interpreter and JIT can skip method frame creation.
///
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ISeqHint {
    /// Normal method, no optimization hint.
    Normal,
    /// Always returns the same constant value (e.g. `def nil?; false; end`).
    ConstReturn(Immediate),
    /// Always returns self (e.g. `def to_s; self; end`).
    SelfReturn,
}

///
/// Information of instruction sequences.
///
#[derive(Clone)]
pub struct ISeqInfo {
    ///
    /// *FuncId* of this function.
    ///
    func_id: FuncId,
    ///
    /// Mother method.
    ///
    mother: (ISeqId, usize),
    ///
    /// outer ISeqId.
    ///
    pub outer: Option<ISeqId>,
    ///
    /// Whether this ISeq is a `def`-transparent eval frame: a `def`
    /// in `eval`'d (or `binding.eval`'d) source behaves as if written
    /// at the eval site, so the definee search walks out of this
    /// frame. `class_eval` / `instance_eval` string bodies are *not*
    /// transparent — they anchor `def` to their receiver (via the
    /// runtime cref the builtin pushes), and reset this flag in
    /// `compile_script_eval`.
    ///
    pub(crate) is_eval: bool,
    ///
    /// Name of this function.
    ///
    name: Option<IdentId>,
    ///
    /// Bytecode.
    ///
    bytecode: Option<Pin<Box<[Bytecode]>>>,
    ///
    /// Location of the function in a source code.
    ///
    pub loc: Loc,
    ///
    /// Source map.
    ///
    pub sourcemap: Vec<Loc>,
    ///
    /// Valid temp register information.
    ///
    pub(crate) sp: Vec<SlotId>,
    ///
    /// Exception handling map.
    ///
    exception_map: Vec<ExceptionMapEntry>,
    ///
    /// Information of parameters.
    ///
    pub(crate) args: ParamsInfo,
    ///
    /// Name of local variables
    ///
    pub(crate) locals: indexmap::IndexMap<IdentId, bytecodegen::BcLocal>,
    ///
    /// literal values. (for GC)
    ///
    pub literals: Vec<Value>,
    ///
    /// The number of non-temporary registers.
    ///
    pub non_temp_num: u16,
    ///
    /// The number of temporary registers.
    ///
    pub temp_num: u16,
    ///
    /// Lexical module stack of this method.
    ///
    pub lexical_context: Vec<ClassId>,
    ///
    /// Source code information.
    ///
    pub sourceinfo: SourceInfoRef,
    ///
    /// JIT code info for each class of *self*.
    ///
    pub(super) jit_entry: HashMap<ClassId, JitInfo>,
    /// aarch64 only: per-self-class address of the indirect-dispatch slot
    /// (the wrapper's / a class-guard chain link's heap word). Recorded at
    /// `compile_patch` so the recompiler can overwrite the slot in place
    /// (aarch64 installs JIT code via this slot, not x86 branch patching).
    #[cfg(target_arch = "aarch64")]
    pub(super) jit_slot: HashMap<ClassId, u64>,
    /// Bounded per-self-class warm-up profile used to decide *which* receiver
    /// class is hot enough to specialize for. Without it, the first call that
    /// trips the global warm-up counter compiles for whatever class happens to
    /// be current — so a hot loop that calls a method on a fresh `Class.new`
    /// object every iteration compiles the method once per (singleton-class of
    /// each) class. Capped so transient classes evict each other and never
    /// accumulate. See `profile_self_class`.
    pub(super) jit_class_profile: Vec<(ClassId, u32)>,
    pub(super) jit_invalidated: bool,
    ///
    /// Basic block information.
    ///
    pub(crate) bb_info: BasicBlockInfo,
    ///
    /// Map for BcIndex to CallsiteId.
    ///
    pub(super) callsite_map: HashMap<BcIndex, CallSiteId>,
    ///
    /// Optimization hint detected during bytecode compilation.
    ///
    pub(crate) hint: ISeqHint,
}

impl std::fmt::Debug for ISeqInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "RubyFuncInfo {{ id:{} name:{} method:{:?} args: {} non_temp: {} temp: {}}}",
            self.func_id().get(),
            self.name(),
            self.mother,
            self.args.args_names.len(),
            self.non_temp_num,
            self.temp_num
        )
    }
}

impl alloc::GC<RValue> for ISeqInfo {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        self.literals.iter().for_each(|v| v.mark(alloc));
    }
}

impl ISeqInfo {
    fn new(
        id: FuncId,
        mother: (ISeqId, usize),
        outer: Option<ISeqId>,
        name: Option<IdentId>,
        args: ParamsInfo,
        loc: Loc,
        sourceinfo: SourceInfoRef,
    ) -> Self {
        ISeqInfo {
            func_id: id,
            mother,
            outer,
            is_eval: false,
            name,
            bytecode: None,
            loc,
            sourcemap: vec![],
            sp: vec![],
            exception_map: vec![],
            args,
            locals: Default::default(),
            literals: vec![],
            non_temp_num: 0,
            temp_num: 0,
            lexical_context: vec![],
            sourceinfo,
            jit_entry: HashMap::default(),
            #[cfg(target_arch = "aarch64")]
            jit_slot: HashMap::default(),
            jit_class_profile: Vec::new(),
            jit_invalidated: false,
            bb_info: BasicBlockInfo::default(),
            callsite_map: HashMap::default(),
            hint: ISeqHint::Normal,
        }
    }

    pub(super) fn new_block(
        id: FuncId,
        mother: (ISeqId, usize),
        outer: ISeqId,
        args: ParamsInfo,
        loc: Loc,
        sourceinfo: SourceInfoRef,
    ) -> Self {
        Self::new(id, mother, Some(outer), None, args, loc, sourceinfo)
    }

    pub(super) fn new_method(
        id: FuncId,
        iseq_id: ISeqId,
        name: Option<IdentId>,
        args: ParamsInfo,
        loc: Loc,
        sourceinfo: SourceInfoRef,
    ) -> Self {
        Self::new(id, (iseq_id, 0), None, name, args, loc, sourceinfo)
    }

    pub(crate) fn func_id(&self) -> FuncId {
        self.func_id
    }

    pub(crate) fn mother(&self) -> (ISeqId, usize) {
        self.mother
    }

    ///
    /// Set bytecode to the *ISeqInfo*.
    ///
    pub(crate) fn set_bytecode(&mut self, bc: Vec<Bytecode>) {
        self.bytecode = Some(Box::into_pin(bc.into_boxed_slice()));
    }

    /// True if this ISeq contains a `BlockArg` bytecode (opcode 23) —
    /// the `&block` forwarding / materialisation operation. Methods
    /// with BlockArg must not be JIT-specialised (inlined) into their
    /// callers because BlockArg triggers `move_frame_to_heap` on an
    /// outer frame; an inlined context has no `pop_frame` to refresh
    /// r14 to the heap copy, so locals/outer access in the caller
    /// after the capture point would silently diverge between stack
    /// tombstone and heap.
    pub(crate) fn has_block_arg(&self) -> bool {
        let Some(bc) = self.bytecode.as_ref() else {
            return false;
        };
        bc.iter().any(|b| (b.op1() >> 48) as u8 == 23)
    }

    ///
    /// Whether this iseq contains a `Redo` op (opcode 87), i.e. a `redo`
    /// targeting a `while`/`until` loop body.
    ///
    /// The loop (partial) JIT declines to compile such an iseq: a `redo`
    /// resumes the interpreter in the current native frame (the error
    /// path's `goto` does not tear the frame down), and re-entering a
    /// loop-JIT'd loop from there corrupts the native stack. The method
    /// JIT and the interpreter both run `redo` correctly, so only the
    /// loop JIT opts out.
    ///
    pub(crate) fn contains_redo(&self) -> bool {
        self.bytecode
            .as_ref()
            .is_some_and(|bc| bc.iter().any(|b| (b.op1() >> 48) as u8 == 87))
    }

    ///
    /// Get a number of registers.
    ///
    pub(crate) fn total_reg_num(&self) -> usize {
        1 + (self.non_temp_num + self.temp_num) as usize
    }

    ///
    /// Get a number of non-temp registers. (not includes `self`)
    ///
    pub(crate) fn local_num(&self) -> usize {
        self.non_temp_num as usize
    }

    pub(crate) fn stack_offset(&self) -> usize {
        let reg_num = self.total_reg_num() - 1;
        (reg_num * 8 + (RSP_LOCAL_FRAME + LFP_ARG0) as usize + 31) / 16 * 16 + 16
    }

    ///
    /// Get a block argument name.
    ///
    pub(crate) fn block_param(&self) -> Option<IdentId> {
        self.args.block_param
    }

    ///
    /// Get the name of iseq.
    ///
    pub(crate) fn name(&self) -> String {
        match &self.name {
            Some(name) => name.to_string(),
            None => "<unnamed>".to_string(),
        }
    }

    ///
    /// Get a reference of bytecode.
    ///
    fn bytecode(&self) -> &[Bytecode] {
        self.bytecode.as_ref().unwrap()
    }

    ///
    /// True iff the body contains exactly one method-call instruction
    /// (opcodes 30..=33 in the short instruction class — the only
    /// `TraceIr::MethodCall` encoding). Used by the D1 forwarding-rest
    /// gate: a `...`-forwarding method's rest can only appear in a
    /// forwarding-call argument position, so a single call means the
    /// synthetic rest is consumed exactly once.
    ///

    ///
    /// Get length of bytecode.
    ///
    #[cfg(feature = "emit-bc")]
    pub(crate) fn bytecode_len(&self) -> usize {
        self.bytecode.as_ref().unwrap().len()
    }

    ///
    /// Get pc(*BytecodePtr*) for instruction index(*idx*).
    ///
    pub(crate) fn get_pc(&self, idx: BcIndex) -> BytecodePtr {
        BytecodePtr::from_bc(&self.bytecode()[idx.to_usize()])
    }

    ///
    /// Get pc(*BytecodePtr*) for the beginning of the basic block(*idx*).
    ///
    pub(crate) fn get_bb_pc(&self, idx: BasicBlockId) -> BytecodePtr {
        self.get_pc(self.bb_info[idx].begin)
    }

    ///
    /// Get pc(*BytecodePtr*) for instruction index(*idx*).
    ///
    pub(crate) fn get_top_pc(&self) -> BytecodePtrBase {
        BytecodePtrBase::from_bc(&self.bytecode()[0])
    }

    ///
    /// Returns `true` if *pc* points to an instruction within this ISeq's bytecode.
    ///
    pub(crate) fn contains_pc(&self, pc: BytecodePtr) -> bool {
        let Some(bc) = self.bytecode.as_ref() else {
            return false;
        };
        let top = bc.as_ptr() as usize;
        let end = top + bc.len() * std::mem::size_of::<Bytecode>();
        let addr = pc.as_ptr() as usize;
        addr >= top && addr < end
    }

    ///
    /// Get an instruction index(*usize*) corresponding to pc(*BytecodePtr*).
    ///
    pub(crate) fn get_pc_index(&self, pc: Option<BytecodePtr>) -> BcIndex {
        if let Some(pos) = pc {
            pos - self.get_top_pc()
        } else {
            BcIndex::default()
        }
    }

    pub(crate) fn get_sp(&self, i: BcIndex) -> SlotId {
        self.sp[i.to_usize()]
    }

    pub(crate) fn get_bb(&self, bc_pos: BcIndex) -> BasicBlockId {
        self.bb_info.is_bb_head(bc_pos).unwrap()
    }

    pub(crate) fn get_location(&self) -> String {
        let loc = self.loc;
        format!(
            "{}:{}",
            self.sourceinfo.short_file_name(),
            self.sourceinfo.get_line(&loc)
        )
    }

    ///
    /// Explore exception table for pc(*BcPc*) and return error handler's pc(*BcPc*) and the slot where an error object is to be stored.
    ///
    pub(crate) fn get_exception_dest(
        &self,
        pc: BcIndex,
    ) -> Option<(Option<BcIndex>, Option<BcIndex>, Option<SlotId>)> {
        self.exception_map
            .iter()
            .filter_map(|entry| {
                if entry.range.contains(&pc) {
                    Some((entry.rescue_pc, entry.ensure_pc, entry.error_slot))
                } else {
                    None
                }
            })
            .nth(0)
    }

    pub(crate) fn exception_push(
        &mut self,
        range: std::ops::Range<BcIndex>,
        rescue: Option<BcIndex>,
        ensure: Option<BcIndex>,
        err_reg: Option<SlotId>,
    ) {
        self.exception_map
            .push(ExceptionMapEntry::new(range, rescue, ensure, err_reg));
    }

    pub(crate) fn has_exception_handler(&self) -> bool {
        !self.exception_map.is_empty()
    }

    #[cfg(feature = "emit-bc")]
    pub(crate) fn get_exception_map(
        &self,
    ) -> Vec<(
        std::ops::Range<BcIndex>,
        Option<BcIndex>,
        Option<BcIndex>,
        Option<SlotId>,
    )> {
        self.exception_map
            .iter()
            .map(|entry| {
                let start = entry.range.start;
                let end = entry.range.end;
                let rescue = entry.rescue_pc;
                let ensure = entry.ensure_pc;
                (start..end, rescue, ensure, entry.error_slot)
            })
            .collect::<Vec<_>>()
    }

    pub(crate) fn add_jit_code(
        &mut self,
        self_class: ClassId,
        entry: DestLabel,
        class_version_label: DestLabel,
    ) -> Option<JitInfo> {
        self.jit_entry.insert(
            self_class,
            JitInfo {
                entry,
                class_version_label,
                inline_cache_map: Vec::new(),
            },
        )
    }

    pub(crate) fn dump_jit_enntry(&self) {
        eprintln!("{:?}", self.jit_entry);
    }

    /// aarch64 only: record the indirect-dispatch slot address for a self
    /// class (see `jit_slot`). Called from `compile_patch` after publishing
    /// the compiled guard stub into the slot.
    #[cfg(target_arch = "aarch64")]
    pub(crate) fn set_jit_slot(&mut self, self_class: ClassId, slot: u64) {
        self.jit_slot.insert(self_class, slot);
    }

    /// aarch64 only: the dispatch-slot address for a self class, if compiled.
    #[cfg(target_arch = "aarch64")]
    pub(crate) fn get_jit_slot(&self, self_class: ClassId) -> Option<u64> {
        self.jit_slot.get(&self_class).copied()
    }

    pub(crate) fn get_cache_map(
        &self,
        self_class: ClassId,
    ) -> Option<&Vec<(ClassId, Option<IdentId>, FuncId)>> {
        self.jit_entry
            .get(&self_class)
            .map(|info| &info.inline_cache_map)
    }

    pub(crate) fn set_cache_map(
        &mut self,
        self_class: ClassId,
        cache: Vec<(ClassId, Option<IdentId>, FuncId)>,
    ) {
        self.jit_entry
            .get_mut(&self_class)
            .map(|info| info.inline_cache_map = cache);
    }

    pub(crate) fn jit_invalidated(&self) -> bool {
        self.jit_invalidated
    }

    pub(crate) fn get_jit_entry(&self, self_class: ClassId) -> Option<DestLabel> {
        if self.jit_invalidated {
            return None;
        }
        self.jit_entry
            .get(&self_class)
            .map(|info| info.entry.clone())
    }

    pub(crate) fn get_jit_class_version(&self, self_class: ClassId) -> Option<DestLabel> {
        self.jit_entry
            .get(&self_class)
            .map(|info| info.class_version_label.clone())
    }

    pub(crate) fn invalidate_jit_code(&mut self) {
        self.jit_invalidated = true;
        self.jit_entry.clear();
        self.jit_class_profile.clear();
        #[cfg(target_arch = "aarch64")]
        self.jit_slot.clear();
    }

    /// Record one warm-up sample of *self_class* for this iseq and report
    /// whether that class has now been seen enough times to justify compiling
    /// a specialization for it.
    ///
    /// The profile is a small bounded set: a class must be sampled
    /// `PROFILE_THRESHOLD` times before it is considered hot. Because the
    /// sampler runs only when the stub's warm-up counter expires (every
    /// `COUNT_START_COMPILE` calls), a receiver class that is used for just a
    /// single call — e.g. every `Class.new` object in a hot loop — is sampled
    /// at most once and is evicted by later classes, so it never reaches the
    /// threshold and is never compiled for. A genuinely hot (mono- or modestly
    /// polymorphic) receiver is sampled repeatedly and crosses the threshold.
    pub(crate) fn profile_self_class(&mut self, class: ClassId) -> bool {
        const PROFILE_CAP: usize = 8;
        const PROFILE_THRESHOLD: u32 = 2;
        let prof = &mut self.jit_class_profile;
        if let Some(idx) = prof.iter().position(|(c, _)| *c == class) {
            prof[idx].1 += 1;
            if prof[idx].1 >= PROFILE_THRESHOLD {
                // hot: drop the profile entry (the class is about to be
                // compiled and will fast-path the guard from now on).
                prof.remove(idx);
                return true;
            }
            return false;
        }
        if prof.len() >= PROFILE_CAP {
            prof.remove(0);
        }
        prof.push((class, 1));
        false
    }
}

impl Store {
    pub(crate) fn outer_locals(&self, iseq: ISeqId) -> ExternalContext {
        if let Some(iseq) = self[iseq].outer {
            self.scoped_locals(iseq)
        } else {
            ExternalContext::new()
        }
    }

    pub(crate) fn outer_locals_in(
        &self,
        mut iseq: ISeqId,
        mut outer: usize,
    ) -> Option<(IndexMap<IdentId, bytecodegen::BcLocal>, Option<IdentId>)> {
        loop {
            let info = &self[iseq];
            if outer == 0 {
                return Some((info.locals.clone(), info.block_param()));
            }
            if let Some(outer_iseq) = info.outer {
                iseq = outer_iseq;
                outer -= 1;
            } else {
                return None;
            }
        }
    }

    pub(crate) fn scoped_locals(&self, iseq: ISeqId) -> ExternalContext {
        let mut context = ExternalContext::new();
        let mut current_iseq = iseq;
        loop {
            let info = &self[current_iseq];
            context
                .scope
                .push((info.locals.clone(), info.block_param()));
            if let Some(outer) = info.outer {
                current_iseq = outer;
            } else {
                break;
            }
        }
        context
    }

    ///
    /// Get names of local variables.
    ///
    pub(crate) fn local_variables(&self, mut iseq: ISeqId) -> Vec<Value> {
        let mut map = indexmap::IndexSet::<IdentId>::default();
        loop {
            // The implicit `it` block parameter (Ruby 3.4) is not a reportable
            // local variable, while an explicit `it = ...` local is — so only
            // skip `it` in the iseq that actually takes the implicit parameter.
            let it_param = self[iseq].args.it_param();
            self[iseq].locals.keys().for_each(|id| {
                if it_param && id.get_name() == "it" {
                    return;
                }
                map.insert(*id);
            });
            if let Some(id) = self[iseq].block_param() {
                map.insert(id);
            }
            match self[iseq].outer {
                Some(outer) => iseq = outer,
                None => break,
            }
        }
        // Drop reserved, unspellable slots (anonymous `*` / `**` / `&`, the
        // hidden `for`-loop index, the numbered block parameters `_1`..`_9`, …)
        // that are not real local variables.
        map.into_iter()
            .filter(|id| is_local_variable_name(&id.get_name()))
            .map(Value::symbol)
            .collect()
    }
}

/// Whether `name` is spellable as a Ruby local variable — i.e. begins with a
/// lowercase letter or `_` and consists only of word characters, and is not a
/// numbered block parameter (`_1`..`_9`, which Ruby reserves and never reports
/// as a local variable). Filters out the compiler's reserved slots (`*`, `**`,
/// `&`'s empty name, `(for)`, …).
fn is_local_variable_name(name: &str) -> bool {
    // `**nil` (a keyword-forbidding definition) is modeled internally as a
    // kwrest slot literally named `nil` so `#parameters` can report it as
    // `[:nokey]`. `nil` is a reserved word and can never be a real local
    // variable, so it must not surface in `local_variables`.
    if name == "nil" {
        return false;
    }
    // Numbered block parameters `_1`..`_9`.
    if let Some(rest) = name.strip_prefix('_')
        && rest.len() == 1
        && matches!(rest.as_bytes()[0], b'1'..=b'9')
    {
        return false;
    }
    let mut chars = name.chars();
    match chars.next() {
        Some(c) if c == '_' || (!c.is_ascii()) || c.is_lowercase() => {}
        _ => return false,
    }
    chars.all(|c| c == '_' || !c.is_ascii() || c.is_alphanumeric())
}

///
/// Parameters information in *ISeqInfo*.
///
#[derive(Debug, Clone, Default, PartialEq)]
pub(crate) struct ParamsInfo {
    /// required
    required_num: usize,
    /// optional
    optional_num: usize,
    /// rest
    rest: Option<usize>,
    /// True when `rest` was introduced by a trailing comma (Prism's
    /// `ImplicitRestNode`, e.g. `|a,|`) rather than an explicit
    /// `*name` / `*`. An implicit rest behaves like a normal rest at
    /// block dispatch (extras absorbed, total positional bumped so
    /// `single_arg_expand` still triggers single-Array auto-splat),
    /// but does *not* count toward `arity` / strict-arity checks —
    /// `define_method(:m) { |a,| }; m.arity` is `1`, `m(1, 2)` raises
    /// `ArgumentError`, matching CRuby.
    rest_is_implicit: bool,
    /// post
    post_num: usize,
    // for param, req(incl. destruct slot), opt, rest, keyword, kw_rest, destructed local, block
    pub args_names: Vec<Option<IdentId>>,
    pub kw_names: Vec<IdentId>,
    /// Parallel to `kw_names`: `true` for a required keyword (`a:` with
    /// no default), `false` for an optional one (`a: default`). May be
    /// shorter than `kw_names` (e.g. native methods) — treat a missing
    /// entry as optional via `kw_is_required`.
    pub kw_required: Vec<bool>,
    pub kw_rest: Option<SlotId>,
    pub block_param: Option<IdentId>,
    forwarding: bool,
    /// `true` when the sole parameter is the implicit `it` (Ruby 3.4).
    /// `#parameters` then reports it without a name.
    it_param: bool,
    /// `true` for `**nil` — the definition explicitly accepts no keywords,
    /// so passing any keyword raises `ArgumentError("no keywords accepted")`.
    forbid_keyword: bool,
}

impl ParamsInfo {
    pub fn new(
        required_num: usize,
        optional_num: usize,
        rest: Option<usize>,
        rest_is_implicit: bool,
        post_num: usize,
        args_names: Vec<Option<IdentId>>,
        keyword_names: Vec<IdentId>,
        kw_required: Vec<bool>,
        kw_rest: Option<SlotId>,
        block_param: Option<IdentId>,
        forwarding: bool,
        it_param: bool,
        forbid_keyword: bool,
    ) -> Self {
        ParamsInfo {
            required_num,
            optional_num,
            rest,
            rest_is_implicit,
            post_num,
            args_names,
            kw_names: keyword_names,
            kw_required,
            kw_rest,
            block_param,
            forwarding,
            it_param,
            forbid_keyword,
        }
    }

    /// `**nil` — the definition explicitly forbids keyword arguments.
    pub(crate) fn forbid_keyword(&self) -> bool {
        self.forbid_keyword
    }

    /// Whether the sole parameter is the implicit `it` (reported
    /// anonymously by `#parameters`).
    pub(crate) fn it_param(&self) -> bool {
        self.it_param
    }

    pub fn new_attr_reader() -> Self {
        ParamsInfo::default()
    }

    pub fn new_attr_writer() -> Self {
        ParamsInfo {
            required_num: 1,
            optional_num: 0,
            rest: None,
            rest_is_implicit: false,
            post_num: 0,
            args_names: vec![],
            kw_names: vec![],
            kw_required: vec![],
            kw_rest: None,
            block_param: None,
            forwarding: false,
            it_param: false,
            forbid_keyword: false,
        }
    }

    pub fn new_native(
        min: usize,
        max: usize,
        rest: bool,
        kw_names: Vec<IdentId>,
        kw_rest: bool,
    ) -> Self {
        let mut p = max;
        let kw_num = kw_names.len();
        ParamsInfo {
            required_num: min,
            optional_num: max - min,
            rest: if rest {
                p += 1;
                Some(max)
            } else {
                None
            },
            rest_is_implicit: false,
            post_num: 0,
            args_names: vec![],
            kw_required: vec![false; kw_num],
            kw_names,
            kw_rest: if kw_rest {
                Some(SlotId::new((1 + p + kw_num) as u16))
            } else {
                None
            },
            block_param: None,
            forwarding: false,
            it_param: false,
            forbid_keyword: false,
        }
    }

    ///
    /// The number of required arguments.
    ///
    pub(crate) fn req_num(&self) -> usize {
        self.required_num
    }

    ///
    /// The number of required arguments.
    ///
    pub(crate) fn opt_num(&self) -> usize {
        self.optional_num
    }

    ///
    /// The number of required + optional arguments.
    ///
    pub(crate) fn reqopt_num(&self) -> usize {
        self.required_num + self.optional_num
    }

    ///
    /// The number of post arguments.
    ///
    pub(crate) fn post_num(&self) -> usize {
        self.post_num
    }

    ///
    /// The number of required (no-default) keyword arguments.
    ///
    pub(crate) fn required_kw_num(&self) -> usize {
        self.kw_required.iter().filter(|b| **b).count()
    }

    ///
    /// Whether the keyword at `idx` in `kw_names` is required.
    ///
    pub(crate) fn kw_is_required(&self, idx: usize) -> bool {
        self.kw_required.get(idx).copied().unwrap_or(false)
    }

    ///
    /// Whether this function accepts any keyword argument or `**kwrest`.
    ///
    pub(crate) fn has_keyword(&self) -> bool {
        !self.kw_names.is_empty() || self.kw_rest.is_some()
    }

    ///
    /// The name of the `**kwrest` parameter, if it has one. Anonymous
    /// `**` (and the synthetic kwrest of `...`) return `None`.
    ///
    pub(crate) fn kw_rest_name(&self) -> Option<IdentId> {
        let slot = self.kw_rest?;
        self.args_names
            .get(slot.0 as usize - 1)
            .copied()
            .flatten()
    }

    ///
    pub fn is_rest(&self) -> Option<u16> {
        self.rest.map(|i| i as u16)
    }

    /// Like `is_rest`, but only returns the position when `rest` is
    /// an explicit `*name` / `*`. The trailing-comma `|a,|` form is
    /// not counted — used by arity reporting and the method-style
    /// strict-arity check so `|a,|` reports `arity = 1` and rejects
    /// extras at `define_method` call time, while still letting block
    /// dispatch absorb extras (via the regular `is_rest` path).
    pub fn is_explicit_rest(&self) -> Option<u16> {
        if self.rest_is_implicit {
            None
        } else {
            self.is_rest()
        }
    }

    pub fn rest_is_implicit(&self) -> bool {
        self.rest_is_implicit
    }

    #[allow(dead_code)]
    pub fn forwarding(&self) -> bool {
        self.forwarding
    }

    ///
    /// The number of required + post arguments.
    ///
    pub fn min_positional_args(&self) -> usize {
        self.required_num + self.post_num
    }

    ///
    /// The number of required + optional + post arguments.
    ///
    pub fn max_positional_args(&self) -> usize {
        self.required_num + self.optional_num + self.post_num
    }

    ///
    /// The number of required + optional + rest + post arguments.
    ///
    pub fn total_positional_args(&self) -> usize {
        self.max_positional_args() + self.is_rest().is_some() as usize
    }

    /// The posiiton of keyword arguments.
    pub(crate) fn kw_reg_pos(&self) -> SlotId {
        // 1 is for self.
        SlotId(self.total_positional_args() as u16 + 1)
    }

    pub fn total_args(&self) -> usize {
        self.required_num
            + self.optional_num
            + self.post_num
            + self.rest.is_some() as usize
            + self.kw_names.len()
            + self.kw_rest.is_some() as usize
            + self.block_param.is_some() as usize
    }

    ///
    /// If `self` is "simple", return true.
    ///
    /// "simple" means that the function has no optional, post, rest, keyword, keywoed rest, and block parameters.
    ///
    pub fn is_simple(&self) -> bool {
        self.optional_num == 0
            && self.post_num == 0
            && self.is_rest().is_none()
            && self.kw_names.is_empty()
            && self.kw_rest.is_none()
            && self.block_param.is_none()
    }
}
