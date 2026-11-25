use inline::InlineTable;
use monoasm::DestLabel;

use crate::ast::{DestructEntry, LvarCollector, ParseResult};
use crate::bytecodegen::{BcLocal, CompileInfo, DestructureInfo, ForParamInfo, OptionalInfo};

use super::*;
use std::{cell::RefCell, pin::Pin};

mod basic_op;
mod class;
mod class_dispatch;
mod function;
mod iseq;
mod refinement;
pub use class::*;
pub(crate) use class_dispatch::*;
pub use function::*;
pub(crate) use iseq::*;
pub use refinement::RefinementSetId;
pub(crate) use refinement::RefinementTable;

/// A class's `==` as C code calls it (`Store::funcall_eq_entry`).
#[derive(Clone, Copy)]
pub(crate) struct FuncallEq {
    pub(crate) func_id: FuncId,
    /// Its entry point and frame `Meta` when it is a native method of one
    /// argument: what `BasicObject#!=`, whose own frame already holds the
    /// receiver and that argument, needs to run it there instead of
    /// pushing a second frame through the invoker.
    pub(crate) native: Option<(Meta, BuiltinFn)>,
}

impl std::fmt::Debug for FuncallEq {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "FuncallEq({:?}, native: {})",
            self.func_id,
            self.native.is_some()
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct MethodTableEntry {
    owner: ClassId,
    func_id: Option<FuncId>,
    visibility: Visibility,
    /// The method's *original* definition name, tracked through
    /// `alias_method` and `define_method`-from-(Unbound)Method so that
    /// `Method#original_name` and the `#name(original)` form of
    /// `Method#inspect` can recover it. For a plainly-defined method
    /// (`def`, `attr_*`, builtin) this equals the entry's own name.
    original_name: IdentId,
    /// This entry only re-declares an *inherited* method's visibility
    /// (`public :inherited_method` in a subclass): it shares the
    /// inherited `func_id` so visibility introspection works, but it is
    /// not a definition — `super` resolution must not treat the class
    /// as a position of the method body (`Store::body_dispatched_by`).
    visibility_shadow: bool,
    /// Created by `alias_method` / the `alias` keyword. `original_name`
    /// cannot stand in for this: `define_method(name, method_obj)` also
    /// records the source's name there, yet CRuby treats it as an
    /// ordinary ISEQ definition while an alias entry is its own method
    /// type (`VM_METHOD_TYPE_ALIAS`) — which `Refinement#import_methods`
    /// refuses even when the aliased body is written in Ruby.
    is_alias: bool,
}

impl MethodTableEntry {
    pub fn func_id(&self) -> Option<FuncId> {
        self.func_id
    }

    pub fn owner(&self) -> ClassId {
        self.owner
    }

    pub fn original_name(&self) -> IdentId {
        self.original_name
    }

    pub fn visibility(&self) -> Visibility {
        self.visibility
    }

    pub fn is_visibility_shadow(&self) -> bool {
        self.visibility_shadow
    }

    pub fn is_alias(&self) -> bool {
        self.is_alias
    }

    pub fn is_public(&self) -> bool {
        self.func_id.is_some() && self.visibility == Visibility::Public
    }

    pub fn is_private(&self) -> bool {
        self.func_id.is_some() && self.visibility == Visibility::Private
    }

    pub fn is_public_protected(&self) -> bool {
        self.func_id.is_some()
            && matches!(self.visibility, Visibility::Protected | Visibility::Public)
    }
}

pub struct Store {
    /// function info.
    pub(crate) functions: function::Funcs,
    /// `(class, method)` pairs the VM / JIT fast paths assume — see
    /// `basic_op::BasicOpTable` and `doc/bop_redefinition.md`.
    basic_ops: basic_op::BasicOpTable,
    /// `FuncId` of the builtin `Kernel#hash` — the identity hash every
    /// object inherits unless it defines its own. Recorded at bootstrap so
    /// `Value::ruby_hash` can ask "is this receiver's `hash` still that
    /// one?" and compute the digest inline instead of dispatching. See
    /// `doc/bop_redefinition.md`.
    kernel_hash_fid: Option<FuncId>,
    /// `Kernel#eql?`, for [`Store::is_identity_key`].
    kernel_eql_fid: Option<FuncId>,
    /// The builtin `Kernel#binding`, which refuses a caller that is not a
    /// Ruby frame (#1665). See `method_call_inline_bails`.
    kernel_binding_fid: Option<FuncId>,
    array_hash_fid: Option<FuncId>,
    /// The standard `Kernel#respond_to?` builtin, recorded at registration:
    /// the runtime conversion probes (`default_responds_to`) take their
    /// frame-free fast path only when a receiver resolves `respond_to?` to
    /// exactly this body.
    respond_to_fid: Option<FuncId>,
    /// Direct-mapped memo of `(class, name, class_version)` triples for
    /// which [`Self::no_conversion`] holds.
    no_conversion_memo: [std::cell::Cell<(u32, u32, u32)>; Self::NO_CONVERSION_SLOTS],
    hash_hash_fid: Option<FuncId>,
    /// `FuncId` of the Ruby `Class#new` trampoline (`builtins/class.rb`),
    /// recorded once the bootstrap sources have loaded. The JIT compares a
    /// call site's resolved target against it to recognise `Foo.new` and
    /// emit the construction frame-free — see
    /// `JitContext::inline_class_new`. `None` when the bootstrap was
    /// skipped (`MONORUBY_SKIP_STARTUP`), which simply disables that
    /// specialization.
    class_new_fid: Option<FuncId>,
    /// `FuncId` of the native twin of `Class#new` a call site resolves to
    /// instead when its scope may refine `initialize` — see
    /// [`Self::site_target`].
    class_new_refined_fid: Option<FuncId>,
    /// `FuncId` of the native `Struct#initialize`, recorded at
    /// registration. `inline_class_new` recognises a struct whose
    /// constructor is still this one and writes the arguments straight
    /// into the new object's slots.
    struct_initialize_fid: Option<FuncId>,
    /// The ids of the `Errno::*` classes `errno::init` defines, which it
    /// defines one after another, and each one's errno by
    /// `id - errno_classes.start` (0 for an id in between that is not
    /// one); see [`Self::errno_of_class`].
    errno_classes: std::ops::Range<u32>,
    errno_numbers: Vec<i32>,
    /// `/__errno`, `SystemCallError#errno`'s hidden ivar, interned once.
    errno_ivar: Option<IdentId>,
    /// `FuncId`s of the builtin `send` / `__send__` (one on `Kernel`, one
    /// on `BasicObject`), recorded at registration. A call site that
    /// resolves to one of them and whose method-name argument is a literal
    /// Symbol is compiled as a direct call to the named method instead of
    /// the runtime name lookup — see `CallSiteInfo::send_direct` and
    /// `JitContext::compile_method_call`.
    object_send_fids: Vec<FuncId>,
    /// ISeq info.
    pub(crate) iseqs: Vec<ISeqInfo>,
    ///
    /// Every literal `Value` any bytecode holds, in one flat pool.
    ///
    /// These exist only to keep the literals alive (nothing reads them —
    /// the bytecode carries the `Value` in its operand), and they are
    /// immortal once bytecodegen has emitted them. Pooling them here rather
    /// than in a `Vec` per `ISeqInfo` is what makes the GC's cost
    /// proportional to the number of literals instead of the number of
    /// ISeqs: activerecord has 8.7k literals spread over 23k ISeqs, so the
    /// per-ISeq form chased 23k cold `Vec` headers per collection to reach
    /// them.
    ///
    literals: Vec<Value>,
    ///
    /// The ISeqs that have at least one compiled unit, i.e. the only ones
    /// whose `mark` can reach anything (a unit's `const_map` snapshots the
    /// constants it folded). Membership is recorded by `note_jit_iseq`,
    /// and `ISeqInfo::on_jit_list` keeps it duplicate-free.
    ///
    jit_iseqs: Vec<ISeqId>,
    /// Which compilation units resolved which method names — the index
    /// that lets a method-table change invalidate only the code it can
    /// affect. A unit's salvage record already lists every
    /// `(recv_class, name)` it resolved; this is that list turned around,
    /// name-first, with the unit named by its salvage-record key (a
    /// whole-method unit is `(iseq, self_class, None)`, an OSR loop body
    /// `(iseq, self_class, Some(LoopStart))`). Re-registering a key drops
    /// its old names first, and a dropped iseq (`jit_forget_iseq`) leaves
    /// nothing behind, so the index describes only live records.
    jit_method_deps: HashMap<IdentId, HashSet<JitUnitId>>,
    /// The reverse map of `jit_method_deps`: the names each unit is filed
    /// under, so a unit can be withdrawn without scanning every name.
    jit_unit_names: HashMap<JitUnitId, Vec<IdentId>>,
    /// Units filed under every name: a `super` site in a body whose
    /// enclosing method has no name to resolve it under. Poisoned on
    /// every change, which is what the global word used to do for all.
    jit_wildcard_units: HashSet<JitUnitId>,
    /// Which compilation units carried a class proof across a call on the
    /// assumption that no instance of the class gets a singleton class
    /// (`ClassInfo::instance_singleton`), class-first. The first singleton
    /// class given to an instance poisons exactly these units
    /// (`note_instance_singleton`), and their salvage refuses from then on.
    jit_singleton_deps: HashMap<ClassId, HashSet<JitUnitId>>,
    /// The reverse map of `jit_singleton_deps`.
    jit_unit_singleton_classes: HashMap<JitUnitId, Vec<ClassId>>,
    /// class table.
    pub(in crate::globals) classes: ClassInfoTable,
    /// call site info.
    callsite_info: Vec<CallSiteInfo>,
    /// const access site info.
    constsite_info: Vec<ConstSiteInfo>,
    ///
    /// The const sites whose inline cache is populated — the only ones
    /// holding a `Value`. Appended by `set_const_cache` on the `None` →
    /// `Some` transition, which is the only way a cache appears; a cache is
    /// never cleared, only overwritten.
    ///
    cached_constsites: Vec<ConstSiteId>,
    /// opt case branch info.
    optcase_info: Vec<OptCaseInfo>,
    /// inline info.
    pub(crate) inline_info: InlineTable,
    /// Receiver escape summaries of builtins (`RecvEscape`).
    recv_escape: HashMap<FuncId, inline::RecvEscape>,
    /// `BasicObject#==`: the `==` that is object identity.
    identity_eq_fid: Option<FuncId>,
    /// The builtins a frameless body may call ([`LeafCall`]).
    leaf_builtins: HashMap<FuncId, LeafCall>,
    /// FuncId of the default `BasicObject#!=`, used to recognize that an
    /// unredefined `!=` may be computed as `!(a == b)` without dispatch.
    default_neq: Option<FuncId>,
    /// FuncId of the builtin `Symbol#==`, the other `==` that compares the
    /// two words; see [`Self::is_identity_eq`].
    symbol_eq_fid: Option<FuncId>,
    /// FuncIds of the default `initialize_copy` / `initialize_dup` /
    /// `initialize_clone` builtins. `Kernel#dup` / `#clone` compare a
    /// class's resolved hooks against these to recognize the no-op case
    /// and skip the copy-hook dispatch. Set once during builtin init.
    default_copy_hooks: Option<[FuncId; 3]>,
    /// What `to_s` resolved to, once the builtins had finished defining
    /// themselves, on each class whose instances an interpolation formats
    /// without a dispatch (`TO_S_SHORTCUT_CLASSES`). `None` until then:
    /// while the core library is still loading, its own definitions are
    /// the builtin ones. See `to_s_builtin_classes`.
    builtin_to_s: Option<[Option<FuncId>; TO_S_SHORTCUT_CLASSES.len()]>,
    /// The builtin iterators `Enumerator#with_index` calls directly
    /// (`record_enum_direct_sources`); all `None` until the core library
    /// has loaded.
    enum_direct_sources: [Option<FuncId>; 3],
    /// String's builtin methods that the Symbol methods of the same name
    /// run on the symbol's name (`builtins::symbol::SymStr`), recorded
    /// with `builtin_to_s`.
    symbol_string_methods:
        Option<[Option<FuncId>; crate::builtins::symbol::SYMBOL_STRING_METHODS.len()]>,
    /// Version-stamped memo: the classes of `TO_S_SHORTCUT_CLASSES` whose
    /// `to_s` still resolves to `builtin_to_s`, as a bit per class in the
    /// low byte, under the class_version they were resolved at
    /// (`to_s_memo`). One word, so that "nothing has changed and every
    /// class is builtin" is a single compare.
    to_s_builtin_at: std::cell::Cell<u64>,
    /// Global method cache: memoizes `name` × `class` → method-table
    /// entry lookups, cleared as a whole when class_version moves on.
    /// In a `RefCell` because lookups happen behind `&Store` (including
    /// during JIT compilation); the borrow never outlives a single
    /// cache probe/fill.
    method_cache: RefCell<GlobalMethodCache>,
    /// Warnings produced during parsing / bytecode compilation (e.g. a
    /// hash literal with a duplicated literal key, or prism parse
    /// warnings forwarded via `ParseResult::warnings`). Bytecodegen has
    /// no `Executor`, so it can't route a warning through the
    /// redirectable `$stderr`; the messages are buffered here and
    /// flushed by `Executor::flush_compile_warnings` right after
    /// compilation, before the compiled code runs. The `bool` marks
    /// verbose-level warnings, printed only when `$VERBOSE` is `true`.
    pub(crate) compile_warnings: Vec<(String, bool)>,
    /// Intern pool for frozen string literals emitted under a
    /// `# frozen_string_literal: true` file. Keyed by `(bytes, encoding)`
    /// so literals with identical content share one shared, frozen object
    /// across the whole program — including across `require`d files, since
    /// `Store` outlives any single compilation. Matches CRuby's behaviour
    /// where `"abc".equal?("abc")` is true under the magic comment. Rooted
    /// for GC in [`Store::mark`].
    ///
    /// Split by encoding first so the inner map is keyed by the bytes alone
    /// and can be probed with a borrowed `&[u8]`: a hit (every `String#-@`
    /// on an already pooled string) allocates nothing.
    frozen_str_pool: HashMap<crate::value::Encoding, HashMap<Box<[u8]>, Value>>,
    /// Interned refinement sets, the union of refined method names, and
    /// the "any refinement exists" gate. See `globals/store/refinement.rs`
    /// and `doc/refinements.md` §6.
    refinements: RefinementTable,
    /// `FuncId` of the *main script*'s toplevel body, once compiled.
    /// Distinguishes it from a `require`d file's toplevel (both are
    /// `new_main` bodies stamped `IdentId::_MAIN`): backtraces label
    /// this one `<main>` rather than `<top (required)>`, and a first
    /// read of `TOPLEVEL_BINDING` looks for this frame to build the
    /// binding over (`Executor::materialize_toplevel_binding`).
    main_script_fid: Option<FuncId>,
    /// `FuncId` of an empty toplevel body, compiled once at startup.
    /// Supplies the `Meta` for the outer-less heap frame a
    /// `TOPLEVEL_BINDING` gets when it is read while no main script
    /// frame is running (during a `-r` require, or from a thread).
    empty_toplevel_fid: Option<FuncId>,
    /// The encoding `#inspect` renders into when that is not UTF-8 —
    /// CRuby's `rb_str_inspect` `resenc`, which is
    /// `Encoding.default_internal` if one is set and `default_external`
    /// otherwise (US-ASCII in its place when it is not
    /// ASCII-compatible). `None` means UTF-8, where monoruby's own
    /// strings already live and nothing needs escaping.
    ///
    /// A cache of those two gvars, refreshed whenever either moves
    /// (`encoding::refresh_inspect_escape`). It lives here, rather than
    /// being read from the gvars where it is defined, because the
    /// renderers that need it — `Value::debug`, `inspect_symbol` — are
    /// handed a `Store` and nothing more.
    inspect_escape: Option<crate::value::Encoding>,
}

impl Store {
    /// Does `#inspect` escape non-ASCII to `\uXXXX` in this process?
    /// See the [`inspect_escape`](Store::inspect_escape) field.
    pub(crate) fn inspect_escape(&self) -> bool {
        self.inspect_escape.is_some()
    }

    /// The encoding an escaped `#inspect` rendering carries. Only
    /// meaningful where [`inspect_escape`](Store::inspect_escape) holds.
    pub(crate) fn inspect_escape_encoding(&self) -> crate::value::Encoding {
        self.inspect_escape.unwrap_or(crate::value::Encoding::UsAscii)
    }

    pub(crate) fn set_inspect_escape(&mut self, enc: Option<crate::value::Encoding>) {
        self.inspect_escape = enc;
    }
}

impl std::ops::Deref for Store {
    type Target = ClassInfoTable;
    fn deref(&self) -> &Self::Target {
        &self.classes
    }
}

impl std::ops::DerefMut for Store {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.classes
    }
}

impl std::ops::Index<FuncId> for Store {
    type Output = FuncInfo;
    fn index(&self, index: FuncId) -> &FuncInfo {
        &self.functions[index]
    }
}

impl std::ops::IndexMut<FuncId> for Store {
    fn index_mut(&mut self, index: FuncId) -> &mut FuncInfo {
        &mut self.functions[index]
    }
}

impl std::ops::Index<ISeqId> for Store {
    type Output = ISeqInfo;
    fn index(&self, index: ISeqId) -> &ISeqInfo {
        &self.iseqs[index.get()]
    }
}

impl std::ops::IndexMut<ISeqId> for Store {
    fn index_mut(&mut self, index: ISeqId) -> &mut ISeqInfo {
        &mut self.iseqs[index.get()]
    }
}

impl std::ops::Index<ClassId> for Store {
    type Output = ClassInfo;
    fn index(&self, index: ClassId) -> &ClassInfo {
        &self.classes[index]
    }
}

impl std::ops::IndexMut<ClassId> for Store {
    fn index_mut(&mut self, index: ClassId) -> &mut ClassInfo {
        &mut self.classes[index]
    }
}

impl std::ops::Index<ConstSiteId> for Store {
    type Output = ConstSiteInfo;
    fn index(&self, index: ConstSiteId) -> &ConstSiteInfo {
        &self.constsite_info[index.0 as usize]
    }
}

// NOTE: deliberately no `IndexMut<ConstSiteId>`. The only mutable field is
// the inline cache, and writing it must go through `set_const_cache` so the
// GC's `cached_constsites` index stays in step.

impl std::ops::Index<CallSiteId> for Store {
    type Output = CallSiteInfo;
    fn index(&self, index: CallSiteId) -> &CallSiteInfo {
        &self.callsite_info[index.0 as usize]
    }
}

impl std::ops::IndexMut<CallSiteId> for Store {
    fn index_mut(&mut self, index: CallSiteId) -> &mut CallSiteInfo {
        &mut self.callsite_info[index.0 as usize]
    }
}

//impl std::ops::IndexMut<CallSiteId> for Store {
//    fn index_mut(&mut self, index: CallSiteId) -> &mut CallSiteInfo {
//        &mut self.callsite_info[index.0 as usize]
//    }
//}

impl std::ops::Index<OptCaseId> for Store {
    type Output = OptCaseInfo;
    fn index(&self, index: OptCaseId) -> &Self::Output {
        &self.optcase_info[index.0 as usize]
    }
}

impl alloc::GC<RValue> for Store {
    ///
    /// Scan the interpreter's own tables for `Value`s.
    ///
    /// This is the bulk of every collection — the object graph reachable
    /// from frames and globals is comparatively tiny — so each table is
    /// walked through a side index of exactly the entries that can hold a
    /// `Value`, never the table itself. On activerecord that is the
    /// difference between visiting ~52k `FuncInfo`/`ISeqInfo`/`ConstSiteInfo`
    /// entries and visiting the ~13k that actually carry one.
    ///
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        self.functions.mark(alloc);
        self.literals.iter().for_each(|v| v.mark(alloc));
        self.jit_iseqs
            .iter()
            .for_each(|&id| self[id].mark(alloc));
        self.cached_constsites
            .iter()
            .for_each(|&id| self[id].mark(alloc));
        // Constants and class variables mutate, so a side index cannot
        // shrink this one: nearly every class holds a `Value`. `ClassInfo`
        // instead carries a dirty flag and skips its own tables when a minor
        // collection cannot free anything in them — see `ClassInfo::mark`.
        #[cfg(feature = "gc-debug")]
        if !alloc.is_full_mark() {
            for info in self.classes.table.iter() {
                if !info.is_dirty() {
                    info.assert_all_values_old(alloc);
                }
            }
        }
        self.classes.table.iter().for_each(|info| info.mark(alloc));
        self.frozen_str_pool
            .values()
            .flat_map(|pool| pool.values())
            .for_each(|v| v.mark(alloc));
    }
}

impl Store {
    /// `"file:line"` of the instruction at `pc`: looked for in the iseq
    /// `cfp` runs, then (a JIT-inlined callee's instruction runs in its
    /// caller's frame) among every iseq.
    pub(crate) fn location_of_pc(&self, cfp: Cfp, pc: BytecodePtr) -> Option<String> {
        let own = self[cfp.lfp().func_id()].is_iseq().map(|id| &self[id]);
        let info = own
            .filter(|info| info.contains_pc(pc))
            .or_else(|| self.iseqs.iter().find(|info| info.contains_pc(pc)))?;
        let loc = info.sourcemap[info.get_pc_index(Some(pc)).to_usize()];
        Some(format!(
            "{}:{}",
            info.sourceinfo.file_name(),
            info.sourceinfo.get_line(&loc)
        ))
    }

    ///
    /// A tally of what the compiled program costs in memory, printed at
    /// exit when `MONORUBY_STORE_STATS` is set.
    ///
    /// Peak RSS says a file's compiled form is the largest thing monoruby
    /// builds from it — several times prism's tree and an order of
    /// magnitude more than the intermediate AST. This says which table.
    ///
    pub fn memory_report(&self) -> String {
        use std::fmt::Write;
        let mut iseq = iseq::ISeqHeapSize::default();
        for info in self.iseqs.iter() {
            iseq += info.heap_size();
        }
        let headers = self.iseqs.capacity() * size_of::<ISeqInfo>();
        let mut out = String::new();
        writeln!(
            out,
            "iseqs: {} ({} bytecode instructions), size_of::<ISeqInfo>() = {}\n\
             callsites: {}, size_of::<CallSiteInfo>() = {}",
            self.iseqs.len(),
            iseq.insts,
            size_of::<ISeqInfo>(),
            self.callsite_info.len(),
            size_of::<CallSiteInfo>(),
        )
        .unwrap();
        writeln!(
            out,
            "basic blocks: {}, edges: {}",
            iseq.bb.blocks, iseq.bb.edges
        )
        .unwrap();
        let with_extras = self
            .callsite_info
            .iter()
            .filter(|c| {
                !c.splat_pos().is_empty() || !c.kw_args().is_empty() || !c.hash_splat_pos().is_empty()
            })
            .count();
        writeln!(
            out,
            "  of which with splat/kw/hash-splat: {with_extras} ({:.1}%)",
            100.0 * with_extras as f64 / self.callsite_info.len().max(1) as f64,
        )
        .unwrap();
        let mut row = |name: &str, bytes: usize| {
            writeln!(out, "  {name:<22} {:>9.2} MB", bytes as f64 / (1 << 20) as f64).unwrap()
        };
        row("ISeqInfo headers", headers);
        row("bytecode", iseq.bytecode);
        row("sourcemap", iseq.sourcemap);
        row("sp", iseq.sp);
        row("bb entries", iseq.bb.entries);
        row("bb pred/succ vecs", iseq.bb.edge_vecs);
        row("bb_map", iseq.bb.bb_map);
        row("bb_head", iseq.bb.bb_head);
        row("bb loops", iseq.bb.loops);
        row("callsite_map", iseq.callsite_map);
        row("locals", iseq.locals);
        row("jit_entry", iseq.jit_entry);
        row("iseq other", iseq.other);
        row(
            "callsite_info",
            self.callsite_info.capacity() * size_of::<CallSiteInfo>(),
        );
        row(
            "constsite_info",
            self.constsite_info.capacity() * size_of::<ConstSiteInfo>(),
        );
        row("literals", self.literals.capacity() * size_of::<Value>());
        let total = headers
            + iseq.bytecode
            + iseq.sourcemap
            + iseq.sp
            + iseq.bb.entries
            + iseq.bb.edge_vecs
            + iseq.bb.bb_map
            + iseq.bb.bb_head
            + iseq.bb.loops
            + iseq.callsite_map
            + iseq.locals
            + iseq.jit_entry
            + iseq.other
            + self.callsite_info.capacity() * size_of::<CallSiteInfo>()
            + self.constsite_info.capacity() * size_of::<ConstSiteInfo>()
            + self.literals.capacity() * size_of::<Value>();
        row("TOTAL", total);
        out
    }

    ///
    /// Root a bytecode literal. Called once per literal, from bytecodegen.
    ///
    pub(crate) fn push_literal(&mut self, v: Value) {
        self.literals.push(v);
    }

    ///
    /// Note that `iseq_id` now has a compiled unit, so the GC visits it.
    /// Idempotent; call before installing the unit or after, either works.
    ///
    pub(crate) fn note_jit_iseq(&mut self, iseq_id: ISeqId) {
        let idx: usize = iseq_id.into();
        if !self.iseqs[idx].on_jit_list {
            self.iseqs[idx].on_jit_list = true;
            self.jit_iseqs.push(iseq_id);
        }
    }

    /// Install a whole-method unit's salvage record and file the unit in
    /// the method-name index (see `jit_method_deps`).
    pub(crate) fn set_salvage_record(
        &mut self,
        iseq_id: ISeqId,
        self_class: ClassId,
        class_version_label: DestLabel,
        cache: Vec<InlineCacheEntry>,
        singleton_deps: Vec<ClassId>,
        const_map: ConstSalvageMap,
    ) {
        self.jit_register_unit((iseq_id, self_class, None), &cache, &singleton_deps);
        self[iseq_id].set_salvage_record(
            self_class,
            class_version_label,
            cache,
            singleton_deps,
            const_map,
        );
    }

    /// The loop-body twin of [`Self::set_salvage_record`].
    pub(crate) fn set_loop_jit_info(
        &mut self,
        iseq_id: ISeqId,
        self_class: ClassId,
        index: crate::bytecodegen::BcIndex,
        class_version_label: DestLabel,
        cache: Vec<InlineCacheEntry>,
        singleton_deps: Vec<ClassId>,
        const_map: ConstSalvageMap,
    ) {
        self.jit_register_unit(
            (iseq_id, self_class, Some(index)),
            &cache,
            &singleton_deps,
        );
        self[iseq_id].set_loop_jit_info(
            self_class,
            index,
            class_version_label,
            cache,
            singleton_deps,
            const_map,
        );
    }

    /// Permanently give up on JIT-compiling *iseq_id*, withdrawing its
    /// units from the index along with their records.
    pub(crate) fn invalidate_jit_code(&mut self, iseq_id: ISeqId) {
        self.jit_forget_iseq(iseq_id);
        self[iseq_id].invalidate_jit_code();
    }

    /// A method table changed under *name*: definition, removal or a
    /// visibility change. The VM's caches all key on the global word, so
    /// that moves as before; the JIT's units are invalidated by name.
    pub(crate) fn method_table_changed(&mut self, name: IdentId) {
        Globals::vm_class_version_inc();
        self.jit_method_changed(name);
    }

    fn jit_register_unit(
        &mut self,
        unit: JitUnitId,
        cache: &[InlineCacheEntry],
        singleton_deps: &[ClassId],
    ) {
        self.jit_forget_unit(unit);
        for class in singleton_deps {
            self.jit_singleton_deps
                .entry(*class)
                .or_default()
                .insert(unit);
        }
        if !singleton_deps.is_empty() {
            self.jit_unit_singleton_classes
                .insert(unit, singleton_deps.to_vec());
        }
        let mut names: Vec<IdentId> = Vec::with_capacity(cache.len());
        let mut wildcard = false;
        for entry in cache {
            // A `super` site (no name of its own) re-resolves under the
            // name the running method was *called* by. For a method body
            // that is its own name; for a block it need not be — a
            // `define_method` body runs under whatever name installed it —
            // so a block's `super` is filed under every name.
            let name = match entry.name {
                Some(name) => Some(name),
                // The body the site's `super` was resolved for: the root's
                // name is a specialized callee's only by accident.
                None => entry
                    .super_of
                    .and_then(|fid| self[fid].name())
                    .or_else(|| self.jit_unit_method_name(unit.0)),
            };
            match name {
                Some(name) => {
                    if !names.contains(&name) {
                        names.push(name);
                    }
                }
                None => wildcard = true,
            }
        }
        if wildcard {
            self.jit_wildcard_units.insert(unit);
        }
        for name in &names {
            self.jit_method_deps.entry(*name).or_default().insert(unit);
        }
        self.jit_unit_names.insert(unit, names);
    }

    /// The name a `super` in *iseq_id* resolves under, when that is fixed:
    /// the method's own name for a method body, and nothing for a block
    /// (see `jit_register_unit`) or a body without a name.
    fn jit_unit_method_name(&self, iseq_id: ISeqId) -> Option<IdentId> {
        let (mother, _) = self[iseq_id].mother();
        if mother != iseq_id {
            return None;
        }
        self[self[mother].func_id()].name()
    }

    fn jit_forget_unit(&mut self, unit: JitUnitId) {
        if let Some(names) = self.jit_unit_names.remove(&unit) {
            for name in names {
                if let Some(set) = self.jit_method_deps.get_mut(&name) {
                    set.remove(&unit);
                    if set.is_empty() {
                        self.jit_method_deps.remove(&name);
                    }
                }
            }
        }
        self.jit_wildcard_units.remove(&unit);
        if let Some(classes) = self.jit_unit_singleton_classes.remove(&unit) {
            for class in classes {
                if let Some(set) = self.jit_singleton_deps.get_mut(&class) {
                    set.remove(&unit);
                    if set.is_empty() {
                        self.jit_singleton_deps.remove(&class);
                    }
                }
            }
        }
    }

    /// Withdraw every unit of *iseq_id* from the index (its records are
    /// being dropped). A scan of the unit table: eviction is rare.
    pub(crate) fn jit_forget_iseq(&mut self, iseq_id: ISeqId) {
        let units: Vec<JitUnitId> = self
            .jit_unit_names
            .keys()
            .chain(self.jit_unit_singleton_classes.keys())
            .filter(|unit| unit.0 == iseq_id)
            .copied()
            .collect();
        for unit in units {
            self.jit_forget_unit(unit);
        }
    }

    /// Invalidate every compiled unit that resolved *name*: its version
    /// word and guard immediates are set to a value the JIT word can never
    /// hold, so its next class-version guard fails and the unit goes
    /// through salvage (re-validated and re-stamped if the resolution it
    /// recorded still holds, recompiled otherwise) — exactly what a bump
    /// of the global word did to it, without touching any other unit.
    fn jit_method_changed(&mut self, name: IdentId) {
        let mut units: Vec<JitUnitId> = self.jit_wildcard_units.iter().copied().collect();
        if let Some(set) = self.jit_method_deps.get(&name) {
            units.extend(set.iter().copied());
        }
        self.jit_poison_units(units);
    }

    /// Give *units* a class-version miss on their next guard: their version
    /// word and guard immediates are set to a value the JIT word can never
    /// hold. What the miss then does is up to the unit's salvage record.
    /// CRuby's `rb_class_modify_check`: the class *class_id* is about to
    /// gain, lose or re-scope a method (or an ancestor). A frozen one
    /// raises FrozenError — and for a singleton class the error is about
    /// the object the class is attached to, which is what the program
    /// froze: `o.freeze; class << o; def x; end; end` is "can't modify
    /// frozen Object: #<Object:0x…>", not "... frozen Class:
    /// #<Class:#<Object:0x…>>".
    pub(crate) fn class_modify_check(&self, class_id: ClassId) -> Result<()> {
        let module = self[class_id].get_module();
        if !module.as_val().is_frozen() {
            return Ok(());
        }
        let named = module.is_singleton().unwrap_or(module.as_val());
        Err(MonorubyErr::cant_modify_frozen(self, named))
    }

    ///
    /// Get the singleton class of *obj*, creating it if needed — and when
    /// that is the first singleton class any instance of the object's class
    /// has had, invalidate the compiled units that relied on there being
    /// none (see `ClassInfo::instance_singleton`). Shadows the
    /// `ClassInfoTable` method so every `store.get_singleton` goes through
    /// here.
    ///
    pub(crate) fn get_singleton(&mut self, obj: Value) -> Result<Module> {
        if self.is_fstring(obj) {
            return Err(MonorubyErr::typeerr("can't define singleton"));
        }
        let res = self.classes.get_singleton(obj);
        self.note_instance_singletons();
        res
    }

    /// `ClassInfoTable::clone_singleton_class`, followed by the same
    /// bookkeeping as [`Self::get_singleton`].
    pub(crate) fn clone_singleton_class(&mut self, original: Value, copy: Value) -> Result<()> {
        let res = self.classes.clone_singleton_class(original, copy);
        self.note_instance_singletons();
        res
    }

    /// Poison every unit that carried a proof of a class across a call on
    /// the assumption that no instance of it acquires a singleton class,
    /// for the classes one of whose instances just did. The running frames of
    /// those units deopt at the class-version guard that follows each such
    /// call; their salvage refuses (`singleton_deps_hold`), so they are
    /// recompiled without the assumption.
    fn note_instance_singletons(&mut self) {
        if self.classes.new_instance_singletons.is_empty() {
            return;
        }
        let classes = std::mem::take(&mut self.classes.new_instance_singletons);
        let mut units = vec![];
        for class in classes {
            // Latch the class only when some unit relied on it: from then on
            // its proofs are not carried across calls, so a class whose
            // instances keep acquiring singleton classes costs each unit one
            // recompile, not one per acquisition.
            if let Some(set) = self.jit_singleton_deps.remove(&class) {
                self.classes[class].set_instance_singleton();
                units.extend(set);
            }
        }
        self.jit_poison_units(units);
    }

    /// Can an object proved to be of *class* stop being of it — by being
    /// given a singleton class? Not an immediate, a `Range` or a `Complex`
    /// (they cannot have one), not a class or module object (whose class is
    /// its metaclass, reasoned about elsewhere), and not an object whose
    /// class already is a singleton class.
    pub(crate) fn class_proof_may_break(&self, class: ClassId) -> bool {
        !matches!(
            class,
            NIL_CLASS
                | TRUE_CLASS
                | FALSE_CLASS
                | BOOL_CLASS
                | INTEGER_CLASS
                | BIGNUM_CLASS
                | FLOAT_CLASS
                | SYMBOL_CLASS
                | RANGE_CLASS
                | COMPLEX_CLASS
                | CLASS_CLASS
                | MODULE_CLASS
        ) && self[class]
            .try_get_module()
            .is_some_and(|m| m.is_singleton().is_none())
    }

    /// Does every class a unit carried a proof of across calls still have
    /// no instance with a singleton class? A salvage must not re-stamp a
    /// unit for which this fails.
    pub(crate) fn singleton_deps_hold(&self, deps: &[ClassId]) -> bool {
        deps.iter().all(|class| !self[*class].instance_singleton())
    }

    fn jit_poison_units(&mut self, units: Vec<JitUnitId>) {
        if units.is_empty() {
            return;
        }
        let mut labels = Vec::with_capacity(units.len());
        for (iseq_id, self_class, index) in units {
            let label = match index {
                None => self[iseq_id].get_jit_class_version(self_class),
                Some(index) => self[iseq_id]
                    .get_loop_jit_info(self_class, index)
                    .map(|info| info.class_version_label.clone()),
            };
            if let Some(label) = label {
                labels.push(label);
            }
        }
        CODEGEN.with(|codegen| {
            let mut codegen = codegen.borrow_mut();
            for label in &labels {
                // A unit poisoned since its last entry is poisoned already:
                // its word still reads the sentinel, and re-stamping it
                // (word, immediates, a protection flip on macOS) buys
                // nothing. Cold units would otherwise pay for every
                // definition of a name they resolved.
                let word = codegen.jit.get_label_address(label).as_ptr() as *const u32;
                // SAFETY: the label names the unit's 4-byte snapshot word,
                // emitted by `jit_compile` and alive for the process.
                if unsafe { *word } == crate::codegen::VERSION_IMM_SENTINEL as u32 {
                    continue;
                }
                #[cfg(feature = "jit-log")]
                crate::codegen::jit_stats::bump(&crate::codegen::jit_stats::JIT_UNITS_POISONED);
                codegen.set_class_version(crate::codegen::VERSION_IMM_SENTINEL as u32, label);
            }
        });
    }

    ///
    /// Populate a const site's inline cache, keeping the GC's side index of
    /// cache-holding sites in step. The only way a cache is created.
    ///
    pub(crate) fn set_const_cache(&mut self, site_id: ConstSiteId, cache: ConstCache) {
        let site = &mut self.constsite_info[site_id.0 as usize];
        let first = site.cache.is_none();
        site.cache = Some(cache);
        if first {
            self.cached_constsites.push(site_id);
        }
    }

    pub(super) fn new() -> Self {
        Self {
            functions: function::Funcs::default(),
            basic_ops: basic_op::BasicOpTable::new(),
            kernel_hash_fid: None,
            kernel_eql_fid: None,
            errno_classes: 0..0,
            errno_numbers: vec![],
            errno_ivar: None,
            kernel_binding_fid: None,
            class_new_fid: None,
            class_new_refined_fid: None,
            struct_initialize_fid: None,
            object_send_fids: vec![],
            array_hash_fid: None,
            respond_to_fid: None,
            no_conversion_memo: std::array::from_fn(|_| std::cell::Cell::new((0, 0, 0))),
            hash_hash_fid: None,
            iseqs: vec![],
            literals: vec![],
            jit_iseqs: vec![],
            jit_method_deps: HashMap::default(),
            jit_unit_names: HashMap::default(),
            jit_wildcard_units: HashSet::default(),
            jit_singleton_deps: HashMap::default(),
            jit_unit_singleton_classes: HashMap::default(),
            constsite_info: vec![],
            cached_constsites: vec![],
            callsite_info: vec![],
            optcase_info: vec![],
            classes: ClassInfoTable::new(),
            inline_info: InlineTable::default(),
            recv_escape: HashMap::default(),
            leaf_builtins: HashMap::default(),
            identity_eq_fid: None,
            default_neq: None,
            symbol_eq_fid: None,
            default_copy_hooks: None,
            builtin_to_s: None,
            enum_direct_sources: [None; 3],
            symbol_string_methods: None,
            // No class_version is this wide: nothing is memoized yet.
            to_s_builtin_at: std::cell::Cell::new(u64::MAX),
            method_cache: RefCell::new(GlobalMethodCache::default()),
            compile_warnings: vec![],
            frozen_str_pool: HashMap::default(),
            refinements: RefinementTable::new(),
            main_script_fid: None,
            empty_toplevel_fid: None,
            inspect_escape: None,
        }
    }

    ///
    /// Record the *main script*'s toplevel body (see the field docs).
    ///
    pub(crate) fn set_main_script_fid(&mut self, fid: FuncId) {
        self.main_script_fid = Some(fid);
    }

    pub(crate) fn main_script_fid(&self) -> Option<FuncId> {
        self.main_script_fid
    }

    ///
    /// Number of constant sites recorded so far. Paired with
    /// [`Self::names_constant_since`] to ask what a just-finished
    /// compilation referenced.
    ///
    pub(crate) fn const_site_len(&self) -> usize {
        self.constsite_info.len()
    }

    ///
    /// Does any constant site recorded since *start* name *name*
    /// (as the constant itself or as a qualifier)?
    ///
    pub(crate) fn names_constant_since(&self, start: usize, name: IdentId) -> bool {
        self.constsite_info[start.min(self.constsite_info.len())..]
            .iter()
            .any(|site| site.name == name || site.prefix.contains(&name))
    }

    pub(crate) fn set_empty_toplevel_fid(&mut self, fid: FuncId) {
        self.empty_toplevel_fid = Some(fid);
    }

    pub(crate) fn empty_toplevel_fid(&self) -> Option<FuncId> {
        self.empty_toplevel_fid
    }

    ///
    /// Intern a frozen string literal for a `# frozen_string_literal: true`
    /// file. Returns the shared, frozen `Value` for `(bytes, enc)`, creating
    /// and caching it on first use so identical literals (in the same or a
    /// different file) resolve to the same object.
    ///
    pub(crate) fn intern_frozen_str(&mut self, bytes: &[u8], enc: crate::value::Encoding) -> Value {
        let pool = self.frozen_str_pool.entry(enc).or_default();
        if let Some(v) = pool.get(bytes) {
            return *v;
        }
        // The fstring of this content, if one is alive already (a Hash
        // made it as the copy of a key), is the one to answer — and is
        // held from now on.
        let v = crate::value::rvalue::fstring::intern(bytes, enc, || {
            let mut v = Value::string_from_source_bytes(bytes, enc);
            v.set_fstring();
            v
        });
        pool.insert(bytes.into(), v);
        v
    }

    ///
    /// *obj* is an fstring (CRuby's `RSTRING_FSTR`): the pooled String of
    /// its content — a frozen string literal, `-str`, a `"lit".freeze` —
    /// or one of the Strings made as such without going through the pool
    /// (`Symbol#name`, `Module#name`, a Hash's copy of a String key). Such
    /// a String is shared by everything that names the same content, so
    /// it cannot be given a singleton class: `rb_singleton_class` raises
    /// TypeError for it, where any other frozen String gets a (frozen)
    /// one. The answer is a header bit (`Header::is_fstr`).
    ///
    pub(crate) fn is_fstring(&self, obj: Value) -> bool {
        obj.try_rvalue().is_some_and(|rv| rv.is_fstr())
    }

    ///
    /// [`Store::intern_frozen_str`] for *s*, a frozen, bare `String` whose
    /// content is `(bytes, enc)`: when no equal string is pooled yet, *s*
    /// itself becomes the pooled one instead of a copy of it, as CRuby's
    /// `rb_fstring` registers an already frozen receiver.
    ///
    pub(crate) fn intern_frozen_string(
        &mut self,
        mut s: Value,
        bytes: &[u8],
        enc: crate::value::Encoding,
    ) -> Value {
        debug_assert!(s.is_frozen());
        let pool = self.frozen_str_pool.entry(enc).or_default();
        if let Some(v) = pool.get(bytes) {
            return *v;
        }
        let v = crate::value::rvalue::fstring::lookup(bytes, enc).unwrap_or_else(|| {
            s.set_fstring();
            crate::value::rvalue::fstring::insert(s);
            s
        });
        pool.insert(bytes.into(), v);
        v
    }

    pub fn iseq(&self, func_id: FuncId) -> &ISeqInfo {
        let iseq = self[func_id].as_iseq();
        &self[iseq]
    }

    pub fn iseq_mut(&mut self, func_id: FuncId) -> Option<&mut ISeqInfo> {
        let iseq = self[func_id].is_iseq()?;
        Some(&mut self[iseq])
    }

    pub fn ancestors(&self, class_id: ClassId) -> Vec<Module> {
        // CRuby's `Module#ancestors` returns just the module itself
        // (plus iclasses for any included/prepended modules) when called
        // on a plain Module — `Module.new.ancestors == [#<Module>]` and
        // `module M; end; M.ancestors == [M]`. monoruby's class store
        // sets every Module's superclass to `Object` for method-lookup
        // purposes, so the naive walk would include
        // `[M, Object, Kernel, BasicObject]`. Stop the walk when leaving
        // the iclass-included region under a non-class receiver.
        //
        // For prepend support we follow CRuby's rule: a class/module that
        // has an `origin` pointer (i.e. has been the target of a
        // `prepend`) is *not* rendered at its head position; instead it
        // shows up where its origin iclass sits in the chain. Iclass
        // entries are rendered as the underlying module/class that they
        // wrap so `[Klass, Module, Klass]` becomes `[Module, Klass]` —
        // matching CRuby's `[m, c, Object, ...]` output.
        let start = self[class_id].get_module();
        let receiver_is_class = start.as_val().ty() == Some(ObjTy::CLASS);
        let mut class = start;
        let mut v = Vec::new();
        // Decide whether to push the head node: skip if it has an
        // origin (its content surfaces via the origin iclass later).
        if !class.has_origin() {
            v.push(class);
        }
        while let Some(super_class) = class.superclass() {
            // For a Module receiver, stop walking once we leave the
            // mixed-in iclass chain — i.e. once the next ancestor is the
            // first real Class (`Object`). The mixed-in modules are
            // wrapped in iclass entries that still report themselves with
            // `is_iclass()`, so the walk stays correct for things like
            // `m.include otherm; m.ancestors == [m, otherm]`.
            if !receiver_is_class
                && super_class.as_val().ty() == Some(ObjTy::CLASS)
                && !super_class.is_iclass()
            {
                break;
            }
            // Render this node. For an iclass, render the wrapped
            // module/class (look up by class_id) so a prepended class
            // doesn't surface as a `Module` value. For a real
            // class/module, push it directly — but if it has an origin
            // it's already shadowed by its origin iclass and should be
            // skipped (matching CRuby's `p == RCLASS_ORIGIN(p)` test).
            if super_class.is_iclass() {
                let underlying = self[super_class.id()].get_module();
                v.push(underlying);
            } else if !super_class.has_origin() {
                v.push(super_class);
            }
            class = super_class;
        }
        v
    }

    pub fn show_ancestors(&self, class_id: ClassId) {
        let mut class = self[class_id].get_module();
        eprint!(
            "ancestors of {:?}<{}>: ",
            class.id(),
            class.id().get_name(self),
        );
        while let Some(class_) = class.superclass() {
            eprint!("{:?}<{}> ", class_.id(), class_.id().get_name(self),);
            class = class_;
        }
        eprintln!("");
    }
}

impl Store {
    /// Start reporting basic-op redefinitions. Called once, after the Rust
    /// builtins, `builtins/*.rb` and the gems have finished defining
    /// themselves — their own definitions are exactly the ones the table
    /// names, so arming any earlier reports the interpreter monkey-patching
    /// itself. See `basic_op::BasicOpTable`.
    pub(crate) fn arm_basic_ops(&mut self) {
        // Snapshot what every pair resolves to right now. The mixin
        // checks compare against this baseline, so that including a
        // module a basic-op class already carries — `Comparable`, say —
        // is recognised as changing nothing.
        let resolved = self
            .basic_ops
            .pairs()
            .into_iter()
            .map(|(class_id, name)| ((class_id, name), self.resolve_basic_op(class_id, name)))
            .collect();
        self.basic_ops.arm(resolved);
    }

    /// Whether any basic op has been redefined. Read by the fast paths that
    /// live in Rust rather than in the swappable dispatch table — currently
    /// `runtime::{get_index, set_index}`.
    /// Record the builtin `Kernel#hash`. Called once, at bootstrap.
    pub(crate) fn set_kernel_hash_fid(&mut self, fid: FuncId) {
        self.kernel_hash_fid = Some(fid);
    }

    pub(crate) fn set_kernel_eql_fid(&mut self, fid: FuncId) {
        self.kernel_eql_fid = Some(fid);
    }

    /// Record the builtin `Kernel#binding`. Called once, at bootstrap.
    pub(crate) fn set_kernel_binding_fid(&mut self, fid: FuncId) {
        self.kernel_binding_fid = Some(fid);
    }

    /// The Methods the JIT's inlined `Method#call` must not call directly
    /// but hand to the builtin, so that its own frame is on the stack:
    /// `Kernel#binding`, which refuses a caller that is not a Ruby frame,
    /// and `send` / `__send__`, which `binding` looks through (#1665).
    pub(crate) fn method_call_inline_bails(&self) -> impl Iterator<Item = FuncId> + '_ {
        self.kernel_binding_fid
            .into_iter()
            .chain(self.object_send_fids.iter().copied())
    }

    ///
    /// Record the Ruby `Class#new` trampoline. Called once, right after the
    /// bootstrap Ruby sources load (it does not exist before that).
    ///
    pub(crate) fn record_class_new_fid(&mut self) {
        self.class_new_fid = self
            .search_method_by_class_id(CLASS_CLASS, IdentId::NEW)
            .and_then(|e| e.func_id());
    }

    /// The Ruby `Class#new` trampoline's `FuncId`, if the bootstrap ran.
    pub(crate) fn class_new_fid(&self) -> Option<FuncId> {
        self.class_new_fid
    }

    /// Record the native twin of `Class#new`. Called at its registration.
    pub(crate) fn record_class_new_refined_fid(&mut self, fid: FuncId) {
        self.class_new_refined_fid = Some(fid);
    }

    ///
    /// The function a call site written under the refinements *set*
    /// dispatches to, given that the lookup of *name* found *fid*.
    ///
    /// That is *fid*, with one exception. `Foo.new(...)` written at a
    /// call site is, in CRuby 4.0, an allocation followed by a call of
    /// `initialize` *from that site* (`opt_new`) — so an `initialize`
    /// the site's scope refines is the one that runs, although the
    /// `Class#new` method itself (reached through `send`, a `Method`
    /// object, or C) calls `initialize` with no refinement. monoruby's
    /// `Class#new` is a Ruby method of the core library, whose own call
    /// of `initialize` sees no refinement either (#2029); a site in a
    /// refining scope therefore gets a native twin of it that asks its
    /// caller's scope. Only once some refinement defines `initialize`:
    /// until then the two cannot differ, and `Class#new` keeps its
    /// inlined fast paths.
    ///
    pub(crate) fn site_target(&self, name: IdentId, set: RefinementSetId, fid: FuncId) -> FuncId {
        if !set.is_empty()
            && name == IdentId::NEW
            && Some(fid) == self.class_new_fid
            && self.refinements.is_refined_name(IdentId::INITIALIZE)
            && let Some(twin) = self.class_new_refined_fid
        {
            twin
        } else {
            fid
        }
    }

    /// Record the `Errno::*` classes `errno::init` defined, with their
    /// errnos.
    pub(crate) fn record_errno_classes(&mut self, classes: &[(ClassId, i32)]) {
        let start = classes.iter().map(|(c, _)| c.u32()).min().unwrap_or(0);
        let end = classes.iter().map(|(c, _)| c.u32() + 1).max().unwrap_or(0);
        let mut numbers = vec![0; (end - start) as usize];
        for (c, n) in classes {
            numbers[(c.u32() - start) as usize] = *n;
        }
        self.errno_classes = start..end;
        self.errno_numbers = numbers;
        self.errno_ivar = Some(IdentId::get_id("/__errno"));
    }

    /// The hidden ivar `SystemCallError#errno` reads.
    pub(crate) fn errno_ivar(&self) -> IdentId {
        self.errno_ivar.unwrap()
    }

    /// The errno of an `Errno::*` class, or of a class just below one
    /// (`IO::EAGAINWaitReadable` < `IO::WaitReadable`'s iclass <
    /// `Errno::EAGAIN`). `None` for every other class.
    pub(crate) fn errno_of_class(&self, class_id: ClassId) -> Option<Value> {
        let number = |id: ClassId| {
            let i = id.u32().checked_sub(self.errno_classes.start)?;
            let n = *self.errno_numbers.get(i as usize)?;
            (n != 0).then(|| Value::integer(n as i64))
        };
        if let Some(n) = number(class_id) {
            return Some(n);
        }
        let mut module = self.get_module(class_id);
        for _ in 0..2 {
            module = module.superclass()?;
            if let Some(n) = number(module.id()) {
                return Some(n);
            }
        }
        None
    }

    /// Record the native `Struct#initialize`. Called at its registration.
    pub(crate) fn record_struct_initialize_fid(&mut self, fid: FuncId) {
        self.struct_initialize_fid = Some(fid);
    }

    /// The native `Struct#initialize`'s `FuncId`.
    pub(crate) fn struct_initialize_fid(&self) -> Option<FuncId> {
        self.struct_initialize_fid
    }

    /// Record a builtin `send` / `__send__`. Called at registration
    /// (`Kernel#send` and `BasicObject#__send__`).
    pub(crate) fn record_object_send_fid(&mut self, fid: FuncId) {
        self.object_send_fids.push(fid);
    }

    /// Whether `fid` is a builtin `send` / `__send__`.
    pub(crate) fn is_object_send(&self, fid: FuncId) -> bool {
        self.object_send_fids.contains(&fid)
    }

    pub(crate) fn set_recv_escape(&mut self, fid: FuncId, info: inline::RecvEscape) {
        self.recv_escape.insert(fid, info);
    }

    pub(crate) fn recv_escape(&self, fid: FuncId) -> Option<inline::RecvEscape> {
        self.recv_escape.get(&fid).copied()
    }

    pub(crate) fn set_identity_eq_fid(&mut self, fid: FuncId) {
        self.identity_eq_fid = Some(fid);
    }

    pub(crate) fn identity_eq_fid(&self) -> Option<FuncId> {
        self.identity_eq_fid
    }

    pub(crate) fn set_leaf_builtin(&mut self, fid: FuncId, leaf: LeafCall) {
        self.leaf_builtins.insert(fid, leaf);
    }

    pub(crate) fn leaf_builtin(&self, fid: FuncId) -> Option<LeafCall> {
        self.leaf_builtins.get(&fid).copied()
    }

    pub(crate) fn set_array_hash_fid(&mut self, fid: FuncId) {
        self.array_hash_fid = Some(fid);
    }

    pub(crate) fn set_respond_to_fid(&mut self, fid: FuncId) {
        self.respond_to_fid = Some(fid);
    }

    pub(crate) fn respond_to_fid(&self) -> Option<FuncId> {
        self.respond_to_fid
    }

    pub(crate) fn set_hash_hash_fid(&mut self, fid: FuncId) {
        self.hash_hash_fid = Some(fid);
    }

    ///
    /// Whether the Array / Hash `obj` still resolves `:hash` to its
    /// builtin structural implementation, so the native digest may be
    /// used without a dispatch. A singleton (mock), a subclass override,
    /// or a monkey-patched `#hash` makes this false and the caller
    /// dispatches instead — the mock-count specs pin exactly one `#hash`
    /// call per probe. Like `has_builtin_identity_hash`, the probe
    /// behind `check_method` is cached on the class version.
    ///
    pub(crate) fn has_builtin_container_hash(&self, obj: Value, ty: ObjTy) -> bool {
        let builtin = match ty {
            ObjTy::ARRAY => self.array_hash_fid,
            ObjTy::HASH => self.hash_hash_fid,
            _ => return false,
        };
        match (builtin, self.hash_method(obj.class())) {
            (Some(builtin), Some(found)) => builtin == found,
            _ => false,
        }
    }

    ///
    /// Resolve `hash` on `class_id`, memoized per class_version — the same
    /// shape as `match_method`. Both callers below run on every Hash / Set
    /// operation whose key is not an immediate, so the uncached path (a
    /// global-method-cache probe, and an ancestor walk on a miss) is worth
    /// collapsing to a `Cell` load and a compare.
    ///
    fn hash_method(&self, class_id: ClassId) -> Option<FuncId> {
        let version = Globals::class_version();
        if let Some((v, fid)) = self[class_id].hash_method_at()
            && v == version
        {
            return fid;
        }
        let fid = self
            .check_method_for_class_with_version(class_id, IdentId::HASH, version)
            .and_then(|e| e.func_id());
        self[class_id].set_hash_method_at(version, fid);
        fid
    }

    ///
    /// Whether `obj` still inherits the builtin identity `hash`.
    ///
    /// A plain object — no `hash` of its own anywhere in its ancestry —
    /// otherwise costs a full dispatch on *every* Hash / Set lookup, which
    /// is the one place monoruby lost to CRuby (whose `rb_any_hash` special
    /// cases the same thing). The method-table probe behind this is cached
    /// on the class version, so it is a load and a compare against the
    /// interpreter frame it replaces.
    ///
    pub(crate) fn has_builtin_identity_hash(&self, obj: Value) -> bool {
        match (self.kernel_hash_fid, self.hash_method(obj.class())) {
            (Some(builtin), Some(found)) => builtin == found,
            _ => false,
        }
    }

    ///
    /// Whether an object of `class_id` is a Hash key by its identity alone: it inherits
    /// both `Kernel#hash` and `Kernel#eql?`, so it is `eql?` to nothing
    /// but itself and a Hash can file and find it without a call.
    ///
    /// With `Kernel#hash` and an `eql?` of its own it is still hashed by
    /// its identity, but a key filed under that number — one whose own
    /// `#hash` answers it — has to be shown to the method.
    ///
    /// Memoized per class_version, as `hash_method` is. `class_id` is the
    /// object's own class (`Value::class`), a singleton class included.
    ///
    #[inline]
    pub(crate) fn is_identity_key_class(&self, class_id: ClassId) -> bool {
        let version = Globals::class_version();
        if let Some((v, answer)) = self[class_id].identity_key_at()
            && v == version
        {
            return answer;
        }
        self.resolve_identity_key(class_id, version)
    }

    #[cold]
    #[inline(never)]
    fn resolve_identity_key(&self, class_id: ClassId, version: u32) -> bool {
        let resolves_to = |name: IdentId, builtin: Option<FuncId>| {
            let found = self
                .check_method_for_class_with_version(class_id, name, version)
                .and_then(|e| e.func_id());
            matches!((builtin, found), (Some(builtin), Some(found)) if builtin == found)
        };
        let answer = resolves_to(IdentId::HASH, self.kernel_hash_fid)
            && resolves_to(IdentId::EQL_, self.kernel_eql_fid);
        self[class_id].set_identity_key_at(version, answer);
        answer
    }

    pub(crate) fn basic_op_redefined(&self) -> bool {
        self.basic_ops.redefined()
    }

    /// The basic ops replaced since the last call, for CRuby's
    /// `:performance` warning. See
    /// [`Executor::warn_redefined_basic_ops`].
    pub(crate) fn take_basic_op_warnings(&mut self) -> Vec<(ClassId, IdentId)> {
        self.basic_ops.take_pending_warnings()
    }

    /// Whether `class_id#name` is one of the [`BASIC_OP_DEFS`] pairs — i.e.
    /// whether redefining it is *tracked*, so a body that inlined it
    /// guard-free gets evicted. A pair outside the table is never marked
    /// redefined, so a guard-free inline of it would survive a redefinition
    /// forever; the JIT's direct-fire paths check this before recording a
    /// dependency on one.
    pub(crate) fn is_basic_op_pair(&self, class_id: ClassId, name: IdentId) -> bool {
        self.basic_ops.contains(class_id, name)
    }

    /// Whether `class_id#name` — a pair in `BASIC_OP_DEFS` — has been
    /// replaced, so the fast path that assumed it must dispatch instead.
    /// Gated on the cheap global bool, so a program that redefines nothing
    /// never reaches the set probe.
    pub(crate) fn basic_op_redefined_for(&self, class_id: ClassId, name: IdentId) -> bool {
        self.basic_ops.redefined_pair(class_id, name)
    }

    /// What `class_id#name` resolved to when the basic-op table was armed
    /// (after the builtins finished defining themselves, before user code
    /// ran) — the definition monoruby's own Ruby-written core is bound to,
    /// whatever the live method table says now. `None` for a pair the table
    /// does not track, or before it is armed.
    /// Whether a `case` whose `when` values are all literals compares by
    /// value still (CRuby's `opt_case_dispatch`): no `===` of a class it
    /// dispatches on was ever replaced.
    pub(crate) fn case_dispatch_enabled(&self) -> bool {
        self.basic_ops.case_dispatch_enabled()
    }

    pub(crate) fn basic_op_armed_func(&self, class_id: ClassId, name: IdentId) -> Option<FuncId> {
        self.basic_ops.armed_func(class_id, name).flatten()
    }

    /// Whether `class_id#name` was replaced by something no lexical scope
    /// escapes — an ordinary redefinition rather than a refinement. The JIT
    /// must never inline one of these; see
    /// [`Self::basic_op_refined_in_scope`] for the other half.
    pub(crate) fn basic_op_globally_redefined_for(
        &self,
        class_id: ClassId,
        name: IdentId,
    ) -> bool {
        self.basic_ops.globally_redefined_pair(class_id, name)
    }

    ///
    /// Whether resolving `class_id#name` under *set* gives a different method
    /// than resolving it with no refinements at all.
    ///
    /// This is the question a body being compiled has to ask before inlining
    /// an operator that *some* refinement replaces: a refinement binds
    /// lexically, so a scope that never activated it still gets the builtin
    /// and may still inline.
    ///
    /// Answered by walking *set*'s own entries rather than by resolving the
    /// method twice and comparing. Resolving the unrefined side would go
    /// through the method cache, which reads the class version out of the
    /// thread-local `CODEGEN` — already mutably borrowed while the JIT is
    /// compiling, so it panics there. Walking the entries touches nothing
    /// but the class table.
    ///
    /// Deliberately inclusive: a refinement module's ancestors count too
    /// (CRuby activates them), and an entry found anywhere in that chain
    /// means "do not inline". Erring towards a real call is always sound;
    /// erring the other way would emit arithmetic the refinement was
    /// supposed to replace.
    ///
    pub(crate) fn basic_op_refined_in_scope(
        &self,
        class_id: ClassId,
        name: IdentId,
        set: RefinementSetId,
    ) -> bool {
        if set.is_empty() || !self.basic_ops.refined_pair(class_id, name) {
            return false;
        }
        self.refinements
            .entries(set)
            .iter()
            .filter(|(refined, _)| *refined == class_id)
            .any(|(_, module)| {
                self.ancestors(*module)
                    .iter()
                    .any(|m| self[m.id()].own_method_table().any(|(n, _)| *n == name))
            })
    }

    #[cfg(feature = "emit-bc")]
    pub(super) fn functions(&self) -> &[FuncInfo] {
        self.functions.functions()
    }

    pub(crate) fn func_len(&self) -> usize {
        self.functions.function_len()
    }

    /// Get class name of *ClassId*.
    pub(crate) fn get_class_name(&self, class: ClassId) -> String {
        // A refinement is identified by what it refines and where, never
        // by a constant it got bound to (`rb_mod_to_s`): as a method's
        // owner and as the receiver in a NameError message alike.
        if let (Some(refined), Some(owner)) = (
            self.classes[class].refined_class(),
            self.classes[class].refinement_owner(),
        ) {
            return format!(
                "#<refinement:{}@{}>",
                self.get_class_name(refined),
                self.get_class_name(owner)
            );
        }
        self.get_class_path(class)
    }

    /// How a NameError message shows a class or module (`name_err_mesg_to_str`):
    /// its `name` when it has one — a refinement bound to a constant is
    /// that constant here — and its `inspect` otherwise.
    pub(crate) fn nameerr_class_name(&self, class: ClassId) -> String {
        if self.classes[class].get_name().is_some() && !self.classes.is_name_erased(class) {
            self.get_class_path(class)
        } else {
            self.get_class_name(class)
        }
    }

    /// [`Self::get_class_path`] as bytes, when a name on the path is not
    /// UTF-8 text; `None` otherwise (and for an anonymous class or one
    /// with a temporary name).
    #[inline]
    pub(crate) fn get_class_path_raw(
        &self,
        class: ClassId,
    ) -> Option<(Vec<u8>, crate::value::Encoding)> {
        if !class::any_raw_class_name() {
            return None;
        }
        self.get_class_path_raw_named(class)
    }

    #[cold]
    fn get_class_path_raw_named(&self, class: ClassId) -> Option<(Vec<u8>, crate::value::Encoding)> {
        if self.classes[class].get_name().is_none()
            || self.classes.is_name_erased(class)
            || self.classes[class].is_name_explicit_temporary()
        {
            return None;
        }
        self.classes.get_class_path_raw(class)
    }

    /// The name of *ClassId* as `Module#name` gives it (`rb_class_path`):
    /// [`Self::get_class_name`] without the refinement form.
    pub(crate) fn get_class_path(&self, class: ClassId) -> String {
        let class_obj = self.classes[class].get_module();
        match self.classes[class].get_name().filter(|_| {
            // An ancestor erased by `set_temporary_name(nil)` makes the
            // whole subtree anonymous again, so fall through to the
            // `#<Module:0x..>` rendering below.
            !self.classes.is_name_erased(class)
        }) {
            Some(name) => {
                // A name set explicitly via `Module#set_temporary_name`
                // overrides parent-chain rendering — CRuby returns the
                // bare leaf string, not `#<Module:0x..>::leaf`.
                if self.classes[class].is_name_explicit_temporary() {
                    return name.to_string();
                }
                let v: Vec<_> = self.classes.get_parents(class).into_iter().rev().collect();
                v.join("::")
            }
            None => match class_obj.is_singleton() {
                None => {
                    // Differentiate Module from Class for anonymous values.
                    let kind = if class_obj.as_val().ty() == Some(ObjTy::MODULE) {
                        "Module"
                    } else {
                        "Class"
                    };
                    format!("#<{kind}:0x{:016x}>", class_obj.as_val().id())
                }
                Some(base) => format!("#<Class:{}>", self.attached_object_name(base)),
            },
        }
    }

    /// How a singleton class names the object it is attached to
    /// (`rb_mod_to_s`): a class or module by its own name, anything else
    /// as `rb_any_to_s` renders it — `#<String:0x…>`, never the object's
    /// `to_s` or `inspect`, so the class of the String `"xyz"` is
    /// `#<Class:#<String:0x…>>` and that of `main`
    /// `#<Class:#<Object:0x…>>`.
    pub(crate) fn attached_object_name(&self, attached: Value) -> String {
        if attached.is_class_or_module().is_some() {
            attached.to_s(self)
        } else {
            format!(
                "#<{}:0x{:016x}>",
                attached.get_real_class_name(self),
                attached.id()
            )
        }
    }

    ///
    /// Get the description of the function *func_id*, for the JIT logs,
    /// the dumps and the profiler.
    ///
    /// If the function is a method, the description is "*class_name*#*method_name*".
    /// if the function is a block, the description is "block in *method_name*".
    ///
    /// This is [`Self::func_label`] except that a method of a class with
    /// no permanent name keeps its owner (`#<Class:0x…>#foo`): the label
    /// CRuby prints drops it, which is what a backtrace wants and not
    /// what a profile of anonymous classes does.
    ///
    pub(crate) fn func_description(&self, func_id: FuncId) -> String {
        self.describe_func(func_id, None, false)
    }

    /// The label of the function *func_id* as CRuby prints it in a
    /// backtrace (`rb_gen_method_name`), without a frame to take a hint
    /// from.
    pub(crate) fn func_label(&self, func_id: FuncId) -> String {
        self.describe_func(func_id, None, true)
    }

    /// Like [`Self::func_label`], but with the executing frame's
    /// `self` as a hint: a method owned by several classes (e.g. the
    /// instance and singleton copies `module_function` creates) is
    /// labeled by the owner that actually dispatched — `Q.f` when
    /// called on the module, `Q#f` from an instance (CRuby).
    pub(crate) fn func_description_for(&self, func_id: FuncId, frame_self: Option<Value>) -> String {
        self.func_description_hinted(func_id, Self::label_hint(frame_self))
    }

    /// The part of a frame's `self` that [`Self::func_description_for`]
    /// labels by: the class or module it is, if it is one. A backtrace
    /// keeps this rather than `self`, so it holds no `Value`.
    pub(crate) fn label_hint(frame_self: Option<Value>) -> Option<ClassId> {
        frame_self
            .and_then(|sv| sv.is_class_or_module())
            .map(|module| module.id())
    }

    /// [`Self::func_description_for`] with the frame's `self` reduced to
    /// [`Self::label_hint`].
    pub(crate) fn func_description_hinted(&self, func_id: FuncId, hint: Option<ClassId>) -> String {
        self.describe_func(func_id, hint, true)
    }

    /// *label*: name the owner only by a permanent name, as a backtrace
    /// label does; otherwise by whatever `Module#inspect` would print.
    fn describe_func(&self, func_id: FuncId, hint: Option<ClassId>, label: bool) -> String {
        let info = &self[func_id];
        let name = if let Some(iseq_id) = info.is_iseq() {
            let iseq = &self[iseq_id];
            let mother = iseq.mother().0;
            if mother != iseq_id {
                // The main script executes inside TOPLEVEL_BINDING, so its
                // body is technically a block of the empty toplevel frame —
                // but it *is* the program's top level. It is the only block
                // fid stamped `_MAIN` (`compile_main_script_binding`).
                if info.name() == Some(IdentId::_MAIN) {
                    return "<main>".to_string();
                }
                // An eval body is a transparent frame: CRuby labels it
                // exactly like the frame the eval was written in.
                if iseq.is_eval
                    && let Some(outer) = iseq.outer
                {
                    return self.describe_func(self[outer].func_id(), hint, label);
                }
                // Block nesting depth: hops up the lexical chain to the
                // first non-block scope — or the `_MAIN`-stamped main
                // script body, which counts as the base — skipping
                // transparent eval frames.
                let mut depth = 1usize;
                let mut base = iseq.outer;
                while let Some(o) = base {
                    let outer_iseq = &self[o];
                    if outer_iseq.outer.is_none()
                        || self[outer_iseq.func_id()].name() == Some(IdentId::_MAIN)
                    {
                        break;
                    }
                    if !outer_iseq.is_eval {
                        depth += 1;
                    }
                    base = outer_iseq.outer;
                }
                let base_desc = match base {
                    Some(o) => self.describe_func(self[o].func_id(), hint, label),
                    None => self.describe_func(self[mother].func_id(), hint, label),
                };
                return if depth == 1 {
                    format!("block in {base_desc}")
                } else {
                    format!("block ({depth} levels) in {base_desc}")
                };
            } else if info.is_classdef() {
                // Class/module body frames: CRuby renders `<class:A>` /
                // `<module:M>` / `singleton class` (for `class << obj`).
                return if iseq.singleton_classdef {
                    "singleton class".to_string()
                } else {
                    let kind = if iseq.module_classdef {
                        "module"
                    } else {
                        "class"
                    };
                    format!("<{kind}:{}>", iseq.name())
                };
            } else {
                // The toplevel frame's internal name is `/main`
                // (`IdentId::_MAIN`). The main script renders through the
                // `_MAIN`-stamped block above; a non-block `/main` is a
                // required/loaded file's toplevel — CRuby's
                // `<top (required)>` — unless it is the main script's
                // own body, which CRuby labels `<main>` (the main
                // script no longer runs inside TOPLEVEL_BINDING, so it
                // is a plain toplevel like any required file and only
                // the recorded fid tells them apart).
                let name = iseq.name();
                if name == "/main" {
                    if self.main_script_fid() == Some(func_id) {
                        "<main>".to_string()
                    } else {
                        "<top (required)>".to_string()
                    }
                } else {
                    name
                }
            }
        } else {
            if let Some(name) = info.name() {
                format!("{:?}", name)
            } else {
                String::new()
            }
        };
        // CRuby (`gen_method_name`, Ruby 3.4+) renders the owner with its
        // *fully-qualified* name (`ExceptionSpecs::Backtrace.foo`, not
        // `Backtrace.foo`) — and only when that name is *permanent*, i.e.
        // reachable from a top-level constant. A method of an anonymous
        // class or module, of one nested under an anonymous module
        // (`#<Class:0x…>::Inner`), or of one that only has a
        // `set_temporary_name`, is labelled with the bare method name
        // (#1983). `qualified_name` returns "" for Object, where the
        // display is the bare "Object", so special-case it.
        let qual = |this: &Self, id: ClassId| {
            if id == OBJECT_CLASS {
                Some("Object".to_string())
            } else if !label || this[id].is_name_permanent() {
                Some(this.qualified_name(id))
            } else {
                None
            }
        };
        // With a frame-self hint and several owners, prefer the owner
        // whose singleton class is attached to that self (the entry
        // `module_function` dispatched), falling back to the first.
        let owner = match hint {
            Some(module) if info.owner_class().len() > 1 => info
                .owner_class()
                .iter()
                .find(|o| {
                    self[**o]
                        .get_module()
                        .is_singleton()
                        .and_then(|obj| obj.is_class_or_module())
                        .is_some_and(|attached| attached.id() == module)
                })
                .or_else(|| info.owner_class().first()),
            _ => info.owner_class().first(),
        };
        let Some(owner) = owner else {
            return name;
        };
        match self[*owner].get_module().is_singleton() {
            // CRuby names a plain object's singleton method by the bare
            // method name (only class/module singletons get the
            // `Owner.name` form).
            Some(obj) => match obj.is_class_or_module().and_then(|class| qual(self, class.id())) {
                Some(owner) => format!("{owner}.{name}"),
                None => name,
            },
            None => match qual(self, *owner) {
                Some(owner) => format!("{owner}#{name}"),
                None => name,
            },
        }
    }

    /// One backtrace entry as CRuby prints it: `file:line:in 'label'`,
    /// `file:line` for a frame with no function, `<internal>:in 'label'`
    /// for a native frame with no source location.
    /// [`Self::trace_entry_location`] as the String a backtrace holds,
    /// in the encoding CRuby gives it (see [`backtrace_line`]).
    pub(crate) fn trace_entry_value(&self, entry: &TraceEntry) -> Value {
        let enc = match &entry.0 {
            Some((_, source)) => path_encoding(source),
            None => crate::value::Encoding::UsAscii,
        };
        backtrace_line(self.trace_entry_location(entry), enc)
    }

    pub(crate) fn trace_entry_location(&self, entry: &TraceEntry) -> String {
        let (source_loc, func_id, hint) = entry;
        match (source_loc, func_id) {
            (Some((loc, source)), Some(func_id)) => format!(
                "{}:{}:in '{}'",
                display_path(source),
                source.get_line(loc),
                self.func_description_hinted(*func_id, *hint)
            ),
            (Some((loc, source)), None) => {
                format!("{}:{}", display_path(source), source.get_line(loc))
            }
            (None, Some(func_id)) => format!(
                "<internal>:in '{}'",
                self.func_description_hinted(*func_id, *hint)
            ),
            (None, None) => "<internal>".to_string(),
        }
    }

    /// Get the caller's file:line location string from a CFP.
    ///
    /// If `pc` is provided, uses the sourcemap to resolve the actual call-site
    /// line number instead of the function definition location.
    pub fn get_caller_loc(&self, cfp: Cfp, pc: Option<BytecodePtr>) -> String {
        let func_id = cfp.lfp().func_id();
        if let Some(iseq_id) = self[func_id].is_iseq() {
            let iseq = &self[iseq_id];
            // The pc is only meaningful if it actually points into this
            // frame's bytecode. It may not: when the call was reached
            // through a JIT-inlined dispatch (`send`), the frame we land
            // on is the inliner's, not the one whose pc we were handed.
            // Fall back to the function's own location there rather than
            // indexing the sourcemap out of range.
            let loc = match pc.filter(|pc| iseq.contains_pc(*pc)) {
                Some(pc) => iseq.sourcemap[iseq.get_pc_index(Some(pc)).to_usize()],
                None => iseq.loc,
            };
            format!(
                "{}:{}",
                iseq.sourceinfo.file_name(),
                iseq.sourceinfo.get_line(&loc)
            )
        } else {
            "<internal>:0".to_string()
        }
    }
}

/// True for sources shipped in the interpreter's own `builtins/`
/// bootstrap tree — the core methods monoruby implements in Ruby but
/// CRuby implements in C. Their frames stand in for C frames
/// (see `Meta::set_internal_builtin`).
fn is_internal_source(source: &SourceInfoRef) -> bool {
    is_builtins_path(&source.path)
}

/// Whether *path* is a file of the `builtins/` bootstrap tree.
///
/// Both sides are canonical: a builtin is loaded from
/// `globals::builtins_dir()` (startup.rb) or by `require_relative` from a
/// file that was, and the install root may be reached through a symlink.
pub(crate) fn is_builtins_path(path: &std::path::Path) -> bool {
    path.starts_with(crate::globals::builtins_dir())
}

/// CRuby-style display path for backtrace strings: sources shipped in
/// the interpreter's own `builtins/` bootstrap tree render as
/// `<internal:NAME>` (CRuby's prelude-style frames — `Kernel#tap`
/// reports `<internal:kernel>`, not an installation path); everything
/// else renders as loaded. `Location#absolute_path` is nil for the
/// internal form.
pub(crate) fn display_path(source: &SourceInfoRef) -> std::borrow::Cow<'_, str> {
    let internal = source
        .path
        .strip_prefix(crate::globals::builtins_dir())
        .ok()
        .map(|rest| {
            format!(
                "<internal:{}>",
                rest.file_stem().unwrap_or_default().to_string_lossy()
            )
        });
    match internal {
        Some(s) => std::borrow::Cow::Owned(s),
        None => source.file_name(),
    }
}

/// The encoding of the path String a backtrace line read from *source*
/// starts with: the source's own (`SourceInfo::path_encoding`), fixed
/// when it was parsed (`parser::source_path_encoding`).
pub(crate) fn path_encoding(source: &SourceInfoRef) -> crate::value::Encoding {
    source.path_encoding()
}

/// A backtrace line as the String CRuby makes it (`location_format`, in
/// `rb_enc_compatible(path, label)`): in the encoding of its path,
/// *path_enc*, while it is all ASCII, and in the label's, UTF-8, when a
/// method name is not (#2090).
#[inline]
pub(crate) fn backtrace_line(line: String, path_enc: crate::value::Encoding) -> Value {
    let ascii = path_enc != crate::value::Encoding::UTF8 && line.is_ascii();
    backtrace_string(line, path_enc, ascii)
}

/// [`backtrace_line`] for a caller that already knows whether *line* is
/// all ASCII (`ascii`, only consulted for a *path_enc* other than UTF-8).
#[inline]
pub(crate) fn backtrace_string(
    line: String,
    path_enc: crate::value::Encoding,
    ascii: bool,
) -> Value {
    if ascii && path_enc != crate::value::Encoding::UTF8 {
        Value::string_from_inner(crate::value::RStringInner::from_vec_cr(
            line.into_bytes(),
            path_enc,
            crate::value::CodeRange::SevenBit,
        ))
    } else {
        Value::string(line)
    }
}

impl Store {
    fn new_iseq(&mut self, info: ISeqInfo) -> ISeqId {
        let id = self.iseqs.len();
        self.iseqs.push(info);
        ISeqId::new(id)
    }

    fn next_iseq_id(&mut self) -> ISeqId {
        let id = self.iseqs.len();
        ISeqId::new(id)
    }

    pub(crate) fn new_main(&mut self, result: ParseResult) -> Result<(FuncId, CompileInfo)> {
        let info = BlockInfo {
            params: vec![],
            body: Box::new(result.node),
            lvar: LvarCollector::new(),
            loc: Loc::default(),
            is_lambda: false,
        };
        let compile_info = Store::handle_args(info, vec![], crate::value::Encoding::UTF8)?;
        self.new_iseq_method(
            Some(IdentId::_MAIN),
            compile_info,
            Loc::default(),
            result.source_info,
            true,
        )
    }

    pub(crate) fn new_classdef(
        &mut self,
        name: Option<IdentId>,
        compile_info: CompileInfo,
        loc: Loc,
        sourceinfo: SourceInfoRef,
        is_singleton: bool,
        is_module: bool,
    ) -> Result<(FuncId, CompileInfo)> {
        let func_id = self.functions.next_func_id();
        let internal = is_internal_source(&sourceinfo);
        let mut info = ISeqInfo::new_method(
            func_id,
            self.next_iseq_id(),
            name,
            ParamsInfo::default(),
            loc,
            sourceinfo,
        );
        info.singleton_classdef = is_singleton;
        info.module_classdef = is_module;
        // The body itself resolves constants through a per-execution
        // singleton class, so it needs the self-keyed constant cache
        // just like defs nested inside it (bytecodegen additionally
        // propagates the flag onto those).
        info.in_singleton_lexical = is_singleton;
        let iseq = self.new_iseq(info);
        let mut info = FuncInfo::new_classdef_iseq(name, func_id, iseq);
        if internal {
            info.set_internal_builtin();
        }
        self.functions.info.push(info);
        Ok((func_id, compile_info))
    }

    pub(crate) fn new_iseq_method(
        &mut self,
        name: Option<IdentId>,
        compile_info: CompileInfo,
        loc: Loc,
        sourceinfo: SourceInfoRef,
        top_level: bool,
    ) -> Result<(FuncId, CompileInfo)> {
        let func_id = self.functions.next_func_id();
        let params_info = compile_info.params.clone();
        let internal = is_internal_source(&sourceinfo);
        let info = ISeqInfo::new_method(
            func_id,
            self.next_iseq_id(),
            name,
            params_info.clone(),
            loc,
            sourceinfo,
        );
        let iseq = self.new_iseq(info);
        let mut info = FuncInfo::new_method_iseq(name, func_id, iseq, params_info, top_level);
        if internal {
            info.set_internal_builtin();
        }
        self.functions.info.push(info);
        Ok((func_id, compile_info))
    }

    pub(crate) fn new_block(
        &mut self,
        outer: ISeqId,
        compile_info: CompileInfo,
        is_block_style: bool,
        loc: Loc,
        sourceinfo: SourceInfoRef,
    ) -> Result<(FuncId, CompileInfo)> {
        let outer_mother = self[outer].mother();
        let mother = (outer_mother.0, outer_mother.1 + 1);
        let func_id = self.functions.next_func_id();
        let params_info = compile_info.params.clone();
        let internal = is_internal_source(&sourceinfo);
        let info =
            ISeqInfo::new_block(func_id, mother, outer, params_info.clone(), loc, sourceinfo);
        let iseq = self.new_iseq(info);
        let mut info = FuncInfo::new_block_iseq(func_id, iseq, params_info, is_block_style);
        if internal {
            info.set_internal_builtin();
        }
        self.functions.info.push(info);
        Ok((func_id, compile_info))
    }

    pub(crate) fn new_eval(
        &mut self,
        outer: ISeqId,
        result: ParseResult,
        loc: Loc,
    ) -> Result<(FuncId, CompileInfo)> {
        let info = BlockInfo {
            params: vec![],
            body: Box::new(result.node),
            lvar: LvarCollector::new(),
            loc,
            is_lambda: false,
        };
        let compile_info = Store::handle_args(info, vec![], crate::value::Encoding::UTF8)?;
        // eval'd code is block-style: a `return` / `break` in the string
        // escapes to the enclosing method, like a block (not locally like
        // a lambda). eval bodies and lambdas are otherwise both
        // method-style (no own params), so this flag is what keeps their
        // non-local-vs-local `return` semantics apart.
        let (fid, compile_info) = self.new_block(outer, compile_info, true, loc, result.source_info)?;
        let iseq = self[fid].is_iseq().unwrap();
        self[iseq].is_eval = true;
        Ok((fid, compile_info))
    }

    pub(crate) fn new_builtin_func(
        &mut self,
        name: &str,
        address: BuiltinFn,
        min: usize,
        max: usize,
        rest: bool,
        kw: &[&str],
        kw_rest: bool,
    ) -> FuncId {
        self.functions
            .new_native_func(name.to_string(), address, min, max, rest, kw, kw_rest)
    }

    pub(super) fn new_basic_op(
        &mut self,
        name: String,
        address: BuiltinFn,
        min: usize,
        max: usize,
        rest: bool,
    ) -> FuncId {
        self.functions
            .new_native_basic_op(name, address, min, max, rest)
    }

    pub(super) fn new_builtin_func_with_effect(
        &mut self,
        name: String,
        address: BuiltinFn,
        min: usize,
        max: usize,
        rest: bool,
        effect: Effect,
    ) -> FuncId {
        self.functions
            .new_native_func_with_effect(name, address, min, max, rest, effect)
    }

    pub(super) fn new_proc_method(&mut self, proc: Proc) -> FuncId {
        self.functions.new_proc_method(proc)
    }

    pub(super) fn new_attr_reader(&mut self, name: IdentId, ivar_name: IdentId) -> FuncId {
        self.functions.new_attr_reader(name, ivar_name)
    }

    pub(super) fn new_attr_writer(&mut self, name: IdentId, ivar_name: IdentId) -> FuncId {
        self.functions.new_attr_writer(name, ivar_name)
    }

    pub(super) fn new_struct_reader(
        &mut self,
        name: IdentId,
        slot_index: u16,
        inline: bool,
    ) -> FuncId {
        self.functions.new_struct_reader(name, slot_index, inline)
    }

    pub(super) fn new_struct_writer(
        &mut self,
        name: IdentId,
        slot_index: u16,
        inline: bool,
    ) -> FuncId {
        self.functions.new_struct_writer(name, slot_index, inline)
    }

    pub(crate) fn new_callsite(
        &mut self,
        name: Option<IdentId>,
        bc_pos: BcIndex,
        pos_num: usize,
        kw_pos: SlotId,
        kw_args: indexmap::IndexMap<IdentId, usize>,
        splat_pos: Vec<usize>,
        hash_splat_pos: Vec<SlotId>,
        kw_order: Vec<KwElem>,
        block_fid: Option<FuncId>,
        block_arg: Option<SlotId>,
        args: SlotId,
        recv: SlotId,
        dst: Option<SlotId>,
        forwarding: bool,
        bypass_visibility: bool,
        vcall: bool,
    ) -> CallSiteId {
        let id = CallSiteId(self.callsite_info.len() as u32);
        // `kw_order` describes the keyword window register by register,
        // so its `Splat` entries must be exactly `hash_splat_pos`.
        debug_assert!(
            kw_order
                .iter()
                .enumerate()
                .filter(|(_, e)| matches!(e, KwElem::Splat))
                .map(|(i, _)| kw_pos + i)
                .eq(hash_splat_pos.iter().copied())
        );
        let extra = if splat_pos.is_empty() && kw_args.is_empty() && hash_splat_pos.is_empty() {
            None
        } else {
            Some(Box::new(CallSiteExtra {
                splat_pos,
                kw_args,
                hash_splat_pos,
                kw_order,
            }))
        };
        self.callsite_info.push(CallSiteInfo {
            id,
            name,
            bc_pos,
            pos_num,
            kw_pos,
            extra,
            block_fid,
            block_arg,
            args,
            recv,
            dst,
            forwarding,
            bypass_visibility,
            vcall,
            pmc: PolyCache::default(),
            send_direct: None,
            class_new_init: None,
            yield_expanded: None,
            newarray_recv: false,
            symbol_twins: Vec::new(),
        });
        id
    }

    /// Attach the direct-call twin of a literal-name `send` site — see
    /// [`CallSiteInfo::send_direct`].
    pub(crate) fn set_send_direct(&mut self, callid: CallSiteId, direct: CallSiteId) {
        self.callsite_info[callid.0 as usize].send_direct = Some(direct);
    }

    /// Attach the `initialize` twin of a `recv.new(args)` site — see
    /// [`CallSiteInfo::class_new_init`].
    pub(crate) fn set_class_new_init(&mut self, callid: CallSiteId, init: CallSiteId) {
        self.callsite_info[callid.0 as usize].class_new_init = Some(init);
    }

    /// Attach the expanded twin of a `yield [e1, .., en]` site — see
    /// [`CallSiteInfo::yield_expanded`].
    pub(crate) fn set_yield_expanded(&mut self, callid: CallSiteId, expanded: CallSiteId) {
        self.callsite_info[callid.0 as usize].yield_expanded = Some(expanded);
    }

    /// Mark a `[e1, .., en].min` / `.max` site — see
    /// [`CallSiteInfo::newarray_recv`].
    pub(crate) fn set_newarray_recv(&mut self, callid: CallSiteId) {
        self.callsite_info[callid.0 as usize].newarray_recv = true;
    }

    ///
    /// A `yield x` at *callid* reached a `&:symbol` block with an *x* of
    /// *class*: keep the site's [`SymbolTwin`] for the symbol up to date,
    /// making it on the first visit.
    ///
    /// Called when the site's yield misses the one-entry `&:sym` cache,
    /// and the first time it hits an entry another site put there
    /// (`Globals::note_symbol_yield_miss`), so a site that keeps yielding
    /// to one symbol and one class pays for this once.
    ///
    #[inline]
    pub(crate) fn note_symbol_yield(
        &mut self,
        callid: CallSiteId,
        symbol: IdentId,
        class: ClassId,
    ) {
        if let Some(t) = self.callsite_info[callid.0 as usize].symbol_twins.first()
            && t.symbol == symbol
            && t.class == class
        {
            return;
        }
        self.note_symbol_yield_slow(callid, symbol, class)
    }

    #[inline(never)]
    fn note_symbol_yield_slow(&mut self, callid: CallSiteId, symbol: IdentId, class: ClassId) {
        let cs = &mut self.callsite_info[callid.0 as usize];
        if let Some(i) = cs.symbol_twins.iter().position(|t| t.symbol == symbol) {
            cs.symbol_twins[i].class = class;
            cs.symbol_twins[..=i].rotate_right(1);
            return;
        }
        if cs.symbol_twins.len() >= SYMBOL_TWINS_MAX || !cs.is_simple() || cs.pos_num != 1 {
            return;
        }
        let (bc_pos, recv, dst) = (cs.bc_pos, cs.args, cs.dst);
        let twin = self.new_callsite(
            Some(symbol),
            bc_pos,
            0,
            recv,
            indexmap::IndexMap::default(),
            vec![],
            vec![],
            vec![],
            None,
            None,
            recv,
            recv,
            dst,
            false,
            false,
            false,
        );
        self.callsite_info[callid.0 as usize].symbol_twins.insert(
            0,
            SymbolTwin {
                symbol,
                twin,
                class,
            },
        );
    }

    /// The [`SymbolTwin`] of the `yield` site *callid* for *symbol*.
    pub(crate) fn symbol_twin(&self, callid: CallSiteId, symbol: IdentId) -> Option<&SymbolTwin> {
        self.callsite_info[callid.0 as usize]
            .symbol_twins
            .iter()
            .find(|t| t.symbol == symbol)
    }

    pub(crate) fn new_constsite(
        &mut self,
        base: Option<SlotId>,
        name: IdentId,
        prefix: Vec<IdentId>,
        toplevel: bool,
    ) -> ConstSiteId {
        self.new_constsite_with_loc(base, name, prefix, toplevel, None)
    }

    pub(crate) fn new_constsite_with_loc(
        &mut self,
        base: Option<SlotId>,
        name: IdentId,
        prefix: Vec<IdentId>,
        toplevel: bool,
        source_loc: Option<(String, u32)>,
    ) -> ConstSiteId {
        let info = ConstSiteInfo {
            name,
            base,
            prefix,
            toplevel,
            source_loc,
            cache: None,
        };
        let id = self.constsite_info.len();
        self.constsite_info.push(info);
        ConstSiteId(id as u32)
    }

    pub(crate) fn new_optcase(
        &mut self,
        min: u16,
        max: u16,
        branch_table: Vec<u32>,
        offsets: Vec<u32>,
    ) -> OptCaseId {
        let id = self.optcase_info.len();
        let branch_table = branch_table.into_boxed_slice();
        self.optcase_info.push(OptCaseInfo {
            min,
            max,
            branch_table,
            offsets,
        });
        OptCaseId::from(id as u32)
    }

    pub(crate) fn new_callsite_map_entry(
        &mut self,
        iseq_id: ISeqId,
        bc_pos: BcIndex,
        callsite_id: CallSiteId,
    ) {
        self[iseq_id].callsite_map.insert(bc_pos, callsite_id);
    }

    pub(crate) fn get_callsite_id(&self, iseq_id: ISeqId, bc_pos: BcIndex) -> Option<CallSiteId> {
        self[iseq_id].callsite_map.get(&bc_pos).cloned()
    }

    pub(crate) fn set_func_data(&mut self, func_id: FuncId) {
        let info = self.iseq(func_id);
        let regs = info.total_reg_num();
        let pc = info.get_top_pc();
        self[func_id].set_pc_regnum(pc, u16::try_from(regs).unwrap());
    }

    ///
    /// Check whether a method *name* of class *class_id* exists.
    ///
    /// This fn does not consider visibility.
    ///
    pub(crate) fn check_method_for_class(
        &self,
        class_id: ClassId,
        name: IdentId,
    ) -> Option<MethodTableEntry> {
        let class_version = Globals::class_version();
        self.check_method_for_class_with_version(class_id, name, class_version)
    }

    ///
    /// `class_id` (its whole ancestor chain) defines no `to_str`.
    ///
    /// Memoized per class_version on the ClassInfo, so hot coercion checks
    /// (e.g. the reverse-dispatch test in `String#==` against a nil rhs)
    /// cost a load + compare instead of a method-table probe. Defining
    /// `to_str` later bumps class_version, which invalidates the memo.
    ///
    /// `version` must be the current class_version; it is taken as a
    /// parameter because `Globals::class_version()` borrows the CODEGEN
    /// RefCell, which is unavailable when this is called from JIT
    /// compilation (use `JitContext::class_version()` there).
    /// The function whose `lexical_context` governs unqualified constant
    /// lookup from `fid`.
    ///
    /// A block carries no lexical context of its own (it is written inside
    /// an enclosing body and inherits that scope), and `Lfp::outermost`
    /// stops at a `define_method` body because that frame *is* the method
    /// boundary — so a bmethod's `method_func_id` is the block itself.
    /// Climb to the nearest enclosing iseq that owns a lexical context, so
    /// `K` inside `define_method(:m) { K }` resolves through the enclosing
    /// `module`/`class` nesting exactly as in a plain block.
    ///
    /// The climb is by `outer`, one enclosing body at a time, not straight
    /// to the mother: a receiver-anchored string eval body
    /// (`K.class_eval "[1].each { C }"`) carries a lexical context of its
    /// own between a block written in it and the method that called the
    /// eval.
    pub(crate) fn lexical_owner(&self, fid: FuncId) -> FuncId {
        let mut fid = fid;
        while let Some(iseq) = self[fid].is_iseq()
            && self[iseq].lexical_context.is_empty()
        {
            let enclosing = self[iseq].outer.unwrap_or(self[iseq].mother().0);
            let enclosing = self[enclosing].func_id();
            if enclosing == fid {
                break;
            }
            fid = enclosing;
        }
        fid
    }

    const NO_CONVERSION_SLOTS: usize = 64;

    ///
    /// Whether CRuby's `rb_check_funcall(obj, name)` on an instance of
    /// `class_id` answers "does not respond" without running anything:
    /// the class defines no `name`, and its `respond_to?`,
    /// `respond_to_missing?` and `method_missing` are the built-in ones
    /// (or absent, as on a `BasicObject`).
    ///
    /// This is the outcome of nearly every conversion probe that misses
    /// (`[1, 2].join`, a block destructuring an Integer, `"a" == :a`
    /// ...), so it is memoized: a direct-mapped table keyed by class,
    /// name and class version, which any method definition moves. A hit
    /// is one load and compare instead of four method-table probes.
    ///
    /// `version` must be the current class version.
    ///
    pub(crate) fn no_conversion(&self, class_id: ClassId, name: IdentId, version: u32) -> bool {
        let (slot, key) = self.no_conversion_slot(class_id, name, version);
        if slot.get() == key {
            return true;
        }
        let fid = |class: ClassId, m: IdentId| {
            self.check_method_for_class_with_version(class, m, version)
                .and_then(|e| e.func_id())
        };
        let builtin = |m: IdentId, owner: ClassId| match fid(class_id, m) {
            None => true,
            f => f == fid(owner, m),
        };
        let holds = fid(class_id, name).is_none()
            && builtin(IdentId::RESPOND_TO_, OBJECT_CLASS)
            && builtin(IdentId::RESPOND_TO_MISSING_, OBJECT_CLASS)
            && builtin(IdentId::METHOD_MISSING, BASIC_OBJECT_CLASS);
        if holds {
            slot.set(key);
        }
        holds
    }

    /// [`Self::no_conversion`] as far as it is already known: `true`
    /// is that answer, `false` is "not decided yet".
    pub(crate) fn no_conversion_memoized(
        &self,
        class_id: ClassId,
        name: IdentId,
        version: u32,
    ) -> bool {
        let (slot, key) = self.no_conversion_slot(class_id, name, version);
        slot.get() == key
    }

    fn no_conversion_slot(
        &self,
        class_id: ClassId,
        name: IdentId,
        version: u32,
    ) -> (&std::cell::Cell<(u32, u32, u32)>, (u32, u32, u32)) {
        let key = (class_id.u32(), name.get(), version);
        let slot = (key.0.wrapping_mul(31) ^ key.1) as usize & (Self::NO_CONVERSION_SLOTS - 1);
        (&self.no_conversion_memo[slot], key)
    }

    pub(crate) fn no_to_str(&self, class_id: ClassId, version: u32) -> bool {
        if self[class_id].no_to_str_at() == Some(version) {
            return true;
        }
        // The JIT hands over the abstract state's class, which may be one
        // of the synthetic tags (no module to walk): BOOL stands for a
        // slot that is `true` or `false`, BIGNUM for a heap Integer.
        // Anything else without a module is unknown — answer "may have
        // `to_str`" so the caller takes the generic path.
        if class_id == BOOL_CLASS {
            return self.no_to_str(TRUE_CLASS, version) && self.no_to_str(FALSE_CLASS, version);
        }
        if class_id == BIGNUM_CLASS {
            return self.no_to_str(INTEGER_CLASS, version);
        }
        if self[class_id].try_get_module().is_none() {
            return false;
        }
        // Resolve via the uncached ancestor walk rather than
        // `method_cache`: this runs at most once per class per
        // class_version (the memo covers repeats), and it must also be
        // callable from JIT compilation, where the global cache's RefCell
        // may already be borrowed.
        if self
            .search_method_by_class_id(class_id, IdentId::TO_STR)
            .is_none()
        {
            self[class_id].set_no_to_str_at(version);
            true
        } else {
            false
        }
    }

    ///
    /// `!=` on `class_id` resolves to a basic op (`String#!=` or the
    /// default `BasicObject#!=`), i.e. `a != b` may be computed as
    /// `!(a == b)` without dispatching `!=`. Memoized per class_version.
    ///
    /// Returns `None` when basic; otherwise the custom entry the caller
    /// must dispatch.
    ///
    pub(crate) fn set_default_neq(&mut self, fid: FuncId) {
        self.default_neq = Some(fid);
    }

    /// The builtin `BasicObject#!=`: what `!=` resolves to for a class
    /// that has none of its own.
    pub(crate) fn default_neq(&self) -> Option<FuncId> {
        self.default_neq
    }

    pub(crate) fn set_symbol_eq_fid(&mut self, fid: FuncId) {
        self.symbol_eq_fid = Some(fid);
    }

    /// Whether *fid* is one of the builtin `==`s that answer by comparing
    /// the two words — `BasicObject#==` (object identity) and `Symbol#==`
    /// (a Symbol is an immediate, equal only to itself). `a != b` through
    /// the default `!=` is then a compare of the words too, which is what
    /// the JIT emits for it (`builtins::object::object_ne`).
    pub(crate) fn is_identity_eq(&self, fid: FuncId) -> bool {
        Some(fid) == self.identity_eq_fid || Some(fid) == self.symbol_eq_fid
    }

    /// Record the FuncIds of the default `initialize_copy` /
    /// `initialize_dup` / `initialize_clone` builtins (see
    /// `uses_default_copy_hooks`).
    pub(crate) fn set_default_copy_hooks(&mut self, copy: FuncId, dup: FuncId, clone: FuncId) {
        self.default_copy_hooks = Some([copy, dup, clone]);
    }

    ///
    /// `class_id` (its whole ancestor chain) uses the *default*
    /// `initialize_copy` / `initialize_dup` / `initialize_clone` builtins,
    /// i.e. none of them is user-overridden. When true, `Kernel#dup` /
    /// `#clone` can return the raw shallow copy without dispatching the
    /// (no-op) copy hook — a hot-path win for `String#dup` and friends.
    ///
    /// Memoized per class_version (same pattern as `no_to_str`): any
    /// method (re)definition bumps class_version and invalidates the memo.
    /// `version` must be the current class_version.
    ///
    pub(crate) fn uses_default_copy_hooks(&self, class_id: ClassId, version: u32) -> bool {
        if self[class_id].default_copy_at() == Some(version) {
            return true;
        }
        let Some([copy, dup, clone]) = self.default_copy_hooks else {
            return false;
        };
        let resolves_to = |name: IdentId, default: FuncId| {
            self.search_method_by_class_id(class_id, name)
                .and_then(|e| e.func_id())
                == Some(default)
        };
        if resolves_to(IdentId::INITIALIZE_COPY, copy)
            && resolves_to(IdentId::INITIALIZE_DUP, dup)
            && resolves_to(IdentId::INITIALIZE_CLONE, clone)
        {
            self[class_id].set_default_copy_at(version);
            true
        } else {
            false
        }
    }

    /// Record the builtin iterators `Enumerator#with_index` / `#with_object`
    /// call directly rather than through their native adapter
    /// (`Enumerator#__direct_source`): `Array#each`, `Array#map` and
    /// `ArithmeticSequence#each`, in the order of `DirectSource` in
    /// `builtins/enumerator.rs`. Called once, when the Ruby core library
    /// has defined them, so that what a program defines later is not one.
    pub(crate) fn record_enum_direct_sources(&mut self) {
        self.enum_direct_sources = [
            (ARRAY_CLASS, "each"),
            (ARRAY_CLASS, "map"),
            (ARITHMETIC_SEQUENCE_CLASS, "each"),
        ]
        .map(|(class_id, name)| {
            self.search_method_by_class_id(class_id, IdentId::get_id(name))
                .and_then(|e| e.func_id())
        });
    }

    /// The builtin iterator recorded at `index`
    /// (`record_enum_direct_sources`).
    pub(crate) fn enum_direct_source(&self, index: usize) -> Option<FuncId> {
        self.enum_direct_sources[index]
    }

    /// Record what `to_s` resolves to on the classes an interpolation
    /// formats directly. Called once, when the builtins (the Rust ones and
    /// the Ruby core library) have finished defining themselves.
    pub(crate) fn record_builtin_to_s(&mut self) {
        let resolved = TO_S_SHORTCUT_CLASSES.map(|class_id| {
            self.search_method_by_class_id(class_id, IdentId::TO_S)
                .and_then(|e| e.func_id())
        });
        self.builtin_to_s = Some(resolved);
        self.symbol_string_methods =
            Some(crate::builtins::symbol::SYMBOL_STRING_METHODS.map(|name| {
                self.search_method_by_class_id(STRING_CLASS, IdentId::get_id(name))
                    .and_then(|e| e.func_id())
            }));
    }

    /// String's builtin `which`, for a Symbol method to run on the
    /// symbol's name whatever `String#upcase` and the like have since
    /// become. `None` while the core library is still loading.
    pub(crate) fn symbol_string_method(
        &self,
        which: crate::builtins::symbol::SymStr,
    ) -> Option<FuncId> {
        self.symbol_string_methods?[which as usize]
    }

    ///
    /// The classes of `TO_S_SHORTCUT_CLASSES` on which `to_s` is still the
    /// builtin one, as a bit per class (`TO_S_ALL_BUILTIN` when nobody has
    /// touched any) — so `"#{v}"` may format a `v` of such a class itself
    /// instead of calling the method.
    ///
    /// This is the test CRuby's `objtostring` makes before its own
    /// shortcuts (`vm_objtostring`: the method entry found for the
    /// receiver's class is `rb_int_to_s`, `rb_nil_to_s`, … ), widened to
    /// the classes monoruby formats directly: a `def to_s` in the class, a
    /// module prepended to it, an `undef_method` or a `remove_method` all
    /// make the lookup answer something else, and the interpolation then
    /// dispatches.
    ///
    /// Memoized per class_version (same pattern as `no_to_str`, kept here
    /// rather than on the classes so one compare answers for all of
    /// them): an interpolation asks once, and the common case is a load
    /// and a compare. `version` must be the current class_version.
    ///
    #[inline]
    pub(crate) fn to_s_builtin_classes(&self, version: u32) -> u8 {
        let memo = self.to_s_builtin_at.get();
        if memo >> 8 == version as u64 {
            memo as u8
        } else {
            self.resolve_to_s_builtin_classes(version)
        }
    }

    /// `to_s_builtin_classes(version) == TO_S_ALL_BUILTIN`, which is what
    /// every interpolation asks first.
    #[inline]
    pub(crate) fn to_s_all_builtin(&self, version: u32) -> bool {
        self.to_s_builtin_at.get() == to_s_memo(version, TO_S_ALL_BUILTIN)
            || self.resolve_to_s_builtin_classes(version) == TO_S_ALL_BUILTIN
    }

    #[cold]
    #[inline(never)]
    fn resolve_to_s_builtin_classes(&self, version: u32) -> u8 {
        let Some(builtin) = self.builtin_to_s else {
            return TO_S_ALL_BUILTIN;
        };
        let mut classes = 0;
        for (i, class_id) in TO_S_SHORTCUT_CLASSES.into_iter().enumerate() {
            let found = self
                .check_method_for_class_with_version(class_id, IdentId::TO_S, version)
                .and_then(|e| e.func_id());
            if found.is_some() && found == builtin[i] {
                classes |= 1 << i;
            }
        }
        self.to_s_builtin_at.set(to_s_memo(version, classes));
        classes
    }

    ///
    /// `to_s` on `class_id` is still the builtin one
    /// (`to_s_builtin_classes`). A class that never had a shortcut is not
    /// builtin in this sense — nor is a singleton class of a Complex or a
    /// Rational.
    ///
    pub(crate) fn to_s_is_builtin(&self, class_id: ClassId, version: u32) -> bool {
        // The Bignum tag is a representation, not a method namespace.
        let class_id = if class_id == BIGNUM_CLASS {
            INTEGER_CLASS
        } else {
            class_id
        };
        match TO_S_SHORTCUT_CLASSES.iter().position(|c| *c == class_id) {
            Some(i) => self.to_s_builtin_classes(version) & (1 << i) != 0,
            None => false,
        }
    }

    ///
    /// Resolve `=~` on `class_id`, memoized per class_version (same
    /// pattern as `no_to_str`: the uncached ancestor walk runs at most
    /// once per class per class_version and is callable during JIT
    /// compilation, where the global cache's RefCell may be borrowed).
    /// `None` when the class (chain) defines no `=~`.
    ///
    pub(crate) fn match_method(&self, class_id: ClassId, version: u32) -> Option<FuncId> {
        if let Some((v, fid)) = self[class_id].match_method_at()
            && v == version
        {
            return Some(fid);
        }
        let fid = self
            .search_method_by_class_id(class_id, IdentId::_MATCH)?
            .func_id()?;
        self[class_id].set_match_method_at(version, fid);
        Some(fid)
    }

    ///
    /// `class_id#==` as C code calling it with `rb_funcall` finds it — no
    /// refinement, no visibility — memoized per class_version on the
    /// ClassInfo. `None`: the class has none (`undef_method :==`).
    ///
    pub(crate) fn funcall_eq_entry(&self, class_id: ClassId, version: u32) -> Option<FuncallEq> {
        if let Some((v, eq)) = self[class_id].eq_method_at()
            && v == version
        {
            return Some(eq);
        }
        let func_id = self
            .search_method_by_class_id(class_id, IdentId::_EQ)?
            .func_id()?;
        let eq = FuncallEq {
            func_id,
            native: self.native_unary_entry(func_id),
        };
        self[class_id].set_eq_method_at(version, eq);
        Some(eq)
    }

    /// The `FuncId` of [`Self::funcall_eq_entry`].
    pub(crate) fn funcall_eq(&self, class_id: ClassId, version: u32) -> Option<FuncId> {
        Some(self.funcall_eq_entry(class_id, version)?.func_id)
    }

    ///
    /// `func_id` as something a native method taking one argument can run
    /// in its own frame (`Lfp::become_frame_of`): a native method of
    /// exactly one required argument. Its frame is laid out as the
    /// caller's is, so only the `Meta` differs.
    ///
    fn native_unary_entry(&self, func_id: FuncId) -> Option<(Meta, BuiltinFn)> {
        let info = &self[func_id];
        let FuncKind::Builtin { abs_address } = info.kind else {
            return None;
        };
        let meta = info.meta();
        if !meta.is_simple() || info.req_num() != 1 {
            return None;
        }
        // SAFETY: `abs_address` of a `FuncKind::Builtin` was stored from a
        // `BuiltinFn` (`FuncInfo::new_native`).
        let func = unsafe { std::mem::transmute::<u64, BuiltinFn>(abs_address) };
        Some((meta, func))
    }

    pub(crate) fn custom_neq(&self, class_id: ClassId, version: u32) -> Option<MethodTableEntry> {
        if self[class_id].neq_basic_at() == Some(version) {
            return None;
        }
        // Uncached walk for the same reason as `no_to_str`. `!=` is the
        // default when it resolves to `BasicObject#!=` itself — the only
        // `!=` the builtins define, as in CRuby.
        match self.search_method_by_class_id(class_id, IdentId::_NEQ) {
            Some(entry) if entry.func_id().is_some() && entry.func_id() == self.default_neq => {
                self[class_id].set_neq_basic_at(version);
                None
            }
            Some(entry) => Some(entry),
            None => None,
        }
    }

    pub(crate) fn check_method_for_class_with_version(
        &self,
        class_id: ClassId,
        name: IdentId,
        class_version: u32,
    ) -> Option<MethodTableEntry> {
        if class_id == BOOL_CLASS {
            return self.check_bool_method_with_version(name, class_version);
        }
        // The Bignum tag is a representation, not a method namespace: every
        // heap Integer resolves methods exactly as a fixnum does.
        let class_id = if class_id == BIGNUM_CLASS {
            INTEGER_CLASS
        } else {
            class_id
        };
        let mut cache = self.method_cache.borrow_mut();
        if let Some(entry) = cache.get(class_id, name, class_version) {
            return entry.cloned();
        };
        let entry = self.classes.search_method_by_class_id(class_id, name);
        cache.insert((name, class_id), class_version, entry.clone());
        entry
    }

    /// Resolve `name` against `BOOL_CLASS` by looking up on both
    /// `TRUE_CLASS` and `FALSE_CLASS` and returning the entry only when
    /// they yield the **same** `FuncId`. The IC tag `BOOL_CLASS` is only
    /// safe under that invariant — if the two classes diverge (e.g. user
    /// code defines a method on only one of them), callers fall back to
    /// the per-class lookup.
    fn check_bool_method_with_version(
        &self,
        name: IdentId,
        class_version: u32,
    ) -> Option<MethodTableEntry> {
        let mut cache = self.method_cache.borrow_mut();
        if let Some(entry) = cache.get(BOOL_CLASS, name, class_version) {
            return entry.cloned();
        }
        let true_entry = self.classes.search_method_by_class_id(TRUE_CLASS, name);
        let false_entry = self.classes.search_method_by_class_id(FALSE_CLASS, name);
        let entry = match (true_entry, false_entry) {
            (Some(t), Some(f)) if t.func_id() == f.func_id() => Some(t),
            _ => None,
        };
        cache.insert((name, BOOL_CLASS), class_version, entry.clone());
        entry
    }

    ///
    /// Throw away the compiled bodies that inlined `class#name` without a
    /// runtime guard — and only those.
    ///
    /// Everything else keeps its JIT code: a method that inlines `Integer#+`
    /// is untouched when `Integer#~` is replaced, and any use of an operator
    /// that went out as a real method call is already covered by the
    /// class-version guard that every redefinition bumps. Returns the evicted
    /// ids so the caller can revert their entry jumps and decide whether any
    /// stale OSR loop body can still exist.
    ///
    pub(super) fn evict_jit_code_for_bop(
        &mut self,
        class: ClassId,
        name: IdentId,
    ) -> HashSet<ISeqId> {
        let mut stale = HashSet::default();
        for (i, info) in self.iseqs.iter_mut().enumerate() {
            if info.depends_on_bop(class, name) {
                info.evict_jit_code();
                stale.insert(ISeqId::new(i));
            }
        }
        for iseq_id in &stale {
            self.jit_forget_iseq(*iseq_id);
        }
        stale
    }
}

impl Store {
    /// Get class name of *ClassId*.
    pub(crate) fn debug_class_name(&self, class: impl Into<Option<ClassId>>) -> String {
        if let Some(class) = class.into() {
            let info = &self.classes[class];
            match info.get_name() {
                Some(_) => {
                    let v: Vec<_> = self.classes.get_parents(class).into_iter().rev().collect();
                    v.join("::")
                }
                None => match info.try_get_module() {
                    Some(class_obj) => match class_obj.is_singleton() {
                        None => format!("#<Class:{:016x}>", class_obj.as_val().id()),
                        Some(base) => format!("#<Class:{}>", base.debug_tos(self)),
                    },
                    // Virtual classes (e.g. BOOL_CLASS) have no Ruby-visible
                    // Module — fall back to the ClassId's Debug repr.
                    None => format!("{:?}", class),
                },
            }
        } else {
            "<INVALID>".to_string()
        }
    }

    #[cfg(feature = "emit-bc")]
    pub fn dump_iseq(&self, iseq_id: ISeqId) {
        use bytecodegen::BcIndex;

        let iseq = &self[iseq_id];
        eprintln!("------------------------------------");
        let loc = iseq.loc;
        let line = iseq.sourceinfo.get_line(&loc);
        let file_name = iseq.sourceinfo.file_name();
        eprintln!(
            "<{}> {file_name}:{line}",
            self.func_description(iseq.func_id()),
        );
        for (class, info) in &iseq.jit_entry {
            eprintln!("  JitEntry: {}", class.get_name(self));
            eprintln!("{:?}", &info.inline_cache_map);
        }
        eprintln!(
            "{:?} owner:{:?} local_vars:{} temp:{}",
            self[iseq.func_id()].meta(),
            self[iseq.func_id()].owner_class(),
            iseq.local_num(),
            iseq.temp_num
        );
        eprintln!("{:?}", self[iseq.func_id()].params());
        eprintln!("{:?}", iseq.get_exception_map());
        for i in 0..iseq.bytecode_len() {
            let bc_pos = BcIndex::from(i);
            if let Some(bbid) = iseq.bb_info.is_bb_head(bc_pos) {
                eprintln!("  {:?}", bbid);
            };
            let pc = iseq.get_pc(bc_pos);
            if let Some(fmt) = jitgen::trace_ir::TraceIr::format(self, iseq_id, pc) {
                eprintln!("    {bc_pos} [{:02}] {fmt}", iseq.sp[i].0);
            };
        }
        eprintln!("------------------------------------");
    }

    #[cfg(feature = "profile")]
    pub fn clear_stats(&mut self) {
        self.method_cache.get_mut().clear_stats();
    }

    #[cfg(feature = "profile")]
    pub(crate) fn show_stats(&self) {
        eprintln!("global method cache stats (top 20)");
        eprintln!("{:30} {:30} {:10}", "func name", "class", "count");
        eprintln!("------------------------------------------------------------------------");
        {
            let cache = self.method_cache.borrow();
            let mut v = cache.global_method_cache_stats();
            v.sort_unstable_by(|(_, a), (_, b)| b.cmp(a));
            for ((class_id, name), count) in v.into_iter().take(20) {
                eprintln!(
                    "{:30} {:30} {:10}",
                    name.to_string(),
                    self.debug_class_name(*class_id),
                    count
                );
            }
        }

        eprintln!();
        eprintln!("full method exploration stats (top 20)");
        eprintln!("{:25} {:45} {:10}", "func name", "class", "count");
        eprintln!(
            "----------------------------------------------------------------------------------"
        );
        {
            let cache = self.method_cache.borrow();
            let mut v = cache.method_exprolation_stats();
            v.sort_unstable_by(|(_, a), (_, b)| b.cmp(a));
            for ((class_id, name), count) in v.into_iter().take(20) {
                eprintln!(
                    "{:25} {:45} {:10}",
                    name.to_string(),
                    self.debug_class_name(*class_id),
                    count
                );
            }
        }

        eprintln!();
        let recorded = self
            .callsite_info
            .iter()
            .filter(|cs| !cs.pmc.entries().is_empty())
            .count();
        let poly: Vec<_> = self
            .callsite_info
            .iter()
            .filter(|cs| cs.pmc.is_polymorphic())
            .collect();
        let mega = poly.iter().filter(|cs| cs.pmc.overflow() != 0).count();
        eprintln!(
            "polymorphic method cache: {} sites recorded, {} polymorphic, {} megamorphic",
            recorded,
            poly.len(),
            mega
        );
        eprintln!("polymorphic sites (top 40 by slow-path observations)");
        eprintln!(
            " CallId  {:20} {:10} ways [class(/arg-class)(=func) xobservations]",
            "name", "overflow"
        );
        eprintln!(
            "------------------------------------------------------------------------------------------------------------------------"
        );
        let mut poly = poly;
        poly.sort_unstable_by_key(|cs| {
            std::cmp::Reverse(cs.pmc.entries().iter().map(|e| e.count as u64).sum::<u64>())
        });
        for cs in poly.into_iter().take(40) {
            let name = cs
                .name
                .map_or_else(|| "<super>".to_string(), |n| n.to_string());
            let ways = cs
                .pmc
                .entries()
                .iter()
                .map(|e| {
                    let mut s = self.debug_class_name(e.recv.id());
                    if let Some(arg) = e.arg {
                        s += &format!("/{}", self.debug_class_name(arg.id()));
                    }
                    if let Some(fid) = e.fid {
                        s += &format!("={}", self.func_description(fid));
                    }
                    format!("{s} x{}", e.count)
                })
                .collect::<Vec<_>>()
                .join(" | ");
            eprintln!(
                "({:6}) {:20} {:10} {}",
                cs.id.get(),
                name,
                cs.pmc.overflow(),
                ways
            );
        }
    }
}

/// Where the source array of an `ExpandArray` lives while `handle_args`
/// is still building the parameter layout. `Surface` slots are already
/// absolute (allocated among the visible params); `Destruct` slots are
/// offsets into the yet-to-be-appended `destruct_args` region and are
/// rebased once its start is known.
enum ExpandSrc {
    Surface(usize),
    Destruct(usize),
}

/// Recursively flattens a destructuring parameter (`|(a, *b, (c, d))|`)
/// into a sequence of `ExpandArray` descriptors. `src` names the slot
/// holding the array to split; each direct child gets a consecutive slot
/// in `destruct_args`, and every `Nested` child recurses with its own
/// slot as the source (a chained `ExpandArray` fills it in). Parent
/// descriptors are pushed before their children so the prologue emits
/// them in dependency order.
fn flatten_destruct(
    entries: &[DestructEntry],
    src: ExpandSrc,
    expand: &mut Vec<(ExpandSrc, usize, usize, Option<usize>)>,
    destruct_args: &mut Vec<Option<IdentId>>,
    src_enc: crate::value::Encoding,
) {
    let len = entries.len();
    let dst_start = destruct_args.len();
    let mut rest_pos = None;
    let mut child_slots = Vec::with_capacity(len);
    for (i, entry) in entries.iter().enumerate() {
        child_slots.push(destruct_args.len());
        match entry {
            DestructEntry::Param(name, _) => {
                destruct_args.push(Some(IdentId::get_id_from_source(name, src_enc)));
            }
            DestructEntry::Rest(name, _) => {
                rest_pos = Some(i);
                destruct_args
                    .push(name.as_ref().map(|n| IdentId::get_id_from_source(n, src_enc)));
            }
            DestructEntry::Nested(_, _) => {
                // Temp slot: the parent `ExpandArray` writes the sub-array
                // here, then the recursive descriptor splits it further.
                destruct_args.push(None);
            }
        }
    }
    expand.push((src, dst_start, len, rest_pos));
    for (i, entry) in entries.iter().enumerate() {
        if let DestructEntry::Nested(sub, _) = entry {
            flatten_destruct(
                sub,
                ExpandSrc::Destruct(child_slots[i]),
                expand,
                destruct_args,
                src_enc,
            );
        }
    }
}

impl Store {
    /// `src_enc` is the encoding of the source the parameters were
    /// written in, under which a non-ASCII name is interned
    /// ([`IdentId::get_id_from_source`]).
    pub(crate) fn handle_args(
        info: BlockInfo,
        for_params: Vec<(usize, BcLocal, IdentId)>,
        src_enc: crate::value::Encoding,
    ) -> Result<CompileInfo> {
        let BlockInfo {
            params,
            body: deref!(body),
            loc,
            ..
        } = info;
        let mut args_names = vec![];
        let mut keyword_names = vec![];
        let mut keyword_initializers = vec![];
        let mut destruct_args = vec![];
        let mut expand = vec![];
        let mut optional_info = vec![];
        let mut required_num = 0;
        let mut optional_num = 0;
        let mut post_num = 0;
        let mut rest = None;
        let mut rest_is_implicit = false;
        let mut kw_rest_param = None;
        let mut block_param = None;
        let mut for_param_info = vec![];
        let mut forwarding = false;
        let mut it_param = false;
        let mut forbid_keyword = false;
        for (dst_outer, dst_reg, _name) in for_params {
            for_param_info.push(ForParamInfo::new(dst_outer, dst_reg, args_names.len()));
            args_names.push(None);
            required_num += 1;
        }
        for param in params {
            match param.kind {
                ParamKind::Param(name) => {
                    args_names.push(Some(IdentId::get_id_from_source(&name, src_enc)));
                    required_num += 1;
                }
                ParamKind::ItParam => {
                    // Implicit `it`: bind a required local named `it` (so
                    // the body's `it` reads resolve), but flag it so
                    // `#parameters` omits the name.
                    args_names.push(Some(IdentId::get_id("it")));
                    required_num += 1;
                    it_param = true;
                }
                ParamKind::Destruct(entries) => {
                    // The whole destructured argument is bound to an
                    // anonymous surface slot; `flatten_destruct` records
                    // the `ExpandArray` that splits it into `destruct_args`
                    // slots, recursing into nested groups (chained
                    // `ExpandArray`s) and honoring an optional splat.
                    let src = args_names.len();
                    args_names.push(None);
                    required_num += 1;
                    flatten_destruct(&entries, ExpandSrc::Surface(src), &mut expand, &mut destruct_args, src_enc);
                }
                ParamKind::PostDestruct(entries) => {
                    // Same as `Destruct`, but the surface slot lives in the
                    // post region (after the rest), so it counts toward
                    // `post_num` rather than `required_num`.
                    let src = args_names.len();
                    args_names.push(None);
                    post_num += 1;
                    flatten_destruct(&entries, ExpandSrc::Surface(src), &mut expand, &mut destruct_args, src_enc);
                }
                ParamKind::Optional(name, deref!(initializer)) => {
                    let local = BcLocal(args_names.len() as u16);
                    args_names.push(Some(IdentId::get_id_from_source(&name, src_enc)));
                    optional_num += 1;
                    optional_info.push(OptionalInfo::new(local, initializer));
                }
                ParamKind::Rest(name) => {
                    assert_eq!(rest, None);
                    rest = Some(args_names.len());
                    args_names.push(name.map(|n| IdentId::get_id_from_source(&n, src_enc)));
                }
                ParamKind::ImplicitRest => {
                    // `|a,|` — synthetic anonymous rest emitted for a
                    // block-param trailing comma. Allocates a slot
                    // (so block dispatch can use it as a regular rest)
                    // but flags `rest_is_implicit` so arity / strict
                    // checks treat it as absent. Semantically it's
                    // equivalent to `|a, *|` for proc/block but to
                    // `|a|` for `define_method` / lambda. See
                    // `ParamsInfo::is_explicit_rest`.
                    assert_eq!(rest, None);
                    rest = Some(args_names.len());
                    rest_is_implicit = true;
                    args_names.push(None);
                }
                ParamKind::Post(name) => {
                    args_names.push(name.map(|n| IdentId::get_id_from_source(&n, src_enc)));
                    post_num += 1;
                }
                ParamKind::Forwarding => {
                    assert_eq!(rest, None);
                    rest = Some(args_names.len());
                    args_names.push(None);
                    assert!(kw_rest_param.is_none());
                    kw_rest_param = Some(SlotId(1 + args_names.len() as u16));
                    args_names.push(None);
                    block_param = Some(IdentId::get_id(""));
                    forwarding = true;
                }
                ParamKind::Keyword(name, init) => {
                    let name = IdentId::get_id_from_source(&name, src_enc);
                    args_names.push(Some(name));
                    keyword_names.push(name);
                    keyword_initializers.push(init);
                }
                ParamKind::KWRest(name) => {
                    // `**nil` is lowered to a kwrest literally named `nil`
                    // (an impossible real variable name), which flags a
                    // keyword-forbidding definition.
                    if name.as_deref() == Some("nil") {
                        forbid_keyword = true;
                    }
                    let name = name.map(|n| IdentId::get_id_from_source(&n, src_enc));
                    assert!(kw_rest_param.is_none());
                    kw_rest_param = Some(SlotId(1 + args_names.len() as u16));
                    args_names.push(name);
                }
                ParamKind::Block(name) => {
                    block_param = Some(match name {
                        Some(name) => IdentId::get_id_from_source(&name, src_enc),
                        None => IdentId::get_id(""),
                    });
                }
            }
        }

        // `destruct_args` is appended after all surface params, so the
        // destruct region begins at the current `args_names.len()`.
        // Surface srcs are already absolute; destruct-relative srcs and
        // all dsts are rebased onto that region here.
        let base = args_names.len();
        let expand_info: Vec<_> = expand
            .into_iter()
            .map(|(src, dst, len, rest_pos)| {
                let src = match src {
                    ExpandSrc::Surface(i) => i,
                    ExpandSrc::Destruct(o) => base + o,
                };
                DestructureInfo::new(src, base + dst, len, rest_pos)
            })
            .collect();
        args_names.append(&mut destruct_args);
        let kw_required: Vec<bool> = keyword_initializers
            .iter()
            .map(|init| init.is_none())
            .collect();
        let params_info = ParamsInfo::new(
            required_num,
            optional_num,
            rest,
            rest_is_implicit,
            post_num,
            args_names,
            keyword_names,
            kw_required,
            kw_rest_param,
            block_param,
            forwarding,
            it_param,
            forbid_keyword,
        );
        let compile_info = CompileInfo::new(
            body,
            params_info,
            keyword_initializers,
            expand_info,
            optional_info,
            for_param_info,
            loc,
        );
        Ok(compile_info)
    }
}

pub struct ClassInfoTable {
    table: Vec<ClassInfo>,
    /// `ClassInfo::object` mirrored into a flat, machine-code-readable
    /// table indexed by `ClassId` (8-byte stride, `None` reads as 0), so
    /// JIT code can go from a value's class id to its class object without
    /// a call: the inline `Module#===` (`AsmInst::KindOfConst`) walks the
    /// superclass chain from here through `GLOBALS_CLASS_OBJECTS`. Kept in
    /// step by `set_object` and the growth paths below.
    pub(in crate::globals) objects: MonoVec<Option<Module>>,
    /// The functions monoruby itself defines — natives and the
    /// `builtins/*.rb` bootstrap bodies — as they are installed as
    /// methods; `is_internal_helper` finds its own helpers among them.
    pub(in crate::globals) builtin_funcs: HashSet<FuncId>,
    /// Classes whose `ClassInfo::instance_singleton` was just set, waiting
    /// for `Store::note_instance_singletons` to poison the compiled units
    /// that assumed otherwise (the unit index lives on `Store`).
    pub(in crate::globals) new_instance_singletons: Vec<ClassId>,
    /// The index behind `Class#subclasses` — see `SubclassIndex`.
    pub(in crate::globals) subclass_index: std::cell::RefCell<SubclassIndex>,
}

/// Direct subclasses by superclass, built lazily from the class table.
///
/// The table only grows and a class's real superclass is fixed once it
/// has one, so the index is extended over the entries added since the
/// last query rather than rebuilt: `Class#subclasses` then costs the
/// number of answers, not the number of classes (singleton classes and
/// iclasses included) the process ever created.
#[derive(Default)]
pub struct SubclassIndex {
    /// Table entries below this are indexed (or parked in `pending`).
    upto: usize,
    /// Real superclass -> its direct subclasses, in creation order.
    children: HashMap<ClassId, Vec<ClassId>>,
    /// Entries that could not be filed yet: no class object, or a class
    /// without a superclass (`Class.allocate` before `initialize`).
    pending: Vec<ClassId>,
}

impl std::ops::Index<ClassId> for ClassInfoTable {
    type Output = ClassInfo;
    fn index(&self, index: ClassId) -> &Self::Output {
        &self.table[index.u32() as usize]
    }
}

impl std::ops::IndexMut<ClassId> for ClassInfoTable {
    fn index_mut(&mut self, index: ClassId) -> &mut Self::Output {
        &mut self.table[index.u32() as usize]
    }
}

impl ClassInfoTable {
    fn new() -> Self {
        let mut objects = MonoVec::with_capacity(256);
        for _ in 0..100 {
            objects.push(None);
        }
        Self {
            table: vec![ClassInfo::new(); 100],
            objects,
            builtin_funcs: HashSet::default(),
            new_instance_singletons: vec![],
            subclass_index: Default::default(),
        }
    }

    /// Attach the class object of *id*, in both the table and its
    /// machine-code mirror.
    pub(in crate::globals) fn set_object(&mut self, id: ClassId, obj: Module) {
        self.table[id.u32() as usize].object = Some(obj);
        self.objects[id.u32() as usize] = Some(obj);
    }

    /// Number of slots in the class table, including the unused
    /// `ClassId(0)` slot at index 0. Used by sweep-style operations
    /// that need to iterate every live class (e.g. propagating a
    /// rebind across descendants for `Module#name`).
    pub(crate) fn classinfo_len(&self) -> usize {
        self.table.len()
    }

    fn add_class(&mut self) -> ClassId {
        let id = self.table.len();
        self.table.push(ClassInfo::new());
        self.objects.push(None);
        ClassId::new(id as u32)
    }

    fn copy_class(&mut self, original_class: ClassId) -> ClassId {
        let id = self.table.len();
        let info = self[original_class].copy();
        self.objects.push(info.object);
        self.table.push(info);
        ClassId::new(id as u32)
    }

    fn def_builtin_class(&mut self, class: ClassId) {
        self[class] = ClassInfo::new();
        self.objects[class.u32() as usize] = None;
    }

    pub(crate) fn search_method_by_class_id(
        &self,
        class_id: ClassId,
        name: IdentId,
    ) -> Option<MethodTableEntry> {
        let module = self[class_id].get_module();
        self.search_method(module, name)
    }

    /// The original definition name of `name` as resolved from
    /// `class_id`'s ancestor chain (following `alias` /
    /// `define_method`), falling back to `name` itself when the
    /// method is not found. Seeds `Method#original_name` for
    /// `Object#method`, `Module#instance_method`, … .
    pub(crate) fn original_name_by_class_id(
        &self,
        class_id: ClassId,
        name: IdentId,
    ) -> IdentId {
        self.search_method_by_class_id(class_id, name)
            .map(|e| e.original_name())
            .unwrap_or(name)
    }
}

#[derive(Debug, Clone)]
pub struct ConstCache {
    pub version: usize,
    pub base_class: Option<Value>,
    /// `Some` when the resolution depended on the receiver (the frame's
    /// `self` had a singleton class — a `def` in a `class << obj` body
    /// resolves constants against the *runtime* singleton cref, see
    /// `Executor::substitute_singleton_cref`). The cache only hits for
    /// the same self class.
    pub self_class: Option<ClassId>,
    pub value: Value,
}

impl alloc::GC<RValue> for ConstCache {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        if let Some(v) = &self.base_class {
            v.mark(alloc)
        }
        self.value.mark(alloc);
    }
}

#[derive(Debug, Clone)]
pub struct ConstSiteInfo {
    /// Name of constants.
    pub name: IdentId,
    /// The slot of base object.
    pub(crate) base: Option<SlotId>,
    /// Qualifier.
    pub prefix: Vec<IdentId>,
    /// Is toplevel?. (e.g. ::Foo)
    pub toplevel: bool,
    /// Source location captured at compile time, used by
    /// `Module#const_source_location` for stores. `None` for load sites
    /// and for stores whose location is not tracked.
    pub source_loc: Option<(String, u32)>,
    /// Inline constant cache.
    pub cache: Option<ConstCache>, //(version, base_class, value)
}

impl alloc::GC<RValue> for ConstSiteInfo {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        if let Some(cache) = &self.cache {
            cache.mark(alloc);
        }
    }
}

impl ConstSiteInfo {
    pub fn format(&self) -> String {
        let ConstSiteInfo {
            name,
            base,
            prefix,
            toplevel,
            ..
        } = self;
        let mut const_name = if *toplevel { "::" } else { "" }.to_string();
        for c in prefix {
            const_name += &format!("{:?}::", c);
        }
        const_name += &format!("{:?}", name);
        format!(
            "{}{}",
            if let Some(base) = base {
                format!("{:?}::", base)
            } else {
                "".to_string()
            },
            const_name
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct ConstSiteId(pub u32);

/// Number of ways a call site's polymorphic method cache keeps before it
/// counts further keys as megamorphic overflow.
pub const PMC_WAYS: usize = 4;

/// The classes whose instances `Executor::tos_direct` and
/// `runtime::concatenate_string_inner` turn into a String without calling
/// `to_s` — while that method is still the builtin one
/// (`Store::to_s_builtin_classes`).
const TO_S_SHORTCUT_CLASSES: [ClassId; 8] = [
    INTEGER_CLASS,
    FLOAT_CLASS,
    NIL_CLASS,
    TRUE_CLASS,
    FALSE_CLASS,
    SYMBOL_CLASS,
    COMPLEX_CLASS,
    RATIONAL_CLASS,
];

/// `Store::to_s_builtin_classes` when every class still has its builtin
/// `to_s`.
const TO_S_ALL_BUILTIN: u8 = u8::MAX;

/// The word `Store::to_s_builtin_at` holds for `classes` at `version`.
const fn to_s_memo(version: u32, classes: u8) -> u64 {
    (version as u64) << 8 | classes as u64
}

/// The key of a compilation unit's salvage record, and of its entry in the
/// method-name index: `(iseq, self_class, None)` for a whole-method unit,
/// `(iseq, self_class, Some(LoopStart))` for an OSR loop body.
pub(crate) type JitUnitId = (ISeqId, ClassId, Option<crate::bytecodegen::BcIndex>);

/// One observed key of a call site's polymorphic method cache.
#[derive(Debug, Clone)]
pub struct PolyCacheEntry {
    /// The receiver's class (`class_for_ic`-unified for method calls, so
    /// `true`/`false` share a way when their method is unified).
    pub recv: CachedClass,
    /// The first argument's class — recorded for the pair-keyed sites
    /// (BinOp / Cmp / Index / StoreIndex), `None` for method calls and
    /// unary operators.
    pub arg: Option<CachedClass>,
    /// The resolved method — method-call sites only; the operator/index
    /// sites re-resolve from the class at JIT compile time.
    pub fid: Option<FuncId>,
    /// How many times the slow path (re-)observed this key. Not a call
    /// count: the fast paths never record, so steady-state monomorphic
    /// traffic does not bump this.
    pub count: u32,
}

/// Polymorphic method cache of a call site: the receiver(-pair) classes
/// the VM observed on its slow paths, up to [`PMC_WAYS`] of them.
/// Recording-only for now — the JIT does not consume it yet (see
/// `doc/polymorphic_call.md` §7); dump it with the `profile` feature.
#[derive(Debug, Clone, Default)]
pub struct PolyCache {
    entries: Vec<PolyCacheEntry>,
    /// Recordings that arrived with a fifth distinct key: the site is
    /// megamorphic and a consumer should give up on way dispatch.
    overflow: u32,
}

impl PolyCache {
    pub fn record(&mut self, recv: CachedClass, arg: Option<CachedClass>, fid: Option<FuncId>) {
        for e in self.entries.iter_mut() {
            if e.recv == recv && e.arg == arg {
                e.count += 1;
                if fid.is_some() {
                    // A later resolution wins: a redefinition changed the
                    // target while the key stayed the same.
                    e.fid = fid;
                }
                return;
            }
        }
        if self.entries.len() < PMC_WAYS {
            self.entries.push(PolyCacheEntry {
                recv,
                arg,
                fid,
                count: 1,
            });
        } else {
            self.overflow += 1;
        }
    }

    pub fn entries(&self) -> &[PolyCacheEntry] {
        &self.entries
    }

    pub fn overflow(&self) -> u32 {
        self.overflow
    }

    ///
    /// Every slow-path observation this site made, the megamorphic overflow
    /// included. A way's `count` divided by this is its **share** of the
    /// site's misses — the signal a consumer needs to tell "two classes in
    /// steady alternation" from "one class plus a rare tail", which the way
    /// count alone cannot distinguish.
    ///
    /// Like [`PolyCacheEntry::count`] this is a miss count, not a call
    /// count, so it says nothing about how hot the site is — only about how
    /// the misses are distributed across the classes.
    ///
    pub fn observations(&self) -> u32 {
        self.entries
            .iter()
            .fold(self.overflow, |acc, e| acc.saturating_add(e.count))
    }

    /// Two or more ways (or an overflow) were observed.
    pub fn is_polymorphic(&self) -> bool {
        self.entries.len() >= 2 || self.overflow != 0
    }
}

///
/// The argument shapes only a small minority of call sites have.
///
/// A site with none of them is the overwhelming majority — 99.8% of the
/// call sites in a definition-heavy file, 98% among the builtins loaded
/// at startup (`MONORUBY_STORE_STATS=1` counts them). Inline, the three
/// empty containers cost 120 bytes at every one of those sites; behind
/// one pointer they cost eight.
///
#[derive(Debug, Clone, Default)]
pub struct CallSiteExtra {
    /// Positions of splat arguments.
    pub splat_pos: Vec<usize>,
    /// Names and positions of keyword arguments.
    pub kw_args: indexmap::IndexMap<IdentId, usize>,
    /// Position of hash splat arguments.
    pub hash_splat_pos: Vec<SlotId>,
    /// What each register of the keyword window holds, in source order.
    pub kw_order: Vec<KwElem>,
}

///
/// One register of a call site's keyword window.
///
/// `kw_order[i]` describes the register at `kw_pos + i`, and the vector
/// is in source order — which is what neither `kw_args` nor
/// `hash_splat_pos` records on its own. The interleaving decides which
/// value wins when a key appears on both sides (`f(**defaults, key:
/// override)`) and the key order of a materialized keyword hash, so
/// every site that builds one walks this list rather than concatenating
/// the two containers (#1407).
///
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KwElem {
    /// A literal `k: v` pair, by key; its value is in this register.
    Kw(IdentId),
    /// A `**hash` splat; its operand is in this register.
    Splat,
}

/// The empty map `CallSiteInfo::kw_args` hands out for a site with no
/// keyword arguments. `IndexMap::new` does not allocate, so this is one
/// shared, empty header.
static NO_KW_ARGS: std::sync::LazyLock<indexmap::IndexMap<IdentId, usize>> =
    std::sync::LazyLock::new(indexmap::IndexMap::new);

/// Infomation for a call site.
#[derive(Debug, Clone)]
pub struct CallSiteInfo {
    /// Call site id.
    pub id: CallSiteId,
    /// Name of method. (None for *super*)
    pub name: Option<IdentId>,
    /// Position of the callsite in bytecode.
    pub(crate) bc_pos: BcIndex,
    /// Position of the receiver.
    pub(crate) recv: SlotId,
    /// Position of the first argument.
    pub(crate) args: SlotId,
    /// Number of positional arguments.
    pub pos_num: usize,
    /// Splat / keyword / hash-splat arguments, or `None` when the site
    /// passes none — see [`CallSiteExtra`].
    extra: Option<Box<CallSiteExtra>>,
    /// *FuncId* of passed block.
    pub block_fid: Option<FuncId>,
    /// Position of block argument.
    pub(crate) block_arg: Option<SlotId>,
    /// Postion of keyword arguments.
    pub(crate) kw_pos: SlotId,
    /// Position where the result is to be stored to.
    pub(crate) dst: Option<SlotId>,
    pub(crate) forwarding: bool,
    /// Treat this call as a function call for visibility purposes
    /// (bypass `private`), regardless of whether the receiver is
    /// `self`. Set for the privileged `recv.__builtin_initialize__(...)`
    /// intrinsic spelling so `Class#new` can drive a class's private
    /// `initialize` while keeping the natural forwarding-call shape
    /// (which the D1 forwarding-rest deferral can specialize).
    pub(crate) bypass_visibility: bool,
    /// A "variable call" (bare identifier, no receiver / args /
    /// parens): a failed lookup raises NameError instead of
    /// NoMethodError.
    pub(crate) vcall: bool,
    /// Polymorphic method cache — the receiver(-pair) classes the VM
    /// observed at this site. See [`PolyCache`].
    pub pmc: PolyCache,
    /// For `recv.send(:name, ...)` / `recv.__send__(:name, ...)` written
    /// with a literal Symbol: an alternate call site describing the direct
    /// call `recv.name(...)` — same receiver, the arguments past the name,
    /// the same result slot, and `bypass_visibility` (a `send` reaches
    /// private methods). Prepared by bytecodegen and never executed by the
    /// VM; the JIT compiles it in place of the name lookup once it has
    /// established that this site still resolves to the builtin `send`
    /// (a redefinition moves the class version, which retires the code).
    pub(crate) send_direct: Option<CallSiteId>,
    /// For `recv.new(args)` with plain positionals and no block: an
    /// alternate call site describing `obj.initialize(args)` — the
    /// receiver is this site's result slot (where the new object lands),
    /// the arguments are this site's, the result is discarded, and
    /// `bypass_visibility` is set as for the Ruby `Class#new`'s
    /// `__builtin_initialize__`. Prepared by bytecodegen and never
    /// executed by the VM; the JIT's inline `Class#new` compiles the
    /// constructor through it as a frameless callee of the allocating
    /// site (`inline_class_new`).
    pub(crate) class_new_init: Option<CallSiteId>,
    /// For `yield [e1, .., en]` (n >= 2, no splat): an alternate call site
    /// describing `yield e1, .., en` — the elements are the temporaries
    /// the Array literal is built from (this site's argument slot + 1 ..),
    /// with the same result slot. A block that auto-splats a lone Array
    /// argument (`FuncInfo::single_arg_expand`) binds its parameters the
    /// same either way, so the JIT yields through this twin and never
    /// builds the Array (`JitContext::yield_expanded_for`). Prepared by
    /// bytecodegen and never executed by the VM.
    pub(crate) yield_expanded: Option<CallSiteId>,
    /// `[e1, .., en].min` / `.max` written on a non-constant Array literal
    /// itself, with no argument and no block — the shape CRuby compiles
    /// to `opt_newarray_send`, which never builds the Array and compares
    /// *each element with the running extreme* (`v <=> best`), where
    /// `Array#min` / `#max` called on an Array compare the running extreme
    /// with each element (`best <=> v`). The two differ in the receiver a
    /// user-defined `<=>` sees and in the operands a failed comparison
    /// names: `[x, "a"].min` is "comparison of String with 1 failed",
    /// `a = [x, "a"]; a.min` "comparison of Integer with String failed".
    /// The builtin (`array::newarray_send`) and the JIT's elided form
    /// (`runtime::opt_newarray_min`) read the flag.
    pub(crate) newarray_recv: bool,
    /// For a `yield x` site: the call `x.sym` it amounts to for each
    /// `&:sym` block it has been reached with — see [`SymbolTwin`]. The
    /// one answered last comes first.
    symbol_twins: Vec<SymbolTwin>,
}

///
/// `yield x` to a `&:sym` block is the call `x.sym`. A method specialized
/// for a call site that passes `&:sym` compiles its `yield` as that call
/// (`JitContext::symbol_yield`), and a call needs a call site with the
/// method's name on it: this is one, made the first time the yield site
/// runs with that symbol, with the yielded value as the receiver and the
/// yield's destination.
///
/// The yield bytecode has no inline cache to say what class the receiver
/// was, so the run-time helpers note the last one here.
///
#[derive(Debug, Clone)]
pub(crate) struct SymbolTwin {
    pub(crate) symbol: IdentId,
    /// The call site of `x.sym`.
    pub(crate) twin: CallSiteId,
    /// The class of the value yielded last.
    pub(crate) class: ClassId,
}

/// How many symbols one `yield` site keeps a [`SymbolTwin`] for.
const SYMBOL_TWINS_MAX: usize = 8;

impl CallSiteInfo {
    /// Positions of splat arguments; empty when the site has none.
    pub fn splat_pos(&self) -> &[usize] {
        self.extra.as_ref().map_or(&[], |e| &e.splat_pos)
    }

    /// Names and positions of keyword arguments; empty when the site
    /// has none.
    pub fn kw_args(&self) -> &indexmap::IndexMap<IdentId, usize> {
        self.extra.as_ref().map_or(&NO_KW_ARGS, |e| &e.kw_args)
    }

    /// Positions of hash splat arguments; empty when the site has none.
    pub fn hash_splat_pos(&self) -> &[SlotId] {
        self.extra.as_ref().map_or(&[], |e| &e.hash_splat_pos)
    }

    ///
    /// The keyword window register by register, in source order:
    /// `kw_order()[i]` describes `kw_pos + i`. Empty when the site
    /// passes no keywords.
    ///
    pub fn kw_order(&self) -> &[KwElem] {
        self.extra.as_ref().map_or(&[], |e| &e.kw_order)
    }

    ///
    /// Whether the literal `k: v` pair in window register `i` leaves no
    /// trace at all, because the site writes the same key again later
    /// *and* builds its keyword hash statically.
    ///
    /// Without a `**hash` splat CRuby's compiler folds the whole keyword
    /// list into one hash, so an overwritten pair is simply dropped and
    /// the key ends up in the *later* pair's place:
    /// `f(a: 1, b: 2, a: 3)` passes `{b: 2, a: 3}`. Add a splat anywhere
    /// and the list is merged at run time instead, where the key keeps
    /// the first place it was given: `f(a: 1, b: 2, a: 3, **{})` passes
    /// `{a: 3, b: 2}`. Both are observable, so both are reproduced here.
    ///
    pub fn kw_overwritten_literal(&self, i: usize) -> bool {
        if !self.hash_splat_pos().is_empty() {
            return false;
        }
        let order = self.kw_order();
        match order[i] {
            KwElem::Kw(name) => order[i + 1..].contains(&KwElem::Kw(name)),
            KwElem::Splat => false,
        }
    }

    pub fn kw_may_exists(&self) -> bool {
        !self.kw_args().is_empty() || !self.hash_splat_pos().is_empty()
    }

    pub fn has_splat(&self) -> bool {
        !self.splat_pos().is_empty()
    }

    pub fn has_hash_splat(&self) -> bool {
        !self.hash_splat_pos().is_empty()
    }

    ///
    /// The size of the keyword window — one register per keyword source,
    /// which is *not* `kw_args().len() + hash_splat_pos().len()`: a key
    /// written twice (`f(a: 1, a: 2)`) holds two registers but one
    /// `kw_args` entry.
    ///
    pub fn kw_len(&self) -> usize {
        self.kw_order().len()
    }

    pub fn is_func_call(&self) -> bool {
        self.recv.is_self() || self.bypass_visibility
    }

    ///
    /// whether a block (or a block argument) is given
    ///
    /// - Some(true): a block is given
    /// - Some(false): no block is given
    /// - None: statically unknown
    ///
    pub fn block_given(&self) -> Option<bool> {
        if self.block_fid.is_some() {
            Some(true)
        } else if self.block_arg.is_some() {
            None
        } else {
            Some(false)
        }
    }

    pub fn block_handler(&self, lfp: Lfp) -> Option<BlockHandler> {
        if let Some(block_fid) = self.block_fid {
            let bh = BlockHandler::from_caller(block_fid);
            Some(bh)
        } else if let Some(block_arg) = self.block_arg {
            match lfp.register(block_arg) {
                Some(v) => Some(BlockHandler::new(v)),
                None => None,
            }
        } else {
            None
        }
    }

    ///
    /// This call site has no keyword arguments, no splat arguments, no hash splat arguments, and no block argument.
    ///
    pub fn is_simple(&self) -> bool {
        self.kw_len() == 0 && !self.has_splat() && self.block_arg.is_none()
    }

    pub(crate) fn object_send_single_splat(&self) -> bool {
        self.splat_pos().len() == 1 && self.pos_num == 1 && !self.kw_may_exists()
    }

    pub fn format_args(&self) -> String {
        let splat_pos = self.splat_pos();
        let CallSiteInfo {
            pos_num,
            kw_pos,
            block_arg,
            block_fid,
            args,
            ..
        } = self;
        let mut s = String::new();
        if *pos_num > 0 {
            for i in 0..*pos_num {
                if i > 0 {
                    s += ",";
                }
                if splat_pos.contains(&i) {
                    s += "*";
                }
                s += &format!("{:?}", *args + i);
            }
        }
        // The keyword window in source order, which is the order it was
        // written in.
        for (i, elem) in self.kw_order().iter().enumerate() {
            if !s.is_empty() {
                s += ",";
            }
            let reg = *kw_pos + i;
            s += &match elem {
                KwElem::Kw(name) => format!("{name}:{reg:?}"),
                KwElem::Splat => format!("**{reg:?}"),
            };
        }
        if let Some(block_arg) = block_arg {
            if s.len() > 0 {
                s += ",";
            }
            s += &format!("&{:?}", block_arg);
        }
        s = format!("({})", s);
        if let Some(block_fid) = block_fid {
            s += &format!(" {{ {:?} }}", block_fid);
        }
        s
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct CallSiteId(pub u32);

impl std::convert::From<u32> for CallSiteId {
    fn from(id: u32) -> Self {
        Self(id)
    }
}

impl CallSiteId {
    pub fn get(&self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone)]
pub struct OptCaseInfo {
    pub min: u16,
    pub max: u16,
    pub branch_table: Box<[u32]>,
    pub offsets: Vec<u32>,
}

impl OptCaseInfo {
    /// The branch for the key *idx*, as CRuby's `opt_case_dispatch` looks
    /// a `case` whose `when` values are all Integer literals up: a Fixnum
    /// or an integral Float by value, any other number, special constant
    /// or String to the `else` body (`offsets[1]`). Any other key is
    /// matched by calling `===` on each `when` value, in the fallback
    /// chain (`offsets[0]`).
    pub(crate) fn find(&self, idx: Value) -> u32 {
        let int = if let Some(i) = idx.try_fixnum() {
            Some(i)
        } else if let Some(f) = idx.try_float() {
            (f.fract() == 0.0 && f.abs() < 65536.0).then_some(f as i64)
        } else if idx.is_packed_value()
            || matches!(idx.unpack(), RV::BigInt(_))
            || idx.is_rstring_inner().is_some()
        {
            None
        } else {
            return self.offsets[0];
        };
        if let Some(i) = int
            && let Ok(i) = u16::try_from(i)
            && i >= self.min
            && i <= self.max
        {
            return self.branch_table[(i - self.min) as usize];
        }
        self.offsets[1]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct OptCaseId(pub u32);

impl std::convert::From<u32> for OptCaseId {
    fn from(id: u32) -> Self {
        Self(id)
    }
}

impl OptCaseId {
    pub fn get(&self) -> u32 {
        self.0
    }
}

#[derive(Default)]
struct GlobalMethodCache {
    version: u32,
    cache: HashMap<(IdentId, ClassId), Option<MethodTableEntry>>,
    #[cfg(feature = "profile")]
    /// Stats for global method cache access.
    global_method_cache_stats: HashMap<(ClassId, IdentId), usize>,
    #[cfg(feature = "profile")]
    /// Stats for method cache miss.
    method_exprolation_stats: HashMap<(ClassId, IdentId), usize>,
}

impl GlobalMethodCache {
    fn get(
        &mut self,
        class_id: ClassId,
        name: IdentId,
        class_version: u32,
    ) -> Option<Option<&MethodTableEntry>> {
        #[cfg(feature = "profile")]
        {
            match self.global_method_cache_stats.get_mut(&(class_id, name)) {
                Some(c) => *c += 1,
                None => {
                    self.global_method_cache_stats.insert((class_id, name), 1);
                }
            }
        }
        if self.version != class_version {
            self.cache.clear();
            self.version = class_version;
            return None;
        }
        self.cache.get(&(name, class_id)).map(|e| e.as_ref())
    }

    fn insert(
        &mut self,
        key: (IdentId, ClassId),
        class_version: u32,
        entry: Option<MethodTableEntry>,
    ) {
        #[cfg(feature = "profile")]
        {
            match self.method_exprolation_stats.get_mut(&(key.1, key.0)) {
                Some(c) => *c += 1,
                None => {
                    self.method_exprolation_stats.insert((key.1, key.0), 1);
                }
            }
        }
        self.version = class_version;
        self.cache.insert(key, entry);
    }

    #[cfg(feature = "profile")]
    pub(super) fn clear_stats(&mut self) {
        self.global_method_cache_stats.clear();
        self.method_exprolation_stats.clear();
    }

    #[cfg(feature = "profile")]
    pub(super) fn global_method_cache_stats(&self) -> Vec<(&(ClassId, IdentId), &usize)> {
        self.global_method_cache_stats.iter().collect()
    }

    #[cfg(feature = "profile")]
    pub(super) fn method_exprolation_stats(&self) -> Vec<(&(ClassId, IdentId), &usize)> {
        self.method_exprolation_stats.iter().collect()
    }
}

#[cfg(test)]
mod tests {
    use crate::Globals;

    ///
    /// The report is only printed behind `MONORUBY_STORE_STATS`, so
    /// nothing else would notice it panicking or going stale — and it is
    /// the evidence a layout change is measured against.
    ///
    #[test]
    fn memory_report_tallies_the_loaded_program() {
        let mut globals = Globals::new_test();
        globals
            .run(
                "def a(x, k: 1) = x + k\na(1, k: 2)\n",
                std::path::Path::new("test.rb"),
            )
            .expect("run");
        let report = globals.store.memory_report();

        // Every table has a row, and the header carries the two counts
        // and the struct size a shrink is judged by.
        for row in [
            "iseqs:",
            "callsites:",
            "basic blocks:",
            "ISeqInfo headers",
            "bytecode",
            "sourcemap",
            "bb entries",
            "bb pred/succ vecs",
            "bb_map",
            "callsite_info",
            "TOTAL",
        ] {
            assert!(report.contains(row), "no {row:?} row in:\n{report}");
        }

        // The script defines and calls a method, so the store is not empty.
        assert!(!globals.store.iseqs.is_empty());
        assert!(!globals.store.callsite_info.is_empty());
        assert!(
            globals.store.iseqs.iter().any(|i| i.heap_size().insts > 0),
            "no iseq carries bytecode"
        );
    }

    ///
    /// `extra` is allocated for exactly the call sites that pass a splat,
    /// a keyword or a hash splat — the invariant `is_simple` and the
    /// accessors rest on.
    ///
    #[test]
    fn only_the_unusual_call_sites_allocate_extra() {
        let mut globals = Globals::new_test();
        globals
            .run(
                "def m(*a, **k) = [a, k]\nm(1)\nm(*[1, 2])\nm(k: 1)\nm(**{k: 1})\n",
                std::path::Path::new("test.rb"),
            )
            .expect("run");
        let with_extra = globals
            .store
            .callsite_info
            .iter()
            .filter(|c| c.extra.is_some())
            .count();
        let unusual = globals
            .store
            .callsite_info
            .iter()
            .filter(|c| {
                !c.splat_pos().is_empty()
                    || !c.kw_args().is_empty()
                    || !c.hash_splat_pos().is_empty()
            })
            .count();
        assert_eq!(with_extra, unusual, "extra allocated for a plain call site");
        assert!(unusual >= 3, "expected the splat / kw / hash-splat sites");

        // A site with no extra reports empty, not a panic, through every
        // accessor — the empty map included.
        let plain = globals
            .store
            .callsite_info
            .iter()
            .find(|c| c.extra.is_none())
            .expect("a plain call site");
        assert!(plain.splat_pos().is_empty());
        assert!(plain.kw_args().is_empty());
        assert!(plain.hash_splat_pos().is_empty());
        assert_eq!(plain.kw_len(), 0);
        assert!(!plain.has_splat());
        assert!(!plain.has_hash_splat());
        assert!(!plain.kw_may_exists());
    }
}

///
/// A builtin that is a *leaf* of the Ruby call graph: one that runs no
/// Ruby code (no method dispatch, no block, no coercion callout), never
/// reaches a safepoint (no GC / preempt poll, no GVL release), and looks
/// at no frame but its own (`binding`, `caller`, `block_given?`, `$~`).
/// Such a call may be made from a frameless callee's body: its window is
/// invisible to the collector and to the frame walk, and a leaf starts
/// neither. An error it raises is taken by a materializing exit.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum LeafCall {
    /// In every positional argument form.
    Always,
    /// When every argument is proven an Integer at the call site (the
    /// builtin would otherwise call `to_int`).
    IntArgs,
}
