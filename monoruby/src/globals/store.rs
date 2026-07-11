use inline::InlineTable;
use monoasm::DestLabel;

use crate::ast::{DestructEntry, LvarCollector, ParseResult};
use crate::bytecodegen::{BcLocal, CompileInfo, DestructureInfo, ForParamInfo, OptionalInfo};

use super::*;
use std::{cell::RefCell, pin::Pin};

mod class;
mod function;
mod iseq;
pub use class::*;
pub use function::*;
pub(crate) use iseq::*;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct MethodTableEntry {
    owner: ClassId,
    func_id: Option<FuncId>,
    visibility: Visibility,
    is_basic_op: bool,
    /// The method's *original* definition name, tracked through
    /// `alias_method` and `define_method`-from-(Unbound)Method so that
    /// `Method#original_name` and the `#name(original)` form of
    /// `Method#inspect` can recover it. For a plainly-defined method
    /// (`def`, `attr_*`, builtin) this equals the entry's own name.
    original_name: IdentId,
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

    pub fn is_basic_op(&self) -> bool {
        self.is_basic_op
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
    /// ISeq info.
    pub(crate) iseqs: Vec<ISeqInfo>,
    /// class table.
    classes: ClassInfoTable,
    /// call site info.
    callsite_info: Vec<CallSiteInfo>,
    /// const access site info.
    constsite_info: Vec<ConstSiteInfo>,
    /// opt case branch info.
    optcase_info: Vec<OptCaseInfo>,
    /// inline info.
    pub(crate) inline_info: InlineTable,
    /// FuncId of the default `BasicObject#!=`, used to recognize that an
    /// unredefined `!=` may be computed as `!(a == b)` without dispatch.
    default_neq: Option<FuncId>,
    /// FuncIds of the default `initialize_copy` / `initialize_dup` /
    /// `initialize_clone` builtins. `Kernel#dup` / `#clone` compare a
    /// class's resolved hooks against these to recognize the no-op case
    /// and skip the copy-hook dispatch. Set once during builtin init.
    default_copy_hooks: Option<[FuncId; 3]>,
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
    frozen_str_pool: HashMap<(Vec<u8>, crate::value::Encoding), Value>,
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

impl std::ops::IndexMut<ConstSiteId> for Store {
    fn index_mut(&mut self, index: ConstSiteId) -> &mut ConstSiteInfo {
        &mut self.constsite_info[index.0 as usize]
    }
}

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
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        self.functions.mark(alloc);
        self.iseqs.iter().for_each(|info| info.mark(alloc));
        self.constsite_info.iter().for_each(|info| info.mark(alloc));
        self.classes.table.iter().for_each(|info| info.mark(alloc));
        self.frozen_str_pool.values().for_each(|v| v.mark(alloc));
    }
}

impl Store {
    pub(super) fn new() -> Self {
        Self {
            functions: function::Funcs::default(),
            iseqs: vec![],
            constsite_info: vec![],
            callsite_info: vec![],
            optcase_info: vec![],
            classes: ClassInfoTable::new(),
            inline_info: InlineTable::default(),
            default_neq: None,
            default_copy_hooks: None,
            method_cache: RefCell::new(GlobalMethodCache::default()),
            compile_warnings: vec![],
            frozen_str_pool: HashMap::default(),
        }
    }

    ///
    /// Intern a frozen string literal for a `# frozen_string_literal: true`
    /// file. Returns the shared, frozen `Value` for `(bytes, enc)`, creating
    /// and caching it on first use so identical literals (in the same or a
    /// different file) resolve to the same object.
    ///
    pub(crate) fn intern_frozen_str(&mut self, bytes: &[u8], enc: crate::value::Encoding) -> Value {
        if let Some(v) = self.frozen_str_pool.get(&(bytes.to_vec(), enc)) {
            return *v;
        }
        let mut v = Value::string_from_source_bytes(bytes, enc);
        v.set_frozen();
        self.frozen_str_pool.insert((bytes.to_vec(), enc), v);
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

    ///
    /// Search for the `ISeqId` whose bytecode contains *pc*.
    ///
    /// Returns `Some(ISeqId)` if found, `None` otherwise.
    ///
    #[allow(dead_code)]
    pub(crate) fn iseq_id_from_pc(&self, pc: BytecodePtr) -> Option<(ISeqId, BcIndex)> {
        self.iseqs
            .iter()
            .enumerate()
            .find(|(_, iseq)| iseq.contains_pc(pc))
            .map(|(idx, info)| (ISeqId::new(idx), info.get_pc_index(Some(pc))))
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
    #[cfg(feature = "emit-bc")]
    pub(super) fn functions(&self) -> &[FuncInfo] {
        self.functions.functions()
    }

    pub(crate) fn func_len(&self) -> usize {
        self.functions.function_len()
    }

    /// Get class name of *ClassId*.
    pub(crate) fn get_class_name(&self, class: ClassId) -> String {
        let class_obj = self.classes[class].get_module();
        match self.classes[class].get_name() {
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
                Some(base) => format!("#<Class:{}>", base.to_s(self)),
            },
        }
    }

    ///
    /// Get the description of the function *func_id*.
    ///
    /// If the function is a method, the description is "*class_name*#*method_name*".
    /// if the function is a block, the description is "block in *method_name*".
    ///
    pub(crate) fn func_description(&self, func_id: FuncId) -> String {
        let info = &self[func_id];
        let name = if let Some(iseq_id) = info.is_iseq() {
            let iseq = &self[iseq_id];
            let mother = iseq.mother().0;
            if mother != iseq_id {
                return format!("block in {}", self.func_description(self[mother].func_id()));
            } else {
                iseq.name()
            }
        } else {
            if let Some(name) = info.name() {
                format!("{:?}", name)
            } else {
                String::new()
            }
        };
        match info.owner_class().get(0) {
            Some(owner) => {
                if let Some(obj) = self[*owner].get_module().is_singleton() {
                    if let Some(class) = obj.is_class_or_module() {
                        format!("{}.{name}", class.id().get_name(self))
                    } else {
                        // CRuby names a plain object's singleton
                        // method by the bare method name (only
                        // class/module singletons get the
                        // `Owner.name` form).
                        name
                    }
                } else {
                    format!("{}#{name}", owner.get_name(self))
                }
            }
            None => name,
        }
    }

    pub fn location(&self, func_id: Option<FuncId>, source: &SourceInfoRef, loc: Loc) -> String {
        let source = source.clone();
        if let Some(func_id) = func_id {
            format!(
                "{}:{}:in '{}'",
                source.file_name(),
                source.get_line(&loc),
                self.func_description(func_id)
            )
        } else {
            format!("{}:{}", source.file_name(), source.get_line(&loc))
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
            let loc = if let Some(pc) = pc {
                let bc_index = iseq.get_pc_index(Some(pc));
                iseq.sourcemap[bc_index.to_usize()]
            } else {
                iseq.loc
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

    pub fn internal_location(&self, func_id: FuncId) -> String {
        format!("<internal>:in '{}'", self.func_description(func_id))
    }

    fn new_iseq(&mut self, info: ISeqInfo) -> ISeqId {
        let id = self.iseqs.len();
        self.iseqs.push(info);
        ISeqId::new(id)
    }

    fn next_iseq_id(&mut self) -> ISeqId {
        let id = self.iseqs.len();
        ISeqId::new(id)
    }

    pub(crate) fn new_main(&mut self, result: ParseResult) -> Result<FuncId> {
        let info = BlockInfo {
            params: vec![],
            body: Box::new(result.node),
            lvar: LvarCollector::new(),
            loc: Loc::default(),
            is_lambda: false,
        };
        let compile_info = Store::handle_args(info, vec![])?;
        let fid = self.new_iseq_method(
            Some(IdentId::_MAIN),
            compile_info,
            Loc::default(),
            result.source_info,
            true,
        )?;
        Ok(fid)
    }

    pub(crate) fn new_classdef(
        &mut self,
        name: Option<IdentId>,
        compile_info: CompileInfo,
        loc: Loc,
        sourceinfo: SourceInfoRef,
        is_singleton: bool,
    ) -> Result<FuncId> {
        let func_id = self.functions.next_func_id();
        self.functions.add_compile_info(compile_info);
        let mut info = ISeqInfo::new_method(
            func_id,
            self.next_iseq_id(),
            name,
            ParamsInfo::default(),
            loc,
            sourceinfo,
        );
        info.singleton_classdef = is_singleton;
        // The body itself resolves constants through a per-execution
        // singleton class, so it needs the self-keyed constant cache
        // just like defs nested inside it (bytecodegen additionally
        // propagates the flag onto those).
        info.in_singleton_lexical = is_singleton;
        let iseq = self.new_iseq(info);
        let info = FuncInfo::new_classdef_iseq(name, func_id, iseq);
        self.functions.info.push(info);
        Ok(func_id)
    }

    pub(crate) fn new_iseq_method(
        &mut self,
        name: Option<IdentId>,
        compile_info: CompileInfo,
        loc: Loc,
        sourceinfo: SourceInfoRef,
        top_level: bool,
    ) -> Result<FuncId> {
        let func_id = self.functions.next_func_id();
        let params_info = compile_info.params.clone();
        self.functions.add_compile_info(compile_info);
        let info = ISeqInfo::new_method(
            func_id,
            self.next_iseq_id(),
            name,
            params_info.clone(),
            loc,
            sourceinfo,
        );
        let iseq = self.new_iseq(info);
        let info = FuncInfo::new_method_iseq(name, func_id, iseq, params_info, top_level);
        self.functions.info.push(info);
        Ok(func_id)
    }

    pub(crate) fn new_block(
        &mut self,
        outer: ISeqId,
        compile_info: CompileInfo,
        is_block_style: bool,
        loc: Loc,
        sourceinfo: SourceInfoRef,
    ) -> Result<FuncId> {
        let outer_mother = self[outer].mother();
        let mother = (outer_mother.0, outer_mother.1 + 1);
        let func_id = self.functions.next_func_id();
        let params_info = compile_info.params.clone();
        self.functions.add_compile_info(compile_info);
        let info =
            ISeqInfo::new_block(func_id, mother, outer, params_info.clone(), loc, sourceinfo);
        let iseq = self.new_iseq(info);
        let info = FuncInfo::new_block_iseq(func_id, iseq, params_info, is_block_style);
        self.functions.info.push(info);
        Ok(func_id)
    }

    pub(crate) fn new_eval(
        &mut self,
        outer: ISeqId,
        result: ParseResult,
        loc: Loc,
    ) -> Result<FuncId> {
        let info = BlockInfo {
            params: vec![],
            body: Box::new(result.node),
            lvar: LvarCollector::new(),
            loc,
            is_lambda: false,
        };
        let compile_info = Store::handle_args(info, vec![])?;
        // eval'd code is block-style: a `return` / `break` in the string
        // escapes to the enclosing method, like a block (not locally like
        // a lambda). eval bodies and lambdas are otherwise both
        // method-style (no own params), so this flag is what keeps their
        // non-local-vs-local `return` semantics apart.
        let fid = self.new_block(outer, compile_info, true, loc, result.source_info)?;
        let iseq = self[fid].is_iseq().unwrap();
        self[iseq].is_eval = true;
        Ok(fid)
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
        self.callsite_info.push(CallSiteInfo {
            id,
            name,
            bc_pos,
            pos_num,
            kw_pos,
            kw_args,
            splat_pos,
            hash_splat_pos,
            block_fid,
            block_arg,
            args,
            recv,
            dst,
            forwarding,
            bypass_visibility,
            vcall,
        });
        id
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
    pub(crate) fn no_to_str(&self, class_id: ClassId, version: u32) -> bool {
        if self[class_id].no_to_str_at() == Some(version) {
            return true;
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

    pub(crate) fn custom_neq(&self, class_id: ClassId, version: u32) -> Option<MethodTableEntry> {
        if self[class_id].neq_basic_at() == Some(version) {
            return None;
        }
        // Uncached walk for the same reason as `no_to_str`. `!=` counts
        // as default when it is a basic op (`String#!=`) or resolves to
        // `BasicObject#!=` itself. (The latter is deliberately NOT
        // registered as a basic op: overwriting a basic-op entry trips
        // the global BOP-redefine slow mode, and `BasicObject#!=` gets
        // legitimately shadowed during startup, e.g. by Comparable.)
        match self.search_method_by_class_id(class_id, IdentId::_NEQ) {
            Some(entry)
                if entry.is_basic_op()
                    || (entry.func_id().is_some() && entry.func_id() == self.default_neq) =>
            {
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

    pub(super) fn invalidate_jit_code(&mut self) {
        self.iseqs
            .iter_mut()
            .for_each(|info| info.invalidate_jit_code())
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
) {
    let len = entries.len();
    let dst_start = destruct_args.len();
    let mut rest_pos = None;
    let mut child_slots = Vec::with_capacity(len);
    for (i, entry) in entries.iter().enumerate() {
        child_slots.push(destruct_args.len());
        match entry {
            DestructEntry::Param(name, _) => {
                destruct_args.push(Some(IdentId::get_id_from_string(name.clone())));
            }
            DestructEntry::Rest(name, _) => {
                rest_pos = Some(i);
                destruct_args
                    .push(name.as_ref().map(|n| IdentId::get_id_from_string(n.clone())));
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
            flatten_destruct(sub, ExpandSrc::Destruct(child_slots[i]), expand, destruct_args);
        }
    }
}

impl Store {
    pub(crate) fn handle_args(
        info: BlockInfo,
        for_params: Vec<(usize, BcLocal, IdentId)>,
    ) -> Result<CompileInfo> {
        let BlockInfo {
            params,
            box body,
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
                    args_names.push(Some(IdentId::get_id_from_string(name)));
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
                    flatten_destruct(&entries, ExpandSrc::Surface(src), &mut expand, &mut destruct_args);
                }
                ParamKind::PostDestruct(entries) => {
                    // Same as `Destruct`, but the surface slot lives in the
                    // post region (after the rest), so it counts toward
                    // `post_num` rather than `required_num`.
                    let src = args_names.len();
                    args_names.push(None);
                    post_num += 1;
                    flatten_destruct(&entries, ExpandSrc::Surface(src), &mut expand, &mut destruct_args);
                }
                ParamKind::Optional(name, box initializer) => {
                    let local = BcLocal(args_names.len() as u16);
                    args_names.push(Some(IdentId::get_id_from_string(name)));
                    optional_num += 1;
                    optional_info.push(OptionalInfo::new(local, initializer));
                }
                ParamKind::Rest(name) => {
                    assert_eq!(rest, None);
                    rest = Some(args_names.len());
                    args_names.push(name.map(IdentId::get_id_from_string));
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
                    args_names.push(name.map(IdentId::get_id_from_string));
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
                    let name = IdentId::get_id_from_string(name);
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
                    let name = name.map(IdentId::get_id_from_string);
                    assert!(kw_rest_param.is_none());
                    kw_rest_param = Some(SlotId(1 + args_names.len() as u16));
                    args_names.push(name);
                }
                ParamKind::Block(name) => {
                    block_param = Some(match name {
                        Some(name) => IdentId::get_id_from_string(name.clone()),
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
        Self {
            table: vec![ClassInfo::new(); 100],
        }
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
        ClassId::new(id as u32)
    }

    fn copy_class(&mut self, original_class: ClassId) -> ClassId {
        let id = self.table.len();
        let info = self[original_class].copy();
        self.table.push(info);
        ClassId::new(id as u32)
    }

    fn def_builtin_class(&mut self, class: ClassId) {
        self[class] = ClassInfo::new();
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
    /// Positions of splat arguments.
    pub splat_pos: Vec<usize>,
    /// *FuncId* of passed block.
    pub block_fid: Option<FuncId>,
    /// Position of block argument.
    pub(crate) block_arg: Option<SlotId>,
    /// Postion of keyword arguments.
    pub(crate) kw_pos: SlotId,
    /// Names and positions of keyword arguments.
    pub kw_args: indexmap::IndexMap<IdentId, usize>,
    /// Position of hash splat arguments.
    pub(crate) hash_splat_pos: Vec<SlotId>,
    /// Position where the result is to be stored to.
    pub(crate) dst: Option<SlotId>,
    #[allow(dead_code)]
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
}

impl CallSiteInfo {
    pub fn kw_may_exists(&self) -> bool {
        !self.kw_args.is_empty() || !self.hash_splat_pos.is_empty()
    }

    pub fn has_splat(&self) -> bool {
        !self.splat_pos.is_empty()
    }

    pub fn has_hash_splat(&self) -> bool {
        !self.hash_splat_pos.is_empty()
    }

    pub fn kw_len(&self) -> usize {
        self.kw_args.len() + self.hash_splat_pos.len()
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
        self.splat_pos.len() == 1 && self.pos_num == 1 && !self.kw_may_exists()
    }

    pub fn format_args(&self) -> String {
        let CallSiteInfo {
            pos_num,
            splat_pos,
            kw_pos,
            kw_args,
            hash_splat_pos,
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
        for (i, (k, v)) in kw_args.iter().enumerate() {
            if i > 0 || *pos_num > 0 {
                s += ",";
            }
            s += &format!("{}:{:?}", k, *kw_pos + *v);
        }
        for pos in hash_splat_pos.iter() {
            if s.len() > 0 {
                s += ",";
            }
            s += &format!("**{:?}", pos);
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    pub(crate) fn find(&self, idx: Value) -> u32 {
        if let Some(idx) = idx.try_fixnum() {
            if let Ok(idx) = u16::try_from(idx) {
                if idx >= self.min && idx <= self.max {
                    return self.branch_table[(idx - self.min) as usize];
                }
            }
        }
        self.offsets[0]
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
