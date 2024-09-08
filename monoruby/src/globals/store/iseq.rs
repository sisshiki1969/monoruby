use super::*;
use crate::{bytecodegen::BcIndex, compiler::jitgen::BasicBlockInfo};

#[derive(Clone, Debug)]
struct ExceptionMapEntry {
    range: std::ops::Range<BytecodePtr>, // range of capturing exception
    rescue_pc: Option<BytecodePtr>,      // rescue destination pc
    ensure_pc: Option<BytecodePtr>,      // ensure destination pc
    error_slot: Option<SlotId>,          // a slot where an error object is assigned
}

impl ExceptionMapEntry {
    fn new(
        range: std::ops::Range<BytecodePtr>,
        rescue_pc: Option<BytecodePtr>,
        ensure_pc: Option<BytecodePtr>,
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
/// Information of instruction sequences.
///
#[derive(Clone)]
pub struct ISeqInfo {
    ///
    /// *FuncId* of this function.
    ///
    id: FuncId,
    ///
    /// Mother method.
    ///
    pub mother: (FuncId, usize),
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
    pub(crate) locals: IndexMap<IdentId, bytecodegen::BcLocal>,
    ///
    /// outer local variables. (dynamic_locals, block_param)
    ///
    pub(crate) outer_locals: ExternalContext,
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
    pub lexical_context: Vec<Module>,
    pub sourceinfo: SourceInfoRef,
    is_block_style: bool,
    ///
    /// Basic block information.
    ///
    pub(crate) bb_info: BasicBlockInfo,
}

impl std::fmt::Debug for ISeqInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "RubyFuncInfo {{ id:{} name:{} method:{:?} args: {} non_temp: {} temp: {}}}",
            self.id().get(),
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
        self.lexical_context.iter().for_each(|m| m.mark(alloc));
    }
}

impl ISeqInfo {
    fn new(
        id: FuncId,
        mother: (FuncId, usize),
        outer_locals: ExternalContext,
        name: Option<IdentId>,
        args: ParamsInfo,
        loc: Loc,
        sourceinfo: SourceInfoRef,
        is_block_style: bool,
    ) -> Self {
        ISeqInfo {
            id,
            mother,
            name,
            bytecode: None,
            loc,
            sourcemap: vec![],
            sp: vec![],
            exception_map: vec![],
            args: args.clone(),
            locals: IndexMap::default(),
            outer_locals,
            literals: vec![],
            non_temp_num: 0,
            temp_num: 0,
            lexical_context: vec![],
            sourceinfo,
            is_block_style,
            bb_info: BasicBlockInfo::default(),
        }
    }

    pub fn is_block_style(&self) -> bool {
        self.is_block_style
    }

    pub(super) fn new_block(
        id: FuncId,
        mother: (FuncId, usize),
        outer: (FuncId, ExternalContext),
        args: ParamsInfo,
        loc: Loc,
        sourceinfo: SourceInfoRef,
    ) -> Self {
        Self::new(id, mother, outer.1, None, args, loc, sourceinfo, true)
    }

    pub(super) fn new_method(
        id: FuncId,
        name: Option<IdentId>,
        args: ParamsInfo,
        loc: Loc,
        sourceinfo: SourceInfoRef,
    ) -> Self {
        Self::new(
            id,
            (id, 0),
            ExternalContext::new(),
            name,
            args,
            loc,
            sourceinfo,
            false,
        )
    }

    pub(crate) fn id(&self) -> FuncId {
        self.id
    }

    ///
    /// Set bytecode to the *ISeqInfo*.
    ///
    pub(crate) fn set_bytecode(&mut self, bc: Vec<Bytecode>) {
        self.bytecode = Some(Box::into_pin(bc.into_boxed_slice()));
    }

    ///
    /// Get a number of registers.
    ///
    pub(crate) fn total_reg_num(&self) -> usize {
        1 + (self.non_temp_num + self.temp_num) as usize
    }

    ///
    /// Get a number of non-temp registers.
    ///
    pub(crate) fn local_num(&self) -> usize {
        self.non_temp_num as usize
    }

    /*///
    /// Get a number of optional and rest parameters.
    ///
    pub(crate) fn optional_num(&self) -> usize {
        self.args.pos_num - self.args.required_num
    }*/

    ///
    /// Get a number of required parameters.
    ///
    pub(crate) fn req_num(&self) -> usize {
        self.args.required_num
    }

    ///
    /// get a number of required or optional parameters.
    ///
    pub(crate) fn reqopt_num(&self) -> usize {
        self.args.reqopt_num
    }

    ///
    /// Get a number of required + optional + rest arguments.
    ///
    pub(crate) fn pos_num(&self) -> usize {
        self.args.pos_num
    }

    ///
    /// Get a parameter info.
    ///
    /// bit 0:rest(yes=1 no =0) bit 1:block
    pub(crate) fn info(&self) -> usize {
        (if self.args.block_param.is_some() {
            2
        } else {
            0
        }) + (self.args.pos_num - self.args.reqopt_num)
    }

    ///
    /// Get a block argument name.
    ///
    pub(crate) fn block_param(&self) -> Option<IdentId> {
        self.args.block_param
    }

    ///
    /// Get names of local variables.
    ///
    pub(crate) fn local_variables(&self) -> Vec<Value> {
        let mut map = IndexSet::default();
        self.locals.keys().for_each(|id| {
            map.insert(*id);
        });

        self.outer_locals.scope.iter().for_each(|(locals, block)| {
            locals.keys().for_each(|id| {
                map.insert(*id);
            });
            if let Some(id) = block {
                map.insert(*id);
            }
        });
        map.into_iter().map(Value::symbol).collect()
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
    pub(crate) fn bytecode(&self) -> &[Bytecode] {
        self.bytecode.as_ref().unwrap()
    }

    ///
    /// Get pc(*BcPc*) for instruction index(*idx*).
    ///
    pub(crate) fn get_pc(&self, idx: BcIndex) -> BytecodePtr {
        BytecodePtr::from_bc(&self.bytecode()[idx.0 as usize])
    }

    ///
    /// Get pc(*BcPc*) for instruction index(*idx*).
    ///
    pub(crate) fn get_top_pc(&self) -> BytecodePtr {
        BytecodePtr::from_bc(&self.bytecode()[0])
    }

    ///
    /// Get an instruction index(*usize*) corresponding to pc(*BcPc*).
    ///
    pub(crate) fn get_pc_index(&self, pc: Option<BytecodePtr>) -> BcIndex {
        let i = if let Some(pos) = pc {
            pos - self.get_top_pc()
        } else {
            0
        };
        BcIndex::from(i)
    }

    pub(crate) fn get_sp(&self, i: BcIndex) -> SlotId {
        self.sp[i.0 as usize]
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
        pc: BytecodePtr,
    ) -> Option<(Option<BytecodePtr>, Option<BytecodePtr>, Option<SlotId>)> {
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
        range: std::ops::Range<BytecodePtr>,
        rescue: Option<BytecodePtr>,
        ensure: Option<BytecodePtr>,
        err_reg: Option<SlotId>,
    ) {
        self.exception_map
            .push(ExceptionMapEntry::new(range, rescue, ensure, err_reg));
    }

    #[cfg(feature = "emit-bc")]
    pub(super) fn get_exception_map(
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
                let start = self.get_pc_index(Some(entry.range.start));
                let end = self.get_pc_index(Some(entry.range.end));
                let rescue = entry.rescue_pc.map(|pc| self.get_pc_index(Some(pc)));
                let ensure = entry.ensure_pc.map(|pc| self.get_pc_index(Some(pc)));
                (start..end, rescue, ensure, entry.error_slot)
            })
            .collect::<Vec<_>>()
    }
}

///
/// Parameters information in *ISeqInfo*.
///
#[derive(Debug, Clone, Default, PartialEq)]
pub(crate) struct ParamsInfo {
    required_num: usize,
    // required + optional
    reqopt_num: usize,
    // required + optional + rest
    pos_num: usize,
    // for param, req(incl. destruct slot), opt, rest, keyword, kw_rest, destructed local, block
    pub args_names: Vec<Option<IdentId>>,
    pub kw_names: Vec<IdentId>,
    pub kw_rest: Option<SlotId>,
    block_param: Option<IdentId>,
}

impl ParamsInfo {
    pub fn new(
        required_num: usize,
        reqopt_num: usize,
        pos_num: usize,
        args_names: Vec<Option<IdentId>>,
        keyword_names: Vec<IdentId>,
        kw_rest: Option<SlotId>,
        block_param: Option<IdentId>,
    ) -> Self {
        ParamsInfo {
            required_num,
            reqopt_num,
            pos_num,
            args_names,
            kw_names: keyword_names,
            kw_rest,
            block_param,
        }
    }

    pub fn new_attr_reader() -> Self {
        ParamsInfo::default()
    }

    pub fn new_attr_writer() -> Self {
        ParamsInfo {
            required_num: 1,
            reqopt_num: 1,
            pos_num: 1,
            args_names: vec![],
            kw_names: vec![],
            kw_rest: None,
            block_param: None,
        }
    }

    pub fn new_native(min: usize, max: usize, rest: bool, kw_names: Vec<IdentId>) -> Self {
        ParamsInfo {
            required_num: min,
            reqopt_num: max,
            pos_num: max + rest as usize,
            args_names: vec![],
            kw_names,
            kw_rest: None,
            block_param: None,
        }
    }

    ///
    /// The number of required arguments.
    ///
    pub(crate) fn req_num(&self) -> usize {
        self.required_num
    }

    ///
    /// The number of required + optional arguments.
    ///
    pub(crate) fn reqopt_num(&self) -> usize {
        self.reqopt_num
    }

    ///
    /// The number of optional + rest arguments.
    ///
    pub(crate) fn opt_rest_num(&self) -> usize {
        self.pos_num - self.required_num
    }

    ///
    /// The number of required + optional + rest arguments.
    ///
    pub fn pos_num(&self) -> usize {
        self.pos_num
    }

    pub fn max_positional_args(&self) -> usize {
        self.reqopt_num
    }

    pub fn total_args(&self) -> usize {
        self.pos_num
            + self.kw_names.len()
            + self.kw_rest.is_some() as usize
            + self.block_param.is_some() as usize
    }

    pub fn is_rest(&self) -> bool {
        self.pos_num != self.reqopt_num
    }

    ///
    /// If `self` is "simple", return true.
    ///
    /// "simple" means that the function has no optional, rest, keyword, keywoed rest, and block parameters.
    ///
    pub fn is_simple(&self) -> bool {
        self.opt_rest_num() == 0
            && self.kw_names.is_empty()
            && self.kw_rest.is_none()
            && self.block_param.is_none()
    }
}
