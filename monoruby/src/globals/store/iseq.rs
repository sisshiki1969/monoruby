use super::*;
use crate::{bytecodegen::BcIndex, compiler::jitgen::BasicBlockInfo};

///
/// Information of instruction sequences.
///
#[derive(Clone)]
pub(crate) struct ISeqInfo {
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
    bytecode: Option<Pin<Box<[Bc]>>>,
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
    pub sp: Vec<SlotId>,
    ///
    /// Exception handling map.
    ///
    exception_map: Vec<(
        std::ops::Range<BcPc>, // range of capturing exception
        Option<BcPc>,          // rescue destination pc
        Option<BcPc>,          // ensure destination pc
        Option<SlotId>,        // a slot where an error object is assigned
    )>,
    ///
    /// Information of parameters..
    ///
    pub args: ParamsInfo,
    ///
    /// Name of local variabl
    ///
    pub locals: HashMap<IdentId, bytecodegen::BcLocal>,
    ///
    /// outer local variables. (dynamic_locals, block_param)
    ///
    pub outer_locals: ExternalContext,
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
    pub bb_info: BasicBlockInfo,
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
            locals: HashMap::default(),
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
    pub(crate) fn set_bytecode(&mut self, bc: Vec<Bc>) {
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

    pub(crate) fn no_keyword(&self) -> bool {
        self.args.kw_names.is_empty() && self.kw_rest().is_none()
    }

    pub(crate) fn kw_rest(&self) -> Option<SlotId> {
        self.args.kw_rest
    }

    pub(crate) fn required_num(&self) -> usize {
        self.args.required_num
    }

    pub(crate) fn is_rest(&self) -> bool {
        self.args.is_rest()
    }

    ///
    /// Get a number of optional and rest parameters.
    ///
    pub(crate) fn optional_num(&self) -> usize {
        self.args.pos_num - self.args.required_num
    }

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
    pub(crate) fn bytecode(&self) -> &[Bc] {
        self.bytecode.as_ref().unwrap()
    }

    ///
    /// Get pc(*BcPc*) for instruction index(*idx*).
    ///
    pub fn get_pc(&self, idx: BcIndex) -> BcPc {
        BcPc::from(&self.bytecode()[idx.0 as usize])
    }

    ///
    /// Get pc(*BcPc*) for instruction index(*idx*).
    ///
    pub(crate) fn get_top_pc(&self) -> BcPc {
        BcPc::from(&self.bytecode()[0])
    }

    ///
    /// Get an instruction index(*usize*) corresponding to pc(*BcPc*).
    ///
    pub(crate) fn get_pc_index(&self, pc: Option<BcPc>) -> BcIndex {
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
        pc: BcPc,
    ) -> Option<(Option<BcPc>, Option<BcPc>, Option<SlotId>)> {
        self.exception_map
            .iter()
            .filter_map(|(range, rescue, ensure, slot)| {
                if range.contains(&pc) {
                    Some((*rescue, *ensure, *slot))
                } else {
                    None
                }
            })
            .nth(0)
    }

    pub(crate) fn exception_push(
        &mut self,
        range: std::ops::Range<BcPc>,
        rescue: Option<BcPc>,
        ensure: Option<BcPc>,
        err_reg: Option<SlotId>,
    ) {
        self.exception_map.push((range, rescue, ensure, err_reg));
    }

    ///
    /// Get bytecode length.
    ///
    pub(crate) fn bytecode_len(&self) -> usize {
        self.bytecode().len()
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
            .map(|(range, rescue, ensure, err_reg)| {
                let start = self.get_pc_index(Some(range.start));
                let end = self.get_pc_index(Some(range.end));
                let rescue = rescue.map(|pc| self.get_pc_index(Some(pc)));
                let ensure = ensure.map(|pc| self.get_pc_index(Some(pc)));
                (start..end, rescue, ensure, *err_reg)
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

    pub fn pos_num(&self) -> usize {
        self.pos_num
    }

    pub fn max_positional_args(&self) -> usize {
        self.reqopt_num
    }

    pub fn is_rest(&self) -> bool {
        self.pos_num != self.reqopt_num
    }
}
