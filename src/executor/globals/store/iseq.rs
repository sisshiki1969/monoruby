use super::*;

///
/// Information of instruction sequences.
///
#[derive(Clone, Default)]
pub(crate) struct ISeqInfo {
    /// ID of this function.
    id: Option<FuncId>,
    pub(crate) mother: Option<FuncId>,
    name: Option<IdentId>,
    /// Bytecode.
    pub(super) bytecode: Option<Pin<Box<[Bc]>>>,
    /// Source map.
    pub sourcemap: Vec<Loc>,
    /// Exception handling map.
    exception_map: Vec<(
        std::ops::Range<BcPc>, // range of capturing exception
        Option<BcPc>,          // rescue destination pc
        Option<BcPc>,          // ensure destination pc
        Option<SlotId>,        // a slot where an error object is assigned
    )>,
    /// the name of arguments.
    pub(in crate::executor) args: ParamsInfo,
    /// outer local variables. (dynamic_locals, block_param)
    pub outer_locals: Vec<(HashMap<IdentId, u16>, Option<IdentId>)>,
    /// literal values. (for GC)
    pub literals: Vec<Value>,
    /// The number of non-temporary registers.
    pub non_temp_num: u16,
    /// The number of temporary registers.
    pub temp_num: u16,
    pub lexical_context: Vec<Module>,
    pub sourceinfo: SourceInfoRef,
    pub(crate) is_block_style: bool,
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
    pub(in crate::executor) fn new(
        id: Option<FuncId>,
        mother: Option<FuncId>,
        outer_locals: Vec<(HashMap<IdentId, u16>, Option<IdentId>)>,
        name: Option<IdentId>,
        args: ParamsInfo,
        sourceinfo: SourceInfoRef,
        is_block: bool,
    ) -> Self {
        ISeqInfo {
            id,
            mother,
            name,
            bytecode: None,
            sourcemap: vec![],
            exception_map: vec![],
            args: args.clone(),
            outer_locals,
            literals: vec![],
            non_temp_num: 0,
            temp_num: 0,
            lexical_context: vec![],
            sourceinfo,
            is_block_style: is_block,
        }
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

    pub(in crate::executor) fn new_block(
        id: Option<FuncId>,
        mother: FuncId,
        outer: (FuncId, Vec<(HashMap<IdentId, u16>, Option<IdentId>)>),
        args: ParamsInfo,
        sourceinfo: SourceInfoRef,
    ) -> Self {
        Self::new(id, Some(mother), outer.1, None, args, sourceinfo, true)
    }

    pub(in crate::executor) fn new_method(
        id: Option<FuncId>,
        name: Option<IdentId>,
        args: ParamsInfo,
        sourceinfo: SourceInfoRef,
    ) -> Self {
        Self::new(id, id, vec![], name, args, sourceinfo, false)
    }

    pub(crate) fn id(&self) -> FuncId {
        self.id.unwrap()
    }

    ///
    /// Set bytecode to the *ISeqInfo*.
    ///
    pub(in crate::executor) fn set_bytecode(&mut self, bc: Vec<Bc>) {
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

    ///
    /// Get a number of keyword arguments.
    ///
    pub(crate) fn key_num(&self) -> usize {
        self.args.keyword_names.len()
    }

    ///
    /// Get a position of keyword arguments.
    ///
    pub(crate) fn block_pos(&self) -> usize {
        if self.args.block_param.is_some() {
            self.args.pos_num + self.key_num() + 1
        } else {
            0
        }
    }

    ///
    /// Get a number of required arguments.
    ///
    pub(crate) fn req_num(&self) -> usize {
        self.args.required_num
    }

    ///
    /// get a number of required + optional arguments.
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
    pub(crate) fn block_param_name(&self) -> Option<IdentId> {
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
    pub(in crate::executor) fn bytecode(&self) -> &[Bc] {
        self.bytecode.as_ref().unwrap()
    }

    ///
    /// Get pc(*BcPc*) for instruction index(*idx*).
    ///
    pub(in crate::executor) fn get_pc(&self, idx: BcIndex) -> BcPc {
        BcPc::from(&self.bytecode()[idx.0 as usize])
    }

    ///
    /// Get pc(*BcPc*) for instruction index(*idx*).
    ///
    pub(in crate::executor) fn get_top_pc(&self) -> BcPc {
        BcPc::from(&self.bytecode()[0])
    }

    ///
    /// Get an instruction index(*usize*) corresponding to pc(*BcPc*).
    ///
    pub(in crate::executor) fn get_pc_index(&self, pc: Option<BcPc>) -> BcIndex {
        let i = if let Some(pos) = pc {
            pos - self.get_top_pc()
        } else {
            0
        };
        BcIndex::from(i)
    }

    ///
    /// Explore exception table for pc(*BcPc*) and return error handler's pc(*BcPc*) and the slot where an error object is to be stored.
    ///
    pub(in crate::executor) fn get_exception_dest(
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

    pub(in crate::executor) fn exception_push(
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
    fn bytecode_len(&self) -> usize {
        self.bytecode().len()
    }

    ///
    /// Get bytecode address.
    ///
    #[cfg(feature = "emit-bc")]
    pub(in crate::executor) fn bytecode_top(&self) -> *const Bc {
        self.bytecode().as_ptr()
    }

    ///
    /// Get basic block information.
    ///
    /// This returns a Vec which represents whether it is a start of a basic block for each bytecode.
    ///
    /// Some((basic_block_id, Vec of source bytecodes)) => a start bytecode of a basic block.
    ///
    pub(crate) fn get_incoming(&self) -> Vec<Vec<BcIndex>> {
        let mut info = vec![vec![]; self.bytecode_len() + 1];
        for (idx, pc) in self.bytecode().iter().enumerate() {
            let pc = BcPc::from(pc);
            if let Some(disp) = TraceIr::is_branch(pc) {
                let dest = ((idx + 1) as i32 + disp) as usize;
                info[dest].push(BcIndex::from(idx));
                if !TraceIr::is_terminal(pc) {
                    // "not taken" edge for conditional branches.
                    info[idx + 1].push(BcIndex::from(idx));
                }
            }
        }
        for (idx, pc) in self.bytecode().iter().enumerate() {
            let pc = BcPc::from(pc);
            if !info[idx + 1].is_empty() && TraceIr::is_branch(pc).is_none() {
                // merging edge (back edge).
                info[idx + 1].push(BcIndex::from(idx));
            }
        }
        assert_eq!(0, info[self.bytecode_len()].len());
        info
    }
}
