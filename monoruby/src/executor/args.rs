use super::*;

fn push(
    arg_num: &mut usize,
    rest: &mut Vec<Value>,
    max_pos: usize,
    dst: *mut Value,
    v: Value,
    no_push: bool,
) {
    if *arg_num >= max_pos {
        if !no_push {
            rest.push(v);
        }
    } else {
        unsafe { *dst.sub(*arg_num) = v };
        *arg_num += 1;
    }
}

fn memcpy(src: *const Value, dst: *mut Value, len: usize) {
    if len != 0 {
        unsafe {
            std::ptr::copy_nonoverlapping(src.sub(len - 1), dst.sub(len - 1), len);
        }
    }
}

fn fill(lfp: LFP, start_pos: usize, len: usize, val: Option<Value>) {
    unsafe {
        let ptr = lfp.register_ptr(start_pos);
        std::slice::from_raw_parts_mut(ptr, len).fill(val);
    }
}

///
/// Set positional arguments.
///
/// returns (the number of arguments which is set in this function, the rest arguments).
///
fn positional(
    caller: &CallSiteInfo,
    callee_info: &FuncInfo,
    src: *const Value,
    dst: *mut Value,
) -> (usize, Vec<Value>) {
    let max_pos = callee_info.max_positional_args();
    let no_push = callee_info.discard_excess_positional_args();
    let splat_pos = &caller.splat_pos;
    let len = caller.pos_num;

    if splat_pos.is_empty() {
        if len <= max_pos {
            memcpy(src, dst, len);
            (len, vec![])
        } else {
            memcpy(src, dst, max_pos);
            // handle the rest arguments.
            let rest = if !no_push {
                unsafe { std::slice::from_raw_parts(src.sub(len - 1), len - max_pos) }
                    .iter()
                    .cloned()
                    .rev()
                    .collect()
            } else {
                vec![]
            };
            (max_pos, rest)
        }
    } else {
        let mut arg_num = 0;
        let mut rest = vec![];
        for i in 0..len {
            let v = unsafe { *src.sub(i) };
            if splat_pos.contains(&i) {
                if let Some(ary) = v.try_array_ty() {
                    for v in ary.iter() {
                        push(&mut arg_num, &mut rest, max_pos, dst, *v, no_push);
                    }
                } else if let Some(_range) = v.is_range() {
                    unimplemented!()
                } else if let Some(_hash) = v.is_hash() {
                    unimplemented!()
                } else {
                    push(&mut arg_num, &mut rest, max_pos, dst, v, no_push);
                };
            } else {
                push(&mut arg_num, &mut rest, max_pos, dst, v, no_push);
            }
        }
        (arg_num, rest)
    }
}

impl Executor {
    ///
    /// Set positional arguments (req, opt, rest) and keyword arguments (kw, kw_rest) to the callee frame.
    ///
    pub(crate) fn set_frame_arguments(
        &mut self,
        globals: &mut Globals,
        callee_lfp: LFP,
        callid: CallSiteId,
        src: *const Value,
    ) -> Option<usize> {
        let callee_func_id = callee_lfp.meta().func_id();
        let callee_info = &globals[callee_func_id];
        let max_pos = callee_info.max_positional_args();
        let no_push = callee_info.discard_excess_positional_args();
        let caller = &globals.store[callid];

        // TODO: if caller is simple (no splat, no keywords), and callee is also simple (no optional, no rest, no keywords), we can optimize this.

        let dst = unsafe { callee_lfp.register_ptr(1) as *mut Value };
        let (mut arg_num, mut rest) = positional(caller, callee_info, src, dst);
        // single array argument expansion for blocks
        if arg_num == 1 && callee_info.single_arg_expand() {
            let v = unsafe { *dst };
            if let Some(ary) = v.try_array_ty() {
                arg_num = 0;
                for v in ary.iter() {
                    push(&mut arg_num, &mut rest, max_pos, dst, *v, no_push);
                }
            }
        }

        match &callee_info.kind {
            FuncKind::Builtin { .. } => {} // no keyword param and rest param for native func, attr_accessor, etc.
            FuncKind::AttrReader { .. } => {} // no keyword param and rest param for native func, attr_accessor, etc.
            FuncKind::AttrWriter { .. } => {} // no keyword param and rest param for native func, attr_accessor, etc.
            _ => {
                if callee_info.no_keyword() && caller.kw_num() != 0 {
                    // handle excessive keyword arguments
                    let mut h = IndexMap::default();
                    for (k, id) in caller.kw_args.iter() {
                        let v = unsafe { self.get_slot(caller.kw_pos + *id as u16).unwrap() };
                        h.insert(HashKey(Value::symbol(*k)), v);
                    }
                    let ex: Value = Value::hash(h);
                    self.handle_positional2(&callee_info, arg_num, callee_lfp, Some(ex), rest)?;
                } else {
                    self.handle_positional2(&callee_info, arg_num, callee_lfp, None, rest)?;
                    self.handle_keyword2(&callee_info, caller, callee_lfp);
                }
            }
        }

        Some(arg_num)
    }

    ///
    /// Set block argument to the callee frame.
    ///
    pub(crate) fn set_frame_block(&mut self, caller: &CallSiteInfo, callee_lfp: LFP) {
        let CallSiteInfo {
            block_fid,
            block_arg,
            ..
        } = *caller;

        let bh = if let Some(block_fid) = block_fid {
            let bh = BlockHandler::from(block_fid);
            Some(bh)
        } else if let Some(block_arg) = block_arg {
            unsafe { Some(BlockHandler(self.get_slot(block_arg).unwrap())) }
        } else {
            None
        };
        callee_lfp.set_block(bh);
    }

    ///
    /// if argument mismatch occurs, return None.
    ///
    fn handle_positional2(
        &mut self,
        info: &FuncInfo,
        arg_num: usize,
        mut callee_lfp: LFP,
        ex: Option<Value>,
        mut rest_arg: Vec<Value>,
    ) -> Option<Value> {
        let req_num = info.req_num();
        let reqopt_num = info.reqopt_num();
        let is_rest = info.is_rest();
        let is_block_style = info.is_block_style();
        let ex_num = ex.is_some() as usize;
        let total_pos_num = arg_num + rest_arg.len() + ex_num;
        unsafe {
            if total_pos_num > reqopt_num {
                if is_rest {
                    if let Some(h) = ex {
                        rest_arg.push(h);
                    }
                    callee_lfp.set_register(1 + reqopt_num, Some(Value::array_from_vec(rest_arg)));
                } else if !is_block_style {
                    self.err_wrong_number_of_arg_range(total_pos_num, req_num..=reqopt_num);
                    return None;
                }
                return Some(Value::nil());
            }

            if total_pos_num >= req_num {
                let len = reqopt_num - arg_num;
                fill(callee_lfp, reqopt_num, len, None);
            } else {
                if !is_block_style {
                    self.err_wrong_number_of_arg_range(total_pos_num, req_num..=reqopt_num);
                    return None;
                }
                let len = req_num - (arg_num + ex_num);
                fill(callee_lfp, req_num, len, Some(Value::nil()));
                let len = reqopt_num - req_num;
                fill(callee_lfp, reqopt_num, len, None);
            }
            if let Some(ex) = ex {
                callee_lfp.set_register(1 + arg_num, Some(ex));
            }

            if is_rest {
                callee_lfp.set_register(1 + reqopt_num, Some(Value::array_empty()));
            }
        }
        Some(Value::nil())
    }

    ///
    /// Handle keyword arguments.
    ///
    fn handle_keyword2(&mut self, info: &FuncInfo, callsite: &CallSiteInfo, mut callee_lfp: LFP) {
        let CallSiteInfo {
            kw_pos,
            kw_args,
            hash_splat_pos,
            ..
        } = callsite;

        let caller_lfp = self.cfp().lfp();
        let callee_kw_pos = info.pos_num() + 1;
        for (id, param_name) in info.kw_names().iter().enumerate() {
            unsafe {
                let v = kw_args
                    .get(param_name)
                    .map(|i| caller_lfp.get_slot(*kw_pos + *i).unwrap());
                callee_lfp.set_register(callee_kw_pos + id, v);
            }
        }

        for h in hash_splat_pos
            .iter()
            .map(|pos| unsafe { caller_lfp.register(pos.0 as usize).unwrap() })
        {
            for (id, param_name) in info.kw_names().iter().enumerate() {
                unsafe {
                    let sym = Value::symbol(*param_name);
                    if let Some(v) = h.as_hash().get(sym) {
                        let ptr = callee_lfp.register_ptr(callee_kw_pos + id);
                        if (*ptr).is_some() {
                            eprintln!(
                                " warning: key :{} is duplicated and overwritten",
                                param_name
                            );
                        }
                        *ptr = Some(v);
                    }
                }
            }
        }

        if let Some(rest) = info.kw_rest() {
            let mut kw_rest = IndexMap::default();
            for (name, i) in kw_args.iter() {
                if info.kw_names().contains(name) {
                    continue;
                }
                let v = unsafe { caller_lfp.get_slot(*kw_pos + *i).unwrap() };
                kw_rest.insert(HashKey(Value::symbol(*name)), v);
            }
            for h in hash_splat_pos
                .iter()
                .map(|pos| unsafe { caller_lfp.register(pos.0 as usize).unwrap() })
            {
                let mut h = h.as_hash().clone();
                for name in info.kw_names().iter() {
                    let sym = Value::symbol(*name);
                    h.remove(sym);
                }
                for (k, v) in h {
                    kw_rest.insert(HashKey(k), v);
                }
            }

            unsafe { callee_lfp.set_register(rest.0 as usize, Some(Value::hash(kw_rest))) }
        }
    }

    ///
    /// Handle keyword arguments.
    ///
    fn handle_keyword(&mut self, info: &ISeqInfo, callsite: &CallSiteInfo, mut callee_lfp: LFP) {
        let CallSiteInfo {
            kw_pos,
            kw_args,
            hash_splat_pos,
            ..
        } = callsite;

        let caller_lfp = self.cfp().lfp();
        let callee_kw_pos = info.pos_num() + 1;
        for (id, param_name) in info.args.kw_names.iter().enumerate() {
            unsafe {
                let v = kw_args
                    .get(param_name)
                    .map(|i| caller_lfp.get_slot(*kw_pos + *i).unwrap());
                callee_lfp.set_register(callee_kw_pos + id, v);
            }
        }

        for h in hash_splat_pos
            .iter()
            .map(|pos| unsafe { caller_lfp.register(pos.0 as usize).unwrap() })
        {
            for (id, param_name) in info.args.kw_names.iter().enumerate() {
                unsafe {
                    let sym = Value::symbol(*param_name);
                    if let Some(v) = h.as_hash().get(sym) {
                        let ptr = callee_lfp.register_ptr(callee_kw_pos + id);
                        if (*ptr).is_some() {
                            eprintln!(
                                " warning: key :{} is duplicated and overwritten",
                                param_name
                            );
                        }
                        *ptr = Some(v);
                    }
                }
            }
        }

        if let Some(rest) = info.kw_rest() {
            let mut kw_rest = IndexMap::default();
            for (name, i) in kw_args.iter() {
                if info.args.kw_names.contains(name) {
                    continue;
                }
                let v = unsafe { caller_lfp.get_slot(*kw_pos + *i).unwrap() };
                kw_rest.insert(HashKey(Value::symbol(*name)), v);
            }
            for h in hash_splat_pos
                .iter()
                .map(|pos| unsafe { caller_lfp.register(pos.0 as usize).unwrap() })
            {
                let mut h = h.as_hash().clone();
                for name in info.args.kw_names.iter() {
                    let sym = Value::symbol(*name);
                    h.remove(sym);
                }
                for (k, v) in h {
                    kw_rest.insert(HashKey(k), v);
                }
            }

            unsafe { callee_lfp.set_register(rest.0 as usize, Some(Value::hash(kw_rest))) }
        }
    }

    pub(crate) fn handle_arguments(
        &mut self,
        globals: &mut Globals,
        callid: CallSiteId,
        arg_num: usize,
        callee_lfp: LFP,
        meta: Meta,
    ) -> Option<Value> {
        let callee_func_id = meta.func_id();
        match &globals[callee_func_id].kind {
            FuncKind::ISeq(info) => {
                let caller = &globals.store[callid];
                if info.no_keyword() && caller.kw_num() != 0 {
                    // handle excessive keyword arguments
                    let mut h = IndexMap::default();
                    for (k, id) in caller.kw_args.iter() {
                        let v = unsafe { self.get_slot(caller.kw_pos + *id as u16).unwrap() };
                        h.insert(HashKey(Value::symbol(*k)), v);
                    }
                    let ex: Value = Value::hash(h);
                    self.handle_positional(&info, arg_num, callee_lfp, Some(ex))?;
                } else {
                    self.handle_positional(&info, arg_num, callee_lfp, None)?;
                    self.handle_keyword(&info, caller, callee_lfp);
                }
            }
            _ => {} // no keyword param and rest param for native func, attr_accessor, etc.
        }

        Some(Value::nil())
    }

    ///
    /// if argument mismatch occurs, return None.
    ///
    pub(crate) fn handle_positional(
        &mut self,
        info: &ISeqInfo,
        arg_num: usize,
        mut callee_lfp: LFP,
        ex: Option<Value>,
    ) -> Option<Value> {
        let req_num = info.required_num();
        let reqopt_num = info.reqopt_num();
        let pos_num = info.pos_num();
        let is_rest = pos_num != reqopt_num;
        let is_block_style = info.is_block_style();
        let ex_num = ex.is_some() as usize;
        unsafe {
            if (arg_num + ex_num) > reqopt_num {
                if is_rest {
                    let len = arg_num - reqopt_num;
                    let mut ary: Vec<_> = callee_lfp.slice(reqopt_num, len).collect();
                    if let Some(ex) = ex {
                        ary.push(ex);
                    }
                    callee_lfp.set_register(1 + reqopt_num, Some(Value::array_from_vec(ary)));
                } else if !is_block_style {
                    self.err_wrong_number_of_arg_range(arg_num + ex_num, req_num..=reqopt_num);
                    return None;
                }
                return Some(Value::nil());
            }

            if (arg_num + ex_num) >= req_num {
                let len = reqopt_num - arg_num;
                fill(callee_lfp, reqopt_num, len, None);
            } else {
                if !is_block_style {
                    self.err_wrong_number_of_arg_range(arg_num + ex_num, req_num..=reqopt_num);
                    return None;
                }
                let len = req_num - (arg_num + ex_num);
                fill(callee_lfp, req_num, len, Some(Value::nil()));
                let len = reqopt_num - req_num;
                fill(callee_lfp, reqopt_num, len, None);
            }
            if let Some(ex) = ex {
                callee_lfp.set_register(1 + arg_num, Some(ex));
            }

            if is_rest {
                callee_lfp.set_register(1 + reqopt_num, Some(Value::array_empty()));
            }
        }
        Some(Value::nil())
    }
}
