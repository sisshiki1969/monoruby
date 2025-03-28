use super::*;

///
/// Handle hash splat arguments and a keyword rest parameter.
///
pub(crate) fn jit_hash_splat_kw_rest(
    globals: &mut Globals,
    callid: CallSiteId,
    callee_lfp: Lfp,
    caller_lfp: Lfp,
    meta: Meta,
) -> Result<()> {
    let callee_func_id = meta.func_id();
    let callee = &globals.store[callee_func_id];
    let caller = &globals.store[callid];
    hash_splat_and_kw_rest(callee, caller, callee_lfp, caller_lfp)
}
///
/// if argument mismatch occurs, return None.
///
pub(crate) fn handle_positional(
    info: &FuncInfo,
    arg_num: usize,
    mut callee_lfp: Lfp,
) -> Result<()> {
    let req_num = info.req_num();
    let reqopt_num = info.reqopt_num();
    let is_rest = info.is_rest();
    let is_block_style = info.is_block_style();
    unsafe {
        if arg_num > reqopt_num {
            if is_rest {
                let len = arg_num - reqopt_num;
                let ary: Vec<_> = callee_lfp.slice(reqopt_num, len).collect();
                callee_lfp.set_register(1 + reqopt_num, Some(Value::array_from_vec(ary)));
            } else if !is_block_style {
                return Err(MonorubyErr::wrong_number_of_arg_range(
                    arg_num,
                    req_num..=reqopt_num,
                ));
            }
            return Ok(());
        }

        if arg_num >= req_num {
            let len = reqopt_num - arg_num;
            fill(callee_lfp, reqopt_num, len, None);
        } else {
            if !is_block_style {
                return Err(MonorubyErr::wrong_number_of_arg_range(
                    arg_num,
                    req_num..=reqopt_num,
                ));
            }
            let len = req_num - arg_num;
            fill(callee_lfp, req_num, len, Some(Value::nil()));
            let len = reqopt_num - req_num;
            fill(callee_lfp, reqopt_num, len, None);
        }

        if is_rest {
            callee_lfp.set_register(1 + reqopt_num, Some(Value::array_empty()));
        }
    }
    Ok(())
}

///
/// Set positional arguments (req, opt, rest) and keyword arguments (kw, kw_rest) to the callee frame.
///
/// This function solves the match of arguments-parameters dynamically.
///
pub(crate) fn set_frame_arguments(
    globals: &mut Globals,
    callee_lfp: Lfp,
    caller_lfp: Lfp,
    callid: CallSiteId,
) -> Result<()> {
    let callee_fid = callee_lfp.meta().func_id();
    let callee = &globals.store[callee_fid];
    let caller = &globals.store[callid];

    positional(caller, callee, callee_lfp, caller_lfp)?;
    if !callee.no_keyword() || !caller.kw_may_exists() {
        handle_keyword(callee, caller, callee_lfp, caller_lfp)?;
    }

    Ok(())
}

pub(crate) fn set_frame_arguments_simple(
    globals: &mut Globals,
    callee_lfp: Lfp,
    caller_lfp: Lfp,
    callid: CallSiteId,
    src: *const Value,
    pos_num: usize,
) -> Result<()> {
    let callee_fid = callee_lfp.meta().func_id();
    let callee = &globals.store[callee_fid];
    let caller = &globals.store[callid];

    positional_simple(callee, src, pos_num, callee_lfp)?;
    if !callee.no_keyword() {
        handle_keyword(callee, caller, callee_lfp, caller_lfp)?;
    }

    Ok(())
}

///
/// Set block argument to the callee frame.
///
pub(crate) fn set_frame_block(caller: &CallSiteInfo, callee_lfp: Lfp, caller_lfp: Lfp) {
    let CallSiteInfo {
        block_fid,
        block_arg,
        ..
    } = *caller;

    let bh = if let Some(block_fid) = block_fid {
        let bh = BlockHandler::from_caller(block_fid);
        Some(bh)
    } else if let Some(block_arg) = block_arg {
        match caller_lfp.register(block_arg.0 as usize) {
            Some(v) => Some(BlockHandler::new(v)),
            None => None,
        }
    } else {
        None
    };
    callee_lfp.set_block(bh);
}

pub(crate) extern "C" fn jit_generic_set_arguments(
    vm: &mut Executor,
    globals: &Globals,
    caller: CallSiteId,
    callee_lfp: Lfp,
    meta: Meta,
) -> Option<Value> {
    let caller_lfp = vm.cfp().lfp();
    let caller = &globals.store[caller];
    let callee_fid = meta.func_id();
    let callee = &globals.store[callee_fid];
    match positional(caller, callee, callee_lfp, caller_lfp) {
        Ok(_) => Some(Value::nil()),
        Err(err) => {
            vm.set_error(err);
            None
        }
    }
}

///
/// Set positional arguments.
///
fn positional(
    caller: &CallSiteInfo,
    callee: &FuncInfo,
    callee_lfp: Lfp,
    caller_lfp: Lfp,
) -> Result<()> {
    let splat_pos = &caller.splat_pos;
    let src = caller_lfp.register_ptr(caller.args.0 as usize) as *mut Value;
    let dst = callee_lfp.register_ptr(1);
    let pos_args = caller.pos_num;

    let ex = if callee.no_keyword() && caller.kw_may_exists() {
        // handle excessive keyword arguments
        let mut h = IndexMap::default();
        for (k, id) in caller.kw_args.iter() {
            let v = caller_lfp.register(caller.kw_pos.0 as usize + *id).unwrap();
            h.insert(HashKey(Value::symbol(*k)), v);
        }
        for v in caller
            .hash_splat_pos
            .iter()
            .map(|pos| caller_lfp.register(pos.0 as usize).unwrap())
        {
            for (k, v) in v.expect_hash()?.iter() {
                h.insert(HashKey(k), v);
            }
        }
        if h.is_empty() {
            None
        } else {
            Some(Value::hash(h))
        }
    } else {
        None
    };

    let mut buf = vec![];
    for i in 0..pos_args {
        let v = unsafe { *src.sub(i) };
        if splat_pos.contains(&i) {
            let ary = v.try_array_ty().expect("splat arguments must be an array");
            buf.extend_from_slice(&ary);
        } else {
            buf.push(v);
        }
    }
    if let Some(v) = ex {
        buf.push(v);
    }

    // single array argument expansion for blocks
    if buf.len() == 1
        && callee.single_arg_expand()
        && let Some(ary) = buf[0].try_array_ty()
    {
        return fill_positional_args(dst, callee, &ary);
    }

    fill_positional_args(dst, callee, &buf)
}

fn fill_positional_args(dst: *mut Option<Value>, callee: &FuncInfo, buf: &[Value]) -> Result<()> {
    fn fill(dst: *mut Option<Value>, start: usize, end: usize, val: Option<Value>) {
        unsafe { std::slice::from_raw_parts_mut(dst.sub(end).add(1), end - start).fill(val) }
    }

    fn memcpy<'a>(dst: *mut Option<Value>, offset: usize, buf: &[Value]) {
        let ptr = buf.as_ptr();
        let len = buf.len();
        for i in 0..len {
            unsafe { *dst.sub(i + offset) = Some(*ptr.add(i)) };
        }
    }
    let buf_len = buf.len();
    let min_args = callee.req_num() + callee.post_num();
    let max_args = callee.max_positional_args();
    let is_block_style = callee.is_block_style();
    if !is_block_style && (buf_len < min_args || (buf_len > max_args && !callee.is_rest())) {
        return Err(MonorubyErr::wrong_number_of_arg_range(
            buf_len,
            min_args..=max_args,
        ));
    }

    let opt_pos = callee.req_num();
    let rest_pos = callee.reqopt_num();
    let post_pos = callee.reqopt_num() + callee.is_rest() as usize;
    let end_pos = callee.total_positional_args();
    if buf_len <= callee.req_num() {
        memcpy(dst, 0, &buf);
        fill(dst, buf_len, opt_pos, Some(Value::nil()));
        fill(dst, opt_pos, rest_pos, None);
        if let Some(rest) = callee.rest_pos() {
            unsafe { *dst.sub(rest as usize) = Some(Value::array_empty()) };
        }
        fill(dst, post_pos, end_pos, Some(Value::nil()));
    } else if buf_len <= callee.req_num() + callee.post_num() {
        memcpy(dst, 0, &buf[0..opt_pos]);
        fill(dst, opt_pos, rest_pos, None);
        if let Some(rest) = callee.rest_pos() {
            unsafe { *dst.sub(rest as usize) = Some(Value::array_empty()) };
        }
        let args_num = buf_len - opt_pos;
        memcpy(dst, post_pos, &buf[opt_pos..]);
        fill(dst, post_pos + args_num, end_pos, Some(Value::nil()));
    } else if buf_len <= callee.max_positional_args() {
        let args_num = buf_len - callee.req_num() - callee.post_num();
        memcpy(dst, 0, &buf[0..opt_pos + args_num]);
        fill(dst, opt_pos + args_num, rest_pos, None);
        if let Some(rest) = callee.rest_pos() {
            unsafe { *dst.sub(rest as usize) = Some(Value::array_empty()) };
        }
        memcpy(dst, post_pos, &buf[opt_pos + args_num..]);
    } else {
        memcpy(dst, 0, &buf[0..rest_pos]);
        if let Some(rest) = callee.rest_pos() {
            unsafe {
                *dst.sub(rest as usize) = Some(Value::array_from_iter(
                    buf[rest_pos..buf_len + post_pos - end_pos].iter().cloned(),
                ))
            };
        }
        memcpy(dst, post_pos, &buf[buf_len + post_pos - end_pos..]);
    }

    Ok(())
}

fn positional_simple(
    callee: &FuncInfo,
    src: *const Value,
    pos_num: usize,
    mut callee_lfp: Lfp,
) -> Result<()> {
    let max_pos = callee.max_positional_args();
    let dst = callee_lfp.register_ptr(1) as *mut Value;

    let (arg_num, rest) = if pos_num <= max_pos {
        memcpy(src, dst, pos_num);
        (pos_num, vec![])
    } else {
        memcpy(src, dst, max_pos);
        // handle the rest arguments.
        let rest = unsafe { std::slice::from_raw_parts(src.sub(pos_num - 1), pos_num - max_pos) }
            .iter()
            .cloned()
            .rev()
            .collect();
        (max_pos, rest)
    };

    let req_num = callee.req_num();
    let is_rest = callee.is_rest();
    let total_pos_args = arg_num + rest.len();
    if total_pos_args > max_pos {
        if is_rest {
            unsafe { callee_lfp.set_register(1 + max_pos, Some(Value::array_from_vec(rest))) };
        } else {
            return Err(MonorubyErr::wrong_number_of_arg_range(
                total_pos_args,
                req_num..=max_pos,
            ));
        }
        return Ok(());
    }

    if total_pos_args >= req_num {
        let len = max_pos - arg_num;
        fill(callee_lfp, max_pos, len, None);
    } else {
        return Err(MonorubyErr::wrong_number_of_arg_range(
            total_pos_args,
            req_num..=max_pos,
        ));
    }

    if is_rest {
        unsafe { callee_lfp.set_register(1 + max_pos, Some(Value::array_empty())) };
    }

    Ok(())
}

///
/// Handle keyword arguments.
///
fn handle_keyword(
    callee: &FuncInfo,
    caller: &CallSiteInfo,
    callee_lfp: Lfp,
    caller_lfp: Lfp,
) -> Result<()> {
    ordinary_keyword(callee, caller, callee_lfp, caller_lfp)?;
    hash_splat_and_kw_rest(callee, caller, callee_lfp, caller_lfp)
}

fn ordinary_keyword(
    info: &FuncInfo,
    callsite: &CallSiteInfo,
    mut callee_lfp: Lfp,
    caller_lfp: Lfp,
) -> Result<()> {
    let CallSiteInfo {
        kw_pos, kw_args, ..
    } = callsite;

    let callee_kw_pos = info.kw_reg_pos();
    let mut used = 0;
    for (id, param_name) in info.kw_names().iter().enumerate() {
        unsafe {
            let v = kw_args
                .get(param_name)
                .map(|i| caller_lfp.get_slot(*kw_pos + *i).unwrap());
            if v.is_some() {
                used += 1;
            }
            callee_lfp.set_register(callee_kw_pos + id, v);
        }
    }
    if used < kw_args.len() && info.kw_rest().is_none() {
        for (k, _) in kw_args.iter() {
            if !info.kw_names().contains(k) {
                return Err(MonorubyErr::argumenterr(format!("unknown keyword: :{k}")));
            }
        }
    }
    Ok(())
}

///
/// Handle hash splat arguments and a keyword rest parameter.
///
fn hash_splat_and_kw_rest(
    callee: &FuncInfo,
    caller: &CallSiteInfo,
    mut callee_lfp: Lfp,
    caller_lfp: Lfp,
) -> Result<()> {
    let CallSiteInfo {
        kw_pos,
        kw_args,
        hash_splat_pos,
        ..
    } = caller;

    let callee_kw_pos = callee.kw_reg_pos();

    for h in hash_splat_pos
        .iter()
        .map(|pos| caller_lfp.register(pos.0 as usize).unwrap())
    {
        let mut used = 0;
        for (id, param_name) in callee.kw_names().iter().enumerate() {
            let h = h.expect_hash()?;
            unsafe {
                let sym = Value::symbol(*param_name);
                if let Some(v) = h.get(sym) {
                    used += 1;
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
            if used < h.len() && callee.kw_rest().is_none() {
                for (k, _) in h.iter() {
                    let sym = k.as_symbol();
                    if !callee.kw_names().contains(&sym) {
                        return Err(MonorubyErr::argumenterr(format!("unknown keyword: :{sym}")));
                    }
                }
            }
        }
    }

    if let Some(rest) = callee.kw_rest() {
        let mut kw_rest = IndexMap::default();
        for (name, i) in kw_args.iter() {
            if callee.kw_names().contains(name) {
                continue;
            }
            let v = unsafe { caller_lfp.get_slot(*kw_pos + *i).unwrap() };
            kw_rest.insert(HashKey(Value::symbol(*name)), v);
        }
        for h in hash_splat_pos
            .iter()
            .map(|pos| caller_lfp.register(pos.0 as usize).unwrap())
        {
            let mut h = h.as_hashmap_inner().clone();
            for name in callee.kw_names().iter() {
                let sym = Value::symbol(*name);
                h.remove(sym);
            }
            for (k, v) in h.iter() {
                kw_rest.insert(HashKey(k), v);
            }
        }

        unsafe { callee_lfp.set_register(rest.0 as usize, Some(Value::hash(kw_rest))) }
    }
    Ok(())
}

// Utility functions.

fn memcpy(src: *const Value, dst: *mut Value, len: usize) {
    if len != 0 {
        unsafe {
            std::ptr::copy_nonoverlapping(src.sub(len - 1), dst.sub(len - 1), len);
        }
    }
}

fn fill(lfp: Lfp, start_pos: usize, len: usize, val: Option<Value>) {
    unsafe {
        let ptr = lfp.register_ptr(start_pos);
        std::slice::from_raw_parts_mut(ptr, len).fill(val);
    }
}
