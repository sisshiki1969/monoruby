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
    let callee = &globals[callee_func_id];
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
    let pos_num = info.pos_num();
    let is_rest = pos_num != reqopt_num;
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
    src: *const Value,
) -> Result<()> {
    let callee_fid = callee_lfp.meta().func_id();
    let callee = &globals[callee_fid];
    let caller = &globals.store[callid];
    positional(caller, callee, src, callee_lfp, caller_lfp)?;

    if !callee.no_keyword() || !caller.kw_may_exists() {
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
        let bh = BlockHandler::from(block_fid);
        Some(bh)
    } else {
        block_arg.map(|block_arg| unsafe {
            BlockHandler(caller_lfp.register(block_arg.0 as usize).unwrap())
        })
    };
    callee_lfp.set_block(bh);
}

pub(crate) extern "C" fn jit_generic_set_arguments(
    vm: &mut Executor,
    globals: &Globals,
    caller: CallSiteId,
    src: *const Value,
    callee_lfp: Lfp,
    meta: Meta,
) -> Option<Value> {
    let caller_lfp = vm.cfp().lfp();
    let caller = &globals.store[caller];
    let callee_fid = meta.func_id();
    let callee = &globals.store[callee_fid];
    match positional(caller, callee, src, callee_lfp, caller_lfp) {
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
    src: *const Value,
    mut callee_lfp: Lfp,
    caller_lfp: Lfp,
) -> Result<()> {
    let max_pos = callee.max_positional_args();
    let no_push = callee.discard_excess_positional_args();
    let splat_pos = &caller.splat_pos;
    let pos_num = caller.pos_num;
    let dst = unsafe { callee_lfp.register_ptr(1) as *mut Value };

    let ex = if callee.no_keyword() && caller.kw_may_exists() {
        // handle excessive keyword arguments
        let mut h = IndexMap::default();
        for (k, id) in caller.kw_args.iter() {
            let v = unsafe { caller_lfp.register(caller.kw_pos.0 as usize + *id).unwrap() };
            h.insert(HashKey(Value::symbol(*k)), v);
        }
        for v in caller
            .hash_splat_pos
            .iter()
            .map(|pos| unsafe { caller_lfp.register(pos.0 as usize).unwrap() })
        {
            for (k, v) in v.expect_hash()? {
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

    let (mut arg_num, mut rest) = if splat_pos.is_empty() {
        if pos_num <= max_pos {
            memcpy(src, dst, pos_num);
            (pos_num, vec![])
        } else {
            memcpy(src, dst, max_pos);
            // handle the rest arguments.
            let rest = if !no_push {
                unsafe { std::slice::from_raw_parts(src.sub(pos_num - 1), pos_num - max_pos) }
                    .iter()
                    .cloned()
                    .rev()
                    .collect()
            } else {
                vec![]
            };
            (max_pos, rest)
        }
    } else if callee.is_rest()
        && callee.reqopt_num() == 0
        && pos_num == 1
        && unsafe { (*src).is_array_ty() }
        && ex.is_none()
    {
        memcpy(src, dst, 1);
        return Ok(());
    } else {
        let mut arg_num = 0;
        let mut rest = vec![];
        for i in 0..pos_num {
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
    };

    // single array argument expansion for blocks
    if arg_num == 1 && callee.single_arg_expand() {
        let v = unsafe { *dst };
        if let Some(ary) = v.try_array_ty() {
            arg_num = 0;
            for v in ary.iter() {
                push(&mut arg_num, &mut rest, max_pos, dst, *v, no_push);
            }
        }
    }

    let req_num = callee.req_num();
    let ex_num = ex.is_some() as usize;
    let is_rest = callee.is_rest();
    let total_pos_args = arg_num + rest.len() + ex_num;
    let is_block_style = callee.is_block_style();
    if total_pos_args > max_pos {
        if is_rest {
            if let Some(h) = ex {
                rest.push(h);
            }
            unsafe { callee_lfp.set_register(1 + max_pos, Some(Value::array_from_vec(rest))) };
        } else if !is_block_style {
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
        if !is_block_style {
            return Err(MonorubyErr::wrong_number_of_arg_range(
                total_pos_args,
                req_num..=max_pos,
            ));
        }
        let len = req_num - (arg_num + ex_num);
        fill(callee_lfp, req_num, len, Some(Value::nil()));
        let len = max_pos - req_num;
        fill(callee_lfp, max_pos, len, None);
    }
    if let Some(ex) = ex {
        unsafe { callee_lfp.set_register(1 + arg_num, Some(ex)) };
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

    let callee_kw_pos = info.pos_num() + 1;
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

    let callee_kw_pos = callee.pos_num() + 1;

    for h in hash_splat_pos
        .iter()
        .map(|pos| unsafe { caller_lfp.register(pos.0 as usize).unwrap() })
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
            .map(|pos| unsafe { caller_lfp.register(pos.0 as usize).unwrap() })
        {
            let mut h = h.as_hash().clone();
            for name in callee.kw_names().iter() {
                let sym = Value::symbol(*name);
                h.remove(sym);
            }
            for (k, v) in h {
                kw_rest.insert(HashKey(k), v);
            }
        }

        unsafe { callee_lfp.set_register(rest.0 as usize, Some(Value::hash(kw_rest))) }
    }
    Ok(())
}

// Utility functions.

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

fn fill(lfp: Lfp, start_pos: usize, len: usize, val: Option<Value>) {
    unsafe {
        let ptr = lfp.register_ptr(start_pos);
        std::slice::from_raw_parts_mut(ptr, len).fill(val);
    }
}
