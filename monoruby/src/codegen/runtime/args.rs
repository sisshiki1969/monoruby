use super::*;

///
/// Handle hash splat arguments and a keyword rest parameter.
///
pub(crate) fn jit_hash_splat_kw_rest(
    vm: &mut Executor,
    globals: &mut Globals,
    callid: CallSiteId,
    callee_lfp: Lfp,
    caller_lfp: Lfp,
    meta: Meta,
) -> Result<()> {
    let callee_func_id = meta.func_id();
    hash_splat_and_kw_rest(vm, globals, callee_func_id, callid, callee_lfp, caller_lfp)
}

///
/// Set positional arguments (req, opt, rest) and keyword arguments (kw, kw_rest) to the callee frame.
///
/// This function solves the match of arguments-parameters dynamically.
///
pub(crate) fn set_frame_arguments(
    vm: &mut Executor,
    globals: &mut Globals,
    callee_lfp: Lfp,
    caller_lfp: Lfp,
    callid: CallSiteId,
) -> Result<()> {
    let callee_fid = callee_lfp.func_id();
    set_callee_frame_arguments(vm, globals, callid, callee_fid, callee_lfp, caller_lfp)
}

pub(crate) fn set_frame_arguments_simple(
    vm: &mut Executor,
    globals: &mut Globals,
    callee_lfp: Lfp,
    caller_lfp: Lfp,
    callid: CallSiteId,
    src: *const Value,
    pos_num: usize,
) -> Result<()> {
    let callee_fid = callee_lfp.func_id();
    let callee = &globals.store[callee_fid];

    positional_simple(callee, src, pos_num, callee_lfp)?;
    if !callee.no_keyword() {
        handle_keyword(vm, globals, callee_fid, callid, callee_lfp, caller_lfp)?;
    }

    Ok(())
}

pub(crate) fn set_frame_arguments_send_splat(
    globals: &mut Globals,
    callee_lfp: Lfp,
    src: *const Value,
) -> Result<()> {
    let callee_fid = callee_lfp.func_id();
    let callee = &globals.store[callee_fid];

    positional_send_splat(callee, src, callee_lfp)?;
    if !callee.no_keyword() {
        handle_keyword_simple(callee, callee_lfp)?;
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
        match caller_lfp.register(block_arg) {
            Some(v) => Some(BlockHandler::new(v)),
            None => None,
        }
    } else {
        None
    };
    callee_lfp.set_block(bh);
}

///
/// Set self and positional arguments for the callee frame.
///
pub(crate) extern "C" fn jit_generic_set_arguments(
    vm: &mut Executor,
    globals: &mut Globals,
    callid: CallSiteId,
    callee_lfp: Lfp,
    callee_fid: FuncId,
) -> Option<Value> {
    let caller_lfp = vm.cfp().lfp();
    let src = caller_lfp.register_ptr(globals[callid].recv);
    let dst = callee_lfp.register_ptr(SlotId::self_());
    unsafe { *dst = *src };
    match set_callee_frame_arguments(vm, globals, callid, callee_fid, callee_lfp, caller_lfp) {
        Ok(_) => {}
        Err(mut err) => {
            err.push_internal_trace(callee_fid);
            vm.set_error(err);
            return None;
        }
    }

    Some(Value::nil())
}

fn check_single_arg_expand(
    splat_pos: &[usize],
    pos_args: usize,
    src: *const Value,
    ex: Option<Value>,
) -> Option<(*const Value, usize)> {
    if pos_args == 1 && ex.is_none() {
        if splat_pos.is_empty()
            && let Some(ary) = unsafe { *src }.try_array_ty()
        {
            return Some((ary.as_ref().as_ptr(), ary.len()));
        } else if splat_pos == &[0usize]
            && let Some(ary) = unsafe { *src }.try_array_ty()
            && ary.len() == 1
            && let Some(ary) = ary[0].try_array_ty()
        {
            return Some((ary.as_ref().as_ptr(), ary.len()));
        }
    } else if pos_args == 0
        && let Some(ex) = ex
        && let Some(ary) = ex.try_array_ty()
    {
        return Some((ary.as_ref().as_ptr(), ary.len()));
    }
    None
}

///
/// Set arguments for the callee frame.
///
fn set_callee_frame_arguments(
    vm: &mut Executor,
    globals: &mut Globals,
    callid: CallSiteId,
    callee_fid: FuncId,
    callee_lfp: Lfp,
    caller_lfp: Lfp,
) -> Result<()> {
    let src = caller_lfp.register_ptr(globals[callid].args) as *mut Value;
    let dst = callee_lfp.register_ptr(SlotId(1));
    let pos_args = globals[callid].pos_num;

    let ex = if globals[callee_fid].no_keyword() && globals[callid].kw_may_exists() {
        // handle excessive keyword arguments
        let mut h = RubyMap::default();
        for (k, id) in globals[callid].kw_args.clone().iter() {
            let v = caller_lfp.register(globals[callid].kw_pos + *id).unwrap();
            h.insert(Value::symbol(*k), v, vm, globals)?;
        }
        for v in globals[callid]
            .hash_splat_pos
            .clone()
            .into_iter()
            .map(|pos| caller_lfp.register(pos).unwrap())
        {
            if v.is_nil() {
                continue;
            }
            for (k, v) in v.expect_hash_ty(globals)?.iter() {
                h.insert(k, v, vm, globals)?;
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

    let splat_pos = &globals[callid].splat_pos;
    if globals[callee_fid].single_arg_expand()
        && let Some((ptr, len)) = check_single_arg_expand(splat_pos, pos_args, src, ex)
    {
        // single array argument expansion for blocks
        fill_positional_args(dst, &globals[callee_fid], ptr, len, true)?;
    } else if splat_pos.is_empty() && ex.is_none() {
        fill_positional_args2(dst, &globals[callee_fid], src, pos_args)?;
    } else if pos_args == 1
        && ex.is_none()
        && splat_pos == &[0]
        && let Some(ary) = unsafe { *src }.try_array_ty()
    {
        fill_positional_args1(dst, &globals[callee_fid], ary.as_ref())?;
    } else {
        let mut buf = vec![];
        for i in 0..pos_args {
            let v = unsafe { *src.sub(i) };
            if splat_pos.contains(&i) {
                let ary = v
                    .try_array_ty()
                    .expect("internal error: splat arguments must be an array");
                buf.extend_from_slice(&ary);
            } else {
                buf.push(v);
            }
        }
        if let Some(v) = ex {
            buf.push(v);
        }

        fill_positional_args1(dst, &globals[callee_fid], &buf)?;
    }

    // fill keyword arguments
    let callee = &globals.store[callee_fid];
    let caller = &globals.store[callid];
    if !callee.no_keyword() || !caller.kw_may_exists() {
        handle_keyword(vm, globals, callee_fid, callid, callee_lfp, caller_lfp)?;
    }
    Ok(())
}

///
/// Set positional arguments.
///
pub(crate) fn positional_invoker(
    callee: &FuncInfo,
    callee_lfp: Lfp,
    args: *const Value,
    pos_args: usize,
    upward: bool,
    ex: Option<Value>,
) -> Result<()> {
    let dst = callee_lfp.register_ptr(SlotId(1));

    // single array argument expansion for blocks
    if callee.single_arg_expand()
        && let Some((ptr, len)) = check_single_arg_expand(&vec![], pos_args, args, ex)
    {
        return fill_positional_args(dst, callee, ptr, len, true);
    }
    if let Some(ex) = ex {
        let mut buf: Vec<Value> = unsafe {
            if upward {
                std::slice::from_raw_parts(args, pos_args).to_vec()
            } else {
                let slice = std::slice::from_raw_parts_mut(
                    (args as *mut Value).sub(pos_args).add(1),
                    pos_args,
                );
                slice.reverse();
                slice.to_vec()
            }
        };
        buf.push(ex);
        fill_positional_args1(dst, callee, &buf)
    } else {
        fill_positional_args(dst, callee, args, pos_args, upward)
    }
}

fn fill_positional_args1(dst: *mut Option<Value>, callee: &FuncInfo, buf: &[Value]) -> Result<()> {
    fill_positional_args(dst, callee, buf.as_ptr(), buf.len(), true)
}

fn fill_positional_args2(
    dst: *mut Option<Value>,
    callee: &FuncInfo,
    ptr: *const Value,
    len: usize,
) -> Result<()> {
    fill_positional_args(dst, callee, ptr, len, false)
}

fn fill_positional_args(
    dst: *mut Option<Value>,
    callee: &FuncInfo,
    buf_ptr: *const Value,
    buf_len: usize,
    upward: bool,
) -> Result<()> {
    fn fill(dst: *mut Option<Value>, start: usize, end: usize, val: Option<Value>) {
        unsafe { std::slice::from_raw_parts_mut(dst.sub(end).add(1), end - start).fill(val) }
    }

    fn memcpy<'a>(
        dst: *mut Option<Value>,
        offset: usize,
        ptr: *const Value,
        range: std::ops::Range<usize>,
        upward: bool,
    ) {
        let len = range.len();
        for i in 0..len {
            unsafe {
                *dst.sub(offset + i) = Some(if upward {
                    *ptr.add(range.start + i)
                } else {
                    *ptr.sub(range.start + i)
                })
            };
        }
    }
    let min_args = callee.min_positional_args();
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
    let (slice0, slice1, rest) = if buf_len <= callee.req_num() {
        fill(dst, buf_len, opt_pos, Some(Value::nil()));
        fill(dst, opt_pos, rest_pos, None);
        fill(dst, post_pos, end_pos, Some(Value::nil()));
        (
            (0, 0..buf_len),
            (buf_len, buf_len..buf_len),
            buf_len..buf_len,
        )
    } else if buf_len <= callee.min_positional_args() {
        fill(dst, opt_pos, rest_pos, None);
        let args_num = buf_len - opt_pos;
        fill(dst, post_pos + args_num, end_pos, Some(Value::nil()));
        (
            (0, 0..opt_pos),
            (post_pos, opt_pos..buf_len),
            buf_len..buf_len,
        )
    } else if buf_len <= callee.max_positional_args() {
        let args_num = buf_len - callee.req_num() - callee.post_num();
        fill(dst, opt_pos + args_num, rest_pos, None);
        (
            (0, 0..opt_pos + args_num),
            (post_pos, opt_pos + args_num..buf_len),
            buf_len..buf_len,
        )
    } else {
        (
            (0, 0..rest_pos),
            (post_pos, buf_len + post_pos - end_pos..buf_len),
            rest_pos..buf_len + post_pos - end_pos,
        )
    };

    memcpy(dst, slice0.0, buf_ptr, slice0.1, upward);
    memcpy(dst, slice1.0, buf_ptr, slice1.1, upward);
    if let Some(rest_pos) = callee.rest_pos() {
        let ary = unsafe {
            if upward {
                Value::array_from_iter(
                    std::slice::from_raw_parts(buf_ptr.add(rest.start), rest.len())
                        .iter()
                        .cloned(),
                )
            } else {
                Value::array_from_iter(
                    std::slice::from_raw_parts(buf_ptr.sub(rest.end).add(1), rest.len())
                        .iter()
                        .rev()
                        .cloned(),
                )
            }
        };
        unsafe { *dst.sub(rest_pos as usize) = Some(ary) };
    }

    Ok(())
}

fn positional_simple(
    callee: &FuncInfo,
    src: *const Value,
    pos_num: usize,
    callee_lfp: Lfp,
) -> Result<()> {
    let dst = callee_lfp.register_ptr(SlotId(1));
    let pos_args = pos_num;

    // single array argument expansion for blocks
    if callee.single_arg_expand()
        && let Some((ptr, len)) = check_single_arg_expand(&[], pos_args, src, None)
    {
        return fill_positional_args(dst, callee, ptr, len, true);
    }

    fill_positional_args2(dst, callee, src, pos_args)
}

fn positional_send_splat(callee: &FuncInfo, src: *const Value, callee_lfp: Lfp) -> Result<()> {
    let dst = callee_lfp.register_ptr(SlotId(1));
    let ary = unsafe { *src }.try_array_ty().unwrap();
    fill_positional_args1(dst, callee, ary[1..].as_ref())
}

///
/// Handle keyword arguments.
///
fn handle_keyword(
    vm: &mut Executor,
    globals: &mut Globals,
    callee: FuncId,
    caller: CallSiteId,
    callee_lfp: Lfp,
    caller_lfp: Lfp,
) -> Result<()> {
    ordinary_keyword(globals, callee, caller, callee_lfp, caller_lfp)?;
    hash_splat_and_kw_rest(vm, globals, callee, caller, callee_lfp, caller_lfp)
}

fn handle_keyword_simple(callee: &FuncInfo, mut callee_lfp: Lfp) -> Result<()> {
    let callee_kw_pos = callee.kw_reg_pos(); // .pos_num() + 1;
    for (id, _) in callee.kw_names().iter().enumerate() {
        unsafe {
            callee_lfp.set_register(callee_kw_pos + id, None);
        }
    }

    if let Some(rest) = callee.kw_rest() {
        unsafe { callee_lfp.set_register(rest, Some(Value::nil())) }
    }
    Ok(())
}

fn ordinary_keyword(
    globals: &Globals,
    info: FuncId,
    callsite: CallSiteId,
    mut callee_lfp: Lfp,
    caller_lfp: Lfp,
) -> Result<()> {
    let CallSiteInfo {
        kw_pos, kw_args, ..
    } = &globals[callsite];

    let callee_kw_pos = globals[info].kw_reg_pos();
    let mut used = 0;
    for (id, param_name) in globals[info].kw_names().iter().enumerate() {
        unsafe {
            let v = kw_args
                .get(param_name)
                .map(|i| caller_lfp.register(*kw_pos + *i).unwrap());
            if v.is_some() {
                used += 1;
            }
            callee_lfp.set_register(callee_kw_pos + id, v);
        }
    }
    if used < kw_args.len() && globals[info].kw_rest().is_none() {
        for (k, _) in kw_args.iter() {
            if !globals[info].kw_names().contains(k) {
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
    vm: &mut Executor,
    globals: &mut Globals,
    callee: FuncId,
    caller: CallSiteId,
    mut callee_lfp: Lfp,
    caller_lfp: Lfp,
) -> Result<()> {
    if globals[callee].no_keyword() {
        return Ok(());
    }

    let CallSiteInfo {
        kw_pos,
        hash_splat_pos,
        ..
    } = globals[caller].clone();

    let callee_kw_pos = globals[callee].kw_reg_pos();
    let kw_names = globals[callee].kw_names().to_vec();

    for h in hash_splat_pos
        .iter()
        .map(|pos| caller_lfp.register(*pos).unwrap())
    {
        if h.is_nil() {
            continue;
        }
        let h = h.expect_hash_ty(globals)?;
        let mut unused = h.len();
        for (id, param_name) in kw_names.iter().enumerate() {
            unsafe {
                let sym = Value::symbol(*param_name);
                if let Some(v) = h.get(sym, vm, globals)? {
                    unused -= 1;
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
        if unused > 0 && globals[callee].kw_rest().is_none() {
            for (k, _) in h.iter() {
                let sym = k.as_symbol();
                if !globals[callee].kw_names().contains(&sym) {
                    return Err(MonorubyErr::argumenterr(format!("unknown keyword: :{sym}")));
                }
            }
        }
    }

    if let Some(rest) = globals[callee].kw_rest() {
        if !globals[caller].kw_may_exists() {
            // no keyword arguments
            unsafe { callee_lfp.set_register(rest, Some(Value::nil())) }
        } else {
            let mut kw_rest = RubyMap::default();
            for (name, i) in globals[caller].kw_args.clone().into_iter() {
                if kw_names.contains(&name) {
                    continue;
                }
                let v = caller_lfp.register(kw_pos + i).unwrap();
                kw_rest.insert_sym(RubySymbol::new(name), v);
            }

            for h in hash_splat_pos
                .iter()
                .map(|pos| caller_lfp.register(*pos).unwrap())
            {
                let mut h = h.as_hashmap_inner().clone();
                for name in kw_names.iter() {
                    let sym = Value::symbol(*name);
                    h.remove(sym, vm, globals)?;
                }
                for (k, v) in h.iter() {
                    kw_rest.insert(k, v, vm, globals)?;
                }
            }

            unsafe { callee_lfp.set_register(rest, Some(Value::hash(kw_rest))) }
        }
    }
    Ok(())
}
