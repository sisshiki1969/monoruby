use super::*;

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
    // Lazy `(...)` trampoline entry: a flat call site into a lazy
    // forwarding trampoline (`Store::lazy_forwarding_rest`) skips
    // materializing the rest Array — the callee's rest slot receives a
    // Fixnum(callid) marker instead, resolved at its forwarding call by
    // reading this frame's argument slots directly (they stay live and
    // GC-rooted here for the whole call). The `**kwrest` gets nil, the
    // same value the eager path stores for a keyword-less call site.
    if let Some(rest_slot) = globals.store.lazy_forwarding_rest(callee_fid) {
        let cs = &globals[callid];
        if cs.splat_pos.is_empty() && cs.kw_args.is_empty() && cs.hash_splat_pos.is_empty() {
            unsafe {
                *callee_lfp.register_ptr(rest_slot) = Some(Value::integer(callid.get() as i64));
                if let Some(kwr) = globals[callee_fid].kw_rest() {
                    *callee_lfp.register_ptr(kwr) = Some(Value::nil());
                }
            }
            return Ok(());
        }
    }
    // Forwarding callsites (`g(x.., ...)` — most importantly the
    // `o.initialize(...)` in a Ruby-level `Class#new`) have a statically
    // known shape; route them through the same direct positional build
    // the JIT uses instead of the generic CallSiteInfo re-interpretation
    // (splat-position scan + excessive-keyword machinery). Same gate as
    // the JIT dispatch in jitgen/compile/method_call.rs; applies to iseq
    // and native callees alike (identical callee-frame protocol, same
    // `fill_positional_args` terminal). Lazy-forwarding markers are
    // handled inside `forwarded_set_arguments_core`.
    {
        let cs = &globals[callid];
        if cs.forwarding {
            if cs.splat_pos.len() == 1
                && cs.splat_pos[0] < cs.pos_num
                && forwarded_fast_path_ok(&globals.store[callee_fid], cs)
            {
                return forwarded_set_arguments_core(
                    vm, globals, callid, callee_lfp, callee_fid, caller_lfp,
                );
            }
            // Shapes outside the direct gate (callee with *named*
            // keyword parameters, extra user splats as in `g(*a, ...)`)
            // still need any pending lazy marker materialized before the
            // generic machinery interprets the splat slots.
            materialize_lazy_at_callsite(vm, globals, callid, caller_lfp);
        }
    }
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

    positional_simple(vm, globals, callee_fid, src, pos_num, callee_lfp)?;
    let callee = &globals.store[callee_fid];
    if !callee.no_keyword() {
        coerce_hash_splat_args(vm, globals, callid, caller_lfp)?;
        handle_keyword(vm, globals, callee_fid, callid, callee_lfp, caller_lfp, None)?;
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
    let res = resolve_lazy_forwarding(vm, globals, callid, callee_lfp, callee_fid, caller_lfp, true)
        .and_then(|done| {
            if done {
                Ok(())
            } else {
                set_callee_frame_arguments(vm, globals, callid, callee_fid, callee_lfp, caller_lfp)
            }
        });
    match res {
        Ok(_) => {}
        Err(mut err) => {
            err.push_internal_trace(callee_fid);
            vm.set_error(err);
            return None;
        }
    }

    Some(Value::nil())
}

///
/// Specialized self+argument setup for a forwarding call `g(x.., ...)`
/// whose callee `g` is a `no_keyword` iseq (with opt/post/rest, i.e.
/// the cases the zero-alloc inline path does not cover).
///
/// The callsite shape is statically known (single trailing splat = the
/// `...` rest Array, `lead = pos_num-1` ordinary leading args), so the
/// common no-forwarded-keyword case skips the generic
/// `set_callee_frame_arguments` CallSiteInfo re-interpretation
/// (`splat_pos` scan + excessive-keyword `ex` machinery) and builds the
/// positional buffer directly. The uncommon case where keywords are
/// actually forwarded delegates to the proven generic path so subtle
/// kw-to-rest semantics stay byte-identical.
///
pub(crate) extern "C" fn jit_forwarded_set_arguments(
    vm: &mut Executor,
    globals: &mut Globals,
    callid: CallSiteId,
    callee_lfp: Lfp,
    callee_fid: FuncId,
) -> Option<Value> {
    let caller_lfp = vm.cfp().lfp();
    // self
    let src0 = caller_lfp.register_ptr(globals[callid].recv);
    let dst0 = callee_lfp.register_ptr(SlotId::self_());
    unsafe { *dst0 = *src0 };

    match forwarded_set_arguments_core(vm, globals, callid, callee_lfp, callee_fid, caller_lfp) {
        Ok(_) => Some(Value::nil()),
        Err(mut err) => {
            err.push_internal_trace(callee_fid);
            vm.set_error(err);
            None
        }
    }
}

///
/// Resolve a lazy `(...)`-forwarding marker at a forwarding call site.
///
/// A lazy trampoline's rest slot holds `Fixnum(orig_callid)` instead of
/// a materialized Array (see the entry store in `set_frame_arguments`).
/// Its forwarding call `Mov`s that marker into the splat argument slot,
/// so every runtime path that interprets a forwarding call site's
/// arguments must call this first.
///
/// Returns `Ok(true)` when the callee frame's positional (and, trivially,
/// keyword — the gate requires a keyword-less callee) slots were fully
/// set by routing the original caller's argument slots straight into the
/// callee frame. Returns `Ok(false)` when there was no marker — or when
/// the shape needs the generic machinery, in which case the rest Array
/// has been materialized *in place* (both the trampoline's rest slot and
/// the splat argument slot) so downstream code sees an ordinary forward.
///
/// A `Fixnum` in the splat slot is a marker i.f.f. the calling frame is
/// itself lazy-qualified: in such a frame the (unnamed) rest slot is
/// only ever written by frame entry (marker or Array). A zsuper of a
/// reassigned named rest (`def m(*r); r = 7; super; end`) also puts a
/// Fixnum in a splat slot, but its frame never lazy-qualifies, so it
/// falls through to the generic scalar-wrapping path unchanged.
///
pub(crate) fn resolve_lazy_forwarding(
    vm: &mut Executor,
    globals: &mut Globals,
    callid: CallSiteId,
    callee_lfp: Lfp,
    callee_fid: FuncId,
    caller_lfp: Lfp,
    allow_direct: bool,
) -> Result<bool> {
    let cs = &globals[callid];
    if !cs.forwarding {
        return Ok(false);
    }
    if cs.splat_pos.len() != 1 || cs.splat_pos[0] >= cs.pos_num {
        // Odd forwarding shape (e.g. extra user splats, `g(*a, ...)`):
        // materialize any pending marker and let the generic machinery
        // interpret the call site.
        materialize_lazy_at_callsite(vm, globals, callid, caller_lfp);
        return Ok(false);
    }
    let sp = cs.splat_pos[0];
    let args_ptr = caller_lfp.register_ptr(cs.args) as *const Value;
    let splat_v = unsafe { *args_ptr.sub(sp) };
    let Some((orig_ptr, orig_num)) =
        lazy_marker_source(vm, globals, splat_v, caller_lfp, sp + 1 == cs.pos_num)
    else {
        return Ok(false);
    };

    let kw_empty = cs.kw_args.is_empty()
        && cs.hash_splat_pos.iter().all(|p| {
            caller_lfp.register(*p).map_or(true, |v| {
                v.is_nil() || v.try_hash_ty().is_some_and(|h| h.len() == 0)
            })
        });
    if allow_direct
        && kw_empty
        && forwarded_fast_path_ok(&globals.store[callee_fid], &globals[callid])
        && !globals[callee_fid].single_arg_expand()
    {
        lazy_forward_fill(
            globals, callid, callee_lfp, callee_fid, caller_lfp, sp, orig_ptr, orig_num,
        )?;
        return Ok(true);
    }
    // Shape outside the fast gate (keyword-taking or block-style callee,
    // send-splat specialization, …): materialize the Array in place and
    // let the proven generic machinery run on it.
    materialize_lazy_at_callsite(vm, globals, callid, caller_lfp);
    Ok(false)
}

///
/// Fill the callee's positional slots for a lazy-forwarded call: the
/// buffer order matches the materialized forward exactly —
/// lead `[0..sp]` ++ original flat args ++ post `[sp+1..pos_num]`
/// (post is empty for a `...` forward, which must be trailing; kept
/// generic for symmetry with the materialized branch).
///
/// `#[inline(never)]`: this is the hot resolve in the *interpreter*
/// tier, but must not bloat `forwarded_set_arguments_core`'s
/// materialized-Array path, which is the JIT tier's per-call helper.
///
#[inline(never)]
#[allow(clippy::too_many_arguments)]
fn lazy_forward_fill(
    globals: &Globals,
    callid: CallSiteId,
    callee_lfp: Lfp,
    callee_fid: FuncId,
    caller_lfp: Lfp,
    sp: usize,
    orig_ptr: *const Value,
    orig_num: usize,
) -> Result<()> {
    let cs = &globals[callid];
    let pos_args = cs.pos_num;
    let args_ptr = caller_lfp.register_ptr(cs.args) as *const Value;
    let mut buf: smallvec::SmallVec<[Value; 8]> =
        smallvec::SmallVec::with_capacity(pos_args - 1 + orig_num);
    for i in 0..sp {
        buf.push(unsafe { *args_ptr.sub(i) });
    }
    for i in 0..orig_num {
        buf.push(unsafe { *orig_ptr.sub(i) });
    }
    for i in (sp + 1)..pos_args {
        buf.push(unsafe { *args_ptr.sub(i) });
    }
    let dst = callee_lfp.register_ptr(SlotId(1));
    fill_positional_args1(dst, &globals.store[callee_fid], &buf)?;
    store_empty_kw_rest(globals, callee_lfp, callee_fid);
    Ok(())
}

///
/// Whether a forwarding call site may take the direct positional build
/// instead of the generic `set_callee_frame_arguments` re-parse.
///
/// A keyword-less callee always may. A callee whose only keyword surface
/// is a bare `**kwrest` may too — `store_empty_kw_rest` initializes that
/// slot — but only when the call site itself carries keyword syntax.
/// That last condition is what preserves ruby2_keywords: promotion of a
/// flagged trailing Hash into the callee's keywords is gated on the call
/// site passing no keywords of its own (`r2k_promote`), and the direct
/// build does not implement it. In practice the shapes this excludes are
/// `ruby2_keywords def m(*args); super; end` and delegating blocks,
/// while the `...` forward — which always carries the `**kwrest`
/// hash-splat — stays on the fast path.
///
/// Mirrors the JIT-side gate in `jitgen/compile/method_call.rs`.
///
#[inline]
fn forwarded_fast_path_ok(callee: &FuncInfo, cs: &CallSiteInfo) -> bool {
    callee.no_keyword() || (callee.kw_names().is_empty() && cs.kw_may_exists())
}

///
/// Initialize the callee's `**kwrest` slot for a forwarded call that
/// carries no keywords.
///
/// The positional fast paths above write only positional slots, so a
/// callee that declares a bare `**kwrest` (no named keywords — the gate
/// rejects those) would otherwise be left with an uninitialized slot.
/// `nil` is what the generic path's `handle_keyword_simple` stores for a
/// keyword-less call, and it means "no keyword arguments" everywhere
/// downstream: an iseq callee's prologue turns it into `{}` via
/// `CheckKwRest`, and natives that declare a kwrest already have to
/// tolerate it (e.g. `Struct#initialize` filters its kwrest argument on
/// `try_hash_ty()` + non-empty, treating `nil` and `{}` alike).
///
#[inline]
fn store_empty_kw_rest(globals: &Globals, callee_lfp: Lfp, callee_fid: FuncId) {
    if let Some(rest) = globals[callee_fid].kw_rest() {
        unsafe { *callee_lfp.register_ptr(rest) = Some(Value::nil()) };
    }
}

///
/// If `splat_v` (the value found in a forwarding call site's splat slot)
/// is a lazy-forwarding marker of the `caller_lfp` frame, return the
/// original caller's argument window `(ptr, len)` — `ptr` is the first
/// argument slot, arguments live at descending addresses (`ptr.sub(i)`).
///
/// The original caller is the frame that pushed the trampoline: the one
/// directly below it on the control-frame chain. Its argument slots stay
/// live (and GC-scanned) for the whole duration of the trampoline call.
///
/// Returns `None` when `splat_v` is not a marker:
///
/// - not a Fixnum;
/// - a genuine user Fixnum from a zsuper of a reassigned named rest
///   (`def m(*r); r = 7; super; end`) — that frame never lazy-qualifies;
/// - a genuine user Fixnum in a *lazy* frame (`g(*7, ...)`): in a lazy
///   frame the marker only ever sits in the **trailing** splat (`...`
///   must be the last positional), so `trailing` (computed by the caller
///   as `sp + 1 == pos_num`) must hold.
///
#[inline]
fn lazy_marker_source(
    vm: &Executor,
    globals: &Globals,
    splat_v: Value,
    caller_lfp: Lfp,
    trailing: bool,
) -> Option<(*const Value, usize)> {
    if !trailing {
        return None;
    }
    let orig_cid = splat_v.try_fixnum()?;
    globals.store.lazy_forwarding_rest(caller_lfp.func_id())?;
    let mut cfp = vm.cfp();
    while cfp.lfp() != caller_lfp {
        cfp = cfp
            .prev()
            .expect("lazy forwarding: trampoline frame not on cfp chain");
    }
    let orig_lfp = cfp
        .prev()
        .expect("lazy forwarding: original caller frame missing")
        .lfp();
    let orig_cs = &globals[CallSiteId(orig_cid as u32)];
    Some((
        orig_lfp.register_ptr(orig_cs.args) as *const Value,
        orig_cs.pos_num,
    ))
}

///
/// Materialize the pending lazy-forwarding marker (if any) behind the
/// forwarding call site `callid`: build the real rest Array from the
/// original caller's argument slots and write it into both the
/// trampoline's rest slot and the call site's splat argument slot, so
/// every downstream consumer sees an ordinary materialized forward.
/// No-op when the splat slot does not hold a marker.
///
fn materialize_lazy_at_callsite(
    vm: &Executor,
    globals: &Globals,
    callid: CallSiteId,
    caller_lfp: Lfp,
) {
    let cs = &globals[callid];
    if !cs.forwarding {
        return;
    }
    let Some(rest_slot) = globals.store.lazy_forwarding_rest(caller_lfp.func_id()) else {
        return;
    };
    let args_ptr = caller_lfp.register_ptr(cs.args) as *const Value;
    // A forwarding call site can carry extra user splats besides the
    // `...` rest (`g(*a, ...)`); only the rest slot's copy holds the
    // marker, the others are ordinary values.
    for &sp in cs.splat_pos.iter() {
        if sp >= cs.pos_num {
            continue;
        }
        let splat_v = unsafe { *args_ptr.sub(sp) };
        if let Some((orig_ptr, orig_num)) =
            lazy_marker_source(vm, globals, splat_v, caller_lfp, sp + 1 == cs.pos_num)
        {
            let ary =
                Value::array_from_iter((0..orig_num).map(|i| unsafe { *orig_ptr.sub(i) }));
            unsafe {
                *caller_lfp.register_ptr(rest_slot) = Some(ary);
                *(args_ptr as *mut Option<Value>).sub(sp) = Some(ary);
            }
        }
    }
}

///
/// Materialize every pending lazy-forwarding marker on the current
/// control-frame chain.
///
/// Called before compiling string-`eval` code or capturing a `Binding`:
/// code compiled against a live frame can reach that frame's raw
/// parameter slots (e.g. a zsuper in `eval`'d source reads the mother
/// method's rest/kwrest through the outer chain), so no marker may be
/// left in any frame such code could observe. Walks from `start` (the
/// frame the code is compiled against) down the control-frame chain,
/// which covers the whole outer chain: lazy frames are always active
/// frames (a lazy trampoline can contain no block literal, so its frame
/// cannot escape into a Proc that outlives it).
///
pub(crate) fn materialize_lazy_forwarding(globals: &mut Globals, start: Cfp) {
    let mut cfp = Some(start);
    while let Some(c) = cfp {
        let lfp = c.lfp();
        if let Some(rest_slot) = globals.store.lazy_forwarding_rest(lfp.func_id())
            && let Some(v) = lfp.register(rest_slot)
            && let Some(orig_cid) = v.try_fixnum()
        {
            let orig_lfp = c
                .prev()
                .expect("lazy forwarding: original caller frame missing")
                .lfp();
            let orig_cs = &globals[CallSiteId(orig_cid as u32)];
            let orig_ptr = orig_lfp.register_ptr(orig_cs.args) as *const Value;
            let ary = Value::array_from_iter(
                (0..orig_cs.pos_num).map(|i| unsafe { *orig_ptr.sub(i) }),
            );
            unsafe { *lfp.register_ptr(rest_slot) = Some(ary) };
        }
        cfp = c.prev();
    }
}

/// Shared body of the forwarding-call argument setup (see
/// `jit_forwarded_set_arguments`; also reached from the interpreter's
/// `set_frame_arguments`). Positional-only forwards build the callee's
/// positional buffer directly from the statically-known callsite shape;
/// actually-forwarded keywords defer to the generic path.
fn forwarded_set_arguments_core(
    vm: &mut Executor,
    globals: &mut Globals,
    callid: CallSiteId,
    callee_lfp: Lfp,
    callee_fid: FuncId,
    caller_lfp: Lfp,
) -> Result<()> {
    let cs = &globals[callid];
    // A hash-splat slot forwards no keywords when it is nil (`...` kwrest
    // with nothing to forward) — and an *empty* Hash forwards none either
    // (e.g. a `**kwrest` that went through `CheckKwRest`, or `f(**{})`).
    let kw_empty = cs.kw_args.is_empty()
        && cs.hash_splat_pos.iter().all(|p| {
            caller_lfp.register(*p).map_or(true, |v| {
                v.is_nil() || v.try_hash_ty().is_some_and(|h| h.len() == 0)
            })
        });

    if kw_empty
        && forwarded_fast_path_ok(&globals.store[callee_fid], cs)
        && !globals[callee_fid].single_arg_expand()
    {
        let pos_args = cs.pos_num;
        // gate guarantees exactly one splat; `sp` is its position (the
        // `...`/`*rest` slot). It is trailing for `g(x.., ...)` but is
        // *before* post params for implicit `super` of `def m(a,*r,z)`.
        let sp = cs.splat_pos[0];
        let args_ptr = caller_lfp.register_ptr(cs.args) as *const Value;
        // args live at descending addresses: arg i at args_ptr.sub(i).
        // The splat slot is normally an Array, but a rest parameter
        // reassigned to a scalar and forwarded via zsuper reaches here
        // as a non-Array: CRuby wraps a scalar into `[scalar]` and
        // treats `nil` as empty.
        let splat_v = unsafe { *args_ptr.sub(sp) };
        let splat_ary = splat_v.try_array_ty();
        // Lazy `(...)`-forwarding marker in place of the rest Array:
        // source-route the original caller's argument slots straight
        // into the callee frame, skipping the intermediate Array
        // entirely (see `lazy_marker_source`). Out of line so the hot
        // materialized-Array path below keeps its code size (this is
        // the JIT tier's per-call helper).
        if splat_ary.is_none()
            && let Some((orig_ptr, orig_num)) =
                lazy_marker_source(vm, globals, splat_v, caller_lfp, sp + 1 == pos_args)
        {
            return lazy_forward_fill(
                globals, callid, callee_lfp, callee_fid, caller_lfp, sp, orig_ptr, orig_num,
            );
        }
        let splat_len = splat_ary.as_ref().map_or(
            if splat_v.is_nil() { 0 } else { 1 },
            |a| a.len(),
        );
        // buffer order matches the generic splat branch exactly:
        // lead [0..sp] ++ splat ++ post [sp+1..pos_args]
        let mut buf: smallvec::SmallVec<[Value; 8]> =
            smallvec::SmallVec::with_capacity(pos_args - 1 + splat_len);
        for i in 0..sp {
            buf.push(unsafe { *args_ptr.sub(i) });
        }
        match &splat_ary {
            Some(ary) => buf.extend_from_slice(ary),
            None if !splat_v.is_nil() => buf.push(splat_v),
            None => {}
        }
        for i in (sp + 1)..pos_args {
            buf.push(unsafe { *args_ptr.sub(i) });
        }
        let dst = callee_lfp.register_ptr(SlotId(1));
        fill_positional_args1(dst, &globals.store[callee_fid], &buf)?;
        store_empty_kw_rest(globals, callee_lfp, callee_fid);
        Ok(())
    } else {
        // keywords actually forwarded: defer to the proven generic path
        // (materializing any pending lazy marker first).
        materialize_lazy_at_callsite(vm, globals, callid, caller_lfp);
        set_callee_frame_arguments(vm, globals, callid, callee_fid, callee_lfp, caller_lfp)
    }
}

///
/// Block auto-splat coercion of a single non-Array argument.
///
/// When a block taking multiple parameters is passed a single argument
/// that is not already an Array, CRuby coerces it once via `#to_ary`:
/// an Array result is splatted into the parameters, `nil` or a missing
/// `#to_ary` leaves the argument a scalar, and any other result raises
/// `TypeError`. Returns `Ok(Some(array))` when coerced (the caller must
/// keep the returned `Value` alive while filling from its buffer),
/// `Ok(None)` to leave the value as a single scalar argument.
///
fn block_arg_to_ary(vm: &mut Executor, globals: &mut Globals, v: Value) -> Result<Option<Value>> {
    // Block auto-splat coerces a single non-Array argument via `#to_ary`.
    // CRuby gates that call on the *dynamic* predicate
    // `respond_to?(:to_ary, true)` — which a user may override, and which a
    // dynamically-defined `#to_ary` satisfies through `respond_to_missing?`
    // — rather than a raw method-table lookup. Mirror `expand_array`'s
    // multiple-assignment path and dispatch `#to_ary` through the normal
    // method-resolution machinery (so `method_missing` is honoured too).
    //
    // An object that does not even respond to `#respond_to?` (a bare
    // `BasicObject`) cannot be coerced: pass it through as a single scalar
    // argument rather than raising `NoMethodError`, matching CRuby.
    if globals
        .check_method(v, IdentId::get_id("respond_to?"))
        .is_none()
    {
        return Ok(None);
    }
    let responds = vm
        .invoke_method_inner(
            globals,
            IdentId::get_id("respond_to?"),
            v,
            &[Value::symbol(IdentId::TO_ARY), Value::bool(true)],
            None,
            None,
        )?
        .as_bool();
    if !responds {
        return Ok(None);
    }
    let res = vm.invoke_method_inner(globals, IdentId::TO_ARY, v, &[], None, None)?;
    if res.is_array_ty() {
        Ok(Some(res))
    } else if res.is_nil() {
        Ok(None)
    } else {
        Err(MonorubyErr::cant_convert_error_ary(globals, v, res))
    }
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
/// Coerce every `**splat` argument register at the call site to a real
/// Hash via `#to_hash` (implicit conversion): `f(**obj)` accepts any
/// #to_hash-convertible object. The caller's register is rewritten in
/// place so every downstream consumer sees a Hash.
fn coerce_hash_splat_args(
    vm: &mut Executor,
    globals: &mut Globals,
    callid: CallSiteId,
    mut caller_lfp: Lfp,
) -> Result<()> {
    for pos in globals[callid].hash_splat_pos.clone() {
        if let Some(v) = caller_lfp.register(pos)
            && !v.is_nil()
            && v.try_hash_ty().is_none()
        {
            let converted = vm.invoke_method_if_exists(
                globals,
                IdentId::get_id("to_hash"),
                v,
                &[],
                None,
                None,
            )?;
            let h = match converted {
                Some(h) if h.try_hash_ty().is_some() => h,
                Some(bad) => {
                    return Err(MonorubyErr::typeerr(format!(
                        "can't convert {} to Hash ({}#to_hash gives {})",
                        v.get_real_class_name(globals),
                        v.get_real_class_name(globals),
                        bad.get_real_class_name(globals),
                    )));
                }
                None => {
                    return Err(MonorubyErr::no_implicit_conversion(
                        globals,
                        v,
                        HASH_CLASS,
                    ));
                }
            };
            // SAFETY: `pos` is a live argument slot of the caller frame.
            unsafe { caller_lfp.set_register(pos, Some(h)) };
        }
    }
    Ok(())
}

fn set_callee_frame_arguments(
    vm: &mut Executor,
    globals: &mut Globals,
    callid: CallSiteId,
    callee_fid: FuncId,
    callee_lfp: Lfp,
    caller_lfp: Lfp,
) -> Result<()> {
    // Fast path for the overwhelmingly common call/yield shape: a plain
    // positional call with no splat, no hash-splat and no keyword arguments
    // at the call site, whose callee takes neither keyword parameters (nor
    // `**kwrest`), auto-splats a lone block argument (`single_arg_expand`),
    // nor needs `**nil` / `ruby2_keywords` handling. Every `yield x` to a
    // `{ |a| }` block and most `foo(a, b)` calls land here. Binding is then
    // just `fill_positional_args`, skipping `coerce_hash_splat_args`, the
    // excess-keyword machinery, the `ruby2_keywords` promotion and the
    // otherwise-unconditional `handle_keyword` call (which allocates an
    // `unknowns` Vec and dispatches three sub-helpers even with no keywords).
    {
        let callsite = &globals.store[callid];
        let callee = &globals.store[callee_fid];
        if !callsite.has_splat()
            && !callsite.kw_may_exists()
            && callee.no_keyword()
            && !callee.forbid_keyword()
            && !callee.single_arg_expand()
            && !callee.ruby2_keywords()
        {
            let src = caller_lfp.register_ptr(callsite.args) as *const Value;
            let dst = callee_lfp.register_ptr(SlotId(1));
            return fill_positional_args2(dst, callee, src, callsite.pos_num);
        }
    }

    coerce_hash_splat_args(vm, globals, callid, caller_lfp)?;
    let src = caller_lfp.register_ptr(globals[callid].args) as *mut Value;
    let dst = callee_lfp.register_ptr(SlotId(1));
    let pos_args = globals[callid].pos_num;

    // `**nil` forbids keywords: reject any passed keyword up front, before
    // positional binding, so the error is "no keywords accepted" rather than
    // a positional arity error — `def m(a, **nil); end; m(a: 1)` must raise
    // the former, not "wrong number of arguments" (the keyword is not
    // silently reinterpreted as the positional `a`).
    if globals[callee_fid].forbid_keyword()
        && any_keyword_passed(globals, callid, caller_lfp)?
    {
        return Err(MonorubyErr::argumenterr("no keywords accepted"));
    }

    let ex = if globals[callee_fid].no_keyword() && globals[callid].kw_may_exists() {
        // handle excessive keyword arguments
        let mut h = RubyMap::default();
        for (k, id) in globals[callid].kw_args.clone().iter() {
            let v = caller_lfp.register(globals[callid].kw_pos + *id).unwrap();
            h.insert_sym(RubySymbol::new(*k), v);
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
            // `**obj` accepts any #to_hash-convertible object (implicit
            // conversion), not just a Hash.
            for (k, v) in v.coerce_to_hash(vm, globals)?.iter() {
                h.insert(k, v, vm, globals)?;
            }
        }
        if h.is_empty() {
            None
        } else {
            let mut inner = crate::value::rvalue::HashmapInner::new(h);
            // A ruby2_keywords-marked callee packs its keywords as a
            // *flagged* trailing hash, so a later `*rest` splat can
            // restore them as keywords.
            if globals[callee_fid].ruby2_keywords() {
                inner.set_ruby2_keywords_flag();
            }
            Some(Value::hash_from_inner(inner))
        }
    } else {
        None
    };

    // Block auto-splat: a single non-Array argument to a multi-param block
    // is coerced via `#to_ary` (kept alive here while its buffer is read).
    // Done before binding `splat_pos` so its mutable `globals` borrow ends.
    let coerced = if globals[callee_fid].single_arg_expand()
        && pos_args == 1
        && ex.is_none()
        && globals[callid].splat_pos.is_empty()
        && !unsafe { *src }.is_array_ty()
    {
        block_arg_to_ary(vm, globals, unsafe { *src })?
    } else {
        None
    };
    // ruby2_keywords handling: when the call site passes no keywords of
    // its own and a splat's expanded tail ends with a ruby2_keywords-
    // flagged hash, that hash is "keywords in flight" (CRuby):
    //   - a callee accepting keywords binds it as its keywords;
    //   - a ruby2_keywords-marked callee keeps it as the flagged
    //     trailing positional (same object — the chain continues);
    //   - any other callee receives an *unflagged copy* as the trailing
    //     positional (the chain ends).
    let r2k_promote = !globals[callid].kw_may_exists();
    let callee_takes_kw = !globals[callee_fid].no_keyword();
    let callee_r2k = globals[callee_fid].ruby2_keywords();
    let mut r2k_kw: Option<Value> = None;
    fn r2k_hash(v: Value) -> bool {
        v.try_hash_ty().is_some() && v.as_hashmap_inner().ruby2_keywords_flag()
    }
    fn r2k_unflagged_copy(v: Value) -> Value {
        let dup = v.dup();
        dup.try_hash_ty().unwrap().unset_ruby2_keywords_flag();
        dup
    }
    let splat_pos = &globals[callid].splat_pos;
    if let Some(coerced) = coerced {
        let ary = coerced.try_array_ty().unwrap();
        fill_positional_args(
            dst,
            &globals[callee_fid],
            ary.as_ref().as_ptr(),
            ary.len(),
            true,
        )?;
    } else if globals[callee_fid].single_arg_expand()
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
        let slice: &[Value] = ary.as_ref();
        if r2k_promote && let [head @ .., last] = slice && r2k_hash(*last) {
            if callee_takes_kw {
                r2k_kw = Some(*last);
                fill_positional_args1(dst, &globals[callee_fid], head)?;
            } else if callee_r2k {
                fill_positional_args1(dst, &globals[callee_fid], slice)?;
            } else {
                let mut buf: smallvec::SmallVec<[Value; 8]> =
                    smallvec::SmallVec::from_slice(head);
                buf.push(r2k_unflagged_copy(*last));
                fill_positional_args1(dst, &globals[callee_fid], &buf)?;
            }
        } else {
            fill_positional_args1(dst, &globals[callee_fid], slice)?;
        }
    } else {
        // Forwarding (`g(x, ...)` / `super(x, ...)`) and other splat
        // calls land here. The expanded positional sequence is almost
        // always short, so build it in a stack buffer and only spill
        // to the heap for genuinely large arg lists.
        let mut buf: smallvec::SmallVec<[Value; 8]> = smallvec::SmallVec::new();
        for i in 0..pos_args {
            let v = unsafe { *src.sub(i) };
            if splat_pos.contains(&i) {
                // The splatted value is normally an Array, but a rest
                // parameter reassigned to a scalar and then forwarded
                // via zsuper (`def m(*r); r = x; super; end`) reaches
                // here as a non-Array. CRuby wraps a scalar into a
                // single-element array and treats `nil` as empty.
                if let Some(ary) = v.try_array_ty() {
                    buf.extend_from_slice(&ary);
                } else if !v.is_nil() {
                    buf.push(v);
                }
            } else {
                buf.push(v);
            }
        }
        if let Some(v) = ex {
            buf.push(v);
        }
        // Only a hash that arrived via a splat expansion is "in
        // flight" (`ex` is only built when the call site passes its
        // own keywords, which `r2k_promote` already excludes).
        if r2k_promote
            && !splat_pos.is_empty()
            && let Some(&last) = buf.last()
            && r2k_hash(last)
        {
            if callee_takes_kw {
                r2k_kw = Some(last);
                buf.pop();
            } else if !callee_r2k {
                *buf.last_mut().unwrap() = r2k_unflagged_copy(last);
            }
        }

        fill_positional_args1(dst, &globals[callee_fid], &buf)?;
    }

    // fill keyword arguments
    let callee = &globals.store[callee_fid];
    let caller = &globals.store[callid];
    if !callee.no_keyword() || !caller.kw_may_exists() {
        handle_keyword(vm, globals, callee_fid, callid, callee_lfp, caller_lfp, r2k_kw)?;
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
    // For the method-style strict check, an *implicit* (trailing-
    // comma) rest doesn't accept extras — CRuby's
    // `define_method(:m) { |a,| }; m(1, 2)` raises ArgumentError.
    // Only an *explicit* `*rest`/`*` lifts the upper bound.
    if !is_block_style && (buf_len < min_args || (buf_len > max_args && !callee.is_explicit_rest()))
    {
        return Err(wrong_number_of_arg_with_kw(
            callee,
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
    } else if callee.is_rest() {
        // More args than fit the fixed params, with a `*rest` to absorb the
        // middle: pre + optionals fill from the front, post takes the last
        // `post_num`, and the rest gets everything in between.
        (
            (0, 0..rest_pos),
            (post_pos, buf_len + post_pos - end_pos..buf_len),
            rest_pos..buf_len + post_pos - end_pos,
        )
    } else {
        // More args than the params accept and no rest to absorb them —
        // only reachable for block-style (loose) binding. CRuby fills
        // positionally and drops the tail, so the post params sit right
        // after the (fully-filled) optionals rather than at the very end.
        (
            (0, 0..rest_pos),
            (post_pos, rest_pos..end_pos),
            rest_pos..rest_pos,
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

///
/// Build the `ArgumentError` for keyword arguments a method didn't declare
/// (and that no `**kwrest` can absorb). CRuby lists *every* unknown key,
/// using the singular `unknown keyword:` for one and the plural
/// `unknown keywords:` for several. Each entry is pre-formatted
/// (`:name` for a Symbol key, the key's `#inspect` otherwise).
///
fn unknown_keyword_err(unknowns: Vec<String>) -> MonorubyErr {
    let word = if unknowns.len() == 1 {
        "keyword"
    } else {
        "keywords"
    };
    MonorubyErr::argumenterr(format!("unknown {word}: {}", unknowns.join(", ")))
}

fn positional_simple(
    vm: &mut Executor,
    globals: &mut Globals,
    callee_fid: FuncId,
    src: *const Value,
    pos_num: usize,
    callee_lfp: Lfp,
) -> Result<()> {
    let dst = callee_lfp.register_ptr(SlotId(1));
    let pos_args = pos_num;

    // Block auto-splat: a single non-Array argument to a multi-param block
    // is coerced via `#to_ary` (kept alive while its buffer is read).
    if globals.store[callee_fid].single_arg_expand()
        && pos_args == 1
        && !unsafe { *src }.is_array_ty()
    {
        if let Some(coerced) = block_arg_to_ary(vm, globals, unsafe { *src })? {
            let ary = coerced.try_array_ty().unwrap();
            return fill_positional_args(
                dst,
                &globals.store[callee_fid],
                ary.as_ref().as_ptr(),
                ary.len(),
                true,
            );
        }
    }

    let callee = &globals.store[callee_fid];
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
    r2k_kw: Option<Value>,
) -> Result<()> {
    // `**nil` accepts no keywords: any actual keyword raises.
    if globals[callee].forbid_keyword() {
        if any_keyword_passed(globals, caller, caller_lfp)? {
            return Err(MonorubyErr::argumenterr("no keywords accepted"));
        }
        // `**nil` still reserves a kwrest argument slot that nothing
        // else writes (it binds no local); store a real Value or the
        // callee-entry GC poll marks stack garbage. Mirrors
        // `handle_keyword_simple` / the forwarding fast path.
        if let Some(kwr) = globals[callee].kw_rest() {
            unsafe { *callee_lfp.register_ptr(kwr) = Some(Value::nil()) };
        }
        return Ok(());
    }
    let mut unknowns = ordinary_keyword(globals, callee, caller, callee_lfp, caller_lfp)?;
    unknowns.extend(hash_splat_and_kw_rest(
        vm, globals, callee, caller, callee_lfp, caller_lfp, r2k_kw,
    )?);
    // A missing required keyword is reported before any unknown keyword,
    // matching CRuby (`m(a: 1)` for `def m(x:)` raises "missing keyword: :x",
    // not "unknown keyword: :a").
    check_missing_keyword(&globals.store[callee], callee_lfp)?;
    if !unknowns.is_empty() {
        return Err(unknown_keyword_err(unknowns));
    }
    Ok(())
}

/// Whether the call site actually supplies at least one keyword — either a
/// literal `k: v` pair or a non-empty `**hash` splat. An empty `**{}` splat
/// supplies none.
fn any_keyword_passed(
    globals: &Globals,
    caller: CallSiteId,
    caller_lfp: Lfp,
) -> Result<bool> {
    let cs = &globals[caller];
    if !cs.kw_args.is_empty() {
        return Ok(true);
    }
    for pos in cs.hash_splat_pos.iter() {
        let h = caller_lfp.register(*pos).unwrap();
        if h.is_nil() {
            continue;
        }
        match h.try_hash_ty() {
            Some(hash) if hash.len() == 0 => {}
            _ => return Ok(true),
        }
    }
    Ok(false)
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
    check_missing_keyword(callee, callee_lfp)
}

///
/// Raise ArgumentError if a required keyword parameter (one with no
/// default expression) was left unbound (None) after keyword binding.
/// Must run after *all* keyword sources (ordinary keyword arguments and
/// hash splats) have been applied. An unbound optional keyword slot is
/// left as None on purpose — the method prologue fills in the default.
///
fn check_missing_keyword(callee: &FuncInfo, callee_lfp: Lfp) -> Result<()> {
    let kw_pos = callee.kw_reg_pos();
    let mut missing = vec![];
    for (i, name) in callee.kw_names().iter().enumerate() {
        if callee.kw_is_required(i) && callee_lfp.register(kw_pos + i).is_none() {
            missing.push(*name);
        }
    }
    missing_keyword_err(&missing)
}

///
/// Positional-arity ArgumentError. When the callee also has required
/// keyword parameters, CRuby appends them to the message:
/// `wrong number of arguments (given 0, expected 1; required keyword: x)`.
///
fn wrong_number_of_arg_with_kw(
    callee: &FuncInfo,
    given: usize,
    range: std::ops::RangeInclusive<usize>,
) -> MonorubyErr {
    let required: Vec<_> = callee
        .kw_names()
        .iter()
        .enumerate()
        .filter(|(i, _)| callee.kw_is_required(*i))
        .map(|(_, name)| name.to_string())
        .collect();
    // An explicit `*rest` lifts the upper bound: CRuby reports the required
    // count with a `+` suffix (`expected 2+`) rather than a bounded range.
    // This branch is only reached for the too-few-args case (a too-many
    // error is not raised when an explicit rest is present).
    let has_rest = callee.is_explicit_rest();
    if required.is_empty() {
        return if has_rest {
            MonorubyErr::wrong_number_of_arg_min(given, *range.start())
        } else {
            MonorubyErr::wrong_number_of_arg_range(given, range)
        };
    }
    let expected = if has_rest {
        format!("{}+", range.start())
    } else if range.start() == range.end() {
        format!("{}", range.start())
    } else {
        format!("{}..{}", range.start(), range.end())
    };
    let suffix = if required.len() == 1 {
        format!("; required keyword: {}", required[0])
    } else {
        format!("; required keywords: {}", required.join(", "))
    };
    MonorubyErr::argumenterr(format!(
        "wrong number of arguments (given {given}, expected {expected}{suffix})"
    ))
}

///
/// Build CRuby-compatible `missing keyword(s)` ArgumentError (no-op for
/// an empty list): `missing keyword: :x` / `missing keywords: :x, :y`.
///
fn missing_keyword_err(missing: &[IdentId]) -> Result<()> {
    match missing {
        [] => Ok(()),
        [name] => Err(MonorubyErr::argumenterr(format!(
            "missing keyword: :{name}"
        ))),
        _ => {
            let names = missing
                .iter()
                .map(|name| format!(":{name}"))
                .collect::<Vec<_>>()
                .join(", ");
            Err(MonorubyErr::argumenterr(format!(
                "missing keywords: {names}"
            )))
        }
    }
}

/// Assigns literal `k: v` keyword arguments to the callee's keyword
/// parameters and returns any keys the callee doesn't declare (empty when
/// it has a `**kwrest` to absorb them). The caller reports these *after*
/// the missing-keyword check so a missing required keyword wins, as in
/// CRuby.
fn ordinary_keyword(
    globals: &Globals,
    info: FuncId,
    callsite: CallSiteId,
    mut callee_lfp: Lfp,
    caller_lfp: Lfp,
) -> Result<Vec<String>> {
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
        return Ok(kw_args
            .iter()
            .filter(|(k, _)| !globals[info].kw_names().contains(k))
            .map(|(k, _)| format!(":{k}"))
            .collect());
    }
    Ok(vec![])
}

///
/// Handle hash splat arguments and a keyword rest parameter. Returns any
/// `**hash` keys the callee doesn't declare (empty when it has a
/// `**kwrest`); reported by the caller after the missing-keyword check.
///
/// `r2k_kw` is an additional keyword-hash source: a ruby2_keywords-
/// flagged hash promoted from the tail of a `*args` splat. It behaves
/// exactly like one more `**hash` at the call site.
fn hash_splat_and_kw_rest(
    vm: &mut Executor,
    globals: &mut Globals,
    callee: FuncId,
    caller: CallSiteId,
    mut callee_lfp: Lfp,
    caller_lfp: Lfp,
    r2k_kw: Option<Value>,
) -> Result<Vec<String>> {
    if globals[callee].no_keyword() {
        return Ok(vec![]);
    }

    let CallSiteInfo {
        kw_pos,
        hash_splat_pos,
        ..
    } = globals[caller].clone();

    let callee_kw_pos = globals[callee].kw_reg_pos();
    let kw_names = globals[callee].kw_names().to_vec();
    let mut unknowns = Vec::new();

    for h in hash_splat_pos
        .iter()
        .map(|pos| caller_lfp.register(*pos).unwrap())
        .chain(r2k_kw)
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
            // A non-Symbol key (e.g. a String) can never name a keyword
            // parameter, so it is always "unknown" here. CRuby reports it via
            // the key's `inspect` (`unknown keyword: "b"`); a Symbol key uses
            // `:name`. Collected and reported together, after missing keys.
            for (k, _) in h.iter() {
                match k.try_symbol() {
                    Some(sym) if globals[callee].kw_names().contains(&sym) => {}
                    Some(sym) => unknowns.push(format!(":{sym}")),
                    None => unknowns.push(k.inspect(&globals.store)),
                }
            }
        }
    }

    if let Some(rest) = globals[callee].kw_rest() {
        if !globals[caller].kw_may_exists() && r2k_kw.is_none() {
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
                .chain(r2k_kw)
            {
                // A nil hash-splat is `**nil` — no keyword arguments.
                // (The other hash-splat readers already skip nil; this
                // kw-rest-building loop must too, so a deferred/elided
                // forwarding `**kwrest` left as nil is universally safe.)
                if h.is_nil() {
                    continue;
                }
                let mut h = h.as_hashmap_inner().clone_inner();
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
    Ok(unknowns)
}

///
/// Argument setup for the method/block invokers (`invoke_func` path).
///
/// In an invoker call there is no `CallSiteInfo`: arguments arrive as a
/// flat `*const Value` slice with no splat/hash-splat. `upward` selects
/// the slice direction (`true` for the forward `&[Value]` method path,
/// `false` for the reversed block-arg layout). Shared by the x86 and
/// aarch64 invokers.
pub(crate) extern "C" fn handle_invoker_arguments(
    vm: &mut Executor,
    globals: &mut Globals,
    callee_lfp: Lfp,
    arg_num: usize,
    args: *const Value,
    kw_arg: Option<Hashmap>,
) -> Option<Value> {
    match invoker_arguments_inner(vm, globals, callee_lfp, arg_num, args, true, kw_arg) {
        Ok(val) => Some(val),
        Err(err) => {
            vm.set_error(err);
            None
        }
    }
}

pub(crate) extern "C" fn handle_invoker_arguments2(
    vm: &mut Executor,
    globals: &mut Globals,
    callee_lfp: Lfp,
    arg_num: usize,
    args: *const Value,
    kw_arg: Option<Hashmap>,
) -> Option<Value> {
    match invoker_arguments_inner(vm, globals, callee_lfp, arg_num, args, false, kw_arg) {
        Ok(val) => Some(val),
        Err(err) => {
            vm.set_error(err);
            None
        }
    }
}

fn invoker_arguments_inner(
    vm: &mut Executor,
    globals: &mut Globals,
    mut callee_lfp: Lfp,
    arg_num: usize,
    args: *const Value,
    upward: bool,
    mut kw_arg: Option<Hashmap>,
) -> Result<Value> {
    let callee_fid = callee_lfp.func_id();
    let info = &globals.store[callee_fid];

    // keyword
    let callee_kw_pos = info.kw_reg_pos();
    for (id, name) in info.kw_names().to_vec().into_iter().enumerate() {
        let v = match &mut kw_arg {
            Some(map) => map.remove(Value::symbol(name), vm, globals)?,
            None => None,
        };
        unsafe {
            callee_lfp.set_register(callee_kw_pos + id, v);
        }
    }

    let info = &globals.store[callee_fid];
    let kw_arg = if let Some(kw_arg) = kw_arg
        && !kw_arg.is_empty()
    {
        Some(kw_arg)
    } else {
        None
    };

    // `**nil` accepts no keywords: any remaining keyword raises.
    if info.forbid_keyword() && kw_arg.is_some() {
        return Err(MonorubyErr::argumenterr("no keywords accepted"));
    }

    // keyword rest
    let ex = if let Some(kw_rest) = info.kw_rest() {
        let v = if let Some(kw_arg) = kw_arg {
            kw_arg.into()
        } else {
            Value::nil()
        };
        unsafe {
            callee_lfp.set_register(kw_rest, Some(v));
        }
        None
    } else if info.kw_names().is_empty()
        && let Some(kw_arg) = kw_arg
    {
        // Folding the keywords into a trailing positional hash: a
        // ruby2_keywords-marked callee flags it so a later `*rest`
        // splat can restore them as keywords.
        let v: Value = kw_arg.into();
        if info.ruby2_keywords() {
            v.try_hash_ty().unwrap().set_ruby2_keywords_flag();
        }
        Some(v)
    } else if let Some(kw_arg) = kw_arg {
        let mut s = "unknown keywords: ".to_string();
        for (i, (name, _)) in kw_arg.iter().enumerate() {
            if i == 0 {
                s.push_str(&format!(":{name}"));
            } else {
                s.push_str(&format!(", :{name}"));
            }
        }
        return Err(MonorubyErr::argumenterr(s));
    } else {
        None
    };

    // required + optional + post + rest
    positional_invoker(info, callee_lfp, args, arg_num, upward, ex)?;

    // After the positional check, so a call that is wrong in both ways
    // reports the positional arity error first, like CRuby.
    check_missing_keyword(info, callee_lfp)?;

    Ok(Value::nil())
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    /// The inlined block-style single-Array auto-splat (`YieldArrayExpand`):
    /// a hot specialized `yield` of one value into a plain multi-parameter
    /// block fills the parameters from the Array in machine code. Every
    /// shape the fill has to get right is driven through one hot call site
    /// per case and pinned to CRuby: exact length, short (nil-fill), empty
    /// (whose heap data pointer may be dangling — the bounds check must keep
    /// it unread), long (extras dropped), inline vs heap storage, and a
    /// three-parameter block.
    #[test]
    fn yield_array_expand_jit() {
        run_test(
            r#"
            def drive(a)
              acc = []
              a.each_with_yield { |k, v| acc << [k, v] }
              acc
            end
            class Array
              def each_with_yield
                i = 0
                n = size
                while i < n
                  yield self[i]
                  i += 1
                end
                self
              end
            end
            exact = [[1, 2], [:a, :b]]
            short = [[1], []]
            long  = [[1, 2, 3, 4]]
            heap  = [(1..10).to_a]          # > inline capacity
            out = []
            n = 0
            while n < 30
              out = [drive(exact), drive(short), drive(long), drive(heap)]
              n += 1
            end
            out
            "#,
        );
        // Three parameters, same machinery.
        run_test(
            r#"
            def drive3(a)
              acc = []
              a.each_with_yield3 { |x, y, z| acc << [x, y, z] }
              acc
            end
            class Array
              def each_with_yield3
                i = 0
                n = size
                while i < n
                  yield self[i]
                  i += 1
                end
                self
              end
            end
            r = nil
            n = 0
            while n < 30
              r = drive3([[1, 2, 3], [1, 2], [1], [], [1, 2, 3, 4, 5]])
              n += 1
            end
            r
            "#,
        );
    }

    /// The values the inline fast path must NOT swallow: a non-Array scalar
    /// (first param takes it, rest nil), a `#to_ary` respondent (whose
    /// coercion runs Ruby code), and an Array subclass (splatted directly,
    /// `#to_ary` NOT consulted). One call site sees all of them interleaved
    /// with plain Arrays, so the compiled fast path and its slow branch are
    /// both exercised in a single unit.
    #[test]
    fn yield_array_expand_jit_slow_paths() {
        run_test(
            r#"
            def drive(a)
              acc = []
              a.each_with_yield { |k, v| acc << [k, v] }
              acc
            end
            class Array
              def each_with_yield
                i = 0
                n = size
                while i < n
                  yield self[i]
                  i += 1
                end
                self
              end
            end
            class Pairish
              def initialize(k, v); @k = k; @v = v; end
              def to_ary; [@k, @v]; end
            end
            class MyArray < Array; end

            sub = MyArray.new
            sub << :s1 << :s2 << :s3
            mixed = [[1, 2], :scalar, Pairish.new(:pk, :pv), sub, nil, 4.5]
            out = nil
            n = 0
            while n < 30
              out = drive(mixed)
              n += 1
            end
            out
            "#,
        );
    }

    #[test]
    fn lazy_forwarding() {
        // The lazy `(...)`-forwarding convention: a flat call into a pure
        // forwarding trampoline defers the rest-Array materialization
        // (Fixnum(callid) marker in the rest slot), resolved at the
        // forwarding call from the original caller's argument slots.
        run_test(
            r#"
            def t(a, b = :d, *r, k: 1, &blk); [a, b, r, k, blk ? blk.call : nil]; end
            def f(...) = t(...)
            def lead(x, ...) = t(x, ...)
            def g2(...) = t(...)
            def nested(...) = g2(...)
            res = []
            res << f(1)
            res << f(1, 2, 3, 4)
            res << f(1, 2, k: 9)
            res << (f(1) { :blk })
            res << lead(10, 20, 30)
            res << nested(5, 6, 7, k: 2)
            res
            "#,
        );
    }

    #[test]
    fn lazy_forwarding_escape() {
        // Escapes from the lazy convention must observe a materialized
        // rest: a zsuper of a reassigned named rest (a genuine user
        // Fixnum in a splat slot), zsuper reached through string-eval
        // and through a captured Binding, and `send` forwarding.
        run_test(
            r#"
            class A
              def m(*a, **k); [:A, a, k]; end
            end
            class B < A
              def m(*r); r = 7; super; end
            end
            class C < A
              def m(...); eval("super"); end
            end
            class D < A
              def m(...); b = binding; b.eval("super"); end
            end
            def t(*a, **k) = [:t, a, k]
            def fs(...) = send(:t, ...)
            res = []
            res << B.new.m(1, 2)
            res << C.new.m(1, 2, x: 9)
            res << C.new.m
            res << D.new.m(3, 4)
            res << fs(1, 2, z: 3)
            res << fs
            res
            "#,
        );
    }

    #[test]
    fn lazy_forwarding_class_new() {
        // The motivating shape: a Ruby-level `Class#new` trampoline
        // (allocate + forwarded initialize). Positional, keyword, and
        // arity-error behavior must match an eager forward.
        run_test(
            r#"
            class Foo
              attr_reader :a, :b, :k
              def initialize(a = nil, b = nil, **k); @a = a; @b = b; @k = k; end
            end
            class Class
              def rnew(...)
                o = allocate
                o.send(:initialize, ...)
                o
              end
            end
            def strict(x, y) = [x, y]
            def fwd(...) = strict(...)
            res = []
            o = Foo.rnew(1, 2)
            res << [o.a, o.b, o.k]
            o = Foo.rnew(7, x: 5)
            res << [o.a, o.b, o.k]
            res << (begin; fwd(1); rescue ArgumentError => e; e.message; end)
            res << fwd(1, 2)
            res
            "#,
        );
    }

    #[test]
    fn class_new_native_initialize() {
        // With the Ruby-level `Class#new`, an argument-less class's
        // `initialize` resolves to the native `BasicObject#initialize`
        // (arity 0) — the forward must dispatch there (D1 source-routes
        // natives too) and keep CRuby's strict arity: `Object.new(1)`
        // raises ArgumentError instead of being silently accepted (the
        // old Ruby-level `Object#initialize(...)` swallowed anything).
        run_test(
            r#"
            class NoArg; end
            res = []
            res << NoArg.new.class.to_s
            res << (begin; Object.new(1); rescue ArgumentError => e; e.message; end)
            res << (begin; NoArg.new(k: 1); rescue ArgumentError => e; e.class.to_s; end)
            res << Object.instance_method(:initialize).owner.to_s
            res
            "#,
        );
    }

    #[test]
    fn forwarded_bare_kwrest_callee() {
        // A callee whose only keyword surface is a bare `**kwrest` (no
        // declared keyword names) is admitted to the forwarding fast
        // path; the kwrest slot must still be initialized. An iseq
        // callee sees `{}` (its prologue's `CheckKwRest` turns the
        // stored nil into an empty Hash), and forwarded keywords must
        // still arrive intact.
        run_test(
            r#"
            class K; def initialize(*a, **kw); @a = a; @kw = kw; end
                     attr_reader :a, :kw; end
            class F; def initialize(x, **kw); @x = x; @kw = kw; end
                     attr_reader :x, :kw; end
            def fwd(...) = K.new(...)
            res = []
            o = K.new;            res << [o.a, o.kw]
            o = K.new(1, 2);      res << [o.a, o.kw]
            o = K.new(1, x: 3);   res << [o.a, o.kw]
            o = fwd(5, y: 6);     res << [o.a, o.kw]
            o = fwd;              res << [o.a, o.kw]
            o = F.new(7);         res << [o.x, o.kw]
            o = F.new(7, z: 8);   res << [o.x, o.kw]
            res
            "#,
        );
    }

    #[test]
    fn forwarded_struct_construction() {
        // `Struct#initialize` is a native registered with rest + kwrest
        // (for `keyword_init:`), which used to disqualify it from every
        // forwarding fast path. It now takes the specialized helper, so
        // exercise the shapes that path has to get right.
        run_test(
            r#"
            S = Struct.new(:a, :b)
            K = Struct.new(:x, keyword_init: true)
            U = Struct.new(:v) do
              def initialize(v); super(v * 2); end
            end
            res = []
            res << S.new(1, 2).to_a
            res << S.new(1).to_a
            res << S[3, 4].to_a
            res << K.new(x: 9).x
            res << K[x: 8].x
            res << U.new(21).v
            res << (begin; S.new(1, 2, 3); rescue ArgumentError => e; e.message; end)
            res << S.new(1, 2).frozen?
            res
            "#,
        );
    }

    #[test]
    fn no_keywords_parameter() {
        // `**nil` accepts no keywords: passing any keyword raises
        // ArgumentError("no keywords accepted"), while no keywords / an
        // empty `**{}` splat is fine. Applies to lambdas and methods.
        run_test(
            r#"
            def rescue_msg
              yield; :no_error
            rescue ArgumentError => e
              e.message
            end
            l1 = ->(**nil) { :ok }
            l2 = lambda { |a, **nil| a }
            def m(**nil); :mok; end
            full = { a: 1 }
            empty = {}
            [
              rescue_msg { l1.call(a: 1) },
              l1.call,
              l1.call(**{}),
              l1.call(**empty),
              rescue_msg { l1.call(**full) },   # non-empty hash splat
              l2.call(3),
              rescue_msg { l2.call(3, b: 4) },
              m,                                 # method dispatch, no kw
              rescue_msg { m(x: 1) },            # method dispatch, literal kw
              rescue_msg { m(**full) },          # method dispatch, hash splat
              m(**empty),                        # method dispatch, empty splat
            ]
            "#,
        );
    }

    #[test]
    fn no_keywords_parameter_with_positional() {
        // With a required positional param, `**nil` still rejects keywords
        // with "no keywords accepted" — the keyword must NOT be reinterpreted
        // as the positional argument (which would raise a bogus "wrong
        // number of arguments"). A hash passed positionally still binds.
        run_test(
            r#"
            def rescue_msg
              yield; :no_error
            rescue ArgumentError => e
              e.message
            end
            def m(a, **nil); a; end
            [
              m({a: 1}),                       # positional hash binds to `a`
              m({"a" => 1}),                   # string-key positional hash
              rescue_msg { m(a: 1) },          # literal keyword -> rejected
              rescue_msg { m(**{a: 1}) },      # hash splat -> rejected
              rescue_msg { m("a" => 1) },      # non-symbol keyword -> rejected
              m(7),                            # plain positional
            ]
            "#,
        );
    }

    #[test]
    fn no_keywords_parameter_local_variables_and_parameters() {
        // `**nil` binds no local (it must not appear in `local_variables`)
        // but `#parameters` still reports it as `[:nokey]`.
        run_test(
            r#"
            def m(a, **nil); local_variables; end
            m(1)
            "#,
        );
        run_test(
            r#"
            def m(a, **nil); end
            method(:m).parameters
            "#,
        );
    }

    #[test]
    fn zsuper_reassigned_scalar_rest() {
        // `def m(*r); r = scalar; super; end` forwards the reassigned
        // rest via zsuper. CRuby wraps a scalar into `[scalar]` and
        // treats `nil` as empty; previously this aborted the process
        // (`expect("splat must be array")`).
        run_test(
            r#"
            class A; def a(*r); r; end; end
            class B < A; def a(*r); r = "foo"; super; end; end
            B.new.a("bar")
            "#,
        );
        run_test(
            r#"
            class A2; def a(*r, **k); [r, k]; end; end
            class B2 < A2; def a(*r); r = 7; super; end; end
            B2.new.a(1, 2)
            "#,
        );
        run_test(
            r#"
            class A3; def a(*r); r; end; end
            class B3 < A3; def a(*r); r = nil; super; end; end
            B3.new.a(1, 2)
            "#,
        );
    }

    #[test]
    fn missing_required_keyword() {
        // issue #707: a call without one of the required keyword
        // arguments must raise ArgumentError (`missing keyword: :x`)
        // instead of silently binding nil. Compare the exact message
        // with CRuby via the rescue idiom; `run_test`'s warm-up loop
        // also exercises the JIT path.
        let msg = "def msg; yield; \"no error\"; rescue ArgumentError => e; e.message; end;";
        run_test(&format!("{msg} def m(x:, y: 10) = [x, y]; msg {{ m(y: 1) }}"));
        run_test(&format!("{msg} def m(x:, y:) = [x, y]; msg {{ m() }}"));
        run_test(&format!("{msg} def m(x:, **r) = [x, r]; msg {{ m(a: 1) }}"));
        run_test(&format!("{msg} def m(x:) = x; msg {{ m(**{{}}) }}"));
        // A hash splat *can* supply the required keyword.
        run_test(&format!("{msg} def m(x:) = x; h = {{x: 5}}; [msg {{ m(**h) }}, m(**h)]"));
        // Positional arity errors win and mention the required keywords.
        run_test(&format!("{msg} def m(a, x:) = [a, x]; msg {{ m() }}"));
        run_test(&format!("{msg} msg {{ lambda {{ |a, x:, y:| }}.call }}"));
        // Native forwarding (Class#new), send, Method#call, procs,
        // blocks, and define_method all take the invoker/blocks paths.
        run_test(&format!(
            "{msg} class KwOnly; def initialize(x:, y: 10) = @x = x; end; msg {{ KwOnly.new(y: 1) }}"
        ));
        run_test(&format!("{msg} def m(x:) = x; msg {{ send(:m) }}"));
        run_test(&format!("{msg} def m(x:) = x; msg {{ method(:m).call }}"));
        run_test(&format!("{msg} msg {{ proc {{ |x:| x }}.call }}"));
        run_test(&format!("{msg} msg {{ [1].each {{ |x:| x }} }}"));
        run_test(&format!(
            "{msg} define_method(:dm) {{ |x:| x }}; msg {{ dm }}"
        ));
        run_test(&format!(
            "{msg} def m(x:, y: 10) = [x, y]; def fw(...) = m(...); [msg {{ fw(y: 2) }}, fw(x: 9)]"
        ));
        // Optional-only keywords still default without error.
        run_test("def m(y: 10) = y; [m, m(y: 1)]");
    }

    #[test]
    fn kwarg_nonsymbol_key_no_panic() {
        // A non-Symbol key passed where keyword params are expected
        // (no **rest) is an "unknown keyword" reported via inspect;
        // previously `as_symbol().unwrap()` aborted the process.
        run_test_error(r#"def m(a:); a; end; m("a"=>1)"#);
        run_test_error(r#"def m(a:); end; m(:a=>1, "b"=>2)"#);
        run_test_error(r#"def m(a:); end; m(:a=>1, :c=>2)"#);
        // **rest accepts non-Symbol keys verbatim.
        run_test(r#"def m(**k); k; end; m("a"=>1, :b=>2)"#);
        run_test(r#"def m(a:); a; end; m(a: 5)"#);
    }
}
#[cfg(test)]
mod fast_yield_tests {
    use crate::tests::*;

    #[test]
    fn simple_yield_argument_shapes() {
        // The simple-yield direct-copy path (jit_handle_arguments_no_
        // block_for_yield) must agree with the generic path on every
        // block binding rule. Each case iterates enough for the JIT to
        // compile the yielding loop, and run_test compares the final
        // values against CRuby.
        run_tests(&[
            // exact arity — the direct-copy fast path
            r#"def a; s = 0; 200.times { |i| yield i }; s; end
               acc = []; a { |x| acc << x }; [acc.size, acc.first, acc.last]"#,
            // fewer args than params: nil-fill (loose block binding)
            r#"acc = nil; def b; 100.times { yield 7 }; end
               b { |x, y| acc = [x, y] }; acc"#,
            // more args than params: extras dropped
            r#"acc = nil; def c; 100.times { yield 1, 2, 3 }; end
               c { |x, y| acc = [x, y] }; acc"#,
            // single-Array auto-splat into a multi-param block
            r#"acc = nil; def d; 100.times { yield [4, 5] }; end
               d { |x, y| acc = [x, y] }; acc"#,
            // single-param block keeps a passed Array whole
            r#"acc = nil; def e; 100.times { yield [4, 5] }; end
               e { |x| acc = x }; acc"#,
            // optional / rest / keyword blocks stay on the flexible path
            r#"acc = nil; def f; 100.times { yield 1 }; end
               f { |x, y = 9| acc = [x, y] }; acc"#,
            r#"acc = nil; def g; 100.times { yield 1, 2, 3 }; end
               g { |x, *r| acc = [x, r] }; acc"#,
            r#"acc = nil; def h; 100.times { yield 1 }; end
               h { |x, k: 5| acc = [x, k] }; acc"#,
            // zero-arg yield into a param-less block
            r#"n = 0; def z; 300.times { yield }; end
               z { n += 1 }; n"#,
        ]);
    }
}
