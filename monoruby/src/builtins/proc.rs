use super::*;

//
// Proc class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Proc", PROC_CLASS, ObjTy::PROC);
    // `rest` so that `SubclassOfProc.new(*args) { }` forwards the
    // positional args to the subclass's `#initialize`.
    let proc_meta = globals.store.get_metaclass(PROC_CLASS).id();
    globals.define_builtin_funcs_with_effect(
        proc_meta,
        "new",
        &[],
        new,
        0,
        0,
        true,
        Effect::CAPTURE,
    );
    globals.store[PROC_CLASS].clear_alloc_func();
    globals.define_builtin_funcs_with_kw(
        PROC_CLASS,
        "call",
        &["[]", "yield", "==="],
        call,
        0,
        0,
        true,
        &[],
        true,
    );
    globals.define_builtin_func(PROC_CLASS, "binding", binding_, 0);
    globals.define_builtin_func(PROC_CLASS, "source_location", source_location, 0);
    globals.define_builtin_funcs(PROC_CLASS, "to_s", &["inspect"], to_s, 0);
    globals.define_builtin_func(PROC_CLASS, "lambda?", lambda_, 0);
    globals.define_builtin_func(PROC_CLASS, "arity", proc_arity, 0);
    globals.define_builtin_func_with_kw(
        PROC_CLASS,
        "parameters",
        parameters,
        0,
        0,
        false,
        &["lambda"],
        false,
    );
    // `ruby2_keywords` is a marker method used by CRuby to tag a proc/method
    // so that its rest args can pass a trailing keyword hash through to
    // another method. monoruby does not distinguish keyword args from a
    // trailing hash at the runtime level in a way that needs this marker,
    // so it's a safe no-op that returns the receiver.
    globals.define_builtin_func(PROC_CLASS, "ruby2_keywords", ruby2_keywords, 0);
    globals.define_builtin_func_with(PROC_CLASS, "curry", curry, 0, 1, false);
}

/// Build a curry proc: a Proc backed by the shared native
/// [`PROC_CURRY_BODY_FUNCID`] body, carrying its state — `[orig,
/// collected, arity, lambda?]` — on the proc's `outer_lfp` self.
pub(crate) fn make_curry_proc(
    globals: &Globals,
    orig: Value,
    collected: Value,
    arity: i64,
    is_lambda: bool,
    pc: BytecodePtr,
) -> Value {
    let state = Value::array_from_vec(vec![
        orig,
        collected,
        Value::integer(arity),
        Value::bool(is_lambda),
    ]);
    let frame = Lfp::heap_frame(state, globals[PROC_CURRY_BODY_FUNCID].meta());
    Proc::from_outer(frame, PROC_CURRY_BODY_FUNCID, pc).into()
}

/// Whether `proc` reports `lambda? == true` (the curry procs inherit
/// this from the proc they were built from). A curry proc reads its own
/// stored flag; otherwise it is derived from the body's block-style.
fn proc_is_lambda(globals: &Globals, proc: &Proc) -> bool {
    if proc.func_id() == PROC_CURRY_BODY_FUNCID {
        return proc.self_val().as_array()[3].as_bool();
    }
    !globals[proc.func_id()].is_block_style()
}

/// `Proc#arity`-equivalent for the curry-time default (mirrors
/// `proc_arity`): a `Method#to_proc` proc reports the bound method's
/// arity, everything else the func's arity.
fn proc_arity_value(globals: &Globals, proc: &Proc) -> i64 {
    if proc.func_id() == METHOD_TO_PROC_BODY_FUNCID
        && let Some(m) = proc.self_val().is_method()
    {
        if m.method_missing_name().is_some() {
            return -1;
        }
        return globals[m.func_id()].arity();
    }
    globals[proc.func_id()].arity()
}

///
/// ### Proc#curry
///
/// - curry -> Proc
/// - curry(arity) -> Proc
///
/// [https://docs.ruby-lang.org/ja/latest/method/Proc/i/curry.html]
#[monoruby_builtin]
fn curry(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let proc = Proc::new(self_val);
    let is_lambda = proc_is_lambda(globals, &proc);
    let arity_given = lfp.try_arg(0).filter(|v| !v.is_nil());
    let mut arity = match arity_given {
        Some(v) => v.coerce_to_int_i64(vm, globals)?,
        None => proc_arity_value(globals, &proc),
    };
    if arity < 0 {
        arity = -arity - 1;
    }
    // A lambda only accepts an arity its own signature can satisfy.
    if is_lambda && globals.store.resolve_iseq(proc.func_id()).is_some() {
        let params = globals[proc.func_id()].params();
        let req = (params.req_num() + params.post_num()) as i64;
        let has_rest = params.is_rest().is_some() && !params.rest_is_implicit();
        let max = if has_rest {
            i64::MAX
        } else {
            req + params.opt_num() as i64
        };
        if arity < req || arity > max {
            return Err(MonorubyErr::wrong_number_of_arg_range(
                arity as usize,
                req as usize..=max as usize,
            ));
        }
    }
    if arity <= 0 {
        return Ok(self_val);
    }
    Ok(make_curry_proc(
        globals,
        self_val,
        Value::array_empty(),
        arity,
        is_lambda,
        pc,
    ))
}

/// Shared body of curry procs (see [`PROC_CURRY_BODY_FUNCID`]). Appends
/// the call arguments to the collected ones; once enough are gathered
/// it calls the original proc, otherwise it returns a further-curried
/// proc that captures the accumulated arguments.
#[monoruby_builtin]
pub(crate) fn proc_curry_body(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
) -> Result<Value> {
    let state = lfp
        .outer()
        .expect("proc_curry_body called without outer_lfp")
        .self_val();
    let st = state.as_array();
    let orig = st[0];
    let collected = st[1].as_array();
    let arity = st[2].try_fixnum().unwrap();
    let is_lambda = st[3].as_bool();
    let mut new_args: Vec<Value> = collected.iter().copied().collect();
    new_args.extend(lfp.arg(0).as_array().iter().copied());
    if (new_args.len() as i64) >= arity {
        let call = IdentId::get_id("call");
        let bh = lfp.block();
        vm.invoke_method_inner(globals, call, orig, &new_args, bh, None)
    } else {
        Ok(make_curry_proc(
            globals,
            orig,
            Value::array_from_vec(new_args),
            arity,
            is_lambda,
            pc,
        ))
    }
}

/// `Proc#ruby2_keywords` — no-op marker that returns self.
#[monoruby_builtin]
fn ruby2_keywords(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(lfp.self_val())
}

///
/// ### Proc.new
///
/// - new { ... } -> Proc
///
/// [https://docs.ruby-lang.org/ja/latest/method/Proc/s/new.html]
#[monoruby_builtin]
fn new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let Some(bh) = lfp.block() else {
        return Err(MonorubyErr::create_proc_no_block());
    };
    let p = vm.generate_proc(globals, bh, pc)?;
    let class_id = lfp.self_val().as_class_id();
    if class_id == PROC_CLASS {
        return Ok(p.into());
    }
    // A subclass of Proc: re-tag the freshly built proc with the
    // subclass and run its `#initialize(*args)` (CRuby binds the block
    // before `initialize`, so a custom `initialize` need not `super`).
    let mut obj: Value = p.into();
    obj.change_class(class_id);
    let args: Vec<Value> = lfp.arg(0).as_array().iter().copied().collect();
    let temp = vm.temp_len();
    vm.temp_push(obj);
    vm.invoke_method_inner(globals, IdentId::INITIALIZE, obj, &args, None, None)?;
    vm.temp_clear(temp);
    Ok(obj)
}

///
/// ### Proc#call
///
/// - self[*arg] -> ()
/// - call(*arg) -> ()
/// - self === *arg -> ()
/// - yield(*arg) -> ()
///
/// TODO: we must support [] with >2 args.
/// [https://docs.ruby-lang.org/ja/latest/method/Proc/i/=3d=3d=3d.html]
#[monoruby_builtin]
fn call(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let proc = Proc::new(lfp.self_val());
    // `Proc#call` is declared with keyword-rest, so trailing keyword
    // arguments land in the kw-rest slot (`arg(1)`, after the
    // positional rest at `arg(0)`). Forward them to the block invoker
    // so the *block's own* signature decides whether they bind to
    // keyword parameters or fold into a trailing positional Hash
    // (matches CRuby; previously these were dropped).
    let kw = match lfp.try_arg(1) {
        Some(v)
            if v.ty() == Some(ObjTy::HASH) && !Hashmap::new(v).inner().is_empty() =>
        {
            Some(Hashmap::new(v))
        }
        _ => None,
    };
    // Fast path for Symbol#to_proc procs: dispatch directly so that the
    // block passed to Proc#call is forwarded to the invoked method. The
    // regular invoke_proc/block_invoker path currently drops block handlers.
    if proc.func_id() == SYMBOL_TO_PROC_BODY_FUNCID {
        let symbol = proc.self_val();
        let symbol_id = symbol
            .try_symbol()
            .expect("symbol-to-proc outer self is not a Symbol");
        let args_val = lfp.arg(0);
        let args = args_val.as_array();
        if args.is_empty() {
            return Err(MonorubyErr::argumenterr("no receiver given"));
        }
        let recv = args[0];
        let rest: Vec<Value> = args[1..].to_vec();
        let class_id = recv.class();
        if let Some(entry) = globals.check_method_for_class(class_id, symbol_id) {
            match entry.visibility() {
                Visibility::Private => {
                    return Err(MonorubyErr::private_method_called(globals, symbol_id, recv));
                }
                Visibility::Protected => {
                    return Err(MonorubyErr::protected_method_called(
                        globals, symbol_id, recv,
                    ));
                }
                _ => {}
            }
        }
        let bh = lfp.block();
        return vm.invoke_method_inner(globals, symbol_id, recv, &rest, bh, kw);
    }
    let bh = lfp.block();
    vm.invoke_proc_with_block(globals, &proc, &lfp.arg(0).as_array(), bh, kw)
}

///
/// ### Proc#source_location
///
/// - source_location -> [String, Integer] | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Proc/i/source_location.html]
#[monoruby_builtin]
fn source_location(
    _: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let proc = Proc::new(lfp.self_val());
    // A Method#to_proc proc reports the method's source location.
    let func_id = if proc.func_id() == METHOD_TO_PROC_BODY_FUNCID
        && let Some(m) = proc.self_val().is_method()
    {
        m.func_id()
    } else {
        proc.func_id()
    };
    if let Some(iseq) = globals.store.resolve_iseq(func_id) {
        let iseq_info = &globals.store[iseq];
        // Absolute path, consistent with Method#source_location.
        let file_name = Value::string(iseq_info.sourceinfo.file_name().to_string());
        let line = Value::integer(iseq_info.sourceinfo.get_line(&iseq_info.loc) as i64);
        Ok(Value::array2(file_name, line))
    } else {
        Ok(Value::nil())
    }
}

///
/// ### Proc#to_s, Proc#inspect
///
/// - to_s -> String
/// - inspect -> String
///
/// Returns `#<Proc:0xADDR FILE:LINE>` (plus ` (lambda)` for a lambda).
/// The result has BINARY (ASCII-8BIT) encoding, matching CRuby.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Proc/i/to_s.html]
#[monoruby_builtin]
fn to_s(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let proc = Proc::new(self_);
    let func_id = proc.func_id();
    let is_lambda = !globals[func_id].is_block_style();
    // A Method#to_proc proc reports the method's source location.
    let src_fid = if func_id == METHOD_TO_PROC_BODY_FUNCID
        && let Some(m) = proc.self_val().is_method()
    {
        m.func_id()
    } else {
        func_id
    };
    let mut s = format!("#<Proc:0x{:016x}", self_.id());
    if func_id == SYMBOL_TO_PROC_BODY_FUNCID
        && let Some(sym) = proc.self_val().try_symbol()
    {
        // `:foo.to_proc` renders as `#<Proc:0x..(&:foo) (lambda)>`.
        s.push_str(&format!("(&:{})", sym));
    } else if let Some(iseq) = globals.store.resolve_iseq(src_fid) {
        let info = &globals.store[iseq];
        s.push_str(&format!(
            " {}:{}",
            info.sourceinfo.file_name(),
            info.sourceinfo.get_line(&info.loc)
        ));
    }
    if is_lambda {
        s.push_str(" (lambda)");
    }
    s.push('>');
    Ok(Value::string_from_inner(
        crate::value::rvalue::RStringInner::from_encoding(
            s.as_bytes(),
            crate::value::Encoding::Ascii8,
        ),
    ))
}

///
/// ### Proc#binding
///
/// - binding -> Binding
///
/// [https://docs.ruby-lang.org/ja/latest/method/Proc/i/binding.html]
#[monoruby_builtin]
fn binding_(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let proc = Proc::new(lfp.self_val());
    // Curry procs have no Ruby-level binding (CRuby raises ArgumentError).
    if proc.func_id() == PROC_CURRY_BODY_FUNCID {
        return Err(MonorubyErr::argumenterr("Can't create Binding from C level Proc"));
    }
    // For a Method#to_proc proc, the outer self is the Method object;
    // its binding's receiver must be the *method's* receiver (CRuby).
    if proc.func_id() == METHOD_TO_PROC_BODY_FUNCID
        && let Some(m) = proc.self_val().is_method()
    {
        let frame = Lfp::heap_frame(m.receiver(), globals[m.func_id()].meta());
        return Ok(Binding::from_outer(frame, proc.source()).as_val());
    }
    let outer_lfp = proc.outer_lfp();
    let pc = proc.source();
    Ok(Binding::from_outer(outer_lfp.unwrap(), pc).as_val())
}

/// ### Proc#lambda?
#[monoruby_builtin]
fn lambda_(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let proc = Proc::new(lfp.self_val());
    Ok(Value::bool(proc_is_lambda(globals, &proc)))
}

/// ### Proc#parameters
///
/// - parameters -> [[Symbol, Symbol], ...]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Proc/i/parameters.html]
#[monoruby_builtin]
fn parameters(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // `lambda:` keyword (slot 0, after positional max of 0) overrides
    // whether required positionals are tagged `:req` (lambda) or
    // `:opt` (block). Absent — or explicitly `nil` — ⇒ use the proc's
    // own lambda-ness (CRuby ignores a `nil` override).
    let lambda_override = lfp
        .try_arg(0)
        .filter(|v| !v.is_nil())
        .map(|v| v.as_bool());
    let proc = Proc::new(lfp.self_val());
    if proc.func_id() == METHOD_TO_PROC_BODY_FUNCID
        && let Some(m) = proc.self_val().is_method()
        && m.method_missing_name().is_none()
    {
        return Ok(build_parameters(globals, m.func_id(), lambda_override.unwrap_or(true)));
    }
    let func_id = proc.func_id();
    let is_lambda = lambda_override.unwrap_or_else(|| !globals[func_id].is_block_style());
    Ok(build_parameters(globals, func_id, is_lambda))
}

/// Build the array of `[[type, name], ...]` entries returned by
/// `Proc#parameters`, `Method#parameters`, and `UnboundMethod#parameters`.
///
/// `is_lambda` controls how required positional params are tagged: lambdas
/// and named methods use `:req`, while non-lambda procs (blocks) use `:opt`.
pub(crate) fn build_parameters(globals: &Globals, func_id: FuncId, is_lambda: bool) -> Value {
    let func_info = &globals[func_id];
    let params = func_info.params();
    let req_tag = if is_lambda {
        Value::symbol(IdentId::get_id("req"))
    } else {
        Value::symbol(IdentId::get_id("opt"))
    };
    let opt_tag = Value::symbol(IdentId::get_id("opt"));
    let rest_tag = Value::symbol(IdentId::get_id("rest"));
    let key_tag = Value::symbol(IdentId::get_id("key"));
    let keyreq_tag = Value::symbol(IdentId::get_id("keyreq"));
    let keyrest_tag = Value::symbol(IdentId::get_id("keyrest"));
    let nokey_tag = Value::symbol(IdentId::get_id("nokey"));
    let block_tag = Value::symbol(IdentId::get_id("block"));
    let star_sym = Value::symbol(IdentId::get_id("*"));
    let dstar_sym = Value::symbol(IdentId::get_id("**"));
    let amp_sym = Value::symbol(IdentId::get_id("&"));
    let mut result = vec![];
    let args_names = &params.args_names;
    let mut name_idx = 0;
    // required params
    for _ in 0..params.req_num() {
        // The implicit `it` parameter is reported anonymously
        // (`[[:req]]` / `[[:opt]]`); an explicit `|it|` keeps its name.
        let entry = if params.it_param() {
            Value::array1(req_tag)
        } else if let Some(Some(name)) = args_names.get(name_idx) {
            Value::array2(req_tag, Value::symbol(*name))
        } else {
            Value::array1(req_tag)
        };
        result.push(entry);
        name_idx += 1;
    }
    // optional params
    for _ in 0..params.opt_num() {
        let entry = if let Some(Some(name)) = args_names.get(name_idx) {
            Value::array2(opt_tag, Value::symbol(*name))
        } else {
            Value::array1(opt_tag)
        };
        result.push(entry);
        name_idx += 1;
    }
    // rest param — but skip the implicit trailing-comma rest (`|a,|`)
    // since CRuby's `Method#parameters` reports `[[:req, :a]]`, not
    // `[[:req, :a], [:rest]]`. The slot still exists internally for
    // the runtime arg dispatcher to absorb extras under block style.
    if params.is_rest().is_some() {
        if !params.rest_is_implicit() {
            let entry = match args_names.get(name_idx) {
                Some(Some(name)) => Value::array2(rest_tag, Value::symbol(*name)),
                // A slot exists but is unnamed: Ruby-level anonymous
                // `*` (incl. the rest synthesized by `...`). CRuby
                // reports its name as `:*`.
                Some(None) => Value::array2(rest_tag, star_sym),
                // No slot at all: a native/builtin rest -> `[:rest]`.
                None => Value::array1(rest_tag),
            };
            result.push(entry);
        }
        name_idx += 1;
    }
    // post params (required after rest)
    for _ in 0..params.post_num() {
        let entry = if let Some(Some(name)) = args_names.get(name_idx) {
            Value::array2(req_tag, Value::symbol(*name))
        } else {
            Value::array1(req_tag)
        };
        result.push(entry);
        name_idx += 1;
    }
    // keyword params
    for (i, kw_name) in params.kw_names.iter().enumerate() {
        let tag = if params.kw_is_required(i) {
            keyreq_tag
        } else {
            key_tag
        };
        result.push(Value::array2(tag, Value::symbol(*kw_name)));
    }
    // keyword rest
    if let Some(slot) = params.kw_rest {
        if params.kw_rest_name() == Some(IdentId::get_id("nil")) {
            // `**nil` is modeled by the parser as a kwrest literally
            // named `nil` (impossible as a real identifier). CRuby
            // reports it as the lone parameter `[[:nokey]]`.
            result.push(Value::array1(nokey_tag));
        } else {
            let entry = match params.kw_rest_name() {
                Some(name) => Value::array2(keyrest_tag, Value::symbol(name)),
                // Distinguish a Ruby-level anonymous `**` (a slot
                // exists, unnamed -> CRuby `:**`) from a native
                // kwrest (no slot -> `[:keyrest]`).
                None => match args_names.get(slot.0 as usize - 1) {
                    Some(_) => Value::array2(keyrest_tag, dstar_sym),
                    None => Value::array1(keyrest_tag),
                },
            };
            result.push(entry);
        }
    }
    // block param
    if let Some(block_name) = params.block_param {
        // An empty name is the parser's marker for an anonymous `&`
        // (and the block synthesized by `...`); CRuby reports `:&`.
        let entry = if block_name == IdentId::get_id("") {
            Value::array2(block_tag, amp_sym)
        } else {
            Value::array2(block_tag, Value::symbol(block_name))
        };
        result.push(entry);
    }
    Value::array_from_vec(result)
}

/// Build the CRuby `Method#inspect`-style parameter signature string,
/// e.g. `(a, b=..., *c, key:, opt: ..., **kw, &blk)`. The parameter
/// order mirrors `build_parameters` so the two stay consistent.
pub(crate) fn signature_string(store: &Store, func_id: FuncId) -> String {
    let params = store[func_id].params();
    let args_names = &params.args_names;
    let named = |o: Option<&Option<IdentId>>| -> Option<IdentId> {
        match o {
            Some(Some(id)) => Some(*id),
            _ => None,
        }
    };
    let mut parts: Vec<String> = vec![];
    let mut idx = 0;
    for _ in 0..params.req_num() {
        parts.push(named(args_names.get(idx)).map_or_else(|| "_".to_string(), |id| id.to_string()));
        idx += 1;
    }
    for _ in 0..params.opt_num() {
        let n = named(args_names.get(idx)).map_or_else(|| "_".to_string(), |id| id.to_string());
        parts.push(format!("{n}=..."));
        idx += 1;
    }
    if params.is_rest().is_some() {
        if !params.rest_is_implicit() {
            match named(args_names.get(idx)) {
                Some(id) => parts.push(format!("*{id}")),
                None => parts.push("*".to_string()),
            }
        }
        idx += 1;
    }
    for _ in 0..params.post_num() {
        parts.push(named(args_names.get(idx)).map_or_else(|| "_".to_string(), |id| id.to_string()));
        idx += 1;
    }
    for (i, kw) in params.kw_names.iter().enumerate() {
        if params.kw_is_required(i) {
            parts.push(format!("{kw}:"));
        } else {
            parts.push(format!("{kw}: ..."));
        }
    }
    if params.kw_rest.is_some() {
        match params.kw_rest_name() {
            Some(id) => parts.push(format!("**{id}")),
            None => parts.push("**".to_string()),
        }
    }
    if let Some(bn) = params.block_param {
        let s = bn.to_string();
        if s.is_empty() {
            parts.push("&".to_string());
        } else {
            parts.push(format!("&{s}"));
        }
    }
    format!("({})", parts.join(", "))
}

/// ### Proc#arity
#[monoruby_builtin]
fn proc_arity(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let proc = Proc::new(lfp.self_val());
    // A Method#to_proc proc reports the *method's* arity, not the
    // shared rest-style body's (-1).
    if proc.func_id() == METHOD_TO_PROC_BODY_FUNCID
        && let Some(m) = proc.self_val().is_method()
    {
        if m.method_missing_name().is_some() {
            return Ok(Value::integer(-1));
        }
        return Ok(Value::integer(globals[m.func_id()].arity()));
    }
    let func_id = proc.func_id();
    Ok(Value::integer(globals[func_id].arity()))
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn proc_to_s() {
        // The address differs per run, so check the stable invariants
        // (CRuby: `#<Proc:0xADDR FILE:LINE [(lambda)]>`, BINARY encoding;
        // `inspect` == `to_s`; `:sym.to_proc` shows `(&:sym)`).
        run_test(r#"proc { }.to_s.encoding.to_s"#);
        run_test(
            r##"[proc { }.to_s.start_with?("#<Proc:0x"),
               (-> () {}).to_s.include?(" (lambda)"),
               :foo.to_proc.to_s.include?("(&:foo)"),
               proc { }.inspect.start_with?("#<Proc:0x")]"##,
        );
    }

    #[test]
    fn proc_subclass_new() {
        // A Proc subclass without a custom initialize.
        run_test(r#"class PSubA < Proc; end; p = PSubA.new { 5 }; [p.is_a?(PSubA), p.call]"#);
        // A subclass whose #initialize takes positional args (CRuby binds
        // the block before #initialize, so it need not call super).
        run_test(
            r#"class PSubB < Proc; def initialize(a, b); @a = a; @b = b; end; attr_reader :a, :b; end
               o = PSubB.new(:x, 2) { 9 }; [o.class.name, o.a, o.b, o.call]"#,
        );
        // dup / clone run initialize_dup / initialize_clone (and the
        // result is an instance of the subclass).
        run_test(
            r#"class PSubC < Proc
                 def initialize_dup(o); super; @tag = :dup; end
                 def initialize_clone(o, **k); super; @tag = :clone; end
                 attr_reader :tag
               end
               o = PSubC.new { 1 }; [o.dup.class.name, o.dup.tag, o.clone.tag]"#,
        );
    }

    #[test]
    fn proc_new() {
        run_test_no_result_check("Proc.new {}");
        run_test_error("Proc.new");
        run_tests(&[
            "
            a = 100
            p = Proc.new {|x, y|
                a += x / y
            }
            p.call(42, 7)
        ",
            "
        a = 100
        p = nil
        1.times {
          p = Proc.new {
            3.times {
              a+=1
            }
          }
        }
        p.call
        a
        ",
        ]);
    }

    #[test]
    fn proc1() {
        run_test_with_prelude(
            r#"
        [p.call(3,4), p.yield(3,4), p[3,4], p.===(3,4)]
        "#,
            r#"
        p = Proc.new {|x,y| x * y} 
        "#,
        );
    }

    #[test]
    fn proc2() {
        run_test(
            "
            a = 100
            p = nil
            q = nil
            3.times {
              p = Proc.new {
                3.times {
                  a+=1
                }
              }
              q = Proc.new {
                5.times {
                  a+=10
                }
              }
            }
            p.call
            q.call
            a
        ",
        );
    }

    #[test]
    fn proc_param() {
        run_test(
            "
            a = []
            p = Proc.new {|x|
                a << x
            }
            5.times(&p)
            a
        ",
        );
    }

    #[test]
    fn block_param() {
        run_test_with_prelude(
            r#"
            $ans = []
            [1, 2, 3].each(&Foo.new)
        "#,
            r#"
            class Foo
              def to_proc
                Proc.new {|v| $ans << v * v}
              end
            end
        "#,
        );
        run_test_error(
            r#"
        class Foo
            def to_proc
                :xxx
            end
        end

        [1,2,3].each(&Foo.new)
        "#,
        );
        run_test_error(
            r#"
        class Foo
        end

        [1,2,3].each(&Foo.new)
        "#,
        );
    }

    #[test]
    fn duplicate_underscore_param() {
        // `_` can appear multiple times in block parameters (Ruby spec).
        // The name `_` resolves to the first occurrence.
        // Destructured block parameters with duplicate `_`.
        // `_` used alone multiple times.
        run_tests(&[
            r#"
        [[1,2,3,4]].map { |x, _, y, _| [x, _, y] }
        "#,
            r#"
        [[1,2,3,4]].sort_by { |(_, b, _, d)| d }
        "#,
            r#"
        res = []
        {a: 1, b: 2}.each { |_, _| res << _ }
        res
        "#,
        ]);
    }

    #[test]
    fn proc() {
        run_test_with_prelude(
            r#"
        a = :a
        $b = proc { a = 1 }
        a = nil
        foo
        a.inspect
"#,
            r#"
        def foo
          $b.call
        end
        "#,
        )
    }

    #[test]
    fn proc_binding() {
        run_tests(&[
            r#"
        x = 42
        p = Proc.new { x }
        b = p.binding
        b.is_a?(Binding)
        "#,
            r#"
        x = 42
        p = Proc.new { x }
        b = p.binding
        b.local_variables.include?(:x)
        "#,
            r#"
        x = 10
        p = proc { x + 1 }
        eval("x", p.binding)
        "#,
        ]);
    }

    #[test]
    fn proc_2() {
        run_test_once(
            r#"
        a = 5
        p = proc {
          a = 10
        }
        a = 1
        p.call
        a
        "#,
        );

        run_test_once(
            r#"
        p = proc {
          a = 10
        }
        a = 1
        p.call
        a
        "#,
        );
    }

    #[test]
    fn proc_source_location() {
        // source_location returns [String, Integer]
        // source_location line matches the proc creation line
        // lambda source_location line matches creation line
        // two procs on consecutive lines have consecutive line numbers
        // Proc#binding.source_location returns the same line as the proc
        run_tests(&[
            r#"
        p = proc {}
        sl = p.source_location
        [sl.is_a?(Array), sl.size == 2, sl[0].is_a?(String), sl[1].is_a?(Integer)]
        "#,
            r#"
        line = __LINE__; p = proc {}
        p.source_location[1] == line
        "#,
            r#"
        line = __LINE__; l = lambda {}
        l.source_location[1] == line
        "#,
            r#"
        p1 = proc {}
        p2 = proc {}
        p2.source_location[1] == p1.source_location[1] + 1
        "#,
            r#"
        p = proc {}
        b = p.binding
        p.source_location[1] == b.source_location[1]
        "#,
        ]);
    }

    #[test]
    fn proc_yield_detached_context() {
        // yield in Proc whose enclosing method was called with a block but has
        // already returned should raise LocalJumpError, not panic.
        run_test_error(
            r#"
        def make_proc
          Proc.new { yield }
        end
        def get_proc_with_block
          make_proc { 99 }
        end
        get_proc_with_block.call
        "#,
        );
        // yield in Proc whose enclosing method was called without a block
        run_test_error(
            r#"
        def make_proc
          Proc.new { yield }
        end
        make_proc.call
        "#,
        );
    }

    #[test]
    fn proc_yield_same_context() {
        // yield in Proc called within the same method context should work
        run_test(
            r#"
        def call_with_block
          p = Proc.new { yield }
          p.call
        end
        call_with_block { 42 }
        "#,
        );
    }

    // --- regression tests for PR #328 ---
    //
    // proc_call_block_constant_lookup:
    // `->(&b) { b.call }.call { <const> }` used to panic with
    // `unreachable!` in `FuncInfo::as_iseq()` because the proxy
    // BlockHandler's depth was under-counted by 1 on Proc#call
    // passthrough, leaving the block's outer lfp pointing at
    // Proc#call (a Builtin). Now the constant lookup resolves
    // normally.
    //
    // proc_call_block_raise_class_lookup:
    // Block passed through `&blk` must preserve the outer lexical
    // scope of where the block was defined, even when invoked via
    // `Proc#call`. A `raise` inside the block looks up the
    // exception class through that scope.
    #[test]
    fn proc_lambda_query_arity_parameters_curry_blocklookup() {
        run_tests(&[
            "Proc.new {}.lambda?",
            "lambda {}.lambda?",
            "->(x) { x }.lambda?",
            "Proc.new {}.arity",
            "Proc.new {|x| x}.arity",
            "Proc.new {|x, y| x}.arity",
            "lambda {}.arity",
            "lambda {|x| x}.arity",
            "->(x, y) { x }.arity",
            // lambda: required params are :req
            "->(x, y) {}.parameters",
            // lambda: optional params are :opt
            "->(x, y=1) {}.parameters",
            // lambda: rest params
            "->(x, *rest) {}.parameters",
            // lambda: block param
            "->(x, &blk) {}.parameters",
            // lambda: mixed
            "->(x, y=1, *rest, &blk) {}.parameters",
            // proc: all positional params are :opt
            "Proc.new {|x, y| }.parameters",
            // proc: rest
            "Proc.new {|x, *rest| }.parameters",
            // no params
            "lambda {}.parameters",
            "Proc.new {}.parameters",
            r#"
            p = proc {|*args| args}
            p.ruby2_keywords.equal?(p)
            "#,
            "proc {|a,b,c| a+b+c}.curry[1][2][3]",
            "proc {|a,b,c| a+b+c}.curry[1,2][3]",
            "proc {|a,b,c| a+b+c}.curry[1,2,3]",
            "->(a,b) { a + b }.curry[1][2]",
            "proc {|a,b,c| [a,b,c]}.curry(3)[1][2][3]",
            // curry metadata: lambda-ness is inherited, parameters are
            // anonymous rest, source_location is nil, arity is -1.
            "->(a,b,c){}.curry.lambda?",
            "proc {|a,b,c|}.curry.lambda?",
            "->(a,b,c){}.curry(3).lambda?",
            "->(a,b,c){}.curry.call(1).lambda?",
            "proc {|a,b,c|}.curry.call(1).lambda?",
            "->(a,b,c){}.curry.parameters",
            "->(a,b,c){}.curry.source_location",
            "->(a,b,c){}.curry.arity",
            r#"(->(a,b,c){}.curry.binding rescue $!.class)"#,
            // curry(arity) on a lambda validates the requested arity.
            r#"(->(a,b,c){}.curry(2) rescue $!.class)"#,
            r#"(->(a,b,c){}.curry(4) rescue $!.class)"#,
            // proc curry tolerates extra args; lambda curry rejects them.
            "proc {|a,b,c| (a||0)+(b||0)+(c||0)}.curry[1,2,3,4]",
            r#"(->(a,b,c){ a+b+c }.curry[1,2,3,4] rescue $!.class)"#,
            // `&-> {}` keeps the lambda-ness; `&proc {}` / a literal block
            // stays a non-lambda.
            "proc(&->{}).lambda?",
            "proc(&proc{}).lambda?",
            "Proc.new(&->(x){x}).lambda?",
            "def amp(&b); b; end; amp(&->{}).lambda?",
            "def amp2(&b); b; end; amp2 {}.lambda?",
            // implicit `it` reports an anonymous parameter; explicit keeps it.
            "eval('proc { it }').parameters",
            "eval('lambda { it }').parameters",
            "proc {|it| }.parameters",
            r#"->(&b) { b.call }.call { Integer }.equal?(Integer)"#,
            r#"
              step = ->(a, b, &blk) { a.step(b, &blk) }
              seen = []
              step.call(1, 3) { |i| seen << [i, Integer.name] }
              seen
            "#,
            r#"
              step = ->(a, b, &blk) { a.step(b, &blk) }
              begin
                step.call(1, 2) { raise ArgumentError, "hi" }
                :not_raised
              rescue ArgumentError => e
                e.message
              end
            "#,
        ]);
    }

    /// `&:sym` passed as a block should materialize a Proc only
    /// on demand; capturing it back as `&blk` must work (used to
    /// fail with `not yet implemented: block handler …`).
    #[test]
    fn proc_amp_symbol_reified_via_blk() {
        run_test(
            r#"
              def take_block(&blk); blk; end
              bl = take_block(&:to_s)
              [bl.class, bl.call(42)]
            "#,
        );
        run_test(
            r#"
              def apply(x, &b); b.call(x); end
              [apply(5, &:to_s), apply([1,2], &:length)]
            "#,
        );
    }

    /// `&:sym` forwarded as `&b` through a middle Proc and
    /// re-dispatched must behave the same as a direct `&:sym`
    /// block arg.
    #[test]
    fn proc_amp_symbol_forwarded() {
        run_test(
            r#"
              def forward(&b); [1,2,3].map(&b); end
              forward(&:to_s)
            "#,
        );
    }

    #[test]
    fn proc_call_forwards_trailing_kwargs() {
        // Trailing keyword args passed to Proc#call/[]/=== are
        // forwarded to the block; the block's own signature decides
        // whether they bind to kw params or fold into a *rest Hash.
        run_tests(&[
            r#"->(fmt, *args){ args }.call("x", foo: 123)"#,
            r#"->(*a){ a }.call(1, x: 2)"#,
            r#"proc {|*a| a }.call(1, y: 3)"#,
            r#"lambda {|*a| a }.call(2, z: 4)"#,
            r#"->(a, k:){ [a, k] }.call(1, k: 2)"#,
            r#"->(a, **kw){ [a, kw] }.call(1, x: 9)"#,
            r#"->(*a){ a }.(7, q: 8)"#,
            r#"(->(x){ x } === 4)"#,
            r#"->(*a){ a }.call(**{})"#,
            r#"->(a=0, *r){ [a, r] }.call(k: 1)"#,
        ]);
        run_test(r#"def fwd(&b); b.call(1, m: 2); end; fwd { |*a| a }"#);
    }

    #[test]
    fn proc_parameters_lambda_kw() {
        run_tests(&[
            // lambda: true ⇒ required positionals tagged :req.
            r#"proc {|x| }.parameters(lambda: true)"#,
            r#"proc {|y, *x| }.parameters(lambda: true)"#,
            // lambda: false ⇒ a real lambda's required params become :opt.
            r#"->(x) { }.parameters(lambda: false)"#,
            // Truthy non-bool ⇒ :req.
            r#"proc {|x| }.parameters(lambda: 123).first.first"#,
            // No keyword ⇒ proc's own lambda-ness (block ⇒ :opt).
            r#"proc {|x| }.parameters"#,
            r#"->(x) { }.parameters"#,
            // nil keyword ⇒ ignored, use the proc's own lambda-ness.
            r#"proc {|x| }.parameters(lambda: nil).first.first"#,
            r#"->(x) { }.parameters(lambda: nil).first.first"#,
        ]);
    }

    #[test]
    fn sprintf_named_unnamed_mix_is_error() {
        run_test_error(r#""%d %<foo>d" % [1, {foo: 2}]"#);
        run_test_error(r#""%d %{foo}" % [1, {foo: 2}]"#);
        run_test_error(r#""%{a} %d" % {a: 1}"#);
        run_tests(&[
            r#""%<a>s %<b>s" % {a: 1, b: 2}"#,
            r#""%{a} %{b}" % {a: 1, b: 2}"#,
            r#""%s %s" % [1, 2]"#,
            r#""%<a>d" % {a: 5}"#,
        ]);
    }
}
