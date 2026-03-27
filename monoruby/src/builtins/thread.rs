use super::*;
use std::sync::{Arc, Condvar, Mutex};

//
// Thread class
//

/// Simplified thread result that can be sent across threads.
enum ThreadResult {
    /// Thread is still running.
    Running,
    /// Thread completed successfully with a value (stored as raw u64 bits).
    Ok(u64),
    /// Thread terminated with an error (kind + message only).
    Err(MonorubyErrKind, String),
}

/// Thread handle state shared between the spawning thread and the spawned thread.
struct ThreadState {
    result: Mutex<ThreadResult>,
    done: Condvar,
}

// SAFETY: ThreadResult stores a u64 (Value bits) or a cloneable error.
// All access to the underlying Ruby Value is serialized by the GVL.
unsafe impl Send for ThreadState {}
unsafe impl Sync for ThreadState {}

/// Global thread table mapping thread IDs to their shared state.
static THREAD_TABLE: std::sync::LazyLock<Mutex<HashMap<u64, Arc<ThreadState>>>> =
    std::sync::LazyLock::new(|| Mutex::new(HashMap::default()));

static THREAD_ID_COUNTER: std::sync::atomic::AtomicU64 =
    std::sync::atomic::AtomicU64::new(1);

fn next_thread_id() -> u64 {
    THREAD_ID_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
}

fn ivar_thread_id() -> IdentId {
    IdentId::get_id("__thread_id")
}

fn ivar_keys() -> IdentId {
    IdentId::get_id("@keys")
}

fn ivar_thread_local() -> IdentId {
    IdentId::get_id("@thread_local")
}

fn main_thread_value(globals: &mut Globals) -> Value {
    globals
        .get_gvar(IdentId::get_id("$__main_thread"))
        .unwrap_or_default()
}

fn empty_hash() -> Value {
    Value::hash(RubyMap::default())
}

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Thread", THREAD_CLASS, ObjTy::OBJECT);

    globals.define_builtin_class_func_with_effect(
        THREAD_CLASS,
        "new",
        thread_new,
        0,
        0,
        Effect::CAPTURE,
    );
    globals.define_builtin_class_func(THREAD_CLASS, "current", thread_current, 0);
    globals.define_builtin_class_func(THREAD_CLASS, "pass", thread_pass, 0);
    globals.define_builtin_class_func(THREAD_CLASS, "main", thread_main, 0);
    globals.define_builtin_class_func(THREAD_CLASS, "list", thread_list, 0);

    globals.define_builtin_func(THREAD_CLASS, "join", thread_join, 0);
    globals.define_builtin_func(THREAD_CLASS, "value", thread_value, 0);
    globals.define_builtin_func(THREAD_CLASS, "alive?", thread_alive, 0);
    globals.define_builtin_func(THREAD_CLASS, "stop?", thread_stop, 0);
    globals.define_builtin_func(THREAD_CLASS, "status", thread_status, 0);
    globals.define_builtin_func(THREAD_CLASS, "kill", thread_kill, 0);
    globals.define_builtin_func(THREAD_CLASS, "[]", thread_aref, 1);
    globals.define_builtin_func(THREAD_CLASS, "[]=", thread_aset, 2);
    globals.define_builtin_func(THREAD_CLASS, "thread_variable_get", thread_variable_get, 1);
    globals.define_builtin_func(THREAD_CLASS, "thread_variable_set", thread_variable_set, 2);

    // Thread::Mutex class (nested under Thread)
    let object_class = globals.object_class();
    globals.define_builtin_class("Mutex", MUTEX_CLASS, object_class, THREAD_CLASS, ObjTy::OBJECT);
    globals.define_builtin_funcs_with_effect(
        MUTEX_CLASS,
        "synchronize",
        &[],
        mutex_synchronize,
        0,
        0,
        false,
        Effect::CAPTURE,
    );
    globals.define_builtin_func(MUTEX_CLASS, "owned?", mutex_owned, 0);
    globals.define_builtin_func(MUTEX_CLASS, "lock", mutex_lock, 0);
    globals.define_builtin_func(MUTEX_CLASS, "unlock", mutex_unlock, 0);
    globals.define_builtin_func(MUTEX_CLASS, "locked?", mutex_locked, 0);

    // Create the main thread object
    let main_thread = create_main_thread(globals);
    globals.set_gvar(IdentId::get_id("$__main_thread"), main_thread);
}

fn create_main_thread(globals: &mut Globals) -> Value {
    let thread_val = Value::object(THREAD_CLASS);
    let thread_id: u64 = 0;

    let state = Arc::new(ThreadState {
        result: Mutex::new(ThreadResult::Running),
        done: Condvar::new(),
    });
    THREAD_TABLE.lock().unwrap().insert(thread_id, state);

    globals
        .store
        .set_ivar(thread_val, ivar_thread_id(), Value::integer(thread_id as i64))
        .unwrap();
    globals
        .store
        .set_ivar(thread_val, ivar_keys(), empty_hash())
        .unwrap();
    globals
        .store
        .set_ivar(thread_val, ivar_thread_local(), empty_hash())
        .unwrap();

    thread_val
}

fn get_thread_id(globals: &Globals, thread_val: Value) -> u64 {
    globals
        .store
        .get_ivar(thread_val, ivar_thread_id())
        .and_then(|v| v.try_fixnum())
        .unwrap_or(0) as u64
}

fn is_thread_alive(thread_id: u64) -> bool {
    let table = THREAD_TABLE.lock().unwrap();
    if let Some(state) = table.get(&thread_id) {
        matches!(*state.result.lock().unwrap(), ThreadResult::Running)
    } else {
        false
    }
}

///
/// ### Thread.new
///
/// Thread.new { ... } -> Thread
///
#[monoruby_builtin]
fn thread_new(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
) -> Result<Value> {
    let bh = lfp.expect_block()?;
    let proc = vm.generate_proc(bh, pc)?;

    let thread_val = Value::object(THREAD_CLASS);
    let thread_id = next_thread_id();

    let state = Arc::new(ThreadState {
        result: Mutex::new(ThreadResult::Running),
        done: Condvar::new(),
    });
    THREAD_TABLE.lock().unwrap().insert(thread_id, state.clone());

    globals
        .store
        .set_ivar(thread_val, ivar_thread_id(), Value::integer(thread_id as i64))
        .unwrap();
    globals
        .store
        .set_ivar(thread_val, ivar_keys(), empty_hash())
        .unwrap();
    globals
        .store
        .set_ivar(thread_val, ivar_thread_local(), empty_hash())
        .unwrap();

    // SAFETY: Access to Globals is serialized by the GVL. The spawned thread
    // will only dereference this pointer while holding the GVL.
    let globals_ptr = globals as *mut Globals as usize;

    let invokers_init_stack_limit = globals.invokers.init_stack_limit;
    let block_invoker = globals.invokers.block;

    // Extract the Proc's data for use in the spawned thread.
    // Proc derefs to ProcInner via #[monoruby_object].
    let proc_data = Box::new(ProcData::from_proc(&proc));
    let proc_data_ptr = Box::into_raw(proc_data) as usize;

    std::thread::spawn(move || {
        gvl_acquire();

        // SAFETY: We hold the GVL, so we have exclusive access to Globals.
        let globals = unsafe { &mut *(globals_ptr as *mut Globals) };

        let mut executor = Executor::default();
        invokers_init_stack_limit(&mut executor);

        // SAFETY: proc_data_ptr was created by Box::into_raw above.
        // The ProcData's Lfp was moved to heap by generate_proc.
        let proc_data = unsafe { Box::from_raw(proc_data_ptr as *mut ProcData) };
        let result = block_invoker(
            &mut executor,
            globals,
            &proc_data,
            Value::nil(),
            ([]).as_ptr(),
            0,
            None,
        );

        let mut res = state.result.lock().unwrap();
        match result {
            Some(val) => {
                *res = ThreadResult::Ok(val.id());
            }
            None => {
                let err = executor.take_error();
                *res = ThreadResult::Err(err.kind.clone(), err.message.clone());
            }
        }
        drop(res);
        state.done.notify_all();

        gvl_release();
    });

    Ok(thread_val)
}

#[monoruby_builtin]
fn thread_current(
    _vm: &mut Executor,
    globals: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(main_thread_value(globals))
}

#[monoruby_builtin]
fn thread_main(
    _vm: &mut Executor,
    globals: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(main_thread_value(globals))
}

#[monoruby_builtin]
fn thread_pass(
    _vm: &mut Executor,
    _globals: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    without_gvl(|| std::thread::yield_now());
    Ok(Value::nil())
}

#[monoruby_builtin]
fn thread_list(
    _vm: &mut Executor,
    globals: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let main = main_thread_value(globals);
    Ok(Value::array_from_vec(vec![main]))
}

#[monoruby_builtin]
fn thread_join(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let thread_id = get_thread_id(globals, self_val);

    if thread_id == 0 {
        return Ok(self_val);
    }

    let state = {
        let table = THREAD_TABLE.lock().unwrap();
        match table.get(&thread_id) {
            Some(s) => s.clone(),
            None => return Ok(self_val),
        }
    };

    without_gvl(|| {
        let mut result = state.result.lock().unwrap();
        while matches!(*result, ThreadResult::Running) {
            result = state.done.wait(result).unwrap();
        }
    });

    let result = state.result.lock().unwrap();
    match &*result {
        ThreadResult::Ok(_) => Ok(self_val),
        ThreadResult::Err(kind, msg) => Err(MonorubyErr::new(kind.clone(), msg)),
        ThreadResult::Running => unreachable!(),
    }
}

#[monoruby_builtin]
fn thread_value(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let thread_id = get_thread_id(globals, self_val);

    if thread_id == 0 {
        return Ok(Value::nil());
    }

    let state = {
        let table = THREAD_TABLE.lock().unwrap();
        match table.get(&thread_id) {
            Some(s) => s.clone(),
            None => return Ok(Value::nil()),
        }
    };

    without_gvl(|| {
        let mut result = state.result.lock().unwrap();
        while matches!(*result, ThreadResult::Running) {
            result = state.done.wait(result).unwrap();
        }
    });

    let result = state.result.lock().unwrap();
    match &*result {
        ThreadResult::Ok(val_id) => Ok(Value::from_u64(*val_id)),
        ThreadResult::Err(kind, msg) => Err(MonorubyErr::new(kind.clone(), msg)),
        ThreadResult::Running => unreachable!(),
    }
}

#[monoruby_builtin]
fn thread_alive(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let thread_id = get_thread_id(globals, self_val);
    Ok(Value::bool(is_thread_alive(thread_id)))
}

#[monoruby_builtin]
fn thread_stop(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let thread_id = get_thread_id(globals, self_val);
    Ok(Value::bool(!is_thread_alive(thread_id)))
}

#[monoruby_builtin]
fn thread_status(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let thread_id = get_thread_id(globals, self_val);

    let table = THREAD_TABLE.lock().unwrap();
    if let Some(state) = table.get(&thread_id) {
        let result = state.result.lock().unwrap();
        match &*result {
            ThreadResult::Running => Ok(Value::string_from_str("run")),
            ThreadResult::Ok(_) => Ok(Value::bool(false)),
            ThreadResult::Err(..) => Ok(Value::nil()),
        }
    } else {
        Ok(Value::bool(false))
    }
}

#[monoruby_builtin]
fn thread_kill(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(lfp.self_val())
}

#[monoruby_builtin]
fn thread_aref(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let key = lfp.arg(0);
    let keys = globals.store.get_ivar(self_val, ivar_keys()).unwrap_or_default();
    if keys.is_nil() {
        return Ok(Value::nil());
    }
    let hash = keys.as_hash();
    Ok(hash.get(key, vm, globals)?.unwrap_or_default())
}

#[monoruby_builtin]
fn thread_aset(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let key = lfp.arg(0);
    let val = lfp.arg(1);
    let keys = globals
        .store
        .get_ivar(self_val, ivar_keys())
        .unwrap_or_else(|| empty_hash());
    let mut hash = keys.as_hash();
    hash.insert(key, val, vm, globals)?;
    globals.store.set_ivar(self_val, ivar_keys(), keys)?;
    Ok(val)
}

#[monoruby_builtin]
fn thread_variable_get(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let key = lfp.arg(0);
    let tls = globals
        .store
        .get_ivar(self_val, ivar_thread_local())
        .unwrap_or_default();
    if tls.is_nil() {
        return Ok(Value::nil());
    }
    let hash = tls.as_hash();
    Ok(hash.get(key, vm, globals)?.unwrap_or_default())
}

#[monoruby_builtin]
fn thread_variable_set(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let key = lfp.arg(0);
    let val = lfp.arg(1);
    let tls = globals
        .store
        .get_ivar(self_val, ivar_thread_local())
        .unwrap_or_else(|| empty_hash());
    let mut hash = tls.as_hash();
    hash.insert(key, val, vm, globals)?;
    globals.store.set_ivar(self_val, ivar_thread_local(), tls)?;
    Ok(val)
}

#[monoruby_builtin]
fn mutex_synchronize(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let bh = lfp.expect_block()?;
    vm.invoke_block_once(globals, bh, &[])
}

#[monoruby_builtin]
fn mutex_owned(
    _vm: &mut Executor,
    _globals: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(Value::bool(false))
}

#[monoruby_builtin]
fn mutex_lock(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(lfp.self_val())
}

#[monoruby_builtin]
fn mutex_unlock(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(lfp.self_val())
}

#[monoruby_builtin]
fn mutex_locked(
    _vm: &mut Executor,
    _globals: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(Value::bool(false))
}
