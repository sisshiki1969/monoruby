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

/// Runtime status of a thread (for Thread#status).
#[derive(Clone, Copy, PartialEq, Eq)]
enum ThreadRunState {
    Run,
    Sleep,
}

/// Thread handle state shared between the spawning thread and the spawned thread.
struct ThreadState {
    result: Mutex<ThreadResult>,
    done: Condvar,
    /// Flag to request the thread to terminate. Protected by `killed` mutex.
    killed: Mutex<bool>,
    /// Notified when `killed` is set to true, to wake up sleeping threads.
    kill_notify: Condvar,
    /// Current runtime state (run/sleep). Accessed atomically via Mutex.
    run_state: Mutex<ThreadRunState>,
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

thread_local! {
    /// The current thread's Thread object, stored as raw u64 bits.
    /// 0 means not set (use the main thread from global variable).
    static CURRENT_THREAD: std::cell::Cell<u64> = const { std::cell::Cell::new(0) };
    /// The current thread's ThreadState, for checking kill flag during sleep.
    static CURRENT_THREAD_STATE: std::cell::RefCell<Option<Arc<ThreadState>>> =
        const { std::cell::RefCell::new(None) };
}

/// Perform an interruptible sleep. Returns true if killed during sleep.
pub(crate) fn thread_sleep(dur: std::time::Duration) {
    let state = CURRENT_THREAD_STATE.with(|s| s.borrow().clone());
    if let Some(state) = state {
        // Mark the thread as sleeping.
        *state.run_state.lock().unwrap() = ThreadRunState::Sleep;
        // Use condvar wait with timeout so the thread can be woken by kill.
        let killed = state.killed.lock().unwrap();
        if !*killed {
            let _guard = state.kill_notify.wait_timeout(killed, dur).unwrap().0;
            drop(_guard);
        }
        // Mark the thread as running again.
        *state.run_state.lock().unwrap() = ThreadRunState::Run;
    } else {
        // Main thread: just sleep normally.
        std::thread::sleep(dur);
    }
}

fn set_current_thread_state(state: Arc<ThreadState>) {
    CURRENT_THREAD_STATE.with(|s| *s.borrow_mut() = Some(state));
}

fn set_current_thread(val: Value) {
    CURRENT_THREAD.with(|c| c.set(val.id()));
}

fn get_current_thread(globals: &mut Globals) -> Value {
    let bits = CURRENT_THREAD.with(|c| c.get());
    if bits == 0 {
        main_thread_value(globals)
    } else {
        Value::from_u64(bits)
    }
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
    globals.define_builtin_class_func(THREAD_CLASS, "stop", thread_stop_class, 0);

    globals.define_builtin_func(THREAD_CLASS, "join", thread_join, 0);
    globals.define_builtin_func(THREAD_CLASS, "value", thread_value, 0);
    globals.define_builtin_func(THREAD_CLASS, "alive?", thread_alive, 0);
    globals.define_builtin_func(THREAD_CLASS, "stop?", thread_stop, 0);
    globals.define_builtin_func(THREAD_CLASS, "status", thread_status, 0);
    globals.define_builtin_func(THREAD_CLASS, "kill", thread_kill, 0);
    globals.define_builtin_func(THREAD_CLASS, "exit", thread_kill, 0);
    globals.define_builtin_func(THREAD_CLASS, "terminate", thread_kill, 0);
    globals.define_builtin_class_func(THREAD_CLASS, "kill", thread_kill_class, 1);
    globals.define_builtin_func(THREAD_CLASS, "[]", thread_aref, 1);
    globals.define_builtin_func(THREAD_CLASS, "[]=", thread_aset, 2);
    globals.define_builtin_func(THREAD_CLASS, "wakeup", thread_wakeup, 0);
    globals.define_builtin_func(THREAD_CLASS, "run", thread_wakeup, 0);
    globals.define_builtin_func(THREAD_CLASS, "report_on_exception=", thread_report_on_exception_set, 1);
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
        killed: Mutex::new(false),
        kill_notify: Condvar::new(),
        run_state: Mutex::new(ThreadRunState::Run),
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

    // Set the main thread's current thread value.
    set_current_thread(thread_val);

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
        killed: Mutex::new(false),
        kill_notify: Condvar::new(),
        run_state: Mutex::new(ThreadRunState::Run),
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
    let thread_val_bits = thread_val.id();

    let state_for_tls = state.clone();
    std::thread::spawn(move || {
        // Mark this as a spawned thread (disables JIT compilation).
        crate::codegen::set_spawned_thread();
        // Set the thread state for interruptible sleep.
        set_current_thread_state(state_for_tls);

        gvl_acquire();

        // Set the current thread object for Thread.current.
        set_current_thread(Value::from_u64(thread_val_bits));
        set_current_tid(thread_id);

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
        // Only set the result if it hasn't been set by kill.
        if matches!(*res, ThreadResult::Running) {
            match result {
                Some(val) => {
                    *res = ThreadResult::Ok(val.id());
                }
                None => {
                    let err = executor.take_error();
                    *res = ThreadResult::Err(err.kind.clone(), err.message.clone());
                }
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
    Ok(get_current_thread(globals))
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
            ThreadResult::Running => {
                let run_state = *state.run_state.lock().unwrap();
                match run_state {
                    ThreadRunState::Run => Ok(Value::string_from_str("run")),
                    ThreadRunState::Sleep => Ok(Value::string_from_str("sleep")),
                }
            }
            ThreadResult::Ok(_) => Ok(Value::bool(false)),
            ThreadResult::Err(..) => Ok(Value::nil()),
        }
    } else {
        Ok(Value::bool(false))
    }
}

/// Helper to kill a thread by its ID. Sets the result to Ok(nil) if still running,
/// and wakes the thread if it is sleeping.
fn do_kill_thread(thread_id: u64) {
    let table = THREAD_TABLE.lock().unwrap();
    if let Some(state) = table.get(&thread_id) {
        // Set the killed flag and notify to wake sleeping threads.
        {
            let mut killed = state.killed.lock().unwrap();
            *killed = true;
        }
        state.kill_notify.notify_all();

        // Set the result to nil (thread was killed).
        let mut result = state.result.lock().unwrap();
        if matches!(*result, ThreadResult::Running) {
            *result = ThreadResult::Ok(Value::nil().id());
        }
        drop(result);
        state.done.notify_all();
    }
}

/// Check if the current thread has been killed.
#[allow(dead_code)]
fn is_current_thread_killed() -> bool {
    CURRENT_THREAD_STATE.with(|s| {
        if let Some(state) = s.borrow().as_ref() {
            *state.killed.lock().unwrap()
        } else {
            false
        }
    })
}

/// Get the current thread's ID from thread-local storage.
fn current_thread_id() -> u64 {
    let bits = CURRENT_THREAD.with(|c| c.get());
    // 0 means main thread
    if bits == 0 {
        0
    } else {
        // We can't call get_thread_id without globals, so we use the thread-local ID.
        // Store it in a separate thread-local for simplicity.
        CURRENT_TID.with(|c| c.get())
    }
}

thread_local! {
    static CURRENT_TID: std::cell::Cell<u64> = const { std::cell::Cell::new(0) };
}

fn set_current_tid(id: u64) {
    CURRENT_TID.with(|c| c.set(id));
}

#[monoruby_builtin]
fn thread_kill(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let thread_id = get_thread_id(globals, self_val);

    if thread_id == 0 {
        // Killing the main thread exits the process.
        std::process::exit(0);
    }

    do_kill_thread(thread_id);

    // If killing the current thread (self-kill), abort execution.
    if thread_id == current_thread_id() {
        return Err(MonorubyErr::runtimeerr("thread killed"));
    }

    Ok(self_val)
}

///
/// ### Thread.kill(thread)
///
#[monoruby_builtin]
fn thread_kill_class(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let thread_val = lfp.arg(0);
    let thread_id = get_thread_id(globals, thread_val);

    if thread_id == 0 {
        std::process::exit(0);
    }

    do_kill_thread(thread_id);
    Ok(thread_val)
}

///
/// ### Thread.stop
///
/// Suspends the current thread until woken by Thread#wakeup or Thread#run.
///
#[monoruby_builtin]
fn thread_stop_class(
    _vm: &mut Executor,
    _globals: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    crate::executor::gvl::without_gvl(|| {
        thread_sleep(std::time::Duration::from_secs(u64::MAX));
    });
    Ok(Value::nil())
}

///
/// ### Thread#wakeup / Thread#run
///
/// Wakes a sleeping thread.
///
#[monoruby_builtin]
fn thread_wakeup(
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
        if !matches!(*result, ThreadResult::Running) {
            return Err(MonorubyErr::threaderror("killed thread"));
        }
        drop(result);
        // Wake the thread from sleep.
        state.kill_notify.notify_all();
    } else {
        return Err(MonorubyErr::threaderror("killed thread"));
    }

    Ok(self_val)
}

/// Thread#report_on_exception= (no-op stub)
#[monoruby_builtin]
fn thread_report_on_exception_set(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(lfp.arg(0))
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
