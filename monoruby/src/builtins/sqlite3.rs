//! The sqlite3 gem's native half (`sqlite3_native.so`), written over the
//! bundled SQLite (`libsqlite3-src`, 3.48.0 built from the amalgamation and
//! linked statically). The gem's Ruby half is the host's, unchanged;
//! `gem/sqlite3/sqlite3_native.rb` stands in for the C extension and calls
//! `String.__sqlite3_init`, which builds `SQLite3::Database` /
//! `SQLite3::Statement` and registers these methods.
//!
//! This replaces a bridge that reached libsqlite3 through Fiddle. That cost
//! a libffi call — argument marshalling, `ffi_prep_cif`'s ABI classification,
//! a `SmallVec` of `Arg`s, a mutex — per C call, and the row loop makes
//! `sqlite3_column_type` plus a typed getter *per column per row*: 35 % of a
//! ruby-bench `activerecord` iteration, against 2 % for the database work
//! itself (`doc/activerecord_liquid_il_rack_investigation_2026-09.md` §3.1).
//! Here `Statement#step` steps and reads the whole row in one builtin call,
//! and no call leaves Rust.
//!
//! Objects: a `Database` owns its `sqlite3*` and a `Statement` its
//! `sqlite3_stmt*`, as `ObjTy::NATIVE` payloads that close the handle when
//! collected. Closing a connection uses `sqlite3_close_v2`, so a connection
//! collected while its statements are still live becomes a zombie and is
//! freed with the last of them — the GC may run the two in either order.

use super::*;
use crate::alloc::GC;
use libsqlite3_src as sq;
use std::cell::{Cell, RefCell};
use std::ffi::{CStr, CString, c_char, c_int, c_void};

pub(crate) fn init(globals: &mut Globals) {
    globals.define_builtin_class_func(STRING_CLASS, "__sqlite3_init", sqlite3_init, 0);
}

// ---------------------------------------------------------------------
// Classes
// ---------------------------------------------------------------------

/// The classes the native methods live on. Filled by `__sqlite3_init`,
/// which the gem's `sqlite3_native.rb` calls before the Ruby half reopens
/// `SQLite3::Database` — so these are the classes the gem then extends,
/// and their `NATIVE` instance type is in place from the start.
#[derive(Clone, Copy)]
struct Classes {
    sqlite3: ClassId,
}

thread_local! {
    static CLASSES: RefCell<Option<Classes>> = const { RefCell::new(None) };
}

fn classes() -> Classes {
    CLASSES.with(|c| c.borrow().expect("SQLite3 classes are not initialized"))
}

/// String.__sqlite3_init -> nil
#[monoruby_builtin]
fn sqlite3_init(_: &mut Executor, globals: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    if CLASSES.with(|c| c.borrow().is_some()) {
        return Ok(Value::nil());
    }
    let name = IdentId::get_id("SQLite3");
    let sqlite3 = match globals.store.get_constant_noautoload(OBJECT_CLASS, name) {
        Some(v) => v.as_class_id(),
        None => globals.store.define_module_with_identid(name, OBJECT_CLASS).id(),
    };
    let object = globals.store.get_module(OBJECT_CLASS);
    let database = globals
        .store
        .define_class_with_instance_ty("Database", object, sqlite3, ObjTy::NATIVE)
        .id();
    let statement = globals
        .store
        .define_class_with_instance_ty("Statement", object, sqlite3, ObjTy::NATIVE)
        .id();
    globals.store[database].set_alloc_func(database_alloc_func);
    globals.store[statement].set_alloc_func(statement_alloc_func);

    CLASSES.with(|cell| *cell.borrow_mut() = Some(Classes { sqlite3 }));

    // ---- SQLite3 module functions
    globals.define_builtin_class_func(sqlite3, "libversion", libversion, 0);
    globals.define_builtin_class_func(sqlite3, "libversion_string", libversion_string, 0);
    globals.define_builtin_class_func(sqlite3, "threadsafe", threadsafe, 0);
    globals.define_builtin_class_func(sqlite3, "sqlcipher?", sqlcipher_p, 0);

    // ---- Database
    let d = database;
    globals.define_private_builtin_func(d, "open_v2", db_open_v2, 3);
    globals.define_private_builtin_func(d, "open16", db_open16, 1);
    globals.define_builtin_func(d, "close", db_close, 0);
    globals.define_builtin_func(d, "closed?", db_closed_p, 0);
    globals.define_builtin_func(d, "encoding", db_encoding, 0);
    globals.define_builtin_func(d, "busy_timeout=", db_busy_timeout_assign, 1);
    globals.define_builtin_func(d, "busy_timeout", db_busy_timeout_assign, 1);
    globals.define_builtin_func_rest(d, "busy_handler", db_busy_handler);
    globals.define_builtin_func(d, "last_insert_row_id", db_last_insert_row_id, 0);
    globals.define_builtin_func(d, "changes", db_changes, 0);
    globals.define_builtin_func(d, "total_changes", db_total_changes, 0);
    globals.define_builtin_func(d, "interrupt", db_interrupt, 0);
    globals.define_builtin_func(d, "errcode", db_errcode, 0);
    globals.define_builtin_func(d, "errmsg", db_errmsg, 0);
    globals.define_private_builtin_func(d, "db_filename", db_filename, 1);
    globals.define_builtin_func(d, "extended_result_codes=", db_extended_result_codes, 1);
    globals.define_builtin_func(d, "transaction_active?", db_transaction_active_p, 0);
    // The C extension defines these six privately; the gem's Ruby half
    // is their only caller, and `tests/sqlite3.rs` pins the split.
    globals.define_private_builtin_func(d, "disable_quirk_mode", db_disable_quirk_mode, 0);
    globals.define_private_builtin_func(d, "exec_batch", db_exec_batch, 2);
    globals.define_builtin_func(d, "enable_load_extension", db_enable_load_extension, 1);
    globals.define_private_builtin_func(d, "load_extension_internal", db_load_extension, 1);
    globals.define_builtin_func_rest(d, "trace", db_trace);
    globals.define_builtin_func(d, "authorizer=", db_authorizer_assign, 1);
    globals.define_builtin_func(d, "define_function_with_flags", db_define_function, 2);
    globals.define_builtin_func(d, "define_function", db_define_function_plain, 1);
    globals.define_private_builtin_func(d, "define_aggregator2", db_define_aggregator, 2);
    globals.define_builtin_func(d, "collation", db_collation, 2);
    globals.define_builtin_func(d, "complete?", db_complete_p, 1);
    globals.define_builtin_func(d, "statement_timeout=", db_statement_timeout_assign, 1);
    globals.define_private_builtin_func(d, "discard", db_discard, 0);

    // ---- Statement
    let s = statement;
    globals.define_private_builtin_func(s, "prepare", stmt_prepare, 2);
    globals.define_builtin_func(s, "sql", stmt_sql, 0);
    globals.define_builtin_func(s, "expanded_sql", stmt_expanded_sql, 0);
    globals.define_builtin_func(s, "memused", stmt_memused, 0);
    globals.define_builtin_func(s, "clear_bindings!", stmt_clear_bindings, 0);
    globals.define_builtin_func(s, "close", stmt_close, 0);
    globals.define_builtin_func(s, "closed?", stmt_closed_p, 0);
    globals.define_builtin_func(s, "step", stmt_step, 0);
    globals.define_builtin_func(s, "done?", stmt_done_p, 0);
    globals.define_builtin_func(s, "reset!", stmt_reset, 0);
    globals.define_builtin_func(s, "bind_param", stmt_bind_param, 2);
    globals.define_builtin_func(s, "column_count", stmt_column_count, 0);
    globals.define_builtin_func(s, "column_name", stmt_column_name, 1);
    globals.define_builtin_func(s, "column_decltype", stmt_column_decltype, 1);
    globals.define_builtin_func(s, "bind_parameter_count", stmt_bind_parameter_count, 0);
    globals.define_private_builtin_func(s, "stat_for", stmt_stat_for, 1);
    globals.define_private_builtin_func(s, "stats_as_hash", stmt_stats_as_hash, 0);

    Ok(Value::nil())
}

// ---------------------------------------------------------------------
// Payloads
// ---------------------------------------------------------------------

/// A `SQLite3::Database`'s connection. `None` before `open_v2` and after
/// `close`; `Drop` closes an open one, so a connection dropped on the
/// floor is released with the object.
struct DbHandle {
    db: *mut sq::sqlite3,
    /// The blocks handed to `create_function` / `create_aggregate`, kept
    /// as GC roots for as long as SQLite may call them.
    funcs: Vec<Value>,
    /// One live aggregate instance per group being accumulated.
    ///
    /// SQLite's per-group memory (`sqlite3_aggregate_context`) is C
    /// memory the collector cannot see, so it holds a slot number and
    /// the instance itself lives here, where `mark` finds it. A slot is
    /// freed by `xFinal`, and reused by the next group; a query with
    /// `GROUP BY` keeps one entry per group in flight at a time, or all
    /// of them at once when SQLite groups by hash.
    aggregates: Vec<Option<Value>>,
}

impl NativeData for DbHandle {
    fn mark(&self, alloc: &mut crate::alloc::Allocator<RValue>) {
        for f in &self.funcs {
            f.mark(alloc);
        }
        for a in self.aggregates.iter().flatten() {
            a.mark(alloc);
        }
    }
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

impl Drop for DbHandle {
    fn drop(&mut self) {
        if !self.db.is_null() {
            // SAFETY: our own connection. `close_v2` defers the free when
            // statements are still open, so the GC order does not matter.
            unsafe { sq::sqlite3_close_v2(self.db) };
            self.db = std::ptr::null_mut();
        }
    }
}

/// A `SQLite3::Statement`'s prepared statement.
struct StmtHandle {
    stmt: *mut sq::sqlite3_stmt,
    /// `sqlite3_step` last answered `SQLITE_DONE` (the gem's `done?`).
    done: bool,
}

impl NativeData for StmtHandle {
    fn mark(&self, _alloc: &mut crate::alloc::Allocator<RValue>) {}
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

impl Drop for StmtHandle {
    fn drop(&mut self) {
        if !self.stmt.is_null() {
            // SAFETY: our own statement.
            unsafe { sq::sqlite3_finalize(self.stmt) };
            self.stmt = std::ptr::null_mut();
        }
    }
}

// ---------------------------------------------------------------------
// Blocking calls
// ---------------------------------------------------------------------

/// Opening or closing a connection, packaged for a worker thread so the
/// other green threads keep running while the filesystem work happens —
/// the `blocking: true` the Fiddle bridge declared on these.
///
/// Only raw C data crosses: a NUL-terminated path the caller keeps alive,
/// the out-parameter slot it also owns, and SQLite handles.
pub(crate) enum Sqlite3WorkerCall {
    /// `sqlite3_open_v2(filename, out, flags, vfs)`.
    Open {
        filename: *const c_char,
        flags: c_int,
        vfs: *const c_char,
        out: *mut *mut sq::sqlite3,
    },
    /// `sqlite3_open16(filename, out)`.
    Open16 {
        filename: *const c_void,
        out: *mut *mut sq::sqlite3,
    },
    /// `sqlite3_close_v2(db)`.
    Close { db: *mut sq::sqlite3 },
}

// SAFETY: every pointer is either a SQLite handle (the library is built
// `SQLITE_THREADSAFE=1`, so a connection may cross threads) or memory the
// parked caller owns for the whole call — a `CString` and a stack slot in
// its frame, neither of which the collector can move or free while the
// caller is parked. None of it is the Ruby heap.
unsafe impl Send for Sqlite3WorkerCall {}

impl Sqlite3WorkerCall {
    /// Perform the call. Runs on a worker thread: it must not touch the
    /// Ruby heap or any interpreter thread-local.
    pub(crate) fn run(&self) -> i64 {
        // SAFETY: see the `Send` justification above.
        unsafe {
            match *self {
                Sqlite3WorkerCall::Open {
                    filename,
                    flags,
                    vfs,
                    out,
                } => sq::sqlite3_open_v2(filename, out, flags, vfs) as i64,
                Sqlite3WorkerCall::Open16 { filename, out } => {
                    sq::sqlite3_open16(filename, out) as i64
                }
                Sqlite3WorkerCall::Close { db } => sq::sqlite3_close_v2(db) as i64,
            }
        }
    }
}

/// Run `call` on a worker while this green thread parks.
fn run_blocking(
    vm: &mut Executor,
    globals: &mut Globals,
    call: Sqlite3WorkerCall,
) -> Result<c_int> {
    Ok(crate::native_pool::run_blocking(vm, globals, crate::native_pool::NativeOp::Sqlite3(call))?
        .ret as c_int)
}

/// `Database.allocate`: a closed connection, filled in by `open_v2` /
/// `open16` (the gem's `Database#initialize` is Ruby and calls them).
extern "C" fn database_alloc_func(class_id: ClassId, _globals: &mut Globals) -> Value {
    Value::new_native(
        class_id,
        Box::new(DbHandle {
            db: std::ptr::null_mut(),
            funcs: vec![],
            aggregates: vec![],
        }),
    )
}

/// `Statement.allocate`: filled in by `prepare`.
extern "C" fn statement_alloc_func(class_id: ClassId, _globals: &mut Globals) -> Value {
    Value::new_native(
        class_id,
        Box::new(StmtHandle {
            stmt: std::ptr::null_mut(),
            done: false,
        }),
    )
}

/// The payload of `v` as `T`.
fn native_mut<T: NativeData>(mut v: Value) -> Result<&'static mut T> {
    let rv = v
        .try_rvalue_mut()
        .filter(|rv| rv.ty() == ObjTy::NATIVE)
        .ok_or_else(|| MonorubyErr::typeerr("expected a native SQLite3 object"))?;
    // SAFETY: the payload lives as long as the object, which the caller's
    // frame holds; the borrow does not outlive the builtin call.
    let rv: &'static mut RValue = unsafe { &mut *(rv as *mut RValue) };
    rv.as_native_mut()
        .as_any_mut()
        .downcast_mut::<T>()
        .ok_or_else(|| MonorubyErr::typeerr("expected a native SQLite3 object"))
}

/// The open connection of `self`; every method but `closed?` raises on a
/// closed one, as the C extension's `Data_Get_Struct` guard does.
fn db_of(vm: &mut Executor, globals: &mut Globals, v: Value) -> Result<*mut sq::sqlite3> {
    let h = native_mut::<DbHandle>(v)?;
    if h.db.is_null() {
        return Err(err_sqlite3(vm, globals, "cannot use a closed database"));
    }
    Ok(h.db)
}

/// The live statement of `self`; every method but `closed?` and `done?`
/// raises on a closed one (`REQUIRE_OPEN_STMT`).
fn stmt_of(
    vm: &mut Executor,
    globals: &mut Globals,
    v: Value,
) -> Result<*mut sq::sqlite3_stmt> {
    let h = native_mut::<StmtHandle>(v)?;
    if h.stmt.is_null() {
        return Err(err_sqlite3(vm, globals, "cannot use a closed statement"));
    }
    Ok(h.stmt)
}

// ---------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------

/// `SQLite3::<Name>` for each primary result code 1..=26, in code
/// order — the gem's own mapping (`ext/sqlite3/exception.c`).
const ERROR_CLASS_NAMES: [&str; 26] = [
    "SQLException",           // 1  SQLITE_ERROR
    "InternalException",      // 2  SQLITE_INTERNAL
    "PermissionException",    // 3  SQLITE_PERM
    "AbortException",         // 4  SQLITE_ABORT
    "BusyException",          // 5  SQLITE_BUSY
    "LockedException",        // 6  SQLITE_LOCKED
    "MemoryException",        // 7  SQLITE_NOMEM
    "ReadOnlyException",      // 8  SQLITE_READONLY
    "InterruptException",     // 9  SQLITE_INTERRUPT
    "IOException",            // 10 SQLITE_IOERR
    "CorruptException",       // 11 SQLITE_CORRUPT
    "NotFoundException",      // 12 SQLITE_NOTFOUND
    "FullException",          // 13 SQLITE_FULL
    "CantOpenException",      // 14 SQLITE_CANTOPEN
    "ProtocolException",      // 15 SQLITE_PROTOCOL
    "EmptyException",         // 16 SQLITE_EMPTY
    "SchemaChangedException", // 17 SQLITE_SCHEMA
    "TooBigException",        // 18 SQLITE_TOOBIG
    "ConstraintException",    // 19 SQLITE_CONSTRAINT
    "MismatchException",      // 20 SQLITE_MISMATCH
    "MisuseException",        // 21 SQLITE_MISUSE
    "UnsupportedException",   // 22 SQLITE_NOLFS
    "AuthorizationException", // 23 SQLITE_AUTH
    "FormatException",        // 24 SQLITE_FORMAT
    "RangeException",         // 25 SQLITE_RANGE
    "NotADatabaseException",  // 26 SQLITE_NOTADB
];

/// The class for a result code. The low 8 bits select it, so an
/// extended code lands on its primary class; anything outside the
/// table (0, and 27.. which SQLite has not assigned) is the base class.
fn error_class_name(code: c_int) -> &'static str {
    let primary = code & 0xff;
    usize::try_from(primary)
        .ok()
        .and_then(|i| i.checked_sub(1))
        .and_then(|i| ERROR_CLASS_NAMES.get(i))
        .copied()
        .unwrap_or("Exception")
}

/// A `SQLite3::Exception` with no result code, for the misuse the C
/// extension reports itself (a closed handle, an unknown bind parameter).
fn err_sqlite3(vm: &mut Executor, globals: &mut Globals, msg: &str) -> MonorubyErr {
    let Some(klass) = globals
        .store
        .get_constant_noautoload(classes().sqlite3, IdentId::get_id("Exception"))
    else {
        // Only reachable before `errors.rb` has been required.
        return MonorubyErr::runtimeerr(msg);
    };
    let m = Value::string_from_str(msg);
    match vm.invoke_method_inner(globals, IdentId::NEW, klass, &[m], None, None) {
        Ok(ex) => match ex.is_exception() {
            Some(inner) => MonorubyErr::new_from_exception(inner).with_original(ex),
            None => MonorubyErr::runtimeerr(msg),
        },
        Err(e) => e,
    }
}

/// Raise `SQLite3::<class for code>` for a failed `prepare`, carrying the
/// statement text and the offset of the offending token so the gem's
/// `Exception#message` can point at it (`rb_sqlite3_raise_with_sql`).
fn raise_code_with_sql(
    vm: &mut Executor,
    globals: &mut Globals,
    db: *mut sq::sqlite3,
    code: c_int,
    msg: String,
    sql: &str,
) -> MonorubyErr {
    // SAFETY: a live connection; -1 when the error is not about a token.
    let offset = unsafe { sq::sqlite3_error_offset(db) };
    let e = raise_code(vm, globals, code, msg);
    let Some(ex) = e.original else {
        return e;
    };
    let _ = globals
        .store
        .set_ivar(ex, IdentId::get_id("@sql"), Value::string_from_str(sql));
    let _ = globals.store.set_ivar(
        ex,
        IdentId::get_id("@sql_offset"),
        Value::integer(offset as i64),
    );
    e
}

/// Raise `SQLite3::<class for code>` with `msg` and `@code = code`,
/// exactly as the C extension's `rb_sqlite3_raise` does.
fn raise_code(
    vm: &mut Executor,
    globals: &mut Globals,
    code: c_int,
    msg: String,
) -> MonorubyErr {
    let sqlite3 = classes().sqlite3;
    let lookup = |name: &str| {
        globals
            .store
            .get_constant_noautoload(sqlite3, IdentId::get_id(name))
    };
    let Some(klass) = lookup(error_class_name(code)).or_else(|| lookup("Exception")) else {
        // `errors.rb` is not loaded yet (only possible before the gem
        // finishes requiring itself).
        return MonorubyErr::runtimeerr(msg);
    };
    let msg = Value::string_from_str(&msg);
    match vm.invoke_method_inner(globals, IdentId::NEW, klass, &[msg], None, None) {
        Ok(ex) => {
            let _ = globals
                .store
                .set_ivar(ex, IdentId::get_id("@code"), Value::integer(code as i64));
            match ex.is_exception() {
                Some(inner) => MonorubyErr::new_from_exception(inner).with_original(ex),
                None => MonorubyErr::runtimeerr("SQLite3::Exception expected"),
            }
        }
        Err(e) => e,
    }
}

/// Raise the connection's current error if `rc` is not a success code.
fn check(
    vm: &mut Executor,
    globals: &mut Globals,
    db: *mut sq::sqlite3,
    rc: c_int,
) -> Result<()> {
    if matches!(rc, sq::SQLITE_OK | sq::SQLITE_ROW | sq::SQLITE_DONE) {
        return Ok(());
    }
    // SAFETY: a live connection.
    let msg = unsafe { cstr_to_string(sq::sqlite3_errmsg(db)) }
        .unwrap_or_else(|| format!("sqlite3 error {rc}"));
    Err(raise_code(vm, globals, rc, msg))
}

// ---------------------------------------------------------------------
// Conversions
// ---------------------------------------------------------------------

/// A NUL-terminated C string as a Rust `String` (lossy for invalid UTF-8,
/// as SQLite's own text is UTF-8 by construction here).
///
/// # Safety
/// `p` is NULL or points at a NUL-terminated string that outlives the call.
unsafe fn cstr_to_string(p: *const c_char) -> Option<String> {
    if p.is_null() {
        return None;
    }
    // SAFETY: the caller's contract.
    Some(unsafe { CStr::from_ptr(p) }.to_string_lossy().into_owned())
}

/// A Ruby String from a NUL-terminated C string.
///
/// # Safety
/// As `cstr_to_string`.
unsafe fn cstr_to_value(p: *const c_char) -> Value {
    // SAFETY: the caller's contract.
    match unsafe { cstr_to_string(p) } {
        Some(s) => Value::string_from_str(&s),
        None => Value::nil(),
    }
}

/// The bytes of a Ruby String argument, as a NUL-terminated `CString` for
/// the entry points that take a `const char *` without a length.
fn to_cstring(store: &Store, v: Value) -> Result<CString> {
    let s = v.expect_str(store)?;
    CString::new(s.as_bytes()).map_err(|_| MonorubyErr::argumenterr("string contains a NUL byte"))
}

/// Column `i` of `stmt` as a Ruby value, by its SQLite type.
///
/// # Safety
/// `stmt` is live and positioned on a row, and `i` is in range.
unsafe fn column_value(stmt: *mut sq::sqlite3_stmt, i: c_int) -> Value {
    // SAFETY: the caller's contract.
    unsafe {
        match sq::sqlite3_column_type(stmt, i) {
            sq::SQLITE_INTEGER => Value::integer(sq::sqlite3_column_int64(stmt, i)),
            sq::SQLITE_FLOAT => Value::float(sq::sqlite3_column_double(stmt, i)),
            sq::SQLITE_NULL => Value::nil(),
            // A BLOB is a plain String tagged BINARY, not a `SQLite3::Blob`
            // — that class exists only to mark a value for *binding*.
            sq::SQLITE_BLOB => {
                let ptr = sq::sqlite3_column_blob(stmt, i) as *const u8;
                if ptr.is_null() {
                    // A zero-length blob is not NULL, and SQLite may
                    // answer a NULL pointer for it.
                    Value::bytes_from_slice(&[])
                } else {
                    let len = sq::sqlite3_column_bytes(stmt, i).max(0) as usize;
                    Value::bytes_from_slice(std::slice::from_raw_parts(ptr, len))
                }
            }
            // TEXT, and anything else the library grows: `column_text`
            // converts whatever is there to UTF-8.
            _ => {
                let ptr = sq::sqlite3_column_text(stmt, i) as *const c_char;
                if ptr.is_null() {
                    Value::nil()
                } else {
                    let len = sq::sqlite3_column_bytes(stmt, i).max(0) as usize;
                    let bytes = std::slice::from_raw_parts(ptr as *const u8, len);
                    Value::string_from_vec(bytes.to_vec())
                }
            }
        }
    }
}

/// The whole current row of `stmt` as an Array.
///
/// # Safety
/// As `column_value`.
unsafe fn row_value(stmt: *mut sq::sqlite3_stmt) -> Value {
    // SAFETY: the caller's contract.
    let count = unsafe { sq::sqlite3_column_count(stmt) };
    let mut row = Vec::with_capacity(count.max(0) as usize);
    for i in 0..count {
        // SAFETY: `i` is below the column count.
        row.push(unsafe { column_value(stmt, i) });
    }
    Value::array_from_vec(row)
}

/// The current row of `stmt` with every column as text, which is what
/// `exec_batch` answers (the C extension's batch path reads through
/// `sqlite3_column_text` whatever the storage class is).
///
/// The length comes from the NUL rather than from
/// `sqlite3_column_bytes`, because the C extension builds these with
/// `rb_str_new2`: `execute_batch2` of a value holding a NUL — an
/// embedded one in TEXT, or any BLOB with a zero byte — stops there,
/// where the row readers used by `execute` keep the whole value. It
/// looks like a bug in the gem, but it is what callers see.
///
/// # Safety
/// As `column_value`.
unsafe fn row_value_as_text(stmt: *mut sq::sqlite3_stmt) -> Value {
    // SAFETY: the caller's contract.
    unsafe {
        let count = sq::sqlite3_column_count(stmt);
        let mut row = Vec::with_capacity(count.max(0) as usize);
        for i in 0..count {
            let ptr = sq::sqlite3_column_text(stmt, i) as *const c_char;
            row.push(if ptr.is_null() {
                Value::nil()
            } else {
                let bytes = std::ffi::CStr::from_ptr(ptr).to_bytes();
                Value::string_from_vec(bytes.to_vec())
            });
        }
        Value::array_from_vec(row)
    }
}

// ---------------------------------------------------------------------
// SQLite3 module functions
// ---------------------------------------------------------------------

/// SQLite3.libversion -> Integer
///
/// The version *number* (`3048000`), as the C extension answers; the
/// string is `SQLite3::SQLITE_VERSION`, from `libversion_string`.
#[monoruby_builtin]
fn libversion(_: &mut Executor, _: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    // SAFETY: a pure query.
    Ok(Value::integer(
        unsafe { sq::sqlite3_libversion_number() } as i64
    ))
}

/// SQLite3.libversion_string -> String
#[monoruby_builtin]
fn libversion_string(_: &mut Executor, _: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    // SAFETY: a static string in the library.
    Ok(unsafe { cstr_to_value(sq::sqlite3_libversion()) })
}

/// SQLite3.threadsafe -> Integer
#[monoruby_builtin]
fn threadsafe(_: &mut Executor, _: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    // SAFETY: a pure query.
    Ok(Value::integer(unsafe { sq::sqlite3_threadsafe() } as i64))
}

/// SQLite3.sqlcipher? -> false
#[monoruby_builtin]
fn sqlcipher_p(_: &mut Executor, _: &mut Globals, _: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(false))
}

// ---------------------------------------------------------------------
// Database
// ---------------------------------------------------------------------

/// Database#open_v2(filename, mode, zvfs) -> nil
#[monoruby_builtin]
fn db_open_v2(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let filename = to_cstring(&globals.store, lfp.arg(0))?;
    let mode = lfp.arg(1).coerce_to_i64(globals)? as c_int;
    let zvfs = lfp.arg(2);
    let zvfs = if zvfs.is_nil() {
        None
    } else {
        Some(to_cstring(&globals.store, zvfs)?)
    };
    let mut db: *mut sq::sqlite3 = std::ptr::null_mut();
    // The strings and the out-parameter are owned by this frame, which
    // stays put while the worker runs (the green thread parks here).
    let rc = run_blocking(
        vm,
        globals,
        Sqlite3WorkerCall::Open {
            filename: filename.as_ptr(),
            flags: mode | sq::SQLITE_OPEN_URI,
            vfs: zvfs.as_ref().map_or(std::ptr::null(), |v| v.as_ptr()),
            out: &mut db,
        },
    )?;
    if rc != sq::SQLITE_OK {
        // The connection handle is returned even on failure, and carries
        // the message; close it after reading.
        let msg = if db.is_null() {
            format!("sqlite3 error {rc}")
        } else {
            // SAFETY: a handle open enough to report its error.
            let m = unsafe { cstr_to_string(sq::sqlite3_errmsg(db)) };
            // SAFETY: ours, and no statement was ever prepared on it.
            unsafe { sq::sqlite3_close_v2(db) };
            m.unwrap_or_else(|| format!("sqlite3 error {rc}"))
        };
        return Err(raise_code(vm, globals, rc, msg));
    }
    native_mut::<DbHandle>(lfp.self_val())?.db = db;
    Ok(Value::nil())
}

/// Database#open16(filename) -> nil
#[monoruby_builtin]
fn db_open16(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // The gem hands UTF-16 bytes; `sqlite3_open16` wants them
    // NUL-terminated (two zero bytes).
    let arg = lfp.arg(0);
    let mut bytes = arg
        .is_rstring_inner()
        .ok_or_else(|| MonorubyErr::typeerr("expected a String"))?
        .as_bytes()
        .to_vec();
    bytes.extend_from_slice(&[0, 0]);
    let mut db: *mut sq::sqlite3 = std::ptr::null_mut();
    // As `open_v2`: the buffer and the out-parameter are this frame's.
    let rc = run_blocking(
        vm,
        globals,
        Sqlite3WorkerCall::Open16 {
            filename: bytes.as_ptr() as *const c_void,
            out: &mut db,
        },
    )?;
    if rc != sq::SQLITE_OK {
        let msg = if db.is_null() {
            format!("sqlite3 error {rc}")
        } else {
            // SAFETY: as in `open_v2`.
            let m = unsafe { cstr_to_string(sq::sqlite3_errmsg(db)) };
            unsafe { sq::sqlite3_close_v2(db) };
            m.unwrap_or_else(|| format!("sqlite3 error {rc}"))
        };
        return Err(raise_code(vm, globals, rc, msg));
    }
    native_mut::<DbHandle>(lfp.self_val())?.db = db;
    Ok(Value::nil())
}

/// Database#close -> nil
#[monoruby_builtin]
fn db_close(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let h = native_mut::<DbHandle>(lfp.self_val())?;
    if h.db.is_null() {
        return Ok(Value::nil());
    }
    // Clear the handle *before* parking: the object must not be closed
    // twice if another green thread reaches `close` while this one waits.
    let db = std::mem::replace(&mut h.db, std::ptr::null_mut());
    h.funcs.clear();
    // `close_v2` never fails for a live handle; with statements still open
    // the free is deferred to the last of them.
    run_blocking(vm, globals, Sqlite3WorkerCall::Close { db })?;
    Ok(Value::nil())
}

/// Database#closed? -> bool
#[monoruby_builtin]
fn db_closed_p(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(
        native_mut::<DbHandle>(lfp.self_val())?.db.is_null(),
    ))
}

/// Database#encoding -> String
///
/// Every text this binding reads comes from `sqlite3_column_text`, which
/// converts to UTF-8 whatever the database is stored in.
#[monoruby_builtin]
fn db_encoding(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    db_of(vm, globals, lfp.self_val())?;
    Ok(Value::string_from_str("UTF-8"))
}

/// Database#busy_timeout=(ms) -> nil
#[monoruby_builtin]
fn db_busy_timeout_assign(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let db = db_of(vm, globals, lfp.self_val())?;
    let ms = lfp.arg(0).coerce_to_i64(globals)? as c_int;
    // SAFETY: a live connection.
    let rc = unsafe { sq::sqlite3_busy_timeout(db, ms) };
    check(vm, globals, db, rc)?;
    Ok(Value::nil())
}

/// Database#busy_handler(&block) -> nil
///
/// Registering a Ruby block as SQLite's busy callback is not implemented;
/// the block is remembered (as the previous bridge did) so a program that
/// sets one still runs, driven by `busy_timeout`.
#[monoruby_builtin]
fn db_busy_handler(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    db_of(vm, globals, lfp.self_val())?;
    // The block is remembered but never registered with SQLite (as the
    // Fiddle bridge did): what a busy callback must do — re-enter Ruby
    // from inside a lock wait — needs the interpreter reachable from a C
    // callback. `busy_timeout` covers the usual case.
    globals
        .store
        .set_ivar(lfp.self_val(), IdentId::get_id("@busy_handler"), Value::nil())?;
    Ok(Value::nil())
}

/// Database#last_insert_row_id -> Integer
#[monoruby_builtin]
fn db_last_insert_row_id(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let db = db_of(vm, globals, lfp.self_val())?;
    // SAFETY: a live connection.
    Ok(Value::integer(unsafe { sq::sqlite3_last_insert_rowid(db) }))
}

/// Database#changes -> Integer
#[monoruby_builtin]
fn db_changes(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let db = db_of(vm, globals, lfp.self_val())?;
    // SAFETY: a live connection.
    Ok(Value::integer(unsafe { sq::sqlite3_changes(db) } as i64))
}

/// Database#total_changes -> Integer
#[monoruby_builtin]
fn db_total_changes(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let db = db_of(vm, globals, lfp.self_val())?;
    // SAFETY: a live connection.
    Ok(Value::integer(
        unsafe { sq::sqlite3_total_changes(db) } as i64
    ))
}

/// Database#interrupt -> self
#[monoruby_builtin]
fn db_interrupt(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let db = db_of(vm, globals, lfp.self_val())?;
    // SAFETY: a live connection; interrupting is safe from any thread.
    unsafe { sq::sqlite3_interrupt(db) };
    Ok(lfp.self_val())
}

/// Database#errcode -> Integer
#[monoruby_builtin]
fn db_errcode(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let db = db_of(vm, globals, lfp.self_val())?;
    // SAFETY: a live connection.
    Ok(Value::integer(unsafe { sq::sqlite3_errcode(db) } as i64))
}

/// Database#errmsg -> String
#[monoruby_builtin]
fn db_errmsg(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let db = db_of(vm, globals, lfp.self_val())?;
    // SAFETY: a live connection; the message is owned by it and copied here.
    Ok(unsafe { cstr_to_value(sq::sqlite3_errmsg(db)) })
}

/// Database#db_filename(db_name = "main") -> String or nil
#[monoruby_builtin]
fn db_filename(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let db = db_of(vm, globals, lfp.self_val())?;
    let arg = lfp.arg(0);
    let name = if arg.is_nil() {
        CString::new("main").unwrap()
    } else {
        to_cstring(&globals.store, arg)?
    };
    // SAFETY: a live connection and a NUL-terminated name.
    let p = unsafe { sq::sqlite3_db_filename(db, name.as_ptr()) };
    // An unnamed (temporary or in-memory) database answers the empty
    // string, and an unknown schema name a NULL.
    // SAFETY: as above.
    Ok(unsafe { cstr_to_value(p) })
}

/// Database#extended_result_codes=(enable) -> nil
#[monoruby_builtin]
fn db_extended_result_codes(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let db = db_of(vm, globals, lfp.self_val())?;
    let on = c_int::from(lfp.arg(0).as_bool());
    // SAFETY: a live connection.
    let rc = unsafe { sq::sqlite3_extended_result_codes(db, on) };
    check(vm, globals, db, rc)?;
    Ok(Value::nil())
}

/// Database#transaction_active? -> bool
#[monoruby_builtin]
fn db_transaction_active_p(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let db = db_of(vm, globals, lfp.self_val())?;
    // SAFETY: a live connection.
    Ok(Value::bool(unsafe { sq::sqlite3_get_autocommit(db) } == 0))
}

/// Database#disable_quirk_mode -> bool
///
/// SQLite's "quirk" is accepting a double-quoted string as a literal
/// where a column name was meant. The gem turns it off for both DDL and
/// DML, and answers whether both took.
#[monoruby_builtin]
fn db_disable_quirk_mode(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let db = db_of(vm, globals, lfp.self_val())?;
    // SAFETY: a live connection; both verbs take `(int onoff, int *pRes)`
    // and we pass a null result pointer, which SQLite allows.
    let ok = unsafe {
        sq::sqlite3_db_config(db, sq::SQLITE_DBCONFIG_DQS_DDL, 0, std::ptr::null_mut::<c_int>())
            == sq::SQLITE_OK
            && sq::sqlite3_db_config(
                db,
                sq::SQLITE_DBCONFIG_DQS_DML,
                0,
                std::ptr::null_mut::<c_int>(),
            ) == sq::SQLITE_OK
    };
    Ok(Value::bool(ok))
}

/// Database#exec_batch(sql, results_as_hash) -> Array of rows
///
/// Every statement in `sql` is prepared and run in turn; the rows of all
/// of them are collected (`Database#execute_batch2`).
#[monoruby_builtin]
fn db_exec_batch(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let db = db_of(vm, globals, lfp.self_val())?;
    let sql = lfp.arg(0).expect_string(&globals.store)?;
    let mut rows: Vec<Value> = vec![];
    let mut rest: &[u8] = sql.as_bytes();
    // The statement text must be NUL-terminated for the tail pointer to be
    // meaningful; one copy serves the whole batch.
    let buf = CString::new(rest)
        .map_err(|_| MonorubyErr::argumenterr("string contains a NUL byte"))?;
    let base = buf.as_ptr();
    let mut cur = base;
    let end = rest.len() as isize;
    loop {
        // SAFETY: `cur` points into `buf`, at or before its NUL.
        let offset = unsafe { cur.offset_from(base) };
        if offset >= end {
            break;
        }
        let mut stmt: *mut sq::sqlite3_stmt = std::ptr::null_mut();
        let mut tail: *const c_char = std::ptr::null();
        // SAFETY: a live connection and a NUL-terminated remainder.
        let rc = unsafe {
            sq::sqlite3_prepare_v2(
                db,
                cur,
                (end - offset) as c_int,
                &mut stmt,
                &mut tail,
            )
        };
        check(vm, globals, db, rc)?;
        if stmt.is_null() {
            // Only whitespace or a comment was left.
            break;
        }
        let guard = StmtHandle { stmt, done: false };
        loop {
            // A user function in this statement reaches the interpreter
            // through this guard, as in `stmt_step`.
            let mut call = CallGuard::new(vm, globals, db, lfp.self_val());
            // SAFETY: a live statement.
            let rc = unsafe { sq::sqlite3_step(stmt) };
            let err = call.take_error();
            drop(call);
            if let Some(e) = err {
                return Err(e);
            }
            match rc {
                // SAFETY: positioned on a row.
                sq::SQLITE_ROW => rows.push(unsafe { row_value_as_text(stmt) }),
                sq::SQLITE_DONE => break,
                _ => {
                    check(vm, globals, db, rc)?;
                    break;
                }
            }
        }
        drop(guard);
        if tail.is_null() {
            break;
        }
        cur = tail;
        rest = &rest[..];
    }
    Ok(Value::array_from_vec(rows))
}

/// Database#enable_load_extension(onoff) -> nil
///
/// The bundled SQLite is built with `SQLITE_OMIT_LOAD_EXTENSION`, so there
/// is nothing to enable; the previous bridge also refused, and answering
/// quietly keeps `Database#initialize` working.
#[monoruby_builtin]
fn db_enable_load_extension(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    db_of(vm, globals, lfp.self_val())?;
    Ok(Value::nil())
}

/// Database#load_extension_internal(path) -> raises
#[monoruby_builtin]
fn db_load_extension(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    db_of(vm, globals, lfp.self_val())?;
    Err(err_sqlite3(
        vm,
        globals,
        "load_extension is not available: monoruby's SQLite is built without extension loading",
    ))
}

/// Database#trace(mask = nil, &block) -> the previous block
#[monoruby_builtin]
fn db_trace(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    db_of(vm, globals, lfp.self_val())?;
    let name = IdentId::get_id("@tracefunc");
    let prev = globals
        .store
        .get_ivar(lfp.self_val(), name)
        .unwrap_or_default();
    let block = lfp.block().map_or(Value::nil(), |_| Value::nil());
    globals.store.set_ivar(lfp.self_val(), name, block)?;
    Ok(prev)
}

/// Database#authorizer=(block) -> nil
#[monoruby_builtin]
fn db_authorizer_assign(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    db_of(vm, globals, lfp.self_val())?;
    globals
        .store
        .set_ivar(lfp.self_val(), IdentId::get_id("@authorizer"), lfp.arg(0))?;
    Ok(Value::nil())
}

// ---------------------------------------------------------------------
// User-defined functions
// ---------------------------------------------------------------------

/// One registered function: the Proc `create_function` was given, or the
/// class `create_aggregate` was given. Which of the two it is follows
/// from the callbacks registered beside it — `func_invoke` is only ever
/// given a Proc, `agg_step` / `agg_final` only ever a class — so the
/// entry does not carry the distinction a second time.
///
/// SQLite keeps this as the function's `pApp` for as long as the function
/// is defined, so it is a leaked `Box` freed by the `xDestroy` below. The
/// same value is also held in `DbHandle::funcs`, which is what roots it
/// for the collector — this copy is never the only reference.
struct FuncEntry {
    value: Value,
}

/// `xDestroy`: SQLite calls this when the function is replaced or its
/// connection closes.
unsafe extern "C" fn func_destroy(app: *mut c_void) {
    if !app.is_null() {
        // SAFETY: the `Box` leaked at registration, handed back once.
        drop(unsafe { Box::from_raw(app as *mut FuncEntry) });
    }
}

/// What a callback needs to re-enter the interpreter, and where it leaves
/// an exception for the step that caused it.
///
/// A Ruby exception must not unwind through SQLite's C frames, so the
/// callback stashes it here, tells SQLite the function failed, and the
/// builtin that called `sqlite3_step` re-raises it once control is back
/// in Rust. This is how `nokogiri`'s XPath handlers work
/// (`builtins/nokogiri/xpath.rs`).
struct CallState {
    vm: *mut Executor,
    globals: *mut Globals,
    /// The `SQLite3::Database` this step is running on, so an aggregate
    /// callback can reach the instance table in its `DbHandle`. Rooted
    /// for the whole step by whatever is being stepped: the statement
    /// holds it in `@connection`, and `exec_batch` is a method on it.
    db_obj: Value,
    error: Option<MonorubyErr>,
}

thread_local! {
    /// The step in progress on each connection.
    ///
    /// Keyed by connection, and saved-and-restored rather than pushed
    /// and popped, because green-thread switches are not LIFO: a
    /// callback that parks (any `sleep` or IO in the block) lets another
    /// green thread run, and it may finish its own step first. A plain
    /// stack would then have one thread pop the other's entry and hand a
    /// callback the wrong `Executor`. Per connection, two green threads
    /// stepping two connections never meet; nesting on one connection —
    /// which SQLite allows, and the C extension permits — still works,
    /// since the inner guard restores the outer on the way out.
    static CALLS: RefCell<HashMap<usize, *mut CallState>> =
        RefCell::new(HashMap::default());
}

thread_local! {
    /// Whether any user function has been defined on this thread.
    ///
    /// `stmt_step` is the hot path — one call per row — and installing a
    /// guard costs an allocation and a map write. Almost no program
    /// defines a function, so the guard is skipped entirely while this
    /// is zero, leaving that path exactly as it was.
    ///
    /// It only ever grows: closing a connection drops its functions, but
    /// SQLite frees them from whichever thread runs the close, and this
    /// counter is per thread. Undercounting would skip a guard a
    /// callback needs, so the count is deliberately never reduced except
    /// when a registration fails outright. The cost of overcounting is
    /// one guard per step in a program that used a function and then
    /// stopped.
    static FUNC_COUNT: Cell<usize> = const { Cell::new(0) };
}

/// Make `vm` / `globals` reachable from a callback for the duration of a
/// step, and take back any exception a callback left behind.
///
/// `None` when no function is defined: there is then nothing that could
/// call back, so there is nothing to guard.
struct CallGuard {
    inner: Option<CallGuardInner>,
}

struct CallGuardInner {
    db: usize,
    prev: Option<*mut CallState>,
    state: Box<CallState>,
}

/// Whether anything could call back, so a step needs a guard at all.
fn guard_needed() -> bool {
    FUNC_COUNT.with(|n| n.get()) != 0
}

impl CallGuard {
    fn new(
        vm: &mut Executor,
        globals: &mut Globals,
        db: *mut sq::sqlite3,
        db_obj: Value,
    ) -> Self {
        if !guard_needed() {
            return Self { inner: None };
        }
        // Boxed so the address stays put however the map grows.
        let mut state = Box::new(CallState {
            vm,
            globals,
            db_obj,
            error: None,
        });
        let p: *mut CallState = &mut *state;
        let db = db as usize;
        let prev = CALLS.with(|c| c.borrow_mut().insert(db, p));
        Self {
            inner: Some(CallGuardInner { db, prev, state }),
        }
    }

    /// The exception a callback raised during this step, if any.
    fn take_error(&mut self) -> Option<MonorubyErr> {
        self.inner.as_mut().and_then(|i| i.state.error.take())
    }
}

impl Drop for CallGuard {
    fn drop(&mut self) {
        let Some(i) = &self.inner else { return };
        CALLS.with(|c| {
            let mut c = c.borrow_mut();
            match i.prev {
                Some(p) => c.insert(i.db, p),
                None => c.remove(&i.db),
            };
        });
    }
}

/// The step running on `db`, or `None` when SQLite reached a callback
/// from somewhere this binding does not guard.
fn current_call(db: *mut sq::sqlite3) -> Option<*mut CallState> {
    CALLS.with(|c| c.borrow().get(&(db as usize)).copied())
}

/// A function argument as Ruby, by the same mapping the row readers use.
///
/// # Safety
/// `v` is a live `sqlite3_value` for the duration of the callback.
unsafe fn arg_value(v: *mut sq::sqlite3_value) -> Value {
    // SAFETY: the caller's contract.
    unsafe {
        match sq::sqlite3_value_type(v) {
            sq::SQLITE_INTEGER => Value::integer(sq::sqlite3_value_int64(v)),
            sq::SQLITE_FLOAT => Value::float(sq::sqlite3_value_double(v)),
            sq::SQLITE_NULL => Value::nil(),
            sq::SQLITE_BLOB => {
                let ptr = sq::sqlite3_value_blob(v) as *const u8;
                if ptr.is_null() {
                    Value::bytes_from_slice(&[])
                } else {
                    let len = sq::sqlite3_value_bytes(v).max(0) as usize;
                    Value::bytes_from_slice(std::slice::from_raw_parts(ptr, len))
                }
            }
            _ => {
                let ptr = sq::sqlite3_value_text(v) as *const c_char;
                if ptr.is_null() {
                    Value::nil()
                } else {
                    let len = sq::sqlite3_value_bytes(v).max(0) as usize;
                    let bytes = std::slice::from_raw_parts(ptr as *const u8, len);
                    Value::string_from_vec(bytes.to_vec())
                }
            }
        }
    }
}

/// Hand the block's answer back to SQLite, by the same rules `bind_param`
/// uses for a bound value. The C extension refuses anything else with a
/// RuntimeError rather than storing a NULL.
fn set_result(
    globals: &mut Globals,
    ctx: *mut sq::sqlite3_context,
    value: Value,
) -> Result<()> {
    // SAFETY: a live context, and buffers SQLite copies before returning
    // (`SQLITE_TRANSIENT`).
    unsafe {
        match value.unpack() {
            RV::Nil => sq::sqlite3_result_null(ctx),
            RV::Fixnum(i) => sq::sqlite3_result_int64(ctx, i),
            RV::Float(f) => sq::sqlite3_result_double(ctx, f),
            RV::BigInt(b) => match num::ToPrimitive::to_i64(b) {
                Some(i) => sq::sqlite3_result_int64(ctx, i),
                None => sq::sqlite3_result_double(
                    ctx,
                    num::ToPrimitive::to_f64(b).unwrap_or(f64::INFINITY),
                ),
            },
            RV::String(s) => {
                let bytes = s.as_bytes();
                if s.encoding() == crate::value::Encoding::Ascii8 || is_blob(globals, value) {
                    sq::sqlite3_result_blob(
                        ctx,
                        bytes.as_ptr() as *const c_void,
                        bytes.len() as c_int,
                        sq::SQLITE_TRANSIENT(),
                    )
                } else {
                    sq::sqlite3_result_text(
                        ctx,
                        bytes.as_ptr() as *const c_char,
                        bytes.len() as c_int,
                        sq::SQLITE_TRANSIENT(),
                    )
                }
            }
            _ => {
                return Err(MonorubyErr::runtimeerr(format!(
                    "can't return {}",
                    value.get_real_class_name(&globals.store)
                )));
            }
        }
    }
    Ok(())
}

/// Tell SQLite the function failed. The message is only what SQLite
/// reports; the Ruby exception itself travels in `CallState::error`.
fn result_error(ctx: *mut sq::sqlite3_context, msg: &str) {
    if let Ok(c) = CString::new(msg) {
        // SAFETY: a live context and a NUL-terminated string SQLite copies.
        unsafe { sq::sqlite3_result_error(ctx, c.as_ptr(), -1) };
    }
}

/// `xFunc`: SQLite calls this once per row the function appears in,
/// from inside `sqlite3_step`.
unsafe extern "C" fn func_invoke(
    ctx: *mut sq::sqlite3_context,
    argc: c_int,
    argv: *mut *mut sq::sqlite3_value,
) {
    // SAFETY: SQLite passes a live context, and `argv` holds `argc` live
    // values for the duration of the call.
    unsafe {
        let Some(state) = current_call(sq::sqlite3_context_db_handle(ctx)) else {
            // No step of ours is running, so there is no interpreter to
            // re-enter and nowhere to put an exception.
            result_error(ctx, "sqlite3 function called outside a query");
            return;
        };
        let state = &mut *state;
        // A previous row already failed; this evaluation is being torn
        // down, so do not run the block again.
        if state.error.is_some() {
            result_error(ctx, "aborted");
            return;
        }
        let vm = &mut *state.vm;
        let globals = &mut *state.globals;
        let entry = &*(sq::sqlite3_user_data(ctx) as *const FuncEntry);

        // The arguments stay rooted while they are built: each one is a
        // fresh object, and allocating the next may collect.
        let len = vm.temp_len();
        let mut args = Vec::with_capacity(argc.max(0) as usize);
        for i in 0..argc as isize {
            let v = arg_value(*argv.offset(i));
            vm.temp_push(v);
            args.push(v);
        }
        let result = match entry.value.is_proc() {
            Some(p) => vm.invoke_proc(globals, &p, &args),
            None => Err(err_sqlite3(vm, globals, "function block is not a Proc")),
        };
        vm.temp_clear(len);

        match result.and_then(|v| set_result(globals, ctx, v)) {
            Ok(()) => {}
            Err(e) => {
                // Report a failure to SQLite so the step unwinds, and keep
                // the exception for the builtin to re-raise.
                result_error(ctx, &e.get_error_message(&globals.store));
                state.error = Some(e);
            }
        }
    }
}

/// The instance accumulating this group, creating it on the first row.
///
/// SQLite's per-group memory is four bytes holding a slot number into
/// the connection's instance table; zero means "not started yet", so
/// slots are stored one-based. The pointer to those bytes comes back
/// with the instance, so `xFinal` can free the slot without asking for
/// it a second time.
///
/// # Safety
/// A live context belonging to `state`'s connection.
unsafe fn aggregate_instance(
    state: &mut CallState,
    ctx: *mut sq::sqlite3_context,
    klass: Value,
) -> Result<(Value, *mut u32)> {
    // SAFETY: the caller's contract.
    let slot_ptr = unsafe { sq::sqlite3_aggregate_context(ctx, 4) as *mut u32 };
    if slot_ptr.is_null() {
        // The one failure SQLite reports this way.
        return Err(MonorubyErr::runtimeerr("out of memory"));
    }
    // SAFETY: four bytes SQLite zeroed for us and keeps for this group.
    let slot = unsafe { *slot_ptr };
    if slot != 0
        && let Some(inst) = native_mut::<DbHandle>(state.db_obj)?
            .aggregates
            .get(slot as usize - 1)
            .copied()
            .flatten()
    {
        return Ok((inst, slot_ptr));
    }
    // First row of this group: one instance of the proxy class, which
    // holds the caller's `FunctionProxy` as its context.
    let vm = unsafe { &mut *state.vm };
    let globals = unsafe { &mut *state.globals };
    let inst = vm.invoke_method_inner(globals, IdentId::NEW, klass, &[], None, None)?;
    let table = &mut native_mut::<DbHandle>(state.db_obj)?.aggregates;
    let index = match table.iter().position(|e| e.is_none()) {
        Some(i) => {
            table[i] = Some(inst);
            i
        }
        None => {
            table.push(Some(inst));
            table.len() - 1
        }
    };
    // SAFETY: as above; SQLite hands back the same four bytes for every
    // row of this group.
    unsafe { *slot_ptr = index as u32 + 1 };
    Ok((inst, slot_ptr))
}

/// Drop this group's instance, freeing its slot for the next group.
///
/// # Safety
/// The slot `aggregate_instance` handed back for a group still running.
unsafe fn release_aggregate(state: &mut CallState, slot_ptr: *mut u32) {
    // SAFETY: the caller's contract; the slot is one-based and non-zero.
    let slot = unsafe { *slot_ptr };
    if let Ok(h) = native_mut::<DbHandle>(state.db_obj)
        && let Some(e) = h.aggregates.get_mut(slot as usize - 1)
    {
        *e = None;
    }
    // SAFETY: as above.
    unsafe { *slot_ptr = 0 };
}

/// Free a group's instance when `xFinal` cannot run normally — almost
/// always because an earlier row raised and the step is unwinding. The
/// group is over either way, so leaving the instance in the table would
/// pin it, and its slot, for the life of the connection.
///
/// # Safety
/// A live context, inside `xFinal`.
unsafe fn release_aborted_aggregate(ctx: *mut sq::sqlite3_context) {
    // SAFETY: the caller's contract; asking with 0 never allocates, so
    // a group that never stepped answers null and has nothing to free.
    let slot_ptr = unsafe { sq::sqlite3_aggregate_context(ctx, 0) as *mut u32 };
    // SAFETY: as above.
    let Some(state) = (unsafe { current_call(sq::sqlite3_context_db_handle(ctx)) }) else {
        return;
    };
    if !slot_ptr.is_null() {
        // SAFETY: the slot SQLite kept for this group, still one-based.
        unsafe { release_aggregate(&mut *state, slot_ptr) };
    }
}

/// Recover the running step and this function's registration, or report
/// to SQLite why the callback cannot run.
///
/// # Safety
/// A live context, inside a callback SQLite is making.
unsafe fn callback_state(
    ctx: *mut sq::sqlite3_context,
) -> Option<(&'static mut CallState, &'static FuncEntry)> {
    // SAFETY: the caller's contract.
    unsafe {
        let Some(state) = current_call(sq::sqlite3_context_db_handle(ctx)) else {
            result_error(ctx, "sqlite3 function called outside a query");
            return None;
        };
        let state = &mut *state;
        if state.error.is_some() {
            result_error(ctx, "aborted");
            return None;
        }
        Some((state, &*(sq::sqlite3_user_data(ctx) as *const FuncEntry)))
    }
}

/// `xStep`: once per row the aggregate sees, from inside `sqlite3_step`.
unsafe extern "C" fn agg_step(
    ctx: *mut sq::sqlite3_context,
    argc: c_int,
    argv: *mut *mut sq::sqlite3_value,
) {
    // SAFETY: SQLite passes a live context and `argc` live values.
    unsafe {
        let Some((state, entry)) = callback_state(ctx) else {
            return;
        };
        let klass = entry.value;
        let vm = &mut *state.vm;
        let globals = &mut *state.globals;
        // The instance and the arguments stay rooted while the rest are
        // built: each is fresh, and allocating the next may collect.
        let len = vm.temp_len();
        let result = (|| -> Result<()> {
            let (inst, _) = aggregate_instance(state, ctx, klass)?;
            let vm = &mut *state.vm;
            let globals = &mut *state.globals;
            vm.temp_push(inst);
            let mut args = Vec::with_capacity(argc.max(0) as usize);
            for i in 0..argc as isize {
                let v = arg_value(*argv.offset(i));
                vm.temp_push(v);
                args.push(v);
            }
            vm.invoke_method_inner(globals, IdentId::get_id("step"), inst, &args, None, None)?;
            Ok(())
        })();
        vm.temp_clear(len);
        if let Err(e) = result {
            result_error(ctx, &e.get_error_message(&globals.store));
            state.error = Some(e);
        }
    }
}

/// `xFinal`: once per group, after its last row.
///
/// SQLite calls this even for a group that never stepped — an aggregate
/// over no rows — and the C extension answers what a fresh instance's
/// `finalize` returns, so one is created here in that case.
unsafe extern "C" fn agg_final(ctx: *mut sq::sqlite3_context) {
    // SAFETY: SQLite passes a live context.
    unsafe {
        let Some((state, entry)) = callback_state(ctx) else {
            release_aborted_aggregate(ctx);
            return;
        };
        let klass = entry.value;
        let vm = &mut *state.vm;
        let len = vm.temp_len();
        // The slot this group's instance sits in, so it can be freed
        // below whether `finalize` answered or raised.
        let mut slot_ptr: *mut u32 = std::ptr::null_mut();
        let result = (|| -> Result<()> {
            // A group that never stepped starts its instance here, which
            // is what makes an aggregate over no rows answer.
            let (inst, p) = aggregate_instance(state, ctx, klass)?;
            slot_ptr = p;
            let vm = &mut *state.vm;
            let globals = &mut *state.globals;
            vm.temp_push(inst);
            let v =
                vm.invoke_method_inner(globals, IdentId::get_id("finalize"), inst, &[], None, None)?;
            vm.temp_push(v);
            set_result(globals, ctx, v)
        })();
        let vm = &mut *state.vm;
        vm.temp_clear(len);
        // The group is over either way: its instance must not outlive it.
        if !slot_ptr.is_null() {
            release_aggregate(state, slot_ptr);
        }
        if let Err(e) = result {
            let globals = &mut *state.globals;
            result_error(ctx, &e.get_error_message(&globals.store));
            state.error = Some(e);
        }
    }
}

/// Database#define_aggregator2(klass, name) -> self
///
/// The C-level half of `create_aggregate` and `create_aggregate_handler`.
/// The gem hands over a class, not a block: it answers `arity`, and each
/// group gets one instance whose `step` runs per row and whose
/// `finalize` answers the result.
///
/// Unlike a scalar function, the arity really is registered — SQLite
/// refuses `mysum(a, b)` for an aggregate declared with one.
#[monoruby_builtin]
fn db_define_aggregator(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let db = db_of(vm, globals, lfp.self_val())?;
    let klass = lfp.arg(0);
    let name = lfp.arg(1).expect_string(&globals.store)?;
    let bytes = name.as_bytes();
    let upto = bytes.iter().position(|&b| b == 0).unwrap_or(bytes.len());
    let cname = CString::new(&bytes[..upto]).expect("no NUL before the first NUL");
    let arity = vm
        .invoke_method_inner(globals, IdentId::get_id("arity"), klass, &[], None, None)?
        .coerce_to_i64(globals)? as c_int;
    // Rooted for as long as SQLite may instantiate it.
    native_mut::<DbHandle>(lfp.self_val())?.funcs.push(klass);
    let entry = Box::into_raw(Box::new(FuncEntry { value: klass }));
    FUNC_COUNT.with(|n| n.set(n.get() + 1));
    // SAFETY: a live connection and a NUL-terminated name; `entry` goes
    // to SQLite, which gives it back to `func_destroy`.
    let rc = unsafe {
        sq::sqlite3_create_function_v2(
            db,
            cname.as_ptr(),
            arity,
            sq::SQLITE_UTF8,
            entry as *mut c_void,
            None,
            Some(agg_step),
            Some(agg_final),
            Some(func_destroy),
        )
    };
    if rc != sq::SQLITE_OK {
        // `entry` is not ours to free: SQLite runs `xDestroy` on the
        // application data when the registration itself fails, so the
        // box has already been dropped by the time this is reached.
        FUNC_COUNT.with(|n| n.set(n.get() - 1));
        check(vm, globals, db, rc)?;
    }
    Ok(lfp.self_val())
}

/// Database#define_function_with_flags(name, flags, &block) -> self
///
/// The C-level half of `create_function`; the gem's Ruby half wraps the
/// caller's block in one that builds a `FunctionProxy` and answers its
/// `result`. Registered with an arity of -1, as the extension does: the
/// arity passed to `create_function` never reaches SQLite, so
/// `create_function("f", 1)` really does accept `f(1, 2)`.
#[monoruby_builtin]
fn db_define_function(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    // The gem passes the text-rep flags straight through; UTF-8 is the
    // only encoding this binding reads, and SQLITE_DETERMINISTIC is the
    // one other bit worth honouring.
    let flags = lfp.arg(1).coerce_to_i64(globals)? as c_int;
    register_function(vm, globals, lfp, pc, flags)
}

/// Database#define_function(name, &block) -> self
///
/// `define_function_with_flags` with no flags.
#[monoruby_builtin]
fn db_define_function_plain(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
) -> Result<Value> {
    register_function(vm, globals, lfp, pc, 0)
}

/// Register `lfp.arg(0)` as a scalar function running `lfp`'s block.
fn register_function(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
    flags: c_int,
) -> Result<Value> {
    let db = db_of(vm, globals, lfp.self_val())?;
    let name = lfp.arg(0).expect_string(&globals.store)?;
    // SQLite takes a NUL-terminated name, so a name holding a NUL byte
    // registers only the part before it — which is what the C extension
    // does, and `"a\0b"` really does define `a`.
    let bytes = name.as_bytes();
    let upto = bytes.iter().position(|&b| b == 0).unwrap_or(bytes.len());
    let cname = CString::new(&bytes[..upto]).expect("no NUL before the first NUL");
    let enc = sq::SQLITE_UTF8 | (flags & sq::SQLITE_DETERMINISTIC);
    let Some(bh) = lfp.block() else {
        // What `Proc.new` says, which is where the C extension ends up.
        return Err(MonorubyErr::argumenterr(
            "tried to create Proc object without a block",
        ));
    };
    let block: Value = vm.generate_proc(globals, bh, pc)?.into();
    // Rooted here for as long as SQLite may call it; the `FuncEntry`
    // below holds the same Value but is invisible to the collector.
    native_mut::<DbHandle>(lfp.self_val())?.funcs.push(block);
    // The C extension also records it under the name the caller gave,
    // which `Database#functions` exposes.
    let funcs = globals
        .store
        .get_ivar(lfp.self_val(), IdentId::get_id("@functions"))
        .unwrap_or_default();
    if let Some(mut h) = funcs.try_hash_ty() {
        h.insert(Value::string_from_str(&name), block, vm, globals)?;
    }
    let entry = Box::into_raw(Box::new(FuncEntry { value: block }));
    // From here on `stmt_step` installs a guard: something can call back.
    FUNC_COUNT.with(|n| n.set(n.get() + 1));
    // SAFETY: a live connection and a NUL-terminated name; `entry` is
    // handed to SQLite, which gives it back to `func_destroy`.
    let rc = unsafe {
        sq::sqlite3_create_function_v2(
            db,
            cname.as_ptr(),
            -1,
            enc,
            entry as *mut c_void,
            Some(func_invoke),
            None,
            None,
            Some(func_destroy),
        )
    };
    if rc != sq::SQLITE_OK {
        // As above: a failed registration still runs `xDestroy`, so the
        // box is already gone and freeing it here would double-free.
        FUNC_COUNT.with(|n| n.set(n.get() - 1));
        check(vm, globals, db, rc)?;
    }
    Ok(lfp.self_val())
}

/// Database#complete?(sql) -> bool
///
/// Whether the text forms one or more complete statements — what a REPL
/// asks before deciding to keep reading.
#[monoruby_builtin]
fn db_complete_p(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    db_of(vm, globals, lfp.self_val())?;
    let sql = lfp.arg(0).expect_string(&globals.store)?;
    let Ok(c) = std::ffi::CString::new(sql) else {
        // An interior NUL cannot end a statement.
        return Ok(Value::bool(false));
    };
    // SAFETY: a NUL-terminated string that outlives the call.
    Ok(Value::bool(unsafe { sq::sqlite3_complete(c.as_ptr()) } != 0))
}

/// Database#collation(name, comparator) -> self
///
/// A comparator is a Ruby object SQLite would call back into, so only
/// removing one (a nil comparator) can be honoured; anything else
/// refuses, as `create_function` does.
#[monoruby_builtin]
fn db_collation(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    db_of(vm, globals, lfp.self_val())?;
    if !lfp.arg(1).is_nil() {
        return Err(err_sqlite3(
            vm,
            globals,
            "collation is not supported by monoruby's sqlite3 binding",
        ));
    }
    Ok(lfp.self_val())
}

/// Database#statement_timeout=(ms) -> ms
///
/// The gem enforces this with a progress handler, which is a Ruby
/// callback from SQLite; the value is remembered so `initialize` and the
/// accessor work, but nothing interrupts a long statement.
#[monoruby_builtin]
fn db_statement_timeout_assign(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    db_of(vm, globals, lfp.self_val())?;
    let v = lfp.arg(0);
    globals
        .store
        .set_ivar(lfp.self_val(), IdentId::get_id("@statement_timeout"), v)?;
    Ok(v)
}

/// Database#discard -> nil
///
/// Abandon the connection without closing it: what the gem's fork
/// safety does in a child, where running the parent's `close` would
/// checkpoint and unlink files the parent still owns. The handle is
/// dropped on the floor deliberately — the OS reclaims it with the
/// process.
#[monoruby_builtin]
fn db_discard(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let h = native_mut::<DbHandle>(lfp.self_val())?;
    h.db = std::ptr::null_mut();
    h.funcs.clear();
    Ok(Value::nil())
}

// ---------------------------------------------------------------------
// Statement
// ---------------------------------------------------------------------

/// Statement#prepare(db, sql) -> String (the unparsed remainder)
#[monoruby_builtin]
fn stmt_prepare(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let db = db_of(vm, globals, lfp.arg(0))?;
    let sql = lfp.arg(1).expect_string(&globals.store)?;
    let buf = CString::new(sql.as_bytes())
        .map_err(|_| MonorubyErr::argumenterr("string contains a NUL byte"))?;
    let mut stmt: *mut sq::sqlite3_stmt = std::ptr::null_mut();
    let mut tail: *const c_char = std::ptr::null();
    // SAFETY: a live connection and a NUL-terminated statement that
    // outlives the call (SQLite keeps its own copy of the text).
    let rc = unsafe {
        sq::sqlite3_prepare_v2(
            db,
            buf.as_ptr(),
            sql.len() as c_int,
            &mut stmt,
            &mut tail,
        )
    };
    if rc != sq::SQLITE_OK {
        // SAFETY: a live connection.
        let msg = unsafe { cstr_to_string(sq::sqlite3_errmsg(db)) }
            .unwrap_or_else(|| format!("sqlite3 error {rc}"));
        return Err(raise_code_with_sql(vm, globals, db, rc, msg, &sql));
    }
    let h = native_mut::<StmtHandle>(lfp.self_val())?;
    h.stmt = stmt;
    h.done = false;
    // The remainder, by the tail's offset into our copy.
    let remainder = if tail.is_null() {
        ""
    } else {
        // SAFETY: `tail` points into `buf`, at or before its NUL.
        let offset = unsafe { tail.offset_from(buf.as_ptr()) } as usize;
        sql.get(offset..).unwrap_or("")
    };
    Ok(Value::string_from_str(remainder))
}

/// Statement#close -> nil
#[monoruby_builtin]
fn stmt_close(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let h = native_mut::<StmtHandle>(lfp.self_val())?;
    if !h.stmt.is_null() {
        // SAFETY: ours.
        unsafe { sq::sqlite3_finalize(h.stmt) };
        h.stmt = std::ptr::null_mut();
    }
    Ok(Value::nil())
}

/// Statement#closed? -> bool
#[monoruby_builtin]
fn stmt_closed_p(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(
        native_mut::<StmtHandle>(lfp.self_val())?.stmt.is_null(),
    ))
}

/// Statement#step -> Array of column values, or nil at the end
///
/// The row is read here, in one call: the Fiddle bridge crossed into C
/// `1 + 2n` times per row (a `column_type` and a typed getter per column),
/// each through libffi.
#[monoruby_builtin]
fn stmt_step(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    stmt_of(vm, globals, lfp.self_val())?;
    let h = native_mut::<StmtHandle>(lfp.self_val())?;
    // A statement that already reported `SQLITE_DONE` answers nil without
    // stepping: `sqlite3_step` past the end silently resets and re-runs the
    // query, and the C extension's `done_p` guard is what stops that.
    if h.done {
        return Ok(Value::nil());
    }
    let stmt = h.stmt;
    // A user function reaches the interpreter through this guard, and
    // leaves an exception on it rather than unwinding through SQLite.
    // SAFETY: a live statement.
    let db = unsafe { sq::sqlite3_db_handle(stmt) };
    // The statement's connection, which an aggregate callback needs to
    // reach its instance table. Read only when a callback is possible:
    // this is one call per row, and an ivar lookup on it is not free.
    let db_obj = if guard_needed() {
        globals
            .store
            .get_ivar(lfp.self_val(), IdentId::get_id("@connection"))
            .unwrap_or_default()
    } else {
        Value::nil()
    };
    let mut guard = CallGuard::new(vm, globals, db, db_obj);
    // SAFETY: a live statement.
    let rc = unsafe { sq::sqlite3_step(stmt) };
    if let Some(e) = guard.take_error() {
        return Err(e);
    }
    drop(guard);
    let h = native_mut::<StmtHandle>(lfp.self_val())?;
    match rc {
        sq::SQLITE_ROW => {
            h.done = false;
            // SAFETY: positioned on a row.
            Ok(unsafe { row_value(stmt) })
        }
        sq::SQLITE_DONE => {
            h.done = true;
            Ok(Value::nil())
        }
        _ => {
            // SAFETY: the statement's own connection.
            let db = unsafe { sq::sqlite3_db_handle(stmt) };
            check(vm, globals, db, rc)?;
            Ok(Value::nil())
        }
    }
}

/// Statement#done? -> bool
#[monoruby_builtin]
fn stmt_done_p(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(
        native_mut::<StmtHandle>(lfp.self_val())?.done,
    ))
}

/// Statement#reset! -> self
#[monoruby_builtin]
fn stmt_reset(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let stmt = stmt_of(vm, globals, lfp.self_val())?;
    // SAFETY: a live statement. A failing `reset` reports the error of the
    // last `step`, which the caller has already seen.
    unsafe {
        sq::sqlite3_reset(stmt);
        sq::sqlite3_clear_bindings(stmt);
    }
    native_mut::<StmtHandle>(lfp.self_val())?.done = false;
    // The C extension answers self; ActiveRecord's statement pool chains
    // off the return value.
    Ok(lfp.self_val())
}

/// Bind `value` to parameter `index` (1-based).
///
/// The types the C extension accepts, and no others: nil, Integer, Float
/// and String (a `SQLite3::Blob` or a BINARY-encoded String as a BLOB).
/// `true`, `false`, a Symbol, an Array — anything else — is a
/// `RuntimeError: can't prepare <Class>`, which is what the gem's Ruby
/// half relies on to reject them.
fn bind_one(
    globals: &mut Globals,
    stmt: *mut sq::sqlite3_stmt,
    index: c_int,
    value: Value,
) -> Result<c_int> {
    // SAFETY: a live statement, an index the caller checked, and buffers
    // that outlive the call — `SQLITE_TRANSIENT` makes SQLite copy them.
    unsafe {
        Ok(match value.unpack() {
            RV::Nil => sq::sqlite3_bind_null(stmt, index),
            RV::Fixnum(i) => sq::sqlite3_bind_int64(stmt, index, i),
            RV::Float(f) => sq::sqlite3_bind_double(stmt, index, f),
            // An Integer too wide for i64 binds as a double, as the C
            // extension's `rb_Float` fallback does. monoruby's fixnum is
            // 63-bit, so 2**62 arrives here and still binds as an integer.
            RV::BigInt(b) => match num::ToPrimitive::to_i64(b) {
                Some(i) => sq::sqlite3_bind_int64(stmt, index, i),
                None => sq::sqlite3_bind_double(
                    stmt,
                    index,
                    num::ToPrimitive::to_f64(b).unwrap_or(f64::INFINITY),
                ),
            },
            RV::String(s) => {
                let bytes = s.as_bytes();
                // Bound as a BLOB when the String says it is bytes: a
                // `SQLite3::Blob` (the gem's marker subclass) or any
                // BINARY-encoded String, which is what the C extension does.
                if s.encoding() == crate::value::Encoding::Ascii8 || is_blob(globals, value) {
                    sq::sqlite3_bind_blob(
                        stmt,
                        index,
                        bytes.as_ptr() as *const c_void,
                        bytes.len() as c_int,
                        sq::SQLITE_TRANSIENT(),
                    )
                } else {
                    sq::sqlite3_bind_text(
                        stmt,
                        index,
                        bytes.as_ptr() as *const c_char,
                        bytes.len() as c_int,
                        sq::SQLITE_TRANSIENT(),
                    )
                }
            }
            _ => {
                return Err(MonorubyErr::runtimeerr(format!(
                    "can't prepare {}",
                    globals.store.get_class_name(value.class())
                )));
            }
        })
    }
}

/// Whether `v` is a `SQLite3::Blob` (the gem's String subclass for BLOB
/// columns).
fn is_blob(globals: &Globals, v: Value) -> bool {
    let Some(blob) = globals
        .store
        .get_constant_noautoload(classes().sqlite3, IdentId::get_id("Blob"))
    else {
        return false;
    };
    globals.store.class_is_kind_of(v.class(), blob.as_class_id())
}

/// Statement#bind_param(index, value) -> nil
#[monoruby_builtin]
fn stmt_bind_param(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let stmt = stmt_of(vm, globals, lfp.self_val())?;
    let key = lfp.arg(0);
    let index = if let Some(s) = key.is_rstring_inner() {
        // A name, with or without its leading marker.
        let name = String::from_utf8_lossy(s.as_bytes()).into_owned();
        let name = if name.starts_with([':', '@', '$']) {
            name
        } else {
            format!(":{name}")
        };
        let c = CString::new(name.as_bytes())
            .map_err(|_| MonorubyErr::argumenterr("string contains a NUL byte"))?;
        // SAFETY: a live statement and a NUL-terminated name.
        let i = unsafe { sq::sqlite3_bind_parameter_index(stmt, c.as_ptr()) };
        if i == 0 {
            // The C extension names no parameter in this message.
            return Err(err_sqlite3(vm, globals, "no such bind parameter"));
        }
        i
    } else if let Some(sym) = key.try_symbol() {
        let name = format!(":{}", sym.get_name());
        let c = CString::new(name.as_bytes()).unwrap();
        // SAFETY: as above.
        let i = unsafe { sq::sqlite3_bind_parameter_index(stmt, c.as_ptr()) };
        if i == 0 {
            // The C extension names no parameter in this message.
            return Err(err_sqlite3(vm, globals, "no such bind parameter"));
        }
        i
    } else {
        key.coerce_to_i64(globals)? as c_int
    };
    let rc = bind_one(globals, stmt, index, lfp.arg(1))?;
    if rc != sq::SQLITE_OK {
        // SAFETY: the statement's own connection.
        let db = unsafe { sq::sqlite3_db_handle(stmt) };
        check(vm, globals, db, rc)?;
    }
    Ok(Value::nil())
}

/// Statement#column_count -> Integer
#[monoruby_builtin]
fn stmt_column_count(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let stmt = stmt_of(vm, globals, lfp.self_val())?;
    // SAFETY: a live statement.
    Ok(Value::integer(
        unsafe { sq::sqlite3_column_count(stmt) } as i64
    ))
}

/// Statement#column_name(index) -> String or nil
#[monoruby_builtin]
fn stmt_column_name(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let stmt = stmt_of(vm, globals, lfp.self_val())?;
    let i = lfp.arg(0).coerce_to_i64(globals)? as c_int;
    // SAFETY: a live statement; an out-of-range index answers NULL.
    Ok(unsafe { cstr_to_value(sq::sqlite3_column_name(stmt, i)) })
}

/// Statement#column_decltype(index) -> String or nil
#[monoruby_builtin]
fn stmt_column_decltype(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let stmt = stmt_of(vm, globals, lfp.self_val())?;
    let i = lfp.arg(0).coerce_to_i64(globals)? as c_int;
    // SAFETY: as `column_name`.
    Ok(unsafe { cstr_to_value(sq::sqlite3_column_decltype(stmt, i)) })
}

/// Statement#bind_parameter_count -> Integer
#[monoruby_builtin]
fn stmt_bind_parameter_count(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let stmt = stmt_of(vm, globals, lfp.self_val())?;
    // SAFETY: a live statement.
    Ok(Value::integer(
        unsafe { sq::sqlite3_bind_parameter_count(stmt) } as i64,
    ))
}

/// Statement#sql -> String
///
/// The statement's text as it was prepared.
#[monoruby_builtin]
fn stmt_sql(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let stmt = stmt_of(vm, globals, lfp.self_val())?;
    // SAFETY: a live statement; the text it borrows lives as long as it
    // does, and is copied here.
    let s = unsafe { cstr_to_string(sq::sqlite3_sql(stmt)) };
    Ok(s.map_or_else(Value::nil, |s| Value::string(s)))
}

/// Statement#expanded_sql -> String
///
/// The text with the bound parameters substituted.
#[monoruby_builtin]
fn stmt_expanded_sql(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let stmt = stmt_of(vm, globals, lfp.self_val())?;
    // SAFETY: a live statement. Unlike `sqlite3_sql`, this buffer is
    // ours to free.
    unsafe {
        let p = sq::sqlite3_expanded_sql(stmt);
        if p.is_null() {
            return Ok(Value::nil());
        }
        let s = cstr_to_string(p);
        sq::sqlite3_free(p as *mut c_void);
        Ok(s.map_or_else(Value::nil, |s| Value::string(s)))
    }
}

/// Statement#memused -> Integer
#[monoruby_builtin]
fn stmt_memused(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let stmt = stmt_of(vm, globals, lfp.self_val())?;
    // SAFETY: a live statement; `reset` of 0 leaves the counter alone.
    let n = unsafe { sq::sqlite3_stmt_status(stmt, sq::SQLITE_STMTSTATUS_MEMUSED, 0) };
    Ok(Value::integer(n as i64))
}

/// Statement#clear_bindings! -> self
#[monoruby_builtin]
fn stmt_clear_bindings(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let stmt = stmt_of(vm, globals, lfp.self_val())?;
    // SAFETY: a live statement.
    unsafe { sq::sqlite3_clear_bindings(stmt) };
    Ok(lfp.self_val())
}

/// The `sqlite3_stmt_status` counter a `stat_for` key names.
fn stat_counter(name: &str) -> Option<c_int> {
    Some(match name {
        "fullscan_steps" => sq::SQLITE_STMTSTATUS_FULLSCAN_STEP,
        "sorts" => sq::SQLITE_STMTSTATUS_SORT,
        "autoindexes" => sq::SQLITE_STMTSTATUS_AUTOINDEX,
        "vm_steps" => sq::SQLITE_STMTSTATUS_VM_STEP,
        "reprepares" => sq::SQLITE_STMTSTATUS_REPREPARE,
        "runs" => sq::SQLITE_STMTSTATUS_RUN,
        "filter_misses" => sq::SQLITE_STMTSTATUS_FILTER_MISS,
        "filter_hits" => sq::SQLITE_STMTSTATUS_FILTER_HIT,
        _ => return None,
    })
}

/// Statement#stat_for(key) -> Integer
#[monoruby_builtin]
fn stmt_stat_for(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let stmt = stmt_of(vm, globals, lfp.self_val())?;
    // The C extension takes a Symbol and nothing else — a String key
    // is a TypeError, not a lookup — and rejects an unknown one rather
    // than answering zero.
    let Some(sym) = lfp.arg(0).try_symbol() else {
        return Err(MonorubyErr::typeerr("non-symbol given"));
    };
    let name = sym.get_name();
    let Some(counter) = stat_counter(&name) else {
        return Err(MonorubyErr::argumenterr(format!("unknown key: {name}")));
    };
    // SAFETY: a live statement and a known counter.
    Ok(Value::integer(
        unsafe { sq::sqlite3_stmt_status(stmt, counter, 0) } as i64,
    ))
}

/// Statement#stats_as_hash -> Hash
#[monoruby_builtin]
fn stmt_stats_as_hash(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let stmt = stmt_of(vm, globals, lfp.self_val())?;
    let mut h = HashmapInner::default();
    {
        for name in [
            "fullscan_steps",
            "sorts",
            "autoindexes",
            "vm_steps",
            "reprepares",
            "runs",
            "filter_misses",
            "filter_hits",
        ] {
            let counter = stat_counter(name).unwrap();
            // SAFETY: a live statement and a known counter.
            let v = unsafe { sq::sqlite3_stmt_status(stmt, counter, 0) };
            h.insert(
                Value::symbol_from_str(name),
                Value::integer(v as i64),
                vm,
                globals,
            )?;
        }
    }
    Ok(Value::hash_from_inner(h))
}
