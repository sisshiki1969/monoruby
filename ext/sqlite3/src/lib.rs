//! The sqlite3 gem's native half (`sqlite3_native.so`) as a monoruby
//! extension: `Init_sqlite3_native` builds `SQLite3::Database` /
//! `SQLite3::Statement` over the bundled SQLite (`libsqlite3-src`, 3.48.0
//! linked statically into this library) and registers their methods
//! through the `monoruby-ext` API. The gem's Ruby half is the host's,
//! unchanged; `gem/sqlite3/sqlite3_native.rb` requires this library and
//! adds the pieces of the C extension that are naturally Ruby.
//!
//! This is `src/builtins/sqlite3.rs` moved out of the interpreter
//! (doc/native_extension_loading.md, step 3): the same design — a
//! `Database` owns its `sqlite3*` and a `Statement` its `sqlite3_stmt*`
//! as native payloads closed when collected; `Statement#step` steps and
//! reads the whole row in one call; opening and closing a connection run
//! on the native pool so the other green threads keep going; a user
//! function is a real SQLite function that calls back into Ruby from
//! inside `sqlite3_step`, and an exception it raises is stashed rather
//! than unwound through the C frames, then re-raised once the step
//! returns.

use libsqlite3_src as sq;
use monoruby_ext::*;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::ffi::{CStr, CString, c_char, c_int, c_void};

// ---------------------------------------------------------------------
// Classes
// ---------------------------------------------------------------------

/// The classes the native methods live on. Filled by `Init_`, which the
/// gem's `sqlite3_native.rb` runs (through `require`) before the Ruby
/// half reopens `SQLite3::Database` — so these are the classes the gem
/// then extends, and their native instance type is in place from the
/// start.
///
/// Per interpreter, keyed by `Ctx::interpreter_id`: this library's
/// statics are process-wide while `Init_` runs once in each interpreter
/// of the process (one per test in monoruby's harness). monoruby runs one
/// interpreter per OS thread, so a thread-local holding the last one
/// initialized on this thread is enough, and the id check catches a
/// thread that builds a second.
struct Classes {
    sqlite3: Value,
}

thread_local! {
    static CLASSES: RefCell<Option<(usize, Classes)>> = const { RefCell::new(None) };
}

fn sqlite3_module() -> Value {
    CLASSES.with(|c| {
        c.borrow()
            .as_ref()
            .expect("SQLite3 classes are not initialized")
            .1
            .sqlite3
    })
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn Init_sqlite3_native(ctx: *mut MrContext) -> c_int {
    // SAFETY: the interpreter's contract for `Init_`.
    unsafe { init(ctx, init_sqlite3) }
}

fn init_sqlite3(ctx: &mut Ctx) -> Result<()> {
    let id = ctx.interpreter_id();
    if CLASSES.with(|c| c.borrow().as_ref().is_some_and(|(i, _)| *i == id)) {
        return Ok(());
    }
    let sqlite3 = ctx.define_module(Value::UNDEF, "SQLite3")?;
    let database = ctx.define_class(sqlite3, "Database", Value::UNDEF, MR_CLASS_NATIVE)?;
    let statement = ctx.define_class(sqlite3, "Statement", Value::UNDEF, MR_CLASS_NATIVE)?;
    let backup = ctx.define_class(sqlite3, "Backup", Value::UNDEF, MR_CLASS_NATIVE)?;
    CLASSES.with(|c| *c.borrow_mut() = Some((id, Classes { sqlite3 })));

    // ---- SQLite3 module functions
    let s = MR_METHOD_SINGLETON;
    ctx.define_method(sqlite3, "libversion", method!(libversion), 0, s);
    ctx.define_method(
        sqlite3,
        "libversion_string",
        method!(libversion_string),
        0,
        s,
    );
    ctx.define_method(sqlite3, "threadsafe", method!(threadsafe), 0, s);
    ctx.define_method(sqlite3, "sqlcipher?", method!(sqlcipher_p), 0, s);

    // ---- Backup (the online backup API; `backup.c` of the extension)
    let p = MR_METHOD_PRIVATE;
    let b = backup;
    ctx.define_method(b, "initialize", method!(backup_initialize), 4, p);
    ctx.define_method(b, "step", method!(backup_step), 1, 0);
    ctx.define_method(b, "finish", method!(backup_finish), 0, 0);
    ctx.define_method(b, "remaining", method!(backup_remaining), 0, 0);
    ctx.define_method(b, "pagecount", method!(backup_pagecount), 0, 0);

    // ---- Database
    let d = database;
    ctx.define_method(d, "open_v2", method!(db_open_v2), 3, p);
    ctx.define_method(d, "open16", method!(db_open16), 1, p);
    ctx.define_method(d, "close", method!(db_close), 0, 0);
    ctx.define_method(d, "closed?", method!(db_closed_p), 0, 0);
    ctx.define_method(d, "encoding", method!(db_encoding), 0, 0);
    ctx.define_method(d, "busy_timeout=", method!(db_busy_timeout_assign), 1, 0);
    ctx.define_method(d, "busy_timeout", method!(db_busy_timeout_assign), 1, 0);
    ctx.define_method(
        d,
        "busy_handler",
        method!(db_busy_handler),
        MR_ARGC_VARIADIC,
        0,
    );
    ctx.define_method(
        d,
        "last_insert_row_id",
        method!(db_last_insert_row_id),
        0,
        0,
    );
    ctx.define_method(d, "changes", method!(db_changes), 0, 0);
    ctx.define_method(d, "total_changes", method!(db_total_changes), 0, 0);
    ctx.define_method(d, "interrupt", method!(db_interrupt), 0, 0);
    ctx.define_method(d, "errcode", method!(db_errcode), 0, 0);
    ctx.define_method(d, "errmsg", method!(db_errmsg), 0, 0);
    ctx.define_method(d, "db_filename", method!(db_filename), 1, p);
    ctx.define_method(
        d,
        "extended_result_codes=",
        method!(db_extended_result_codes),
        1,
        0,
    );
    ctx.define_method(
        d,
        "transaction_active?",
        method!(db_transaction_active_p),
        0,
        0,
    );
    // The C extension defines these six privately; the gem's Ruby half
    // is their only caller, and `tests/sqlite3.rs` pins the split.
    ctx.define_method(
        d,
        "disable_quirk_mode",
        method!(db_disable_quirk_mode),
        0,
        p,
    );
    ctx.define_method(d, "exec_batch", method!(db_exec_batch), 2, p);
    ctx.define_method(
        d,
        "enable_load_extension",
        method!(db_enable_load_extension),
        1,
        0,
    );
    ctx.define_method(
        d,
        "load_extension_internal",
        method!(db_load_extension),
        1,
        p,
    );
    ctx.define_method(d, "trace", method!(db_trace), MR_ARGC_VARIADIC, 0);
    ctx.define_method(d, "authorizer=", method!(db_authorizer_assign), 1, 0);
    ctx.define_method(
        d,
        "define_function_with_flags",
        method!(db_define_function),
        2,
        0,
    );
    ctx.define_method(
        d,
        "define_function",
        method!(db_define_function_plain),
        1,
        0,
    );
    ctx.define_method(d, "define_aggregator2", method!(db_define_aggregator), 2, p);
    ctx.define_method(d, "collation", method!(db_collation), 2, 0);
    ctx.define_method(d, "complete?", method!(db_complete_p), 1, 0);
    ctx.define_method(
        d,
        "statement_timeout=",
        method!(db_statement_timeout_assign),
        1,
        0,
    );
    ctx.define_method(d, "discard", method!(db_discard), 0, p);

    // ---- Statement
    let s = statement;
    ctx.define_method(s, "prepare", method!(stmt_prepare), 2, p);
    ctx.define_method(s, "sql", method!(stmt_sql), 0, 0);
    ctx.define_method(s, "expanded_sql", method!(stmt_expanded_sql), 0, 0);
    ctx.define_method(s, "memused", method!(stmt_memused), 0, 0);
    ctx.define_method(s, "clear_bindings!", method!(stmt_clear_bindings), 0, 0);
    ctx.define_method(s, "close", method!(stmt_close), 0, 0);
    ctx.define_method(s, "closed?", method!(stmt_closed_p), 0, 0);
    ctx.define_method(s, "step", method!(stmt_step), 0, 0);
    ctx.define_method(s, "done?", method!(stmt_done_p), 0, 0);
    ctx.define_method(s, "reset!", method!(stmt_reset), 0, 0);
    ctx.define_method(s, "bind_param", method!(stmt_bind_param), 2, 0);
    ctx.define_method(s, "column_count", method!(stmt_column_count), 0, 0);
    ctx.define_method(s, "column_name", method!(stmt_column_name), 1, 0);
    ctx.define_method(s, "column_decltype", method!(stmt_column_decltype), 1, 0);
    ctx.define_method(
        s,
        "bind_parameter_count",
        method!(stmt_bind_parameter_count),
        0,
        0,
    );
    ctx.define_method(s, "stat_for", method!(stmt_stat_for), 1, p);
    ctx.define_method(s, "stats_as_hash", method!(stmt_stats_as_hash), 0, p);
    Ok(())
}

// ---------------------------------------------------------------------
// Payloads
// ---------------------------------------------------------------------

/// A `SQLite3::Database`'s connection. Absent (no payload) before
/// `open_v2`; `db` null after `close`; `Drop` closes an open one, so a
/// connection dropped on the floor is released with the object.
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

native!(DbHandle, "SQLite3::Database", |h, m| {
    for f in &h.funcs {
        m.mark(*f);
    }
    for a in h.aggregates.iter().flatten() {
        m.mark(*a);
    }
});

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

native!(StmtHandle, "SQLite3::Statement");

/// An online backup in progress (`sqlite3_backup`), finished by `finish`
/// or, if the object is collected first, by `Drop`.
struct BackupHandle {
    p: *mut sq::sqlite3_backup,
}

native!(BackupHandle, "SQLite3::Backup");

impl Drop for BackupHandle {
    fn drop(&mut self) {
        if !self.p.is_null() {
            // SAFETY: our own backup object, not finished yet.
            unsafe { sq::sqlite3_backup_finish(self.p) };
            self.p = std::ptr::null_mut();
        }
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

/// The connection payload of `v`, raising the closed-database error when
/// the object has none (allocated but never opened).
fn db_handle(ctx: &mut Ctx, v: Value) -> Result<&'static mut DbHandle> {
    match ctx.native::<DbHandle>(v) {
        Some(h) => Ok(h),
        None => Err(err_sqlite3(ctx, "cannot use a closed database")),
    }
}

/// The open connection of `self`; every method but `closed?` raises on a
/// closed one, as the C extension's `Data_Get_Struct` guard does.
fn db_of(ctx: &mut Ctx, v: Value) -> Result<*mut sq::sqlite3> {
    let h = db_handle(ctx, v)?;
    if h.db.is_null() {
        return Err(err_sqlite3(ctx, "cannot use a closed database"));
    }
    Ok(h.db)
}

/// The statement payload of `v`, raising when it has none.
fn stmt_handle(ctx: &mut Ctx, v: Value) -> Result<&'static mut StmtHandle> {
    match ctx.native::<StmtHandle>(v) {
        Some(h) => Ok(h),
        None => Err(err_sqlite3(ctx, "cannot use a closed statement")),
    }
}

/// The live statement of `self`; every method but `closed?` and `done?`
/// raises on a closed one (`REQUIRE_OPEN_STMT`).
fn stmt_of(ctx: &mut Ctx, v: Value) -> Result<*mut sq::sqlite3_stmt> {
    let h = stmt_handle(ctx, v)?;
    if h.stmt.is_null() {
        return Err(err_sqlite3(ctx, "cannot use a closed statement"));
    }
    Ok(h.stmt)
}

/// The live backup of `self`; the extension's `REQUIRE_OPEN_BACKUP`. An
/// instance whose `initialize` has not run has no payload at all (the
/// generic allocator makes it empty), which reads as closed.
fn backup_of(ctx: &mut Ctx, v: Value) -> Result<*mut sq::sqlite3_backup> {
    match ctx.native::<BackupHandle>(v) {
        Some(h) if !h.p.is_null() => Ok(h.p),
        _ => Err(err_sqlite3(ctx, "cannot use a closed backup")),
    }
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
fn err_sqlite3(ctx: &mut Ctx, msg: &str) -> Error {
    match ctx.const_get(sqlite3_module(), "Exception") {
        Some(klass) => ctx.raise(klass, msg),
        // Only reachable before `errors.rb` has been required.
        None => ctx.runtime_error(msg),
    }
}

/// Raise `SQLite3::<class for code>` with `msg` and `@code = code`,
/// exactly as the C extension's `rb_sqlite3_raise` does; with `sql`,
/// the statement text and the offset of the offending token as well, so
/// the gem's `Exception#message` can point at it
/// (`rb_sqlite3_raise_with_sql`).
fn raise_code(
    ctx: &mut Ctx,
    code: c_int,
    msg: &str,
    sql: Option<(*mut sq::sqlite3, &str)>,
) -> Error {
    let sqlite3 = sqlite3_module();
    let Some(klass) = ctx
        .const_get(sqlite3, error_class_name(code))
        .or_else(|| ctx.const_get(sqlite3, "Exception"))
    else {
        // `errors.rb` is not loaded yet (only possible before the gem
        // finishes requiring itself).
        return ctx.runtime_error(msg);
    };
    let m = ctx.str(msg);
    let ex = match ctx.funcall(klass, "new", &[m], None) {
        Ok(ex) => ex,
        Err(e) => return e,
    };
    ctx.temp_push(ex);
    let _ = ctx.ivar_set(ex, "@code", Value::int(code as i64));
    if let Some((db, sql)) = sql {
        // SAFETY: a live connection; -1 when the error is not about a token.
        let offset = unsafe { sq::sqlite3_error_offset(db) };
        let s = ctx.str(sql);
        let _ = ctx.ivar_set(ex, "@sql", s);
        let _ = ctx.ivar_set(ex, "@sql_offset", Value::int(offset as i64));
    }
    ctx.raise_exception(ex)
}

/// Raise the connection's current error if `rc` is not a success code.
fn check(ctx: &mut Ctx, db: *mut sq::sqlite3, rc: c_int) -> Result<()> {
    if matches!(rc, sq::SQLITE_OK | sq::SQLITE_ROW | sq::SQLITE_DONE) {
        return Ok(());
    }
    // SAFETY: a live connection.
    let msg = unsafe { cstr_to_string(sq::sqlite3_errmsg(db)) }
        .unwrap_or_else(|| format!("sqlite3 error {rc}"));
    Err(raise_code(ctx, rc, &msg, None))
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

/// A Ruby String from a NUL-terminated C string, `nil` for NULL.
///
/// # Safety
/// As `cstr_to_string`.
unsafe fn cstr_to_value(ctx: &Ctx, p: *const c_char) -> Value {
    if p.is_null() {
        return Value::nil();
    }
    // SAFETY: the caller's contract.
    ctx.str(unsafe { CStr::from_ptr(p) }.to_bytes())
}

/// The bytes of a Ruby String argument, as a NUL-terminated `CString` for
/// the entry points that take a `const char *` without a length.
fn to_cstring(ctx: &mut Ctx, v: Value) -> Result<CString> {
    let bytes = ctx.str_vec(v)?;
    CString::new(bytes).map_err(|_| ctx.argument_error("string contains a NUL byte"))
}

/// Column `i` of `stmt` as a Ruby value, by its SQLite type.
///
/// # Safety
/// `stmt` is live and positioned on a row, and `i` is in range.
unsafe fn column_value(ctx: &Ctx, stmt: *mut sq::sqlite3_stmt, i: c_int) -> Value {
    // SAFETY: the caller's contract.
    unsafe {
        match sq::sqlite3_column_type(stmt, i) {
            sq::SQLITE_INTEGER => Value::int(sq::sqlite3_column_int64(stmt, i)),
            sq::SQLITE_FLOAT => Value::float(sq::sqlite3_column_double(stmt, i)),
            sq::SQLITE_NULL => Value::nil(),
            // A BLOB is a plain String tagged BINARY, not a `SQLite3::Blob`
            // — that class exists only to mark a value for *binding*.
            sq::SQLITE_BLOB => {
                let ptr = sq::sqlite3_column_blob(stmt, i) as *const u8;
                if ptr.is_null() {
                    // A zero-length blob is not NULL, and SQLite may
                    // answer a NULL pointer for it.
                    ctx.bytes([])
                } else {
                    let len = sq::sqlite3_column_bytes(stmt, i).max(0) as usize;
                    ctx.bytes(std::slice::from_raw_parts(ptr, len))
                }
            }
            // TEXT, and anything else the library grows: `column_text`
            // converts whatever is there to UTF-8.
            _ => {
                let ptr = sq::sqlite3_column_text(stmt, i) as *const u8;
                if ptr.is_null() {
                    Value::nil()
                } else {
                    let len = sq::sqlite3_column_bytes(stmt, i).max(0) as usize;
                    ctx.str(std::slice::from_raw_parts(ptr, len))
                }
            }
        }
    }
}

/// The whole current row of `stmt` as an Array.
///
/// # Safety
/// As `column_value`.
unsafe fn row_value(ctx: &Ctx, stmt: *mut sq::sqlite3_stmt) -> Value {
    let row = ctx.ary_new();
    // SAFETY: the caller's contract.
    let count = unsafe { sq::sqlite3_column_count(stmt) };
    for i in 0..count {
        // SAFETY: `i` is below the column count.
        let v = unsafe { column_value(ctx, stmt, i) };
        let _ = ctx.ary_push(row, v);
    }
    row
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
unsafe fn row_value_as_text(ctx: &Ctx, stmt: *mut sq::sqlite3_stmt) -> Value {
    let row = ctx.ary_new();
    // SAFETY: the caller's contract.
    unsafe {
        let count = sq::sqlite3_column_count(stmt);
        for i in 0..count {
            let ptr = sq::sqlite3_column_text(stmt, i) as *const c_char;
            let v = if ptr.is_null() {
                Value::nil()
            } else {
                ctx.str(CStr::from_ptr(ptr).to_bytes())
            };
            let _ = ctx.ary_push(row, v);
        }
    }
    row
}

// ---------------------------------------------------------------------
// SQLite3 module functions
// ---------------------------------------------------------------------

/// SQLite3.libversion -> Integer
///
/// The version *number* (`3048000`), as the C extension answers; the
/// string is `SQLite3::SQLITE_VERSION`, from `libversion_string`.
fn libversion(_: &mut Ctx, _: Value, _: &[Value], _: Block) -> Result<Value> {
    // SAFETY: a pure query.
    Ok(Value::int(unsafe { sq::sqlite3_libversion_number() } as i64))
}

/// SQLite3.libversion_string -> String
fn libversion_string(ctx: &mut Ctx, _: Value, _: &[Value], _: Block) -> Result<Value> {
    // SAFETY: a static string in the library.
    Ok(unsafe { cstr_to_value(ctx, sq::sqlite3_libversion()) })
}

/// SQLite3.threadsafe -> Integer
fn threadsafe(_: &mut Ctx, _: Value, _: &[Value], _: Block) -> Result<Value> {
    // SAFETY: a pure query.
    Ok(Value::int(unsafe { sq::sqlite3_threadsafe() } as i64))
}

/// SQLite3.sqlcipher? -> false
fn sqlcipher_p(_: &mut Ctx, _: Value, _: &[Value], _: Block) -> Result<Value> {
    Ok(Value::bool(false))
}

// ---------------------------------------------------------------------
// Database
// ---------------------------------------------------------------------

/// Install `db` as the connection of `this` (a fresh `DbHandle`, or the
/// existing one's `db` slot).
fn set_db(ctx: &mut Ctx, this: Value, db: *mut sq::sqlite3) -> Result<()> {
    match ctx.native::<DbHandle>(this) {
        Some(h) => {
            h.db = db;
            Ok(())
        }
        None => ctx.native_set(
            this,
            DbHandle {
                db,
                funcs: vec![],
                aggregates: vec![],
            },
        ),
    }
}

/// The error of a connection `open` failed to make, which SQLite returns
/// anyway to carry the message; closed after reading.
fn open_failure(rc: c_int, db: *mut sq::sqlite3) -> String {
    if db.is_null() {
        return format!("sqlite3 error {rc}");
    }
    // SAFETY: a handle open enough to report its error.
    let m = unsafe { cstr_to_string(sq::sqlite3_errmsg(db)) };
    // SAFETY: ours, and no statement was ever prepared on it.
    unsafe { sq::sqlite3_close_v2(db) };
    m.unwrap_or_else(|| format!("sqlite3 error {rc}"))
}

/// Database#open_v2(filename, mode, zvfs) -> nil
fn db_open_v2(ctx: &mut Ctx, this: Value, args: &[Value], _: Block) -> Result<Value> {
    let filename = to_cstring(ctx, args[0])?;
    let mode = ctx.int(args[1])? as c_int;
    let zvfs = if args[2].is_nil() {
        None
    } else {
        Some(to_cstring(ctx, args[2])?)
    };
    let mut db: *mut sq::sqlite3 = std::ptr::null_mut();
    // The strings and the out-parameter are owned by this frame, which
    // stays put while the worker runs (the green thread parks here). The
    // library is `SQLITE_THREADSAFE=1`, so a connection opened on a
    // worker is usable from the interpreter thread afterwards.
    let (fname, vfs, out) = (
        filename.as_ptr(),
        zvfs.as_ref().map_or(std::ptr::null(), |v| v.as_ptr()),
        &mut db as *mut _,
    );
    let rc = ctx.call_blocking(|| {
        // SAFETY: see above.
        unsafe { sq::sqlite3_open_v2(fname, out, mode | sq::SQLITE_OPEN_URI, vfs) as i64 }
    })? as c_int;
    if rc != sq::SQLITE_OK {
        let msg = open_failure(rc, db);
        return Err(raise_code(ctx, rc, &msg, None));
    }
    set_db(ctx, this, db)?;
    Ok(Value::nil())
}

/// Database#open16(filename) -> nil
fn db_open16(ctx: &mut Ctx, this: Value, args: &[Value], _: Block) -> Result<Value> {
    // The gem hands UTF-16 bytes; `sqlite3_open16` wants them
    // NUL-terminated (two zero bytes).
    let mut bytes = ctx.str_vec(args[0])?;
    bytes.extend_from_slice(&[0, 0]);
    let mut db: *mut sq::sqlite3 = std::ptr::null_mut();
    // As `open_v2`: the buffer and the out-parameter are this frame's.
    let (fname, out) = (bytes.as_ptr() as *const c_void, &mut db as *mut _);
    let rc = ctx.call_blocking(|| {
        // SAFETY: see `open_v2`.
        unsafe { sq::sqlite3_open16(fname, out) as i64 }
    })? as c_int;
    if rc != sq::SQLITE_OK {
        let msg = open_failure(rc, db);
        return Err(raise_code(ctx, rc, &msg, None));
    }
    set_db(ctx, this, db)?;
    Ok(Value::nil())
}

/// Database#close -> nil
fn db_close(ctx: &mut Ctx, this: Value, _: &[Value], _: Block) -> Result<Value> {
    let Some(h) = ctx.native::<DbHandle>(this) else {
        return Ok(Value::nil());
    };
    if h.db.is_null() {
        return Ok(Value::nil());
    }
    // Clear the handle *before* parking: the object must not be closed
    // twice if another green thread reaches `close` while this one waits.
    let db = std::mem::replace(&mut h.db, std::ptr::null_mut());
    h.funcs.clear();
    // `close_v2` never fails for a live handle; with statements still open
    // the free is deferred to the last of them.
    ctx.call_blocking(|| {
        // SAFETY: our own connection, which may cross threads.
        unsafe { sq::sqlite3_close_v2(db) as i64 }
    })?;
    Ok(Value::nil())
}

/// Database#closed? -> bool
fn db_closed_p(ctx: &mut Ctx, this: Value, _: &[Value], _: Block) -> Result<Value> {
    Ok(Value::bool(
        ctx.native::<DbHandle>(this).is_none_or(|h| h.db.is_null()),
    ))
}

/// Database#encoding -> String
///
/// Every text this binding reads comes from `sqlite3_column_text`, which
/// converts to UTF-8 whatever the database is stored in.
fn db_encoding(ctx: &mut Ctx, this: Value, _: &[Value], _: Block) -> Result<Value> {
    db_of(ctx, this)?;
    Ok(ctx.str("UTF-8"))
}

/// Database#busy_timeout=(ms) -> nil
fn db_busy_timeout_assign(ctx: &mut Ctx, this: Value, args: &[Value], _: Block) -> Result<Value> {
    let db = db_of(ctx, this)?;
    let ms = ctx.int(args[0])? as c_int;
    // SAFETY: a live connection.
    let rc = unsafe { sq::sqlite3_busy_timeout(db, ms) };
    check(ctx, db, rc)?;
    Ok(Value::nil())
}

/// Database#busy_handler(&block) -> nil
///
/// Registering a Ruby block as SQLite's busy callback is not implemented;
/// the block is remembered (as the previous bridge did) so a program that
/// sets one still runs, driven by `busy_timeout`.
fn db_busy_handler(ctx: &mut Ctx, this: Value, _: &[Value], _: Block) -> Result<Value> {
    db_of(ctx, this)?;
    ctx.ivar_set(this, "@busy_handler", Value::nil())?;
    Ok(Value::nil())
}

/// Database#last_insert_row_id -> Integer
fn db_last_insert_row_id(ctx: &mut Ctx, this: Value, _: &[Value], _: Block) -> Result<Value> {
    let db = db_of(ctx, this)?;
    // SAFETY: a live connection.
    Ok(Value::int(unsafe { sq::sqlite3_last_insert_rowid(db) }))
}

/// Database#changes -> Integer
fn db_changes(ctx: &mut Ctx, this: Value, _: &[Value], _: Block) -> Result<Value> {
    let db = db_of(ctx, this)?;
    // SAFETY: a live connection.
    Ok(Value::int(unsafe { sq::sqlite3_changes(db) } as i64))
}

/// Database#total_changes -> Integer
fn db_total_changes(ctx: &mut Ctx, this: Value, _: &[Value], _: Block) -> Result<Value> {
    let db = db_of(ctx, this)?;
    // SAFETY: a live connection.
    Ok(Value::int(unsafe { sq::sqlite3_total_changes(db) } as i64))
}

/// Database#interrupt -> self
fn db_interrupt(ctx: &mut Ctx, this: Value, _: &[Value], _: Block) -> Result<Value> {
    let db = db_of(ctx, this)?;
    // SAFETY: a live connection; interrupting is safe from any thread.
    unsafe { sq::sqlite3_interrupt(db) };
    Ok(this)
}

/// Database#errcode -> Integer
fn db_errcode(ctx: &mut Ctx, this: Value, _: &[Value], _: Block) -> Result<Value> {
    let db = db_of(ctx, this)?;
    // SAFETY: a live connection.
    Ok(Value::int(unsafe { sq::sqlite3_errcode(db) } as i64))
}

/// Database#errmsg -> String
fn db_errmsg(ctx: &mut Ctx, this: Value, _: &[Value], _: Block) -> Result<Value> {
    let db = db_of(ctx, this)?;
    // SAFETY: a live connection; the message is owned by it and copied here.
    Ok(unsafe { cstr_to_value(ctx, sq::sqlite3_errmsg(db)) })
}

/// Database#db_filename(db_name = "main") -> String or nil
fn db_filename(ctx: &mut Ctx, this: Value, args: &[Value], _: Block) -> Result<Value> {
    let db = db_of(ctx, this)?;
    let name = if args[0].is_nil() {
        CString::new("main").unwrap()
    } else {
        to_cstring(ctx, args[0])?
    };
    // SAFETY: a live connection and a NUL-terminated name. An unnamed
    // (temporary or in-memory) database answers the empty string, and an
    // unknown schema name a NULL.
    let p = unsafe { sq::sqlite3_db_filename(db, name.as_ptr()) };
    // SAFETY: as above.
    Ok(unsafe { cstr_to_value(ctx, p) })
}

/// Database#extended_result_codes=(enable) -> nil
fn db_extended_result_codes(ctx: &mut Ctx, this: Value, args: &[Value], _: Block) -> Result<Value> {
    let db = db_of(ctx, this)?;
    let on = c_int::from(args[0].truthy());
    // SAFETY: a live connection.
    let rc = unsafe { sq::sqlite3_extended_result_codes(db, on) };
    check(ctx, db, rc)?;
    Ok(Value::nil())
}

/// Database#transaction_active? -> bool
fn db_transaction_active_p(ctx: &mut Ctx, this: Value, _: &[Value], _: Block) -> Result<Value> {
    let db = db_of(ctx, this)?;
    // SAFETY: a live connection.
    Ok(Value::bool(unsafe { sq::sqlite3_get_autocommit(db) } == 0))
}

/// Database#disable_quirk_mode -> bool
///
/// SQLite's "quirk" is accepting a double-quoted string as a literal
/// where a column name was meant. The gem turns it off for both DDL and
/// DML, and answers whether both took.
fn db_disable_quirk_mode(ctx: &mut Ctx, this: Value, _: &[Value], _: Block) -> Result<Value> {
    let db = db_of(ctx, this)?;
    // SAFETY: a live connection; both verbs take `(int onoff, int *pRes)`
    // and we pass a null result pointer, which SQLite allows.
    let ok = unsafe {
        sq::sqlite3_db_config(
            db,
            sq::SQLITE_DBCONFIG_DQS_DDL,
            0,
            std::ptr::null_mut::<c_int>(),
        ) == sq::SQLITE_OK
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
fn db_exec_batch(ctx: &mut Ctx, this: Value, args: &[Value], _: Block) -> Result<Value> {
    let db = db_of(ctx, this)?;
    let sql = ctx.str_vec(args[0])?;
    let rows = ctx.ary_new();
    ctx.temp_push(rows);
    // The statement text must be NUL-terminated for the tail pointer to be
    // meaningful; one copy serves the whole batch.
    let buf =
        CString::new(sql.clone()).map_err(|_| ctx.argument_error("string contains a NUL byte"))?;
    let base = buf.as_ptr();
    let mut cur = base;
    let end = sql.len() as isize;
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
            sq::sqlite3_prepare_v2(db, cur, (end - offset) as c_int, &mut stmt, &mut tail)
        };
        check(ctx, db, rc)?;
        if stmt.is_null() {
            // Only whitespace or a comment was left.
            break;
        }
        let guard = StmtHandle { stmt, done: false };
        loop {
            // A user function in this statement reaches the interpreter
            // through this guard, as in `stmt_step`.
            let mut call = CallGuard::new(ctx, db, this);
            // SAFETY: a live statement.
            let rc = unsafe { sq::sqlite3_step(stmt) };
            let err = call.take_error();
            drop(call);
            if let Some(e) = err {
                return Err(ctx.raise_exception(e));
            }
            match rc {
                // SAFETY: positioned on a row.
                sq::SQLITE_ROW => {
                    let row = unsafe { row_value_as_text(ctx, stmt) };
                    ctx.ary_push(rows, row)?;
                }
                sq::SQLITE_DONE => break,
                _ => {
                    check(ctx, db, rc)?;
                    break;
                }
            }
        }
        drop(guard);
        if tail.is_null() {
            break;
        }
        cur = tail;
    }
    Ok(rows)
}

/// Database#enable_load_extension(onoff) -> nil
///
/// The bundled SQLite is built with `SQLITE_OMIT_LOAD_EXTENSION`, so there
/// is nothing to enable; the previous bridge also refused, and answering
/// quietly keeps `Database#initialize` working.
fn db_enable_load_extension(ctx: &mut Ctx, this: Value, _: &[Value], _: Block) -> Result<Value> {
    db_of(ctx, this)?;
    Ok(Value::nil())
}

/// Database#load_extension_internal(path) -> raises
fn db_load_extension(ctx: &mut Ctx, this: Value, _: &[Value], _: Block) -> Result<Value> {
    db_of(ctx, this)?;
    Err(err_sqlite3(
        ctx,
        "load_extension is not available: monoruby's SQLite is built without extension loading",
    ))
}

/// Database#trace(mask = nil, &block) -> the previous block
fn db_trace(ctx: &mut Ctx, this: Value, _: &[Value], _: Block) -> Result<Value> {
    db_of(ctx, this)?;
    let prev = ctx.ivar_get(this, "@tracefunc");
    ctx.ivar_set(this, "@tracefunc", Value::nil())?;
    Ok(prev)
}

/// Database#authorizer=(block) -> nil
fn db_authorizer_assign(ctx: &mut Ctx, this: Value, args: &[Value], _: Block) -> Result<Value> {
    db_of(ctx, this)?;
    ctx.ivar_set(this, "@authorizer", args[0])?;
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
/// callback stashes it here (as an exception object, rooted on the temp
/// stack for the rest of the step), tells SQLite the function failed,
/// and the method that called `sqlite3_step` re-raises it once control
/// is back in Rust.
struct CallState {
    /// The context of the method call the step runs in — valid for the
    /// whole step, callbacks and parks included.
    ctx: *mut MrContext,
    /// The `SQLite3::Database` this step is running on, so an aggregate
    /// callback can reach the instance table in its `DbHandle`. Rooted
    /// for the whole step by whatever is being stepped: the statement
    /// holds it in `@connection`, and `exec_batch` is a method on it.
    db_obj: Value,
    error: Option<Value>,
}

thread_local! {
    /// The step in progress on each connection.
    ///
    /// Keyed by connection, and saved-and-restored rather than pushed
    /// and popped, because green-thread switches are not LIFO: a
    /// callback that parks (any `sleep` or IO in the block) lets another
    /// green thread run, and it may finish its own step first. A plain
    /// stack would then have one thread pop the other's entry and hand a
    /// callback the wrong context. Per connection, two green threads
    /// stepping two connections never meet; nesting on one connection —
    /// which SQLite allows, and the C extension permits — still works,
    /// since the inner guard restores the outer on the way out.
    static CALLS: RefCell<HashMap<usize, *mut CallState>> = RefCell::new(HashMap::default());
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
    /// when a registration fails outright.
    static FUNC_COUNT: Cell<usize> = const { Cell::new(0) };
}

/// Make the context reachable from a callback for the duration of a step,
/// and take back any exception a callback left behind.
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
    fn new(ctx: &mut Ctx, db: *mut sq::sqlite3, db_obj: Value) -> Self {
        if !guard_needed() {
            return Self { inner: None };
        }
        // Boxed so the address stays put however the map grows.
        let mut state = Box::new(CallState {
            ctx: ctx.raw(),
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
    fn take_error(&mut self) -> Option<Value> {
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
unsafe fn arg_value(ctx: &Ctx, v: *mut sq::sqlite3_value) -> Value {
    // SAFETY: the caller's contract.
    unsafe {
        match sq::sqlite3_value_type(v) {
            sq::SQLITE_INTEGER => Value::int(sq::sqlite3_value_int64(v)),
            sq::SQLITE_FLOAT => Value::float(sq::sqlite3_value_double(v)),
            sq::SQLITE_NULL => Value::nil(),
            sq::SQLITE_BLOB => {
                let ptr = sq::sqlite3_value_blob(v) as *const u8;
                if ptr.is_null() {
                    ctx.bytes([])
                } else {
                    let len = sq::sqlite3_value_bytes(v).max(0) as usize;
                    ctx.bytes(std::slice::from_raw_parts(ptr, len))
                }
            }
            _ => {
                let ptr = sq::sqlite3_value_text(v) as *const u8;
                if ptr.is_null() {
                    Value::nil()
                } else {
                    let len = sq::sqlite3_value_bytes(v).max(0) as usize;
                    ctx.str(std::slice::from_raw_parts(ptr, len))
                }
            }
        }
    }
}

/// How a Ruby value goes to SQLite — as a bound parameter or a function
/// result. The types the C extension accepts, and no others: nil,
/// Integer, Float and String (a `SQLite3::Blob` or a BINARY-encoded
/// String as a BLOB). Anything else is `Err(None)`, for the caller's
/// RuntimeError.
enum Sql {
    Null,
    Int(i64),
    Float(f64),
    Text(Vec<u8>),
    Blob(Vec<u8>),
}

fn to_sql(ctx: &mut Ctx, value: Value) -> Result<Option<Sql>> {
    Ok(Some(match ctx.type_of(value) {
        MrType::Nil => Sql::Null,
        MrType::Integer => match ctx.int(value) {
            Ok(i) => Sql::Int(i),
            // An Integer too wide for i64 goes as a double, as the C
            // extension's `rb_Float` fallback does.
            Err(_) => {
                let _ = ctx.error_take();
                Sql::Float(ctx.float(value)?)
            }
        },
        MrType::Float => Sql::Float(ctx.float(value)?),
        MrType::String => {
            let bytes = ctx.str_vec(value)?;
            // Bound as a BLOB when the String says it is bytes: a
            // `SQLite3::Blob` (the gem's marker subclass) or any
            // BINARY-encoded String, which is what the C extension does.
            if ctx.str_is_binary(value)? || is_blob(ctx, value) {
                Sql::Blob(bytes)
            } else {
                Sql::Text(bytes)
            }
        }
        _ => return Ok(None),
    }))
}

/// Whether `v` is a `SQLite3::Blob` (the gem's String subclass for BLOB
/// columns).
fn is_blob(ctx: &Ctx, v: Value) -> bool {
    match ctx.const_get(sqlite3_module(), "Blob") {
        Some(blob) => ctx.is_kind_of(v, blob),
        None => false,
    }
}

/// Hand the block's answer back to SQLite. The C extension refuses
/// anything it cannot represent with a RuntimeError rather than storing
/// a NULL.
fn set_result(ctx: &mut Ctx, sctx: *mut sq::sqlite3_context, value: Value) -> Result<()> {
    let Some(sql) = to_sql(ctx, value)? else {
        return Err(ctx.runtime_error(format!("can't return {}", ctx.class_name(value))));
    };
    // SAFETY: a live context, and buffers SQLite copies before returning
    // (`SQLITE_TRANSIENT`).
    unsafe {
        match sql {
            Sql::Null => sq::sqlite3_result_null(sctx),
            Sql::Int(i) => sq::sqlite3_result_int64(sctx, i),
            Sql::Float(f) => sq::sqlite3_result_double(sctx, f),
            Sql::Blob(b) => sq::sqlite3_result_blob(
                sctx,
                b.as_ptr() as *const c_void,
                b.len() as c_int,
                sq::SQLITE_TRANSIENT(),
            ),
            Sql::Text(b) => sq::sqlite3_result_text(
                sctx,
                b.as_ptr() as *const c_char,
                b.len() as c_int,
                sq::SQLITE_TRANSIENT(),
            ),
        }
    }
    Ok(())
}

/// Tell SQLite the function failed. The message is only what SQLite
/// reports; the Ruby exception itself travels in `CallState::error`.
fn result_error(sctx: *mut sq::sqlite3_context, msg: &str) {
    if let Ok(c) = CString::new(msg) {
        // SAFETY: a live context and a NUL-terminated string SQLite copies.
        unsafe { sq::sqlite3_result_error(sctx, c.as_ptr(), -1) };
    }
}

/// Record a callback's failure: tell SQLite, and keep the exception —
/// rooted on the temp stack for the rest of the step — for the method
/// to re-raise.
fn stash_error(ctx: &mut Ctx, state: &mut CallState, sctx: *mut sq::sqlite3_context) {
    let ex = ctx.error_take();
    let msg = ex
        .map(|e| ctx.inspect(e))
        .unwrap_or_else(|| "error".to_string());
    result_error(sctx, &msg);
    if let Some(ex) = ex {
        ctx.temp_push(ex);
    }
    state.error = ex;
}

/// Recover the running step and this function's registration, or report
/// to SQLite why the callback cannot run.
///
/// # Safety
/// A live context, inside a callback SQLite is making.
unsafe fn callback_state(
    sctx: *mut sq::sqlite3_context,
) -> Option<(&'static mut CallState, &'static FuncEntry)> {
    // SAFETY: the caller's contract.
    unsafe {
        let Some(state) = current_call(sq::sqlite3_context_db_handle(sctx)) else {
            // No step of ours is running, so there is no interpreter to
            // re-enter and nowhere to put an exception.
            result_error(sctx, "sqlite3 function called outside a query");
            return None;
        };
        let state = &mut *state;
        // A previous row already failed; this evaluation is being torn
        // down, so do not run the block again.
        if state.error.is_some() {
            result_error(sctx, "aborted");
            return None;
        }
        Some((state, &*(sq::sqlite3_user_data(sctx) as *const FuncEntry)))
    }
}

/// `xFunc`: SQLite calls this once per row the function appears in,
/// from inside `sqlite3_step`.
unsafe extern "C" fn func_invoke(
    sctx: *mut sq::sqlite3_context,
    argc: c_int,
    argv: *mut *mut sq::sqlite3_value,
) {
    // SAFETY: SQLite passes a live context, and `argv` holds `argc` live
    // values for the duration of the call.
    unsafe {
        let Some((state, entry)) = callback_state(sctx) else {
            return;
        };
        let mut ctx = Ctx::from_raw(state.ctx);
        // The arguments stay rooted while they are built: each one is a
        // fresh object, and allocating the next may collect.
        let len = ctx.temp_len();
        let mut args = Vec::with_capacity(argc.max(0) as usize);
        for i in 0..argc as isize {
            let v = arg_value(&ctx, *argv.offset(i));
            ctx.temp_push(v);
            args.push(v);
        }
        let result = ctx
            .proc_call(entry.value, &args)
            .and_then(|v| set_result(&mut ctx, sctx, v));
        ctx.temp_truncate(len);
        if result.is_err() {
            stash_error(&mut ctx, state, sctx);
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
    ctx: &mut Ctx,
    state: &mut CallState,
    sctx: *mut sq::sqlite3_context,
    klass: Value,
) -> Result<(Value, *mut u32)> {
    // SAFETY: the caller's contract.
    let slot_ptr = unsafe { sq::sqlite3_aggregate_context(sctx, 4) as *mut u32 };
    if slot_ptr.is_null() {
        // The one failure SQLite reports this way.
        return Err(ctx.runtime_error("out of memory"));
    }
    // SAFETY: four bytes SQLite zeroed for us and keeps for this group.
    let slot = unsafe { *slot_ptr };
    if slot != 0
        && let Some(inst) = db_handle(ctx, state.db_obj)?
            .aggregates
            .get(slot as usize - 1)
            .copied()
            .flatten()
    {
        return Ok((inst, slot_ptr));
    }
    // First row of this group: one instance of the proxy class, which
    // holds the caller's `FunctionProxy` as its context.
    let inst = ctx.funcall(klass, "new", &[], None)?;
    let table = &mut db_handle(ctx, state.db_obj)?.aggregates;
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
unsafe fn release_aggregate(ctx: &mut Ctx, state: &mut CallState, slot_ptr: *mut u32) {
    // SAFETY: the caller's contract; the slot is one-based and non-zero.
    let slot = unsafe { *slot_ptr };
    if let Some(h) = ctx.native::<DbHandle>(state.db_obj)
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
unsafe fn release_aborted_aggregate(sctx: *mut sq::sqlite3_context) {
    // SAFETY: the caller's contract; asking with 0 never allocates, so
    // a group that never stepped answers null and has nothing to free.
    let slot_ptr = unsafe { sq::sqlite3_aggregate_context(sctx, 0) as *mut u32 };
    // SAFETY: as above.
    let Some(state) = (unsafe { current_call(sq::sqlite3_context_db_handle(sctx)) }) else {
        return;
    };
    if !slot_ptr.is_null() {
        // SAFETY: the slot SQLite kept for this group, still one-based;
        // the state's context is the step's.
        unsafe {
            let mut ctx = Ctx::from_raw((*state).ctx);
            release_aggregate(&mut ctx, &mut *state, slot_ptr);
        }
    }
}

/// `xStep`: once per row the aggregate sees, from inside `sqlite3_step`.
unsafe extern "C" fn agg_step(
    sctx: *mut sq::sqlite3_context,
    argc: c_int,
    argv: *mut *mut sq::sqlite3_value,
) {
    // SAFETY: SQLite passes a live context and `argc` live values.
    unsafe {
        let Some((state, entry)) = callback_state(sctx) else {
            return;
        };
        let klass = entry.value;
        let mut ctx = Ctx::from_raw(state.ctx);
        // The instance and the arguments stay rooted while the rest are
        // built: each is fresh, and allocating the next may collect.
        let len = ctx.temp_len();
        let result = (|| -> Result<()> {
            let (inst, _) = aggregate_instance(&mut ctx, state, sctx, klass)?;
            ctx.temp_push(inst);
            let mut args = Vec::with_capacity(argc.max(0) as usize);
            for i in 0..argc as isize {
                let v = arg_value(&ctx, *argv.offset(i));
                ctx.temp_push(v);
                args.push(v);
            }
            ctx.funcall(inst, "step", &args, None)?;
            Ok(())
        })();
        ctx.temp_truncate(len);
        if result.is_err() {
            stash_error(&mut ctx, state, sctx);
        }
    }
}

/// `xFinal`: once per group, after its last row.
///
/// SQLite calls this even for a group that never stepped — an aggregate
/// over no rows — and the C extension answers what a fresh instance's
/// `finalize` returns, so one is created here in that case.
unsafe extern "C" fn agg_final(sctx: *mut sq::sqlite3_context) {
    // SAFETY: SQLite passes a live context.
    unsafe {
        let Some((state, entry)) = callback_state(sctx) else {
            release_aborted_aggregate(sctx);
            return;
        };
        let klass = entry.value;
        let mut ctx = Ctx::from_raw(state.ctx);
        let len = ctx.temp_len();
        // The slot this group's instance sits in, so it can be freed
        // below whether `finalize` answered or raised.
        let mut slot_ptr: *mut u32 = std::ptr::null_mut();
        let result = (|| -> Result<()> {
            // A group that never stepped starts its instance here, which
            // is what makes an aggregate over no rows answer.
            let (inst, p) = aggregate_instance(&mut ctx, state, sctx, klass)?;
            slot_ptr = p;
            ctx.temp_push(inst);
            let v = ctx.funcall(inst, "finalize", &[], None)?;
            ctx.temp_push(v);
            set_result(&mut ctx, sctx, v)
        })();
        ctx.temp_truncate(len);
        // The group is over either way: its instance must not outlive it.
        if !slot_ptr.is_null() {
            release_aggregate(&mut ctx, state, slot_ptr);
        }
        if result.is_err() {
            stash_error(&mut ctx, state, sctx);
        }
    }
}

/// A function name as SQLite takes it: NUL-terminated, so a name holding
/// a NUL byte registers only the part before it — which is what the C
/// extension does, and `"a\0b"` really does define `a`.
fn function_name(ctx: &mut Ctx, v: Value) -> Result<(String, CString)> {
    let bytes = ctx.str_vec(v)?;
    let upto = bytes.iter().position(|&b| b == 0).unwrap_or(bytes.len());
    let cname = CString::new(&bytes[..upto]).expect("no NUL before the first NUL");
    Ok((String::from_utf8_lossy(&bytes).into_owned(), cname))
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
fn db_define_aggregator(ctx: &mut Ctx, this: Value, args: &[Value], _: Block) -> Result<Value> {
    let db = db_of(ctx, this)?;
    let klass = args[0];
    let (_, cname) = function_name(ctx, args[1])?;
    let arity = ctx.funcall(klass, "arity", &[], None)?;
    let arity = ctx.int(arity)? as c_int;
    // Rooted for as long as SQLite may instantiate it.
    db_handle(ctx, this)?.funcs.push(klass);
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
        check(ctx, db, rc)?;
    }
    Ok(this)
}

/// Database#define_function_with_flags(name, flags, &block) -> self
///
/// The C-level half of `create_function`; the gem's Ruby half wraps the
/// caller's block in one that builds a `FunctionProxy` and answers its
/// `result`. Registered with an arity of -1, as the extension does: the
/// arity passed to `create_function` never reaches SQLite, so
/// `create_function("f", 1)` really does accept `f(1, 2)`.
fn db_define_function(ctx: &mut Ctx, this: Value, args: &[Value], block: Block) -> Result<Value> {
    // The gem passes the text-rep flags straight through; UTF-8 is the
    // only encoding this binding reads, and SQLITE_DETERMINISTIC is the
    // one other bit worth honouring.
    let flags = ctx.int(args[1])? as c_int;
    register_function(ctx, this, args[0], block, flags)
}

/// Database#define_function(name, &block) -> self
///
/// `define_function_with_flags` with no flags.
fn db_define_function_plain(
    ctx: &mut Ctx,
    this: Value,
    args: &[Value],
    block: Block,
) -> Result<Value> {
    register_function(ctx, this, args[0], block, 0)
}

/// Register `name` as a scalar function running `block`.
fn register_function(
    ctx: &mut Ctx,
    this: Value,
    name: Value,
    block: Block,
    flags: c_int,
) -> Result<Value> {
    let db = db_of(ctx, this)?;
    let (name, cname) = function_name(ctx, name)?;
    let enc = sq::SQLITE_UTF8 | (flags & sq::SQLITE_DETERMINISTIC);
    if !block.is_given() {
        // What `Proc.new` says, which is where the C extension ends up.
        return Err(ctx.argument_error("tried to create Proc object without a block"));
    }
    let proc_ = ctx.block_to_proc(block)?;
    // Rooted here for as long as SQLite may call it; the `FuncEntry`
    // below holds the same Value but is invisible to the collector.
    db_handle(ctx, this)?.funcs.push(proc_);
    // The C extension also records it under the name the caller gave,
    // which `Database#functions` exposes.
    let funcs = ctx.ivar_get(this, "@functions");
    if ctx.type_of(funcs) == MrType::Hash {
        let k = ctx.str(&name);
        ctx.hash_set(funcs, k, proc_)?;
    }
    let entry = Box::into_raw(Box::new(FuncEntry { value: proc_ }));
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
        check(ctx, db, rc)?;
    }
    Ok(this)
}

/// Database#complete?(sql) -> bool
///
/// Whether the text forms one or more complete statements — what a REPL
/// asks before deciding to keep reading.
fn db_complete_p(ctx: &mut Ctx, this: Value, args: &[Value], _: Block) -> Result<Value> {
    db_of(ctx, this)?;
    let sql = ctx.str_vec(args[0])?;
    let Ok(c) = CString::new(sql) else {
        // An interior NUL cannot end a statement.
        return Ok(Value::bool(false));
    };
    // SAFETY: a NUL-terminated string that outlives the call.
    Ok(Value::bool(
        unsafe { sq::sqlite3_complete(c.as_ptr()) } != 0,
    ))
}

/// Database#collation(name, comparator) -> self
///
/// A comparator is a Ruby object SQLite would call back into, so only
/// removing one (a nil comparator) can be honoured; anything else
/// refuses, as `create_function` does.
fn db_collation(ctx: &mut Ctx, this: Value, args: &[Value], _: Block) -> Result<Value> {
    db_of(ctx, this)?;
    if !args[1].is_nil() {
        return Err(err_sqlite3(
            ctx,
            "collation is not supported by monoruby's sqlite3 binding",
        ));
    }
    Ok(this)
}

/// Database#statement_timeout=(ms) -> ms
///
/// The gem enforces this with a progress handler, which is a Ruby
/// callback from SQLite; the value is remembered so `initialize` and the
/// accessor work, but nothing interrupts a long statement.
fn db_statement_timeout_assign(
    ctx: &mut Ctx,
    this: Value,
    args: &[Value],
    _: Block,
) -> Result<Value> {
    db_of(ctx, this)?;
    ctx.ivar_set(this, "@statement_timeout", args[0])?;
    Ok(args[0])
}

/// Database#discard -> nil
///
/// Abandon the connection without closing it: what the gem's fork
/// safety does in a child, where running the parent's `close` would
/// checkpoint and unlink files the parent still owns. The handle is
/// dropped on the floor deliberately — the OS reclaims it with the
/// process.
fn db_discard(ctx: &mut Ctx, this: Value, _: &[Value], _: Block) -> Result<Value> {
    if let Some(h) = ctx.native::<DbHandle>(this) {
        h.db = std::ptr::null_mut();
        h.funcs.clear();
    }
    Ok(Value::nil())
}

// ---------------------------------------------------------------------
// Statement
// ---------------------------------------------------------------------

/// Statement#prepare(db, sql) -> String (the unparsed remainder)
fn stmt_prepare(ctx: &mut Ctx, this: Value, args: &[Value], _: Block) -> Result<Value> {
    let db = db_of(ctx, args[0])?;
    let sql = ctx.str_vec(args[1])?;
    let buf =
        CString::new(sql.clone()).map_err(|_| ctx.argument_error("string contains a NUL byte"))?;
    let mut stmt: *mut sq::sqlite3_stmt = std::ptr::null_mut();
    let mut tail: *const c_char = std::ptr::null();
    // SAFETY: a live connection and a NUL-terminated statement that
    // outlives the call (SQLite keeps its own copy of the text).
    let rc = unsafe {
        sq::sqlite3_prepare_v2(db, buf.as_ptr(), sql.len() as c_int, &mut stmt, &mut tail)
    };
    if rc != sq::SQLITE_OK {
        // SAFETY: a live connection.
        let msg = unsafe { cstr_to_string(sq::sqlite3_errmsg(db)) }
            .unwrap_or_else(|| format!("sqlite3 error {rc}"));
        let text = String::from_utf8_lossy(&sql).into_owned();
        return Err(raise_code(ctx, rc, &msg, Some((db, &text))));
    }
    match ctx.native::<StmtHandle>(this) {
        Some(h) => {
            if !h.stmt.is_null() {
                // SAFETY: ours; re-preparing replaces it.
                unsafe { sq::sqlite3_finalize(h.stmt) };
            }
            h.stmt = stmt;
            h.done = false;
        }
        None => ctx.native_set(this, StmtHandle { stmt, done: false })?,
    }
    // The remainder, by the tail's offset into our copy.
    let remainder: &[u8] = if tail.is_null() {
        &[]
    } else {
        // SAFETY: `tail` points into `buf`, at or before its NUL.
        let offset = unsafe { tail.offset_from(buf.as_ptr()) } as usize;
        sql.get(offset..).unwrap_or(&[])
    };
    Ok(ctx.str(remainder))
}

/// Statement#close -> nil
fn stmt_close(ctx: &mut Ctx, this: Value, _: &[Value], _: Block) -> Result<Value> {
    if let Some(h) = ctx.native::<StmtHandle>(this)
        && !h.stmt.is_null()
    {
        // SAFETY: ours.
        unsafe { sq::sqlite3_finalize(h.stmt) };
        h.stmt = std::ptr::null_mut();
    }
    Ok(Value::nil())
}

/// Statement#closed? -> bool
fn stmt_closed_p(ctx: &mut Ctx, this: Value, _: &[Value], _: Block) -> Result<Value> {
    Ok(Value::bool(
        ctx.native::<StmtHandle>(this)
            .is_none_or(|h| h.stmt.is_null()),
    ))
}

/// Statement#step -> Array of column values, or nil at the end
///
/// The row is read here, in one call.
fn stmt_step(ctx: &mut Ctx, this: Value, _: &[Value], _: Block) -> Result<Value> {
    let stmt = stmt_of(ctx, this)?;
    // A statement that already reported `SQLITE_DONE` answers nil without
    // stepping: `sqlite3_step` past the end silently resets and re-runs the
    // query, and the C extension's `done_p` guard is what stops that.
    if stmt_handle(ctx, this)?.done {
        return Ok(Value::nil());
    }
    // A user function reaches the interpreter through this guard, and
    // leaves an exception on it rather than unwinding through SQLite.
    // SAFETY: a live statement.
    let db = unsafe { sq::sqlite3_db_handle(stmt) };
    // The statement's connection, which an aggregate callback needs to
    // reach its instance table. Read only when a callback is possible:
    // this is one call per row, and an ivar lookup on it is not free.
    let db_obj = if guard_needed() {
        ctx.ivar_get(this, "@connection")
    } else {
        Value::nil()
    };
    let mut guard = CallGuard::new(ctx, db, db_obj);
    // SAFETY: a live statement.
    let rc = unsafe { sq::sqlite3_step(stmt) };
    let err = guard.take_error();
    drop(guard);
    if let Some(e) = err {
        return Err(ctx.raise_exception(e));
    }
    let h = stmt_handle(ctx, this)?;
    match rc {
        sq::SQLITE_ROW => {
            h.done = false;
            // SAFETY: positioned on a row.
            Ok(unsafe { row_value(ctx, stmt) })
        }
        sq::SQLITE_DONE => {
            h.done = true;
            Ok(Value::nil())
        }
        _ => {
            check(ctx, db, rc)?;
            Ok(Value::nil())
        }
    }
}

/// Statement#done? -> bool
fn stmt_done_p(ctx: &mut Ctx, this: Value, _: &[Value], _: Block) -> Result<Value> {
    Ok(Value::bool(
        ctx.native::<StmtHandle>(this).is_some_and(|h| h.done),
    ))
}

/// Statement#reset! -> self
fn stmt_reset(ctx: &mut Ctx, this: Value, _: &[Value], _: Block) -> Result<Value> {
    let stmt = stmt_of(ctx, this)?;
    // SAFETY: a live statement. A failing `reset` reports the error of the
    // last `step`, which the caller has already seen.
    unsafe {
        sq::sqlite3_reset(stmt);
        sq::sqlite3_clear_bindings(stmt);
    }
    stmt_handle(ctx, this)?.done = false;
    // The C extension answers self; ActiveRecord's statement pool chains
    // off the return value.
    Ok(this)
}

/// Bind `value` to parameter `index` (1-based). `true`, `false`, a
/// Symbol, an Array — anything `to_sql` refuses — is a
/// `RuntimeError: can't prepare <Class>`, which is what the gem's Ruby
/// half relies on to reject them.
fn bind_one(
    ctx: &mut Ctx,
    stmt: *mut sq::sqlite3_stmt,
    index: c_int,
    value: Value,
) -> Result<c_int> {
    let Some(sql) = to_sql(ctx, value)? else {
        return Err(ctx.runtime_error(format!("can't prepare {}", ctx.class_name(value))));
    };
    // SAFETY: a live statement, an index the caller checked, and buffers
    // that outlive the call — `SQLITE_TRANSIENT` makes SQLite copy them.
    Ok(unsafe {
        match sql {
            Sql::Null => sq::sqlite3_bind_null(stmt, index),
            Sql::Int(i) => sq::sqlite3_bind_int64(stmt, index, i),
            Sql::Float(f) => sq::sqlite3_bind_double(stmt, index, f),
            Sql::Blob(b) => sq::sqlite3_bind_blob(
                stmt,
                index,
                b.as_ptr() as *const c_void,
                b.len() as c_int,
                sq::SQLITE_TRANSIENT(),
            ),
            Sql::Text(b) => sq::sqlite3_bind_text(
                stmt,
                index,
                b.as_ptr() as *const c_char,
                b.len() as c_int,
                sq::SQLITE_TRANSIENT(),
            ),
        }
    })
}

/// Statement#bind_param(index, value) -> nil
fn stmt_bind_param(ctx: &mut Ctx, this: Value, args: &[Value], _: Block) -> Result<Value> {
    let stmt = stmt_of(ctx, this)?;
    let key = args[0];
    let named = match ctx.type_of(key) {
        // A name, with or without its leading marker.
        MrType::String => Some(ctx.str_string(key)?),
        MrType::Symbol => Some(format!(":{}", ctx.sym_name(key)?)),
        _ => None,
    };
    let index = match named {
        Some(name) => {
            let name = if name.starts_with([':', '@', '$']) {
                name
            } else {
                format!(":{name}")
            };
            let c = CString::new(name.as_bytes())
                .map_err(|_| ctx.argument_error("string contains a NUL byte"))?;
            // SAFETY: a live statement and a NUL-terminated name.
            let i = unsafe { sq::sqlite3_bind_parameter_index(stmt, c.as_ptr()) };
            if i == 0 {
                // The C extension names no parameter in this message.
                return Err(err_sqlite3(ctx, "no such bind parameter"));
            }
            i
        }
        None => ctx.int(key)? as c_int,
    };
    let rc = bind_one(ctx, stmt, index, args[1])?;
    if rc != sq::SQLITE_OK {
        // SAFETY: the statement's own connection.
        let db = unsafe { sq::sqlite3_db_handle(stmt) };
        check(ctx, db, rc)?;
    }
    Ok(Value::nil())
}

/// Statement#column_count -> Integer
fn stmt_column_count(ctx: &mut Ctx, this: Value, _: &[Value], _: Block) -> Result<Value> {
    let stmt = stmt_of(ctx, this)?;
    // SAFETY: a live statement.
    Ok(Value::int(unsafe { sq::sqlite3_column_count(stmt) } as i64))
}

/// Statement#column_name(index) -> String or nil
fn stmt_column_name(ctx: &mut Ctx, this: Value, args: &[Value], _: Block) -> Result<Value> {
    let stmt = stmt_of(ctx, this)?;
    let i = ctx.int(args[0])? as c_int;
    // SAFETY: a live statement; an out-of-range index answers NULL.
    Ok(unsafe { cstr_to_value(ctx, sq::sqlite3_column_name(stmt, i)) })
}

/// Statement#column_decltype(index) -> String or nil
fn stmt_column_decltype(ctx: &mut Ctx, this: Value, args: &[Value], _: Block) -> Result<Value> {
    let stmt = stmt_of(ctx, this)?;
    let i = ctx.int(args[0])? as c_int;
    // SAFETY: as `column_name`.
    Ok(unsafe { cstr_to_value(ctx, sq::sqlite3_column_decltype(stmt, i)) })
}

/// Statement#bind_parameter_count -> Integer
fn stmt_bind_parameter_count(ctx: &mut Ctx, this: Value, _: &[Value], _: Block) -> Result<Value> {
    let stmt = stmt_of(ctx, this)?;
    // SAFETY: a live statement.
    Ok(Value::int(
        unsafe { sq::sqlite3_bind_parameter_count(stmt) } as i64,
    ))
}

/// Statement#sql -> String
///
/// The statement's text as it was prepared.
fn stmt_sql(ctx: &mut Ctx, this: Value, _: &[Value], _: Block) -> Result<Value> {
    let stmt = stmt_of(ctx, this)?;
    // SAFETY: a live statement; the text it borrows lives as long as it
    // does, and is copied here.
    Ok(unsafe { cstr_to_value(ctx, sq::sqlite3_sql(stmt)) })
}

/// Statement#expanded_sql -> String
///
/// The text with the bound parameters substituted.
fn stmt_expanded_sql(ctx: &mut Ctx, this: Value, _: &[Value], _: Block) -> Result<Value> {
    let stmt = stmt_of(ctx, this)?;
    // SAFETY: a live statement. Unlike `sqlite3_sql`, this buffer is
    // ours to free.
    unsafe {
        let p = sq::sqlite3_expanded_sql(stmt);
        if p.is_null() {
            return Ok(Value::nil());
        }
        let v = cstr_to_value(ctx, p);
        sq::sqlite3_free(p as *mut c_void);
        Ok(v)
    }
}

/// Statement#memused -> Integer
fn stmt_memused(ctx: &mut Ctx, this: Value, _: &[Value], _: Block) -> Result<Value> {
    let stmt = stmt_of(ctx, this)?;
    // SAFETY: a live statement; `reset` of 0 leaves the counter alone.
    let n = unsafe { sq::sqlite3_stmt_status(stmt, sq::SQLITE_STMTSTATUS_MEMUSED, 0) };
    Ok(Value::int(n as i64))
}

/// Statement#clear_bindings! -> self
fn stmt_clear_bindings(ctx: &mut Ctx, this: Value, _: &[Value], _: Block) -> Result<Value> {
    let stmt = stmt_of(ctx, this)?;
    // SAFETY: a live statement.
    unsafe { sq::sqlite3_clear_bindings(stmt) };
    Ok(this)
}

/// The `sqlite3_stmt_status` counters `stat_for` / `stats_as_hash` name.
const STAT_COUNTERS: [(&str, c_int); 8] = [
    ("fullscan_steps", sq::SQLITE_STMTSTATUS_FULLSCAN_STEP),
    ("sorts", sq::SQLITE_STMTSTATUS_SORT),
    ("autoindexes", sq::SQLITE_STMTSTATUS_AUTOINDEX),
    ("vm_steps", sq::SQLITE_STMTSTATUS_VM_STEP),
    ("reprepares", sq::SQLITE_STMTSTATUS_REPREPARE),
    ("runs", sq::SQLITE_STMTSTATUS_RUN),
    ("filter_misses", sq::SQLITE_STMTSTATUS_FILTER_MISS),
    ("filter_hits", sq::SQLITE_STMTSTATUS_FILTER_HIT),
];

/// Statement#stat_for(key) -> Integer
fn stmt_stat_for(ctx: &mut Ctx, this: Value, args: &[Value], _: Block) -> Result<Value> {
    let stmt = stmt_of(ctx, this)?;
    // The C extension takes a Symbol and nothing else — a String key
    // is a TypeError, not a lookup — and rejects an unknown one rather
    // than answering zero.
    if ctx.type_of(args[0]) != MrType::Symbol {
        return Err(ctx.type_error("non-symbol given"));
    }
    let name = ctx.sym_name(args[0])?;
    let Some((_, counter)) = STAT_COUNTERS.iter().find(|(n, _)| *n == name) else {
        return Err(ctx.argument_error(format!("unknown key: {name}")));
    };
    // SAFETY: a live statement and a known counter.
    Ok(Value::int(
        unsafe { sq::sqlite3_stmt_status(stmt, *counter, 0) } as i64,
    ))
}

/// Statement#stats_as_hash -> Hash
fn stmt_stats_as_hash(ctx: &mut Ctx, this: Value, _: &[Value], _: Block) -> Result<Value> {
    let stmt = stmt_of(ctx, this)?;
    let h = ctx.hash_new();
    for (name, counter) in STAT_COUNTERS {
        // SAFETY: a live statement and a known counter.
        let v = unsafe { sq::sqlite3_stmt_status(stmt, counter, 0) };
        let k = ctx.sym(name);
        ctx.hash_set(h, k, Value::int(v as i64))?;
    }
    Ok(h)
}

// ---------------------------------------------------------------------
// Backup
// ---------------------------------------------------------------------

/// Backup#initialize(dstdb, dstname, srcdb, srcname) -> nil
///
/// `sqlite3_backup_init`: a backup of database `srcname` of `srcdb` into
/// database `dstname` of `dstdb` (`"main"`, `"temp"`, or an attached
/// name). The connections stay open and owned by their Database objects.
fn backup_initialize(ctx: &mut Ctx, this: Value, args: &[Value], _: Block) -> Result<Value> {
    // The extension checks both connections with the Backup type's own
    // `TypedData_Get_Struct`, hence the odd "expected" in its message.
    for v in [args[0], args[2]] {
        if ctx.type_of(v) != MrType::Native {
            let name = ctx.class_name(v);
            return Err(ctx.type_error(format!(
                "wrong argument type {name} (expected SQLite3::Backup)"
            )));
        }
    }
    let dst = db_of(ctx, args[0])?;
    let dst_name = to_cstring(ctx, args[1])?;
    let src = db_of(ctx, args[2])?;
    let src_name = to_cstring(ctx, args[3])?;
    // SAFETY: both connections are live, the names are NUL-terminated.
    let p = unsafe { sq::sqlite3_backup_init(dst, dst_name.as_ptr(), src, src_name.as_ptr()) };
    if p.is_null() {
        // The error is reported on the destination connection, with the
        // gem's per-code exception class.
        // SAFETY: a live connection; the message is its own buffer.
        let (rc, msg) = unsafe {
            (
                sq::sqlite3_errcode(dst),
                cstr_to_string(sq::sqlite3_errmsg(dst)).unwrap_or_default(),
            )
        };
        return Err(raise_code(ctx, rc, &msg, None));
    }
    // A re-`initialize` on the same object releases the old backup; a
    // fresh instance has no payload at all until this point.
    if let Some(h) = ctx.native::<BackupHandle>(this)
        && !h.p.is_null()
    {
        // SAFETY: our own backup object.
        unsafe { sq::sqlite3_backup_finish(h.p) };
        h.p = std::ptr::null_mut();
    }
    ctx.native_set(this, BackupHandle { p })?;
    Ok(Value::nil())
}

/// Backup#step(pages) -> Integer
///
/// Copies up to `pages` pages (`-1` for all of them); the SQLite result
/// code (`SQLITE_OK` while pages remain, `SQLITE_DONE` when finished,
/// `SQLITE_BUSY` / `SQLITE_LOCKED` to retry).
fn backup_step(ctx: &mut Ctx, this: Value, args: &[Value], _: Block) -> Result<Value> {
    let p = backup_of(ctx, this)?;
    let pages = ctx.int(args[0])? as c_int;
    // SAFETY: a live backup.
    let rc = unsafe { sq::sqlite3_backup_step(p, pages) };
    Ok(Value::int(rc as i64))
}

/// Backup#finish -> nil
///
/// Releases the backup (`sqlite3_backup_finish`); the object is closed
/// afterwards.
fn backup_finish(ctx: &mut Ctx, this: Value, _: &[Value], _: Block) -> Result<Value> {
    let p = backup_of(ctx, this)?;
    if let Some(h) = ctx.native::<BackupHandle>(this) {
        h.p = std::ptr::null_mut();
    }
    // SAFETY: a live backup, released exactly once.
    unsafe { sq::sqlite3_backup_finish(p) };
    Ok(Value::nil())
}

/// Backup#remaining -> Integer
fn backup_remaining(ctx: &mut Ctx, this: Value, _: &[Value], _: Block) -> Result<Value> {
    let p = backup_of(ctx, this)?;
    // SAFETY: a live backup.
    Ok(Value::int(unsafe { sq::sqlite3_backup_remaining(p) } as i64))
}

/// Backup#pagecount -> Integer
fn backup_pagecount(ctx: &mut Ctx, this: Value, _: &[Value], _: Block) -> Result<Value> {
    let p = backup_of(ctx, this)?;
    // SAFETY: a live backup.
    Ok(Value::int(unsafe { sq::sqlite3_backup_pagecount(p) } as i64))
}
