//! Hand-written FFI for the vendored SQLite amalgamation (3.48.0): the
//! entry points `src/builtins/sqlite3.rs` calls. The C library is built and
//! linked statically by `build.rs`, so nothing here depends on a host
//! libsqlite3.
//!
//! Only what is used is declared, and everything is `unsafe` to call — the
//! connection and statement handles are owned by the `SQLite3::Database` /
//! `SQLite3::Statement` objects on the monoruby side.

#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals)]

use core::ffi::{c_char, c_double, c_int, c_uchar, c_void};

/// Opaque handles.
pub enum sqlite3 {}
pub enum sqlite3_stmt {}
pub enum sqlite3_context {}
pub enum sqlite3_value {}

/// `sqlite3_int64`.
pub type sqlite3_int64 = i64;

/// Result codes.
pub const SQLITE_OK: c_int = 0;
pub const SQLITE_ERROR: c_int = 1;
pub const SQLITE_BUSY: c_int = 5;
pub const SQLITE_MISUSE: c_int = 21;
pub const SQLITE_ROW: c_int = 100;
pub const SQLITE_DONE: c_int = 101;

/// Column / value types (`sqlite3_column_type`).
pub const SQLITE_INTEGER: c_int = 1;
pub const SQLITE_FLOAT: c_int = 2;
pub const SQLITE_TEXT: c_int = 3;
pub const SQLITE_BLOB: c_int = 4;
pub const SQLITE_NULL: c_int = 5;

/// `sqlite3_open_v2` flags.
pub const SQLITE_OPEN_READONLY: c_int = 0x0000_0001;
pub const SQLITE_OPEN_READWRITE: c_int = 0x0000_0002;
pub const SQLITE_OPEN_CREATE: c_int = 0x0000_0004;
pub const SQLITE_OPEN_URI: c_int = 0x0000_0040;
pub const SQLITE_OPEN_NOMUTEX: c_int = 0x0000_8000;
pub const SQLITE_OPEN_FULLMUTEX: c_int = 0x0001_0000;

/// Text encodings, and the `sqlite3_create_function_v2` flag bits the gem
/// passes through from `Database#create_function`.
pub const SQLITE_UTF8: c_int = 1;
pub const SQLITE_DETERMINISTIC: c_int = 0x0000_0800;

/// The `xDel` sentinel for `sqlite3_bind_text` / `_blob` and
/// `sqlite3_result_text` / `_blob`: SQLite copies the buffer before it
/// returns, so the caller keeps ownership.
///
/// `SQLITE_TRANSIENT` is the C macro `((sqlite3_destructor_type)-1)` — a
/// tag SQLite compares against, never a pointer it calls. Built here
/// rather than as a `const`, since const evaluation rejects a function
/// pointer whose value is not a real function.
#[inline]
pub fn SQLITE_TRANSIENT() -> Option<unsafe extern "C" fn(*mut c_void)> {
    // SAFETY: the value is only ever compared with by SQLite (`if( xDel
    // == SQLITE_TRANSIENT )`, `sqlite3VdbeMemSetStr`), and the branch
    // that would call it is unreachable for this tag.
    unsafe {
        core::mem::transmute::<usize, Option<unsafe extern "C" fn(*mut c_void)>>(usize::MAX)
    }
}

/// `sqlite3_stmt_status` counters.
pub const SQLITE_STMTSTATUS_FULLSCAN_STEP: c_int = 1;
pub const SQLITE_STMTSTATUS_SORT: c_int = 2;
pub const SQLITE_STMTSTATUS_AUTOINDEX: c_int = 3;
pub const SQLITE_STMTSTATUS_VM_STEP: c_int = 4;
pub const SQLITE_STMTSTATUS_REPREPARE: c_int = 5;
pub const SQLITE_STMTSTATUS_RUN: c_int = 6;
pub const SQLITE_STMTSTATUS_FILTER_MISS: c_int = 7;
pub const SQLITE_STMTSTATUS_FILTER_HIT: c_int = 8;
pub const SQLITE_STMTSTATUS_MEMUSED: c_int = 99;

/// `sqlite3_db_config` verbs. Double-quoted string literals are SQLite's
/// "quirk"; the gem's `disable_quirk_mode` turns both off.
pub const SQLITE_DBCONFIG_DQS_DML: c_int = 1013;
pub const SQLITE_DBCONFIG_DQS_DDL: c_int = 1014;

unsafe extern "C" {
    // ---- library
    pub fn sqlite3_libversion() -> *const c_char;
    pub fn sqlite3_libversion_number() -> c_int;
    pub fn sqlite3_threadsafe() -> c_int;
    pub fn sqlite3_free(p: *mut c_void);

    // ---- connection
    pub fn sqlite3_open_v2(
        filename: *const c_char,
        db: *mut *mut sqlite3,
        flags: c_int,
        vfs: *const c_char,
    ) -> c_int;
    pub fn sqlite3_open16(filename: *const c_void, db: *mut *mut sqlite3) -> c_int;
    pub fn sqlite3_close_v2(db: *mut sqlite3) -> c_int;
    pub fn sqlite3_errmsg(db: *mut sqlite3) -> *const c_char;
    pub fn sqlite3_errcode(db: *mut sqlite3) -> c_int;
    /// Byte offset of the token a `prepare` error is about, or -1.
    pub fn sqlite3_error_offset(db: *mut sqlite3) -> c_int;
    pub fn sqlite3_extended_result_codes(db: *mut sqlite3, onoff: c_int) -> c_int;
    pub fn sqlite3_busy_timeout(db: *mut sqlite3, ms: c_int) -> c_int;
    pub fn sqlite3_busy_handler(
        db: *mut sqlite3,
        handler: Option<unsafe extern "C" fn(*mut c_void, c_int) -> c_int>,
        arg: *mut c_void,
    ) -> c_int;
    pub fn sqlite3_last_insert_rowid(db: *mut sqlite3) -> sqlite3_int64;
    pub fn sqlite3_changes(db: *mut sqlite3) -> c_int;
    pub fn sqlite3_total_changes(db: *mut sqlite3) -> c_int;
    pub fn sqlite3_interrupt(db: *mut sqlite3);
    pub fn sqlite3_db_filename(db: *mut sqlite3, name: *const c_char) -> *const c_char;
    pub fn sqlite3_get_autocommit(db: *mut sqlite3) -> c_int;
    /// `SQLITE_DBCONFIG_DQS_DML` / `_DDL` take `(int onoff, int *pRes)`.
    /// Declared variadic because `sqlite3_db_config` is.
    pub fn sqlite3_db_config(db: *mut sqlite3, op: c_int, ...) -> c_int;
    pub fn sqlite3_exec(
        db: *mut sqlite3,
        sql: *const c_char,
        callback: Option<
            unsafe extern "C" fn(*mut c_void, c_int, *mut *mut c_char, *mut *mut c_char) -> c_int,
        >,
        arg: *mut c_void,
        errmsg: *mut *mut c_char,
    ) -> c_int;

    // ---- statement
    pub fn sqlite3_prepare_v2(
        db: *mut sqlite3,
        sql: *const c_char,
        n_byte: c_int,
        stmt: *mut *mut sqlite3_stmt,
        tail: *mut *const c_char,
    ) -> c_int;
    pub fn sqlite3_step(stmt: *mut sqlite3_stmt) -> c_int;
    pub fn sqlite3_finalize(stmt: *mut sqlite3_stmt) -> c_int;
    pub fn sqlite3_reset(stmt: *mut sqlite3_stmt) -> c_int;
    pub fn sqlite3_clear_bindings(stmt: *mut sqlite3_stmt) -> c_int;
    /// The statement's original SQL text (borrowed; valid while the
    /// statement lives).
    /// True when the text ends a complete SQL statement.
    pub fn sqlite3_complete(sql: *const c_char) -> c_int;
    pub fn sqlite3_sql(stmt: *mut sqlite3_stmt) -> *const c_char;
    /// The SQL with the bound parameters substituted. The caller frees
    /// the result with `sqlite3_free`.
    pub fn sqlite3_expanded_sql(stmt: *mut sqlite3_stmt) -> *mut c_char;
    pub fn sqlite3_db_handle(stmt: *mut sqlite3_stmt) -> *mut sqlite3;
    pub fn sqlite3_stmt_status(stmt: *mut sqlite3_stmt, op: c_int, reset: c_int) -> c_int;

    // ---- columns
    pub fn sqlite3_column_count(stmt: *mut sqlite3_stmt) -> c_int;
    pub fn sqlite3_column_name(stmt: *mut sqlite3_stmt, i: c_int) -> *const c_char;
    pub fn sqlite3_column_decltype(stmt: *mut sqlite3_stmt, i: c_int) -> *const c_char;
    pub fn sqlite3_column_type(stmt: *mut sqlite3_stmt, i: c_int) -> c_int;
    pub fn sqlite3_column_int64(stmt: *mut sqlite3_stmt, i: c_int) -> sqlite3_int64;
    pub fn sqlite3_column_double(stmt: *mut sqlite3_stmt, i: c_int) -> c_double;
    pub fn sqlite3_column_text(stmt: *mut sqlite3_stmt, i: c_int) -> *const c_uchar;
    pub fn sqlite3_column_blob(stmt: *mut sqlite3_stmt, i: c_int) -> *const c_void;
    pub fn sqlite3_column_bytes(stmt: *mut sqlite3_stmt, i: c_int) -> c_int;

    // ---- binding
    pub fn sqlite3_bind_parameter_count(stmt: *mut sqlite3_stmt) -> c_int;
    pub fn sqlite3_bind_parameter_index(stmt: *mut sqlite3_stmt, name: *const c_char) -> c_int;
    pub fn sqlite3_bind_null(stmt: *mut sqlite3_stmt, i: c_int) -> c_int;
    pub fn sqlite3_bind_int64(stmt: *mut sqlite3_stmt, i: c_int, v: sqlite3_int64) -> c_int;
    pub fn sqlite3_bind_double(stmt: *mut sqlite3_stmt, i: c_int, v: c_double) -> c_int;
    pub fn sqlite3_bind_text(
        stmt: *mut sqlite3_stmt,
        i: c_int,
        text: *const c_char,
        n: c_int,
        del: Option<unsafe extern "C" fn(*mut c_void)>,
    ) -> c_int;
    pub fn sqlite3_bind_blob(
        stmt: *mut sqlite3_stmt,
        i: c_int,
        blob: *const c_void,
        n: c_int,
        del: Option<unsafe extern "C" fn(*mut c_void)>,
    ) -> c_int;

    // ---- user-defined functions
    pub fn sqlite3_create_function_v2(
        db: *mut sqlite3,
        name: *const c_char,
        n_arg: c_int,
        enc: c_int,
        app: *mut c_void,
        x_func: Option<
            unsafe extern "C" fn(*mut sqlite3_context, c_int, *mut *mut sqlite3_value),
        >,
        x_step: Option<
            unsafe extern "C" fn(*mut sqlite3_context, c_int, *mut *mut sqlite3_value),
        >,
        x_final: Option<unsafe extern "C" fn(*mut sqlite3_context)>,
        x_destroy: Option<unsafe extern "C" fn(*mut c_void)>,
    ) -> c_int;
    pub fn sqlite3_user_data(ctx: *mut sqlite3_context) -> *mut c_void;
    pub fn sqlite3_aggregate_context(ctx: *mut sqlite3_context, n: c_int) -> *mut c_void;
    pub fn sqlite3_result_null(ctx: *mut sqlite3_context);
    pub fn sqlite3_result_int64(ctx: *mut sqlite3_context, v: sqlite3_int64);
    pub fn sqlite3_result_double(ctx: *mut sqlite3_context, v: c_double);
    pub fn sqlite3_result_text(
        ctx: *mut sqlite3_context,
        text: *const c_char,
        n: c_int,
        del: Option<unsafe extern "C" fn(*mut c_void)>,
    );
    pub fn sqlite3_result_blob(
        ctx: *mut sqlite3_context,
        blob: *const c_void,
        n: c_int,
        del: Option<unsafe extern "C" fn(*mut c_void)>,
    );
    pub fn sqlite3_result_error(ctx: *mut sqlite3_context, msg: *const c_char, n: c_int);
    pub fn sqlite3_value_type(v: *mut sqlite3_value) -> c_int;
    pub fn sqlite3_value_int64(v: *mut sqlite3_value) -> sqlite3_int64;
    pub fn sqlite3_value_double(v: *mut sqlite3_value) -> c_double;
    pub fn sqlite3_value_text(v: *mut sqlite3_value) -> *const c_uchar;
    pub fn sqlite3_value_blob(v: *mut sqlite3_value) -> *const c_void;
    pub fn sqlite3_value_bytes(v: *mut sqlite3_value) -> c_int;
}
