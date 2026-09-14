//! Build the vendored SQLite amalgamation (`vendor/sqlite3.c`, 3.48.0) with
//! `cc` and link it statically, so monoruby's `sqlite3` gem needs no host
//! libsqlite3 — the same arrangement as `libxml2-src` and `libz-sys`.
//!
//! The feature set follows the sqlite3 gem's own bundled build
//! (`ext/sqlite3/extconf.rb`), minus the extensions nothing in monoruby
//! reaches (RBU, session, preupdate hook, geopoly). `SQLITE_OMIT_DEPRECATED`
//! is deliberately *not* set: the gem's Ruby half still calls
//! `sqlite3_trace` and friends.

use std::env;
use std::path::PathBuf;

/// `-D` flags for the amalgamation.
const DEFINES: &[(&str, Option<&str>)] = &[
    // Serialized threading: monoruby runs one interpreter per OS thread and
    // a green thread may hand a connection over, so the library must not
    // assume single-threaded use.
    ("SQLITE_THREADSAFE", Some("1")),
    // `column_database_name` / `_table_name` / `_origin_name`, which
    // ActiveRecord's schema reflection asks for.
    ("SQLITE_ENABLE_COLUMN_METADATA", None),
    // Extensions the gem enables and real applications use.
    ("SQLITE_ENABLE_FTS3", None),
    ("SQLITE_ENABLE_FTS3_PARENTHESIS", None),
    ("SQLITE_ENABLE_FTS4", None),
    ("SQLITE_ENABLE_FTS5", None),
    ("SQLITE_ENABLE_RTREE", None),
    ("SQLITE_ENABLE_DBSTAT_VTAB", None),
    ("SQLITE_ENABLE_MATH_FUNCTIONS", None),
    ("SQLITE_ENABLE_STAT4", None),
    ("SQLITE_ENABLE_DESERIALIZE", None),
    ("SQLITE_ENABLE_UNLOCK_NOTIFY", None),
    // `file:` URI filenames, which ActiveRecord uses for in-memory
    // shared-cache databases.
    ("SQLITE_USE_URI", None),
    // The gem's limit; ActiveRecord's bulk inserts exceed the default 999.
    ("SQLITE_MAX_VARIABLE_NUMBER", Some("250000")),
    // No dynamic extension loading: `load_extension` needs dlopen, and
    // monoruby's `enable_load_extension` answers "not supported" instead.
    ("SQLITE_OMIT_LOAD_EXTENSION", None),
];

fn main() {
    let manifest = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let vendor = manifest.join("vendor");

    let mut build = cc::Build::new();
    build
        .file(vendor.join("sqlite3.c"))
        .include(&vendor)
        .opt_level(2)
        .warnings(false)
        .flag_if_supported("-Wno-unused-parameter")
        .flag_if_supported("-Wno-unused-but-set-variable")
        .flag_if_supported("-fvisibility=hidden");
    for (name, value) in DEFINES {
        build.define(name, *value);
    }
    build.compile("sqlite3");

    // The amalgamation's threading and `localtime_r` use.
    if env::var("CARGO_CFG_TARGET_OS").as_deref() != Ok("windows") {
        println!("cargo:rustc-link-lib=pthread");
    }
    println!("cargo:rerun-if-changed=vendor/sqlite3.c");
    println!("cargo:rerun-if-changed=vendor/sqlite3.h");
}
