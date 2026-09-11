extern crate monoruby;
use monoruby::tests::*;

// The sqlite3 gem over monoruby's native binding (src/builtins/sqlite3.rs,
// the bundled SQLite of `libsqlite3-src`). Every case is compared against
// the host CRuby, which runs the gem's real C extension — so these pin the
// binding to the extension's behaviour, not to a reading of it.
//
// `require "rubygems"` first: the harness spawns the reference CRuby with
// `--disable=gems`, and sqlite3 is an ordinary gem rather than a default
// one (the same preamble `tests/bigdecimal.rs` needs).

/// Storage classes round-trip as the C extension maps them: INTEGER to
/// Integer, REAL to Float, TEXT to a UTF-8 String, BLOB to a *binary*
/// String (not `SQLite3::Blob` — that class only marks a value for
/// binding), NULL to nil.
#[test]
fn sqlite3_column_types_round_trip() {
    run_test_once(
        r##"
        require "rubygems"
        require "sqlite3"
        db = SQLite3::Database.new(":memory:")
        db.execute("CREATE TABLE t (i INTEGER, r REAL, t TEXT, b BLOB, n TEXT)")
        db.execute("INSERT INTO t VALUES (?, ?, ?, ?, ?)",
                   [42, 1.5, "text", SQLite3::Blob.new("\x00\xffbin"), nil])
        db.execute("INSERT INTO t VALUES (-9223372036854775808, -0.0, '', X'', 'x')")
        rows = db.execute("SELECT * FROM t")
        res = [rows, rows.map { |r| r.map { |v| v.class.name } }]
        res << rows.map { |r| r.map { |v| v.is_a?(String) ? v.encoding.name : nil } }
        res << db.execute("SELECT typeof(i), typeof(r), typeof(t), typeof(b), typeof(n) FROM t")
        db.close
        res
        "##,
    );
}

/// How a bound value chooses its storage class: a plain String is TEXT, a
/// `SQLite3::Blob` and any BINARY-encoded String are BLOB, true/false bind
/// as 1/0, nil as NULL.
#[test]
fn sqlite3_bind_storage_classes() {
    run_test_once(
        r##"
        require "rubygems"
        require "sqlite3"
        db = SQLite3::Database.new(":memory:")
        db.execute("CREATE TABLE t (x)")
        [
          "utf8", SQLite3::Blob.new("blob"), "bin".dup.force_encoding("BINARY"),
          1, -1, 2.25, nil, 2**62, 2**70, -(2**70), 1.0 / 0
        ].each { |v| db.execute("INSERT INTO t VALUES (?)", [v]) }
        res = [db.execute("SELECT typeof(x), x FROM t")]
        # Types the C extension refuses outright.
        res << [true, false, :sym, [1], Object.new].map { |v|
          begin
            db.execute("INSERT INTO t VALUES (?)", [v])
            :bound
          rescue StandardError => e
            [e.class.name, e.message.sub(/#<Object.*/, "Object")]
          end
        }
        # Named parameters, by String and by Symbol, with and without the
        # leading colon.
        st = db.prepare("SELECT :a, :b, :c")
        st.bind_param(":a", 1)
        st.bind_param("b", 2)
        st.bind_param(:c, 3)
        res << st.step
        st.close
        res << db.execute("SELECT :one + :two", { "one" => 10, ":two" => 20 })
        db.close
        res
        "##,
    );
}

/// `Statement`'s cursor: `step` answers rows then nil, `done?` follows it,
/// `reset!` rewinds and clears the bindings, and the metadata methods
/// answer for the prepared statement.
#[test]
fn sqlite3_statement_cursor_and_metadata() {
    run_test_once(
        r##"
        require "rubygems"
        require "sqlite3"
        db = SQLite3::Database.new(":memory:")
        db.execute("CREATE TABLE t (id INTEGER PRIMARY KEY, name TEXT)")
        db.execute("INSERT INTO t VALUES (1, 'a'), (2, 'b')")
        st = db.prepare("SELECT id, name FROM t WHERE id >= ? ORDER BY id")
        res = [st.column_count, st.bind_parameter_count,
               (0...st.column_count).map { |i| [st.column_name(i), st.column_decltype(i)] }]
        st.bind_param(1, 1)
        res << [st.step, st.done?, st.step, st.done?, st.step, st.done?, st.step]
        st.reset!
        res << st.done?
        st.bind_param(1, 2)
        res << [st.step, st.step]
        res << st.closed?
        st.close
        # Every method but `closed?` and `done?` raises on a closed
        # statement.
        res << st.closed? << st.done?
        res << %i[column_count bind_parameter_count step reset! stats_as_hash].map { |m|
          begin
            st.send(m)
            :ok
          rescue StandardError => e
            [e.class.name, e.message]
          end
        }
        db.close
        res
        "##,
    );
}

/// The remainder of a multi-statement prepare, and `execute_batch2`, which
/// runs every statement and answers all their rows *as text*.
#[test]
fn sqlite3_remainder_and_batch() {
    run_test_once(
        r##"
        require "rubygems"
        require "sqlite3"
        db = SQLite3::Database.new(":memory:")
        res = []
        st = db.prepare("SELECT 1; SELECT 2; -- trailing")
        res << st.remainder << st.step
        st.close
        res << db.prepare("SELECT 1").remainder
        res << db.prepare("SELECT 1;").remainder
        db.execute("CREATE TABLE t (a, b)")
        res << db.execute_batch2("INSERT INTO t VALUES (1, 'x'); SELECT * FROM t; SELECT 2.5;")
        res << db.execute_batch2("   ")
        db.close
        res
        "##,
    );
}

/// Errors: the result code picks the exception class, `@code` carries it,
/// and a failed `prepare` reports the offending SQL the way the gem's
/// `Exception#message` formats it.
#[test]
fn sqlite3_errors() {
    run_test_once(
        r##"
        require "rubygems"
        require "sqlite3"
        db = SQLite3::Database.new(":memory:")
        db.execute("CREATE TABLE t (id INTEGER PRIMARY KEY)")
        db.execute("INSERT INTO t VALUES (1)")
        probe = lambda do |&blk|
          begin
            blk.call
            :no_error
          rescue SQLite3::Exception => e
            [e.class.name, e.message, e.code]
          rescue StandardError => e
            [e.class.name, e.message]
          end
        end
        res = []
        res << probe.call { db.execute("SELECT * FROM missing") }
        res << probe.call { db.execute("INSERT INTO t VALUES (1)") }
        res << probe.call { db.execute("SELECT bogus syntax here") }
        res << probe.call { db.prepare("SELEC 1") }
        res << [db.errcode, db.errmsg.class.name]
        db.close
        res << probe.call { db.execute("SELECT 1") }.first
        res
        "##,
    );
}

/// Connection state and counters: `closed?`, `changes` / `total_changes` /
/// `last_insert_row_id`, the autocommit-derived `transaction_active?`, and
/// closing twice.
#[test]
fn sqlite3_connection_state() {
    run_test_once(
        r##"
        require "rubygems"
        require "sqlite3"
        db = SQLite3::Database.new(":memory:")
        res = [db.closed?, db.encoding.name, db.filename, db.transaction_active?]
        db.execute("CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)")
        db.execute("INSERT INTO t (v) VALUES ('a'), ('b'), ('c')")
        res << [db.changes, db.last_insert_row_id]
        db.execute("UPDATE t SET v = 'z'")
        res << db.changes
        res << (db.total_changes >= 6)
        db.transaction do
          res << db.transaction_active?
          db.execute("INSERT INTO t (v) VALUES ('d')")
        end
        res << db.transaction_active?
        db.busy_timeout = 100
        res << db.execute("SELECT COUNT(*) FROM t")
        db.close
        db.close
        res << db.closed?
        res
        "##,
    );
}

/// A statement dropped without `close`, and a connection dropped without
/// `close`, are released by the collector rather than leaked: the point is
/// that this runs to the end without exhausting anything.
#[test]
fn sqlite3_handles_are_released_by_gc() {
    run_test_once(
        r##"
        require "rubygems"
        require "sqlite3"
        200.times do
          db = SQLite3::Database.new(":memory:")
          db.execute("CREATE TABLE t (a)")
          db.execute("INSERT INTO t VALUES (1)")
          st = db.prepare("SELECT a FROM t")
          st.step
          # neither st.close nor db.close
        end
        GC.start
        db = SQLite3::Database.new(":memory:")
        r = db.execute("SELECT 1")
        db.close
        r
        "##,
    );
}
