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

/// The binding's method surface must match the C extension's exactly,
/// visibility included: the gem's Ruby half calls several of these as
/// private methods, and a public one here would let user code reach an
/// entry point CRuby hides.
#[test]
fn sqlite3_method_surface_matches_the_extension() {
    run_test_once(
        r##"
        require "rubygems"
        require "sqlite3"
        [SQLite3::Database, SQLite3::Statement].flat_map { |k|
          [(k.public_instance_methods(false) - Object.instance_methods).sort,
           (k.private_instance_methods(false) - Object.private_instance_methods).sort]
        }
        "##,
    );
}

/// `Statement#stat_for` and `#stats_as_hash` read `sqlite3_stmt_status`
/// (both private, so reached with `send`). The counters are SQLite's, so
/// only their shape is pinned: a statement that has run reports one run
/// and a non-zero `vm_steps`, and an unknown key raises.
#[test]
fn sqlite3_statement_stats() {
    run_test_once(
        r##"
        require "rubygems"
        require "sqlite3"
        db = SQLite3::Database.new(":memory:")
        db.execute("CREATE TABLE t (a)")
        db.execute("INSERT INTO t VALUES (1), (2), (3)")
        st = db.prepare("SELECT a FROM t ORDER BY a")
        st.to_a
        res = []
        res << st.send(:stat_for, :runs)
        res << (st.send(:stat_for, :vm_steps) > 0)
        res << (st.send(:stat_for, :fullscan_steps) >= 0)
        h = st.send(:stats_as_hash)
        res << h.keys.sort
        res << h[:runs]
        # A Symbol and nothing else; an unknown one is refused rather
        # than answered with zero.
        res << [:nope, "runs", 1].map { |k|
          begin; st.send(:stat_for, k); rescue StandardError => e; [e.class.name, e.message]; end
        }
        # The public wrapper over the same counters.
        res << st.stat(:runs)
        res << st.stat.keys.sort
        res << (begin; st.stat("runs"); rescue StandardError => e; e.class.name; end)
        st.close
        db.close
        res
        "##,
    );
}

/// The statement text accessors and the binding reset, all straight
/// `sqlite3_*` calls: `sql` is the text as prepared, `expanded_sql`
/// substitutes the bound values, and `clear_bindings!` puts them back to
/// NULL.
#[test]
fn sqlite3_statement_text_and_bindings() {
    run_test_once(
        r##"
        require "rubygems"
        require "sqlite3"
        db = SQLite3::Database.new(":memory:")
        db.execute("CREATE TABLE t (a)")
        st = db.prepare("SELECT a FROM t WHERE a = ? OR a = :n")
        res = [st.sql, st.expanded_sql]
        st.bind_param(1, 42)
        st.bind_param(":n", "x")
        res << st.expanded_sql
        res << st.clear_bindings!.equal?(st)
        res << st.expanded_sql
        res << (st.memused >= 0)
        res << st.bind_parameter_count
        st.close
        # A closed statement refuses each of them.
        res << [:sql, :expanded_sql, :memused, :clear_bindings!].map { |m|
          begin; st.public_send(m); rescue SQLite3::Exception => e; e.class.name; end
        }
        db.close
        res
        "##,
    );
}

/// The connection-level switches the gem's `Database#initialize` and its
/// pragmas reach, plus the batch executors. `execute_batch` runs each
/// statement for effect; `execute_batch2` collects the rows of all of
/// them, every column as text.
#[test]
fn sqlite3_batch_and_switches() {
    run_test_once(
        r##"
        require "rubygems"
        require "sqlite3"
        db = SQLite3::Database.new(":memory:")
        res = []
        db.extended_result_codes = true
        db.extended_result_codes = false
        db.execute_batch("CREATE TABLE t (a, b); INSERT INTO t VALUES (1, 'x'); INSERT INTO t VALUES (2, 'y');")
        res << db.execute("SELECT * FROM t ORDER BY a")
        res << db.execute_batch2("SELECT a, b FROM t ORDER BY a")
        res << db.execute_batch2("SELECT 1.5, X'00ff', NULL")
        # Whether a fragment ends a statement.
        res << ["SELECT 1;", "SELECT 1", "", "-- c\n"].map { |q| db.complete?(q) }
        # A no-op collation (removal) is honoured; a real comparator is not.
        res << db.collation("nocase2", nil).equal?(db)
        # Quirk mode: SQLite otherwise reads a double-quoted unknown
        # column name as a string literal. The gem turns that off for
        # DDL and DML both.
        db.execute("CREATE TABLE q (a)")
        res << db.execute(%q{SELECT "no_such_col" FROM q})
        res << db.send(:disable_quirk_mode)
        res << (begin
                  db.execute(%q{SELECT "no_such_col" FROM q})
                rescue SQLite3::Exception => e
                  [e.class.name, e.message]
                end)
        res << (begin
                  db.execute(%q{CREATE TABLE u (a DEFAULT "x")})
                rescue SQLite3::Exception => e
                  e.class.name
                end)
        res << (db.statement_timeout = 250)
        db.close
        res
        "##,
    );
}

/// Errors SQLite raises by code, each mapping to the gem's exception
/// subclass: a constraint violation, a file that is not a database, and
/// a write to a read-only connection.
#[test]
fn sqlite3_error_classes_by_code() {
    run_test_once(
        r##"
        require "rubygems"
        require "sqlite3"
        require "tmpdir"
        res = []
        db = SQLite3::Database.new(":memory:")
        db.execute("CREATE TABLE t (a INTEGER PRIMARY KEY, b NOT NULL)")
        db.execute("INSERT INTO t VALUES (1, 'x')")
        # SQLITE_CONSTRAINT -> ConstraintException, and its subclasses.
        res << (begin; db.execute("INSERT INTO t VALUES (1, 'y')"); rescue SQLite3::Exception => e; e.class.name; end)
        res << (begin; db.execute("INSERT INTO t VALUES (2, NULL)"); rescue SQLite3::Exception => e; e.class.name; end)
        # A syntax error carries the offending SQL.
        res << (begin; db.execute("SELECT FROM"); rescue SQLite3::Exception => e; [e.class.name, e.sql]; end)
        # An unknown table names itself in the message.
        res << (begin; db.execute("SELECT * FROM nope"); rescue SQLite3::Exception => e; [e.class.name, e.message]; end)
        db.close
        Dir.mktmpdir do |dir|
          path = File.join(dir, "not-a-db")
          File.binwrite(path, "this is plainly not a SQLite file, padded " * 8)
          # SQLITE_NOTADB -> NotADatabaseException.
          res << (begin
                    d = SQLite3::Database.new(path)
                    d.execute("SELECT * FROM sqlite_master")
                  rescue SQLite3::Exception => e
                    e.class.name
                  end)
          ro = File.join(dir, "ro.db")
          SQLite3::Database.new(ro) { |d| d.execute("CREATE TABLE t (a)") }
          # SQLITE_READONLY -> ReadOnlyException.
          res << (begin
                    d = SQLite3::Database.new(ro, readonly: true)
                    d.execute("INSERT INTO t VALUES (1)")
                  rescue SQLite3::Exception => e
                    e.class.name
                  end)
        end
        res
        "##,
    );
}

/// A prepared statement left open is finalized when it is collected,
/// so a loop that drops them does not grow without bound.
///
/// A *connection* is a different matter and is deliberately not
/// asserted here: the gem's `ForkSafety` registry holds every
/// `Database` in a `WeakRef`, and monoruby's weakref.rb is a stub that
/// keeps a strong reference (there is no `ObjectSpace::WeakMap`), so an
/// unclosed connection stays reachable and its `Drop` never runs. That
/// predates this binding — the Fiddle bridge leaked the same way — and
/// wants fixing where the weak reference is, not here.
#[test]
fn sqlite3_statements_are_finalized_when_collected() {
    run_test_once(
        r##"
        require "rubygems"
        require "sqlite3"
        db = SQLite3::Database.new(":memory:")
        db.execute("CREATE TABLE t (a)")
        db.execute("INSERT INTO t VALUES (1), (2)")
        # Dropped unclosed, in a method so no local pins the last one.
        def churn(db) = 300.times { db.prepare("SELECT a FROM t").step }
        churn(db)
        GC.start
        # The connection is still usable, and so is a fresh statement.
        st = db.prepare("SELECT COUNT(*) FROM t")
        r = [st.step, st.done?]
        st.close
        r << db.execute("SELECT a FROM t ORDER BY a")
        db.close
        r
        "##,
    );
}

/// The entry points monoruby's binding still does not implement, and the
/// ones it accepts but ignores. Not oracle-checked: the C extension
/// really registers these callbacks, and the point here is that this
/// binding refuses loudly instead, with a `SQLite3::Exception` naming
/// the feature, rather than appearing to work.
///
/// `create_function` and `create_aggregate` are no longer among them —
/// see `sqlite3_create_function` and `sqlite3_create_aggregate`.
#[test]
fn sqlite3_unsupported_callbacks_refuse() {
    let v = run_test_no_result_check(
        r##"
        require "sqlite3"
        db = SQLite3::Database.new(":memory:")
        r = []
        r << (begin; db.collation("c", Object.new); rescue => e; [e.class.name, e.message]; end)
        r << (begin; db.load_extension("x"); rescue => e; [e.class.name, e.message]; end)
        expected = [
          ["SQLite3::Exception", "collation is not supported by monoruby's sqlite3 binding"],
          ["SQLite3::Exception", "load_extension is not available: monoruby's SQLite is built without extension loading"],
        ]
        raise "got #{r.inspect}" unless r == expected
        # Accepted and remembered, but nothing is registered with SQLite:
        # `enable_load_extension` has nothing to enable, and the three
        # callbacks below are never called back into.
        r2 = []
        r2 << db.enable_load_extension(true)
        r2 << db.trace { |sql| sql }
        r2 << (db.authorizer = proc { 0 })
        r2 << db.busy_handler { 0 }
        r2 << db.busy_timeout(50)
        raise "got #{r2.inspect}" unless r2 == [nil, nil, r2[2], nil, nil] && r2[2].is_a?(Proc)
        # `discard` abandons the connection without closing it, which is
        # what the gem's fork safety does in a child.
        db.send(:discard)
        raise "not discarded" unless db.closed?
        r.size + r2.size
        "##,
    );
    assert_eq!(v.try_fixnum(), Some(7));
}

/// The failure paths of opening a connection, and `open16`, which the
/// gem reaches when the filename is UTF-16. A directory that does not
/// exist gives SQLITE_CANTOPEN, whose message comes off the half-open
/// handle before it is closed.
#[test]
fn sqlite3_open_failures_and_utf16() {
    run_test_once(
        r##"
        require "rubygems"
        require "sqlite3"
        require "tmpdir"
        res = []
        res << (begin
                  SQLite3::Database.new("/nonexistent-dir-for-a-test/x.db")
                rescue SQLite3::Exception => e
                  [e.class.name, e.message]
                end)
        # A directory is not a database file either.
        Dir.mktmpdir do |dir|
          res << (begin
                    SQLite3::Database.new(dir)
                  rescue SQLite3::Exception => e
                    e.class.name
                  end)
          # The UTF-16 path: `open16` rather than `open_v2`.
          path = File.join(dir, "u16.db")
          db = SQLite3::Database.new(path.encode("UTF-16LE"))
          db.execute("CREATE TABLE t (a)")
          db.execute("INSERT INTO t VALUES (7)")
          res << db.execute("SELECT a FROM t")
          res << db.encoding.name
          db.close
          res << (begin
                    SQLite3::Database.new(File.join(dir, "no", "x.db").encode("UTF-16LE"))
                  rescue SQLite3::Exception => e
                    e.class.name
                  end)
        end
        res
        "##,
    );
}

/// The error paths of the statement and batch executors: a batch whose
/// second statement is bad has already run the first, and the failure
/// carries the offending SQL.
#[test]
fn sqlite3_batch_error_paths() {
    run_test_once(
        r##"
        require "rubygems"
        require "sqlite3"
        db = SQLite3::Database.new(":memory:")
        db.execute("CREATE TABLE t (a)")
        res = []
        res << (begin
                  db.execute_batch("INSERT INTO t VALUES (1); NOT SQL; INSERT INTO t VALUES (2);")
                rescue SQLite3::Exception => e
                  [e.class.name, e.sql]
                end)
        # The first statement ran before the bad one was reached.
        res << db.execute("SELECT * FROM t")
        res << (begin
                  db.execute_batch2("SELECT * FROM nope")
                rescue SQLite3::Exception => e
                  e.class.name
                end)
        # A constraint failure inside a batch.
        db.execute("CREATE TABLE u (a INTEGER PRIMARY KEY)")
        res << (begin
                  db.execute_batch("INSERT INTO u VALUES (1); INSERT INTO u VALUES (1);")
                rescue SQLite3::Exception => e
                  e.class.name
                end)
        db.close
        # Both refuse on a closed connection — from the gem's own guard
        # in `Statement#initialize`, so an ArgumentError rather than a
        # SQLite3::Exception.
        res << [:execute_batch, :execute_batch2].map { |m|
          begin; db.public_send(m, "SELECT 1"); rescue StandardError => e; [e.class.name, e.message]; end
        }
        res
        "##,
    );
}

/// The small connection and binding accessors, and the two refusals a
/// bind can hit: a named parameter the statement does not declare, and
/// a value SQLite will not store in that column.
#[test]
fn sqlite3_binding_and_accessor_edges() {
    run_test_once(
        r##"
        require "rubygems"
        require "sqlite3"
        db = SQLite3::Database.new(":memory:")
        # `interrupt` answers the connection itself. (Its `inspect` is
        # not compared: monoruby renders an ObjTy::NATIVE object without
        # its ivars, which is a difference in Object#inspect, not here.)
        res = [db.encoding.name, db.interrupt.equal?(db), db.readonly?, db.filename]
        db.execute("CREATE TABLE t (a INTEGER PRIMARY KEY, b)")
        st = db.prepare("SELECT b FROM t WHERE a = :a")
        res << (begin
                  st.bind_param(":nope", 1)
                rescue SQLite3::Exception => e
                  [e.class.name, e.message]
                end)
        # An index past the declared parameters.
        res << (begin
                  st.bind_param(9, 1)
                rescue SQLite3::Exception => e
                  e.class.name
                end)
        st.close
        # SQLITE_MISMATCH: an INTEGER PRIMARY KEY takes only integers.
        res << (begin
                  db.execute("INSERT INTO t (a, b) VALUES (?, ?)", ["not an int", 1])
                rescue SQLite3::Exception => e
                  e.class.name
                end)
        # Interrupting an idle connection is a no-op, and it stays usable.
        db.interrupt
        db.execute("INSERT INTO t (a, b) VALUES (1, 'x')")
        res << db.execute("SELECT * FROM t")
        db.close
        res
        "##,
    );
}

/// `create_function` over the native binding. The gem's Ruby half wraps
/// the caller's block in one that fills a `FunctionProxy`; the block
/// below it runs inside `sqlite3_step`, reached from SQLite's C
/// callback, which is what every case here exercises.
#[test]
fn sqlite3_create_function() {
    run_test_once(
        r##"
        require "rubygems"
        require "sqlite3"
        db = SQLite3::Database.new(":memory:")
        res = []
        # Arguments arrive by the same mapping the row readers use.
        db.create_function("types", -1) { |fp, *a| fp.result = a.map { |x| [x.class.name, x] }.inspect }
        res << db.execute("SELECT types(1, 2.5, NULL, x'00ff', 'txt')")
        # Results: the classes SQLite can store, and the refusal for the rest.
        db.create_function("ret", 1) { |fp, kind|
          fp.result = case kind
            when "int" then 7
            when "big" then 2**70
            when "float" then 1.5
            when "nil" then nil
            when "text" then "plain"
            when "binary" then "bin".dup.force_encoding("BINARY")
            when "blob" then SQLite3::Blob.new("\x00\xff")
            when "bool" then true
            when "obj" then Object.new
            end
        }
        res << %w[int big float nil text binary blob bool obj].map { |k|
          begin
            [k, db.execute("SELECT typeof(ret(?)), ret(?)", [k, k])]
          rescue StandardError => e
            [k, e.class.name, e.message]
          end
        }
        # The arity given to create_function never reaches SQLite, so a
        # call with a different count still runs.
        db.create_function("one", 1) { |fp, a| fp.result = "got #{a.inspect}" }
        res << db.execute("SELECT one(1, 2)")
        # `define_function` is the same thing without flags, and hands
        # the block the arguments directly rather than a FunctionProxy.
        db.define_function("triple") { |v| v.to_i * 3 }
        res << [db.execute("SELECT triple(5)"), db.execute("SELECT triple(NULL)")]
        res << (begin; db.define_function("nb"); rescue StandardError => e; [e.class.name, e.message]; end)
        # SQLite takes a NUL-terminated name, so a name holding a NUL
        # defines only the part before it — while the registry the
        # extension keeps is keyed by the whole string.
        db.define_function("a\0b") { |v| v.to_i * 7 }
        res << db.execute("SELECT a(2)")
        res << db.instance_variable_get(:@functions).keys.sort
        # SQLITE_DETERMINISTIC passes through.
        db.define_function_with_flags("det", 0x800) { |v| v.to_i + 1 }
        res << db.execute("SELECT det(1)")
        db.close
        res
        "##,
    );
}

/// A block that raises must not unwind through SQLite's C frames: the
/// exception is carried out of the step and re-raised unchanged, the
/// scan stops, and the connection stays usable.
#[test]
fn sqlite3_function_exceptions() {
    run_test_once(
        r##"
        require "rubygems"
        require "sqlite3"
        db = SQLite3::Database.new(":memory:")
        db.execute("CREATE TABLE t (a)")
        db.execute("INSERT INTO t VALUES (1), (2), (3)")
        res = []
        db.create_function("failat", 1) { |fp, v| raise ArgumentError, "boom at #{v}" if v.to_i == 2; fp.result = v }
        res << (begin
                  db.execute("SELECT failat(a) FROM t ORDER BY a")
                rescue StandardError => e
                  [e.class.name, e.message]
                end)
        # The same through the batch executor, which steps on its own.
        res << (begin
                  db.execute_batch2("SELECT failat(a) FROM t ORDER BY a")
                rescue StandardError => e
                  [e.class.name, e.message]
                end)
        # Unaffected afterwards.
        res << db.execute("SELECT a FROM t ORDER BY a")
        db.create_function("ok", 1) { |fp, v| fp.result = v.to_i + 10 }
        res << db.execute("SELECT ok(a) FROM t ORDER BY a")
        db.close
        res
        "##,
    );
}

/// SQLite lets a function callback run another statement on the same
/// connection, and the C extension allows it, so the call state a
/// callback finds has to nest.
#[test]
fn sqlite3_function_reentrancy() {
    run_test_once(
        r##"
        require "rubygems"
        require "sqlite3"
        db = SQLite3::Database.new(":memory:")
        db.execute("CREATE TABLE t (a)")
        db.execute("INSERT INTO t VALUES (1), (2), (3)")
        res = []
        db.create_function("count_rows", 0) { |fp| fp.result = db.execute("SELECT COUNT(*) FROM t").flatten.first }
        res << db.execute("SELECT count_rows()")
        # A function whose block calls a query that calls another function.
        db.create_function("outer", 0) { |fp| fp.result = db.execute("SELECT count_rows()").flatten.first }
        res << db.execute("SELECT outer()")
        # A raise from the inner one still reaches the caller.
        db.create_function("inner_bad", 0) { |fp| raise "inner" }
        db.create_function("wrap", 0) { |fp| fp.result = db.execute("SELECT inner_bad()").flatten.first }
        res << (begin; db.execute("SELECT wrap()"); rescue StandardError => e; [e.class.name, e.message]; end)
        res << db.execute("SELECT COUNT(*) FROM t")
        db.close
        res
        "##,
    );
}

/// `create_aggregate` over the native binding. The gem builds a class
/// whose instances hold a `FunctionProxy`; the binding makes one per
/// aggregation group, steps it per row, and finalizes it once.
#[test]
fn sqlite3_create_aggregate() {
    run_test_once(
        r##"
        require "rubygems"
        require "sqlite3"
        db = SQLite3::Database.new(":memory:")
        db.execute("CREATE TABLE t (g, v)")
        [["a", 1], ["a", 2], ["b", 10], ["b", 20], ["b", 30]].each { |g, v|
          db.execute("INSERT INTO t VALUES (?, ?)", [g, v])
        }
        db.create_aggregate("mysum", 1) do
          step { |ctx, v| ctx[:sum] = (ctx[:sum] || 0) + v.to_i }
          finalize { |ctx| ctx.result = ctx[:sum] || 0 }
        end
        res = [db.execute("SELECT mysum(v) FROM t")]
        res << db.execute("SELECT g, mysum(v) FROM t GROUP BY g ORDER BY g")
        # A group with no rows still finalizes, on a fresh instance.
        res << db.execute("SELECT mysum(v) FROM t WHERE g = 'zzz'")
        # Unlike a scalar function, an aggregate's arity is registered.
        res << (begin
                  db.execute("SELECT mysum(v, v) FROM t")
                rescue SQLite3::Exception => e
                  [e.class.name, e.message.lines.first.chomp]
                end)
        # Two aggregates in one query accumulate independently.
        res << db.execute("SELECT mysum(v), mysum(v + 1) FROM t")
        # And one composed with a scalar function.
        db.create_function("dbl", 1) { |fp, v| fp.result = v.to_i * 2 }
        res << db.execute("SELECT mysum(dbl(v)) FROM t")
        # `create_aggregate_handler` takes a class directly.
        handler = Class.new do
          def self.arity = 1
          def self.name = "handlermax"
          def initialize; @m = nil; end
          def step(ctx, v); @m = v.to_i if @m.nil? || v.to_i > @m; end
          def finalize(ctx); ctx.result = @m; end
        end
        db.create_aggregate_handler(handler)
        res << db.execute("SELECT handlermax(v) FROM t")
        db.close
        res
        "##,
    );
}

/// A raise from either callback must not unwind through SQLite's C
/// frames: it is carried out of the step and re-raised unchanged, and
/// the connection stays usable.
#[test]
fn sqlite3_aggregate_exceptions() {
    run_test_once(
        r##"
        require "rubygems"
        require "sqlite3"
        db = SQLite3::Database.new(":memory:")
        db.execute("CREATE TABLE t (v)")
        db.execute("INSERT INTO t VALUES (1), (2), (3)")
        res = []
        db.create_aggregate("boomstep", 1) do
          step { |ctx, v| raise ArgumentError, "step boom" if v.to_i == 2 }
          finalize { |ctx| ctx.result = 0 }
        end
        res << (begin
                  db.execute("SELECT boomstep(v) FROM t")
                rescue StandardError => e
                  [e.class.name, e.message]
                end)
        db.create_aggregate("boomfin", 1) do
          step { |ctx, v| }
          finalize { |ctx| raise "final boom" }
        end
        res << (begin
                  db.execute("SELECT boomfin(v) FROM t")
                rescue StandardError => e
                  [e.class.name, e.message]
                end)
        # Unaffected afterwards, and a working aggregate still runs.
        res << db.execute("SELECT COUNT(*) FROM t")
        db.create_aggregate("ok", 1) do
          step { |ctx, v| ctx[:n] = (ctx[:n] || 0) + v.to_i }
          finalize { |ctx| ctx.result = ctx[:n] }
        end
        res << db.execute("SELECT ok(v) FROM t")
        db.close
        res
        "##,
    );
}

/// Every group holds a live instance the collector must not reclaim,
/// and a finished group's slot must be reused rather than leaked.
#[test]
fn sqlite3_aggregate_group_state() {
    run_test_once(
        r##"
        require "rubygems"
        require "sqlite3"
        db = SQLite3::Database.new(":memory:")
        db.execute("CREATE TABLE t (g, v)")
        db.execute("BEGIN")
        60.times { |i| 3.times { |j| db.execute("INSERT INTO t VALUES (?, ?)", ["g#{i}", j]) } }
        db.execute("COMMIT")
        # The block allocates on every row, so a collection can land
        # between them while every group's instance is in flight.
        db.create_aggregate("collect", 1) do
          step { |ctx, v| (ctx[:a] ||= []) << ("y" * 30 + v.to_s) }
          finalize { |ctx| ctx.result = (ctx[:a] || []).map(&:size).sum }
        end
        rows = db.execute("SELECT g, collect(v) FROM t GROUP BY g ORDER BY g")
        res = [rows.size, rows.first, rows.last, rows.map { |r| r[1] }.uniq]
        # Running it again must reuse the slots the first run released.
        3.times { db.execute("SELECT g, collect(v) FROM t GROUP BY g") }
        GC.start
        res << db.execute("SELECT collect(v) FROM t").flatten
        db.close
        res
        "##,
    );
}
