//! A bignum's `div` by a Float, `Comparable#between?` reading `<=>`
//! answers as `rb_cmpint`, and the order in which `sort` / `min(n)` /
//! `max(n)` call the comparator (#2045, #2046, #2014).
//!
//! - **`div`** (#2045). A bignum receiver over a Float is
//!   `rb_big_divide`'s `rb_dbl2big(x / y)`, truncated toward zero; a
//!   fixnum receiver floors. The division matrix in
//!   `num_coerce_cmpint_float_ties_set_marshal.rs` covers the rows.
//! - **`between?`** (#2046). Each `<=>` answer is read as `rb_cmpint`
//!   reads it: an Integer by its sign, nil as the comparison failure,
//!   anything else by `> 0` and then `< 0`, so a NaN answer is 0.
//! - **Comparator order** (#2014). `sort` is CRuby's `ruby_qsort`, which
//!   on Linux is glibc's `qsort_r`, a top-down merge sort; `min(n)` /
//!   `max(n)` / `min_by(n)` / `max_by(n)` are `rb_nmin_run`. A block or a
//!   user-defined `<=>` sees the same pairs in the same order.
//! - **Instance variables and uninitialized objects** (#2006).
//! - **Marshal of exceptions** (#2013): uninitialized ones, `SystemExit`'s
//!   own `initialize`, `KeyError`'s receiver/key order, ENV, anonymous
//!   Struct classes and `abort`'s message.

use monoruby::tests::*;

const HELPER: &str = r##"
  t = ->(&b) { begin; b.call.inspect.gsub(/0x\h+/, "0x"); rescue Exception => e; "#{e.class}: #{e.message}".gsub(/0x\h+/, "0x"); end }
"##;

#[test]
fn between_reads_answers_as_cmpint() {
    run_test_once(&format!(
        r#"
        {HELPER}
        class A; include Comparable; def initialize(r) = @r = r; def <=>(o) = @r; end
        class W; def >(o) = true; def <(o) = false; end
        class W2; def >(o) = false; def <(o) = true; end
        [Float::NAN, Object.new, true, W.new, W2.new, nil, "a", 1.5, -1.5, 2**70, -(2**70),
         1r, -1r, BasicObject.new, 0, -3, 7].map do |r|
          t.() {{ A.new(r).between?(A.new(r), A.new(r)) }}
        end
        "#
    ));
}

/// The usual answers keep their meaning once `between?` is compiled.
#[test]
fn between_in_compiled_code() {
    run_test(
        r#"
        r = []
        r << 5.between?(1, 10) << 5.between?(6, 10) << 5.between?(1, 4)
        r << 1.5.between?(1, 2) << "b".between?("a", "c") << (2**70).between?(1, 2**71)
        r << (begin; 5.between?(nil, 3); rescue ArgumentError => e; e.message; end)
        r << (begin; 3.between?(1, nil); rescue ArgumentError => e; e.message; end)
        r
        "#,
    );
}

/// Arrays of every small length and a few larger ones, with repeated
/// values, from a fixed sequence.
const ARRAYS: &str = r##"
  seed = 12345
  arrays = [*0..20, 33, 64, 101].map do |len|
    Array.new(len) { seed = (seed * 1103515245 + 12345) % 2**31; seed % (len / 3 + 1) }
  end
"##;

/// `sort` with a block and with a user-defined `<=>`, and `sort_by` with
/// keys that compare through Ruby, call the comparator with the pairs of
/// glibc's merge sort, which is CRuby's `ruby_qsort` on Linux (#2014).
#[test]
fn sort_calls_the_comparator_in_crubys_order() {
    run_test_once(&format!(
        r#"
        {ARRAYS}
        K = Struct.new(:v, :log) do
          include Comparable
          def <=>(o) = (log << [v, o.v]; v <=> o.v)
        end
        arrays.map do |a|
          l1 = []; r1 = a.sort {{ |x, y| l1 << [x, y]; x <=> y }}
          l2 = []; r2 = a.map {{ K.new(_1, l2) }}.sort.map(&:v)
          l3 = []; r3 = a.each_with_index.sort_by {{ |x, i| K.new(x, l3) }}.map(&:last)
          [r1, l1, r2, l2, r3, l3]
        end
        "#
    ));
}

/// `min(n)` / `max(n)` and `min_by(n)` / `max_by(n)` are `rb_nmin_run`:
/// a buffer of `4 * n` cut down by a quickselect, then sorted (#2014).
#[test]
fn nmin_calls_the_comparator_in_crubys_order() {
    run_test_once(&format!(
        r#"
        {ARRAYS}
        arrays.flat_map do |a|
          [1, 2, 3, 7].flat_map do |n|
            l = []
            [
              a.min(n) {{ |x, y| l << [x, y]; x <=> y }}, a.max(n) {{ |x, y| l << [x, y]; x <=> y }},
              a.each.min(n) {{ |x, y| l << [x, y]; x <=> y }}, a.each.max(n) {{ |x, y| l << [x, y]; x <=> y }},
              a.each_with_index.min_by(n) {{ |x, i| l << i; x }}, a.each_with_index.max_by(n) {{ |x, i| l << i; x }},
              (1..a.size).min(n) {{ |x, y| l << [x, y]; a[x - 1] <=> a[y - 1] }},
              a.min(n), a.max(n), l,
            ]
          end
        end
        "#
    ));
}

/// Which pair a failing comparison names follows from the order.
#[test]
fn sort_and_nmin_errors_name_crubys_pair() {
    run_test_once(&format!(
        r#"
        {HELPER}
        [
          t.() {{ [1, nil].max(1) }}, t.() {{ [1, nil].min(1) }}, t.() {{ [1, nil, 2].sort }},
          t.() {{ [3, "a", 1, nil].sort }}, t.() {{ [2, 1, "a"].each.min(2) }},
          t.() {{ [3, 1, 2].min(2) {{ nil }} }}, t.() {{ [3, 1, 2].max(2) {{ 1.5 }} }},
          t.() {{ [1, nil].sort_by {{ _1 }} }}, t.() {{ [3, 1, 2].sort {{ |a, b| raise "x" if a == 1; a <=> b }} }},
        ]
        "#
    ));
}

/// What CRuby keeps in ordinary instance variables is in them here too:
/// `LocalJumpError`'s `@exit_value` / `@reason`, `LoadError`'s and
/// `SyntaxError`'s `@path`, `Digest::SHA2`'s `@sha2` (#2006).
#[test]
fn exception_and_digest_instance_variables() {
    run_test_once(&format!(
        r#"
        {HELPER}
        require "digest/sha2"
        def ff = yield
        [
          t.() {{ e = (begin; ff; rescue LocalJumpError => e; e; end); [e.instance_variables, e.exit_value, e.reason] }},
          t.() {{ e = LocalJumpError.new("x"); [e.instance_variables, e.exit_value, e.reason] }},
          t.() {{ e = (begin; require "zzz_nonexistent"; rescue LoadError => e; e; end); [e.instance_variables, e.path] }},
          t.() {{ e = LoadError.new("x"); e.instance_variable_set(:@path, "p"); [e.instance_variables, e.path] }},
          t.() {{ e = (begin; eval("1 +"); rescue SyntaxError => e; e; end); [e.instance_variables, e.path.class] }},
          t.() {{ e = SyntaxError.new("x"); [e.instance_variables, e.path] }},
          t.() {{ e = (begin; ff; rescue LocalJumpError => e; e; end); e = Marshal.load(Marshal.dump(e)); [e.instance_variables, e.exit_value, e.reason] }},
          t.() {{ e = (begin; require "zzz_nonexistent"; rescue LoadError => e; e; end); e = Marshal.load(Marshal.dump(e)); [e.instance_variables, e.path] }},
          t.() {{ d = Digest::SHA2.new; d << "a"; e = d.dup; e << "b"; [d.instance_variables, d.hexdigest, e.hexdigest, d.size, d.length, d.block_length, d.inspect] }},
          t.() {{ [Digest::SHA2.new(384).length, Digest::SHA2.new(512).hexdigest("x"), Digest::SHA2.hexdigest("a"), Digest::SHA256.new.size] }},
          t.() {{ Digest::SHA2.new(1) }}, t.() {{ Digest::SHA2.instance_methods(false).sort }},
        ]
        "#
    ));
}

/// A `Process::Status` is made by the interpreter only, and one made
/// otherwise is uninitialized; an uninitialized `File::Stat` raises from
/// every reader (#2006).
#[test]
fn uninitialized_status_and_stat() {
    run_test_once(&format!(
        r#"
        {HELPER}
        s = Process::Status.allocate
        st = File::Stat.allocate
        `true`
        [
          t.() {{ Process::Status.new }}, t.() {{ Process::Status.new(0, 1) }},
          t.() {{ [s.inspect, s.to_s, s.pid, s.to_i, s.exitstatus, s.success?, s == 0, s.exited?, s.signaled?] }},
          t.() {{ d = $?.dup; [d.pid == $?.pid, d.pid, d.to_i, d.inspect, d.frozen?] }},
          t.() {{ c = $?.clone; [c.pid == $?.pid, c.inspect, c.frozen?] }},
          t.() {{ [$?.inspect =~ /exit 0/, $?.success?, $?.frozen?] }},
          *%i[dev ino mode nlink uid gid rdev size blksize blocks atime mtime ctime birthtime directory? file?
              ftype setuid? readable? writable_real? executable? world_readable? zero? size? owned? dev_major
              rdev_minor sticky?].map {{ |m| t.() {{ st.public_send(m) }} }},
          t.() {{ st.inspect }}, t.() {{ st <=> File::Stat.new(".") }}, t.() {{ File::Stat.new(".") <=> st }},
          t.() {{ File::Stat.new(".").dup.directory? }},
        ]
        "#
    ));
}

/// The fields `Marshal.dump` writes for an exception follow CRuby's
/// `exc_dump_data` (#2013).
#[test]
fn exception_marshal_fields() {
    run_test_once(&format!(
        r#"
        {HELPER}
        f = ->(e) {{ Marshal.dump(e).b.scan(/[a-z_]{{2,}}/) }}
        class SE2 < SystemExit; def initialize; end; end
        [
          t.() {{ f.(SystemExit.allocate) }}, t.() {{ f.(NameError.allocate) }}, t.() {{ f.(RuntimeError.allocate) }},
          t.() {{ f.(SE2.new) }}, t.() {{ SE2.new.status }}, t.() {{ f.(SystemExit.new) }},
          t.() {{ f.(KeyError.new("x", receiver: 1, key: 2)) }}, t.() {{ f.(KeyError.new("x", key: 2, receiver: 1)) }},
          t.() {{ f.(KeyError.new("x", key: 2)) }},
          t.() {{ e = Marshal.load(Marshal.dump(KeyError.new("x", receiver: 1, key: 2))); [e.receiver, e.key, e.message] }},
          t.() {{ e = Marshal.load(Marshal.dump(SystemExit.allocate)); [e.class, e.message, e.status] }},
          t.() {{ [SystemExit.new(false).status, SystemExit.new(3, "m").message, SystemExit.new("m").status] }},
          t.() {{ SystemExit.new(1, 2, 3) }}, t.() {{ SystemExit.new(true, "a", "b") }},
          t.() {{ Marshal.dump(ENV) }}, t.() {{ Marshal.dump(Struct.new(:a).new(1)) }},
          t.() {{ begin; abort; rescue SystemExit => e; [e.message, e.status]; end }},
          *[RuntimeError.new("x"), KeyError.new("x", receiver: 1, key: 2), SystemExit.allocate].map {{ |e|
            t.() {{ 2.times {{ e = Marshal.load(Marshal.dump(e)) }}; f.(e) }} }},
          t.() {{ e = (raise SystemExit.allocate rescue $!); [f.(e).first(3), e.message] }},
          t.() {{ [RuntimeError.allocate.dup.message, f.(RuntimeError.allocate.dup)] }},
        ]
        "#
    ));
}
