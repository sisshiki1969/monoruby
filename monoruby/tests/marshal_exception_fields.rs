//! What `Marshal` writes for an exception, and for the objects it will
//! not write (#2005).
//!
//! CRuby keeps an exception's own state (`key`, `receiver`, `name`,
//! `status`, `tag`, `result`, …) in `@`-less instance variables, and
//! `Marshal.dump` writes them like any other: after `mesg` and `bt`, by
//! their bare names. monoruby keeps the same state in hidden slots, so
//! the dump maps each slot to its CRuby name and the load maps it back —
//! an exception survives a round trip with its fields, and the bytes are
//! CRuby's. A raised exception also carries `bt_locations`, written as a
//! link to `bt`.
//!
//! An object with no dumpable representation is a T_DATA with no
//! `_dump_data` in CRuby ("no _dump_data is defined for class X"), an IO
//! or a MatchData ("can't dump X").

use monoruby::tests::*;

const HELPER: &str = r#"
  t = ->(&b) { begin; b.call; rescue Exception => e; [e.class.to_s, e.message]; end }
  ex = ->(&b) { begin; b.call; rescue Exception => e; e; end }
  rt = ->(e) { Marshal.load(Marshal.dump(e)) }
  # The field names of a dump, in order (a symbol's length byte is
  # its size + 5).
  fields = ->(e) {
    d = Marshal.dump(e).b
    r = []
    i = 0
    while (i = d.index(":", i))
      n = d.getbyte(i + 1).to_i - 5
      s = n > 0 ? d[i + 2, n] : nil
      r << s if s && s.size == n && s.match?(/\A[@a-z_?]+\z/)
      i += 1
    end
    r
  }
"#;

fn probe(exprs: &[&str]) {
    let list = exprs
        .iter()
        .map(|s| format!("t.() {{ {s} }}"))
        .collect::<Vec<_>>()
        .join(",\n");
    run_test_once(&format!("{HELPER}\n[{list}]"));
}

/// The issue's examples: the fields come back.
#[test]
fn round_trip_keeps_the_fields() {
    probe(&[
        "e = rt.(ex.() { {}.fetch(:a) }); [e.key, e.receiver, e.message]",
        "rt.(ex.() { [].each.next }).result",
        "e = rt.(SystemExit.new(3, \"m\")); [e.status, e.message, e.success?]",
        "e = rt.(ex.() { throw :a }); [e.tag, e.value, e.message]",
        "e = rt.(ex.() { throw :zz, 4 }); [e.tag, e.value, e.message, e.to_s]",
        "e = rt.(ex.() { foo }); [e.name, e.message, e.receiver.class]",
        "e = rt.(ex.() { 1.foo(2, 3) }); [e.name, e.receiver, e.args, e.message]",
        "rt.(ex.() { \"\".freeze << \"a\" }).receiver",
        "e = rt.(ex.() { exit 2 }); [e.status, e.success?]",
        "e = rt.(ex.() { exit }); [e.status, e.success?]",
        "e = rt.(ex.() { raise \"a\", cause: RuntimeError.new(\"c\") }); [e.cause.class, e.cause.message, e.message]",
        "e = rt.(ex.() { def lj2 = proc { return 3 }; lj2.call }); [e.reason, e.exit_value]",
        "e = rt.(ex.() { require \"nonexistent_zzz\" }); [e.path]",
        "e = rt.(NoMatchingPatternKeyError.new(\"x\", matchee: 1, key: 2)); [e.matchee, e.key]",
        "e = rt.(StopIteration.new(\"x\")); [e.result]",
        "e = rt.(ex.() { o = Object.new; def o.each; yield 1; :done; end; en = o.to_enum; en.next; en.next }); [e.result]",
        "e = rt.(NameError.new(\"x\", :n, receiver: 1)); [e.name, e.receiver]",
        "e = rt.(NoMethodError.new(\"x\", :n, [1], true, receiver: 1)); [e.name, e.args, e.private_call?, e.receiver]",
        "e = rt.(FrozenError.new(\"x\", receiver: 1)); [e.receiver]",
        "e = rt.(UncaughtThrowError.new(1, 2, \"x\")); [e.tag, e.value, e.message, e.to_s]",
        "e = rt.(Errno::ENOENT.new(\"x\")); [e.errno == Errno::ENOENT::Errno, e.message]",
        "e = rt.(SignalException.new(\"INT\")); [e.signo, e.message]",
        "e = rt.(Interrupt.new(\"x\")); [e.signo, e.message]",
    ]);
}

/// A field that was never set stays unset.
#[test]
fn unset_fields() {
    probe(&[
        "KeyError.new(\"x\").key",
        "KeyError.new(\"x\").receiver",
        "e = rt.(KeyError.new(\"x\")); [(e.key rescue $!.message), (e.receiver rescue $!.message)]",
        "e = KeyError.new(\"x\", key: nil, receiver: nil); [e.key, e.receiver]",
        "e = rt.(KeyError.new(\"x\", key: nil, receiver: nil)); [e.key, e.receiver]",
        "NameError.new(\"x\").receiver",
        "e = rt.(NameError.new(\"x\", :n)); [e.name, (e.receiver rescue $!.message)]",
        "FrozenError.new(\"x\").receiver",
        "rt.(FrozenError.new(\"x\")).receiver",
        "[NoMethodError.new(\"x\").private_call?, NoMethodError.new(\"x\", :n, [], true).private_call?]",
        "NoMethodError.new(\"x\").args",
        "[StopIteration.new.result, SystemExit.new.status, UncaughtThrowError.new(1, 2).tag]",
    ]);
}

/// The bytes, for exceptions whose message is the program's own string.
#[test]
fn dump_bytes() {
    probe(&[
        "Marshal.dump(RuntimeError.new(\"x\"))",
        "Marshal.dump(Exception.new)",
        "Marshal.dump(StopIteration.new(\"x\"))",
        "Marshal.dump(KeyError.new(\"x\"))",
        "Marshal.dump(NameError.new(\"x\"))",
        "Marshal.dump(NameError.new(\"x\", :n))",
        "Marshal.dump(NameError.new(\"x\", :n, receiver: 1))",
        "Marshal.dump(NoMethodError.new(\"x\", :n, [1], true, receiver: 1))",
        "Marshal.dump(FrozenError.new(\"x\", receiver: 1))",
        "Marshal.dump(FrozenError.new(\"x\"))",
        "Marshal.dump(UncaughtThrowError.new(1, 2, \"x\"))",
        "Marshal.dump(SystemExit.new(3, \"m\"))",
        "Marshal.dump(LoadError.new(\"x\"))",
        "Marshal.dump(SyntaxError.new(\"x\"))",
        "Marshal.dump(Errno::ENOENT.new(\"x\"))",
        "Marshal.dump(Interrupt.new(\"x\"))",
        "e = RuntimeError.new(\"x\"); e.set_backtrace([\"a:1\"]); Marshal.dump(e)",
        "e = RuntimeError.new(\"x\"); e.instance_variable_set(:@a, 1); Marshal.dump(e)",
        "e = KeyError.new(\"x\"); e.instance_variable_set(:@a, 1); Marshal.dump(e)",
    ]);
}

/// The order of the fields of a raised exception: `mesg`, `bt`, the
/// class's own, the program's instance variables, `cause`, and
/// `bt_locations` last.
#[test]
fn dump_field_order() {
    probe(&[
        "fields.(ex.() { raise \"x\" })",
        "fields.(ex.() { {}.fetch(:a) })",
        "fields.(ex.() { foo })",
        "fields.(ex.() { foo() })",
        "fields.(ex.() { 1.foo(2, 3) })",
        "fields.(ex.() { \"\".freeze << \"a\" })",
        "fields.(ex.() { throw :a })",
        "fields.(ex.() { [].each.next })",
        "fields.(ex.() { def lj = proc { return }; lj.call })",
        "fields.(ex.() { require \"nonexistent_zzz\" })",
        "fields.(ex.() { Integer(\"z\") })",
        "fields.(ex.() { 1 / 0 })",
        "fields.(ex.() { exit 2 })",
        "fields.(ex.() { raise \"a\" rescue raise \"b\" })",
        "fields.(ex.() { raise ClosedQueueError })",
        "e = RuntimeError.new(\"x\"); e.instance_variable_set(:@a, 1); fields.(ex.() { raise e })",
        "e = ex.() { raise \"x\" }; Marshal.dump(e).include?(\"bt_locations\")",
        "Marshal.dump(RuntimeError.new(\"x\")).include?(\"bt_locations\")",
        "e = ex.() { raise \"x\" }; e.set_backtrace([\"a:1\"]); fields.(e)",
    ]);
}

/// A loaded exception has its backtrace and no locations.
#[test]
fn loaded_backtrace() {
    probe(&[
        "e = ex.() { raise \"x\" }; e2 = rt.(e); [e2.backtrace == e.backtrace, e2.backtrace_locations]",
        "e = RuntimeError.new(\"x\"); e.set_backtrace([\"a:1\"]); e2 = rt.(e); [e2.backtrace, e2.backtrace_locations]",
        "e = rt.(RuntimeError.new(\"x\")); [e.backtrace, e.backtrace_locations]",
        "rt.(ex.() { 1.foo(2, 3) }).backtrace.size > 0",
        "e = rt.(rt.(ex.() { raise \"x\" })); [e.backtrace.size > 0, e.backtrace_locations, e.message]",
    ]);
}

/// Loading CRuby's bytes: a field of another class, or one nobody
/// knows, is carried along and written back.
#[test]
fn load_foreign_bytes() {
    probe(&[
        "Marshal.load(\"\\x04\\bo:\\rKeyError\\b:\\tmesg\\\"\\x06x:\\abt0:\\bkeyi\\a\").key",
        "Marshal.load(\"\\x04\\bo:\\rKeyError\\t:\\tmesg\\\"\\x06x:\\abt0:\\bkeyi\\a:\\rreceiveri\\x06\").receiver",
        "e = Marshal.load(\"\\x04\\bo:\\x0FSystemExit\\b:\\tmesg\\\"\\x06x:\\abt0:\\vstatusi\\b\"); [e.status, e.success?]",
        "e = Marshal.load(\"\\x04\\bo:\\x11RuntimeError\\b:\\tmesg\\\"\\x06x:\\abt0:\\bkeyi\\a\"); [(e.key rescue 0), e.instance_variables, Marshal.dump(e)]",
        "e = Marshal.load(\"\\x04\\bo:\\rKeyError\\b:\\tmesg\\\"\\x06x:\\abt0:\\vstatusi\\a\"); [(e.status rescue 0), e.instance_variables, Marshal.dump(e)]",
        "e = Marshal.load(\"\\x04\\bo:\\rKeyError\\b:\\tmesg\\\"\\x06x:\\abt0:\\bfooi\\a\"); [e.instance_variables, Marshal.dump(e)]",
        "Marshal.load(\"\\x04\\bo:\\x11RuntimeError\\b:\\tmesg\\\"\\x06x:\\abt[\\x06\\\"\\x08a:1:\\x11bt_locations@\\x06\").backtrace",
    ]);
}

/// `Process::Status` is an ordinary object with two `@`-less fields.
#[test]
fn process_status() {
    run_test_once_live(
        r#"
        `true`
        s = Marshal.load(Marshal.dump($?))
        d = Marshal.dump($?).b
        [s.class, s.exitstatus, s.success?, s.pid == $?.pid, s == $?, d[0, 30]]
        "#,
    );
}

/// Objects that cannot be dumped, and the reason CRuby gives.
#[test]
fn undumpable_objects() {
    run_test_once(
        r##"
        require "stringio"; require "monitor"; require "strscan"; require "digest"
        objs = {
          "Enumerator" => -> { [1].each },
          "Lazy" => -> { [1].lazy },
          "Lazy map" => -> { (1..3).lazy.map { } },
          "Chain" => -> { [1].each + [2].each },
          "Product" => -> { Enumerator::Product.new([1]) },
          "ASeq" => -> { 1.step(3) },
          "Yielder" => -> { y = nil; Enumerator.new { |yy| y = yy; yy << 1 }.next; y },
          "Proc" => -> { proc {} },
          "Method" => -> { 1.method(:+) },
          "UMethod" => -> { Integer.instance_method(:+) },
          "Binding" => -> { binding },
          "Thread" => -> { Thread.current },
          "Fiber" => -> { Fiber.new {} },
          "Mutex" => -> { Mutex.new },
          "Queue" => -> { Queue.new },
          "SizedQueue" => -> { SizedQueue.new(1) },
          "CV" => -> { ConditionVariable.new },
          "Monitor" => -> { Monitor.new },
          "IO" => -> { $stdout },
          "File" => -> { File.open(File::NULL) },
          "MatchData" => -> { "a".match(/a/) },
          "Converter" => -> { Encoding::Converter.new("UTF-8", "UTF-16") },
          "StringIO" => -> { StringIO.new("a") },
          "WeakMap" => -> { ObjectSpace::WeakMap.new },
          "Location" => -> { caller_locations(0).first },
          "ThreadGroup" => -> { ThreadGroup.new },
          "Ractor" => -> { Ractor.current },
          "Stat" => -> { File.stat(".") },
          "Dir" => -> { Dir.new(".") },
          "StringScanner" => -> { StringScanner.new("abc") },
          "Digest::MD5" => -> { Digest::MD5.new },
          "Digest::SHA256" => -> { Digest::SHA256.new },
          "ARGF" => -> { ARGF },
          "main" => -> { TOPLEVEL_BINDING.receiver },
          "singleton" => -> { o = Object.new; def o.x; end; o },
          "in Array" => -> { [1, [proc {}]] },
          "in ivar" => -> { o = Object.new; o.instance_variable_set(:@a, Dir.new(".")); o },
          "subclass" => -> { Object.const_set(:MarshalDirSub, Class.new(Dir)) unless defined?(MarshalDirSub); MarshalDirSub.new(".") },
        }
        objs.map do |k, f|
          r = begin
            Marshal.dump(f.call).bytesize
          rescue Exception => e
            "#{e.class}: #{e.message}"
          end
          [k, r]
        end
        "##,
    );
}

/// What still dumps, unchanged.
#[test]
fn dumpable_objects() {
    run_test(
        r#"
        S = Struct.new(:a, :b) unless defined?(S)
        class Pt; def initialize; @x = 1; @y = [2, "s"]; end; attr_reader :x, :y; end
        objs = [
          nil, true, 1, 2**70, 1.5, "s", :sym, [1, [2]], {a: 1}, 1..2, 1r, 1i, /a/i,
          S.new(1, "x"), Pt.new, String, Kernel, Encoding::UTF_8, Object.new, Time.at(0).utc,
          RuntimeError.new("x"), Exception.new,
        ]
        objs.map { |o| d = Marshal.dump(o); [d, Marshal.load(d).class] }
        "#,
    );
}
