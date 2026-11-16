extern crate monoruby;
use monoruby::tests::*;

// The built-in classes that are C in CRuby and Ruby here keep their state
// in *hidden* instance variables (doc/hidden_ivars.md), so it does not
// show up in `instance_variables` / `inspect`, cannot be reached with
// `instance_variable_get`, does not collide with a subclass's own `@name`,
// and survives `freeze` (#1950). `hidden_ivars.rs` covers the thread
// synchronization classes; this file covers the rest.

const HELPER: &str = r#"
  require "digest"
  require "digest/sha2"
  require "strscan"
  require "etc"
  t = ->(&b) { begin; b.call; rescue Exception => e; [e.class.to_s, e.message.gsub(/0x\h+/, "0x")]; end }
"#;

fn run(body: &str) {
    run_test_once(&format!("{HELPER}\n{body}"));
}

/// The issue's survey: no instance of these classes has instance variables.
#[test]
fn no_instance_variables() {
    run(r#"
        `true`
        objs = [
          $?, Dir.new("."), File.stat("."), 1.step(10, 2), (1..10).step(2),
          [1].lazy, [1].lazy.map { _1 }.select { _1 }, [1].each + [2],
          Enumerator::Product.new([1]), [1].each, Enumerator.new { |y| y << 1 },
          Enumerator.produce(1) { _1 + 1 },
          SystemCallError.new("x", 2), Errno::ENOENT.new, SignalException.new("INT"), Interrupt.new,
          t.() { "\xff".encode("UTF-16") }, t.() { "あ".encode("US-ASCII") },
          (begin; case {a: 1}; in {b:}; end; rescue NoMatchingPatternKeyError => e; e; end),
          (begin; "\xff".encode("UTF-16"); rescue => e; e; end),
          (begin; raise "a"; rescue => e; e.backtrace_locations; e; end),
          Digest::MD5.new, Digest::SHA1.new, Digest::SHA256.new, Digest::SHA512.new,
          StringScanner.new("a"), Process.times, Ractor.current, GC, Ractor,
          SystemCallError, Errno::ENOENT, Process, Signal,
        ]
        objs.map { |o| o.instance_variables }
    "#);
}

#[test]
fn inspect_shows_no_state() {
    run(r##"
        f = ->(x) { x.inspect.gsub(/0x\h+/, "0x") }
        s = StringScanner.new("abc"); s.scan(/a/)
        [
          f.(Dir.new(".")), f.(Dir.allocate), f.(1.step(10, 2)), f.((1..10.0).step(2)),
          f.([1].lazy.map { _1 }), f.([1].each + [2]), f.(Enumerator::Product.new([1])),
          f.(SystemCallError.new("x", 2)), f.(SignalException.new("INT")),
          f.(Digest::MD5.new), f.(Digest::SHA2.new(384)), f.(s), f.(GC),
          File::Stat.allocate.inspect,
          Dir.new(".").instance_variable_get(:@path),
          Dir.new(".").instance_variable_get(:@__path),
          s.instance_variable_get(:@pos), s.instance_variable_defined?(:@__pos),
          SystemCallError.new("x", 2).instance_variable_get(:@__errno),
        ]
    "##);
}

/// A subclass's own instance variables of the same names are its own.
#[test]
fn subclass_ivars_do_not_collide() {
    run(r#"
        class SS < StringScanner; def initialize(s); super; @pos = :mine; @str = 1; end; end
        class DD < Dir; def initialize(*); super; @path = 9; @pos = :x; @closed = true; end; end
        class EE < Errno::ENOENT; def initialize(*); super; @errno = :e; @__errno = :f; end; end
        class MD < Digest::MD5; def initialize; super; @buffer = nil; end; end
        s = SS.new("abc")
        d = DD.new(".")
        [
          s.scan(/a/), s.pos, s.string, s.instance_variables,
          d.path, d.pos, d.read.class, d.instance_variables,
          EE.new.errno == Errno::ENOENT::Errno, EE.new.instance_variables,
          MD.new.update("a").hexdigest, MD.new.instance_variables,
        ]
    "#);
}

/// Frozen objects keep working where CRuby's do: the state is not the
/// frozen object's instance variables. (A frozen Dir is the exception —
/// CRuby checks the flag itself.)
#[test]
fn frozen_objects() {
    run(r#"
        `true`
        [
          $?.frozen?, $?.success?, $?.to_i,
          t.() { s = StringScanner.new("abc").freeze; [s.scan(/a/), s.pos] },
          t.() { Digest::MD5.new.freeze.update("a").hexdigest },
          t.() { Digest::SHA2.new.freeze.update("a").hexdigest },
          t.() { [1].lazy.freeze.map { _1 }.to_a },
          t.() { e = [1].each + [2]; e.freeze; e.to_a },
          t.() { e = [1].each + [2]; e.freeze; e.rewind; 1 },
          t.() { e = Enumerator::Product.new([1], [2]).freeze; e.to_a },
          t.() { e = Errno::ENOENT.new.freeze; e.errno == Errno::ENOENT::Errno },
          t.() { (1..3).step(1).freeze.to_a },
          t.() { s = File::Stat.new(".").freeze; [s.size.class, s.mtime.class, s <=> s, s.directory?] },
          t.() { d = Dir.new(".").freeze; d.read },
          t.() { d = Dir.new(".").freeze; d.close },
          t.() { d = Dir.new(".").freeze; d.chdir { 1 } },
          t.() { d = Dir.new(".").freeze; d.each.class },
          t.() { d = Dir.new(".").freeze; [d.path, d.to_path, d.inspect] },
          t.() { d = Dir.new("."); d.close; d.freeze; d.close },
        ]
    "#);
}

/// `dup` / `clone`: a StringScanner or a Digest copies its state, a Dir
/// does not share its descriptor with the copy.
#[test]
fn copies() {
    run(r#"
        s = StringScanner.new("abc"); d = s.dup; d.scan(/a/)
        md = Digest::MD5.new.update("a"); md2 = md.dup; md2.update("b")
        dir = Dir.new("."); copy = dir.dup
        [
          [s.pos, d.pos], [md.hexdigest, md2.hexdigest],
          copy.inspect.gsub(/0x\h+/, "0x"), t.() { copy.read }, copy.close, copy.path,
          dir.read.class, dir.close,
          File::Stat.new(".").dup.directory?,
          t.() { Dir.allocate.read }, Dir.allocate.close, Dir.allocate.path,
          t.() { Dir.allocate.fileno },
        ]
    "#);
}

/// Structs in CRuby are Structs here: `Process::Tms`, `Etc::Passwd`,
/// `Etc::Group`.
#[test]
fn struct_classes() {
    run(r#"
        tms = Process.times
        [
          Process::Tms.ancestors.take(2), Process::Tms.members, tms.to_a.size,
          tms.utime.class, tms.members, Process::Tms.new(1, 2, 3, 4).to_a,
          Etc::Passwd.ancestors.take(2), Etc::Passwd.members,
          Etc::Group.ancestors.take(2), Etc::Group.members,
          Etc.getpwuid.class, Etc.getgrgid.class, Etc.getpwuid.name.class,
        ]
    "#);
}

/// `Process::Status#==` is CRuby's `pst_equal`.
#[test]
fn process_status_eq() {
    run(r#"
        `true`
        s = $?
        `false`
        [s == 0, s == $?, s == s, $? == 256, $? == 1, s == nil, s == "0", 0 == s]
    "#);
}

/// Marshal carries the fields CRuby dumps as exception "ivars" without an
/// `@` (`errno`, `signo`, `matchee`, `key`, the conversion errors' fields),
/// and they come back.
#[test]
fn marshal_round_trip_of_exception_fields() {
    run(r#"
        rt = ->(e) { Marshal.load(Marshal.dump(e)) }
        pat = (begin; case {a: 1}; in {b:}; end; rescue NoMatchingPatternKeyError => e; e; end)
        enc = (begin; "\xff".encode("UTF-16"); rescue => e; e; end)
        [
          Marshal.dump(SystemCallError.new("x", 2)),
          Marshal.dump(Errno::ENOENT.new("x")),
          rt.(SystemCallError.new("x", 2)).errno,
          rt.(Errno::ENOENT.new("x")).errno,
          rt.(SignalException.new("INT")).signo,
          Marshal.dump(SignalException.new("INT")).include?(":\nsignoi\a"),
          [rt.(pat).key, rt.(pat).matchee],
          [rt.(enc).error_bytes, rt.(enc).source_encoding_name, rt.(enc).destination_encoding_name,
           rt.(enc).readagain_bytes, rt.(enc).incomplete_input?],
          rt.(enc).instance_variables,
          rt.(SystemCallError.new("x", 2)).instance_variables,
        ]
    "#);
}

/// The helpers the Ruby halves call on each other do not show up in the
/// classes' method lists.
#[test]
fn no_helper_methods_leak() {
    run(r#"
        [Dir, File::Stat, Process::Status, Enumerator::Lazy, Enumerator::Chain,
         Enumerator::Product, Enumerator::ArithmeticSequence, StringScanner, Digest::MD5].map { |c|
          (c.instance_methods(false) + c.private_instance_methods(false)).grep(/\A__/)
        }
    "#);
}
