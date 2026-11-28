//! The class a NameError names is asked for its name when the message
//! is read, and `UncaughtThrowError`'s message is a format (#2018).
//!
//! - CRuby's NameError keeps a format and the receiver
//!   (`rb_name_err_mesg`), and renders `%2$s` the moment `#message` is
//!   called: a class shows the name it has *then* — its `name` method if
//!   it defines one, the constant it was assigned to after the error
//!   was made.
//! - `UncaughtThrowError.new(tag, value, message = nil)` takes at least
//!   two arguments, and `#to_s` applies the message, as a format, to
//!   the tag.

use monoruby::tests::*;

const HELPER: &str = r####"
def try = (yield).inspect.gsub(/0x\h+/, "0x") rescue "#{$!.class}: #{$!.message.gsub(/0x\h+/, "0x")}"
"####;

fn probe(exprs: &[&str]) {
    let list = exprs
        .iter()
        .map(|s| format!("try {{ {s} }}"))
        .collect::<Vec<_>>()
        .join(",\n");
    run_test_once(&format!("{HELPER}\n[{list}]"));
}

/// A class or module that defines `name` / `inspect`, in each of the
/// messages that name it.
#[test]
fn class_with_its_own_name() {
    probe(&[
        r####"c = Class.new { def self.inspect = "custom" }; c.instance_method(:zz)"####,
        r####"c = Class.new { def self.name = "custom_name" }; c.instance_method(:zz)"####,
        r####"c = Class.new { def self.inspect = "custom" }; c.zork"####,
        r####"c = Class.new { def self.name = "custom_name" }; c.zork"####,
        r####"c = Class.new { def self.name = "custom_name" }; c.new.zork"####,
        r####"c = Class.new { def self.inspect = "custom" }; c.new.zork"####,
        r####"c = Class.new { def self.name = "custom_name" }; c::Zork"####,
        r####"c = Class.new { def self.inspect = "custom" }; c::Zork"####,
        r####"c = Class.new { def self.name = "custom_name" }; c.class_variable_get(:@@zz)"####,
        r####"c = Class.new { def self.name = "custom_name" }; c.send(:remove_method, :zz)"####,
        r####"c = Class.new { def self.name = "custom_name" }; c.send(:undef_method, :zz)"####,
        r####"c = Class.new { def self.name = "custom_name" }; c.send(:remove_const, :Zz)"####,
        r####"c = Class.new { def self.name = "custom_name" }; c.send(:alias_method, :a, :zz)"####,
        r####"c = Class.new { def self.name = "custom_name" }; c.send(:public, :zz)"####,
        r####"c = Class.new { def self.name = "custom_name" }; c.method(:zz)"####,
        r####"c = Class.new { def self.name = "custom_name" }; c.new.method(:zz)"####,
        r####"c = Class.new { def self.name = "custom_name"; def pp; end; private :pp }; c.public_instance_method(:pp)"####,
        r####"c = Class.new { def self.name = "custom_name"; def pp; end; private :pp }; c.new.pp"####,
        r####"c = Class.new { def self.name = "custom_name"; }; c.new.instance_eval { zork }"####,
        r####"m = Module.new { def self.name = "mod_name" }; m.zork"####,
        r####"m = Module.new { def self.name = "mod_name" }; m.instance_method(:zz)"####,
        r####"c = Class.new { def self.name = "custom_name" }; c.new.instance_variable_get("bad")"####,
        r####"c = Class.new { def self.name = "custom_name" }; c.const_get(:Zork)"####,
        r####"c = Class.new { def self.name = "custom_name"; def self.inspect = "insp" }; c.new.send(:zork_v)"####,
        r####"c = Class.new { def self.name = "custom_name"; def self.inspect = "insp" }; c.new.instance_eval { zork_v }"####,
        r####"c = Class.new { def self.name = "custom_name"; def foo = super }; c.new.foo"####,
        r####"c = Class.new { def self.name = "custom_name"; def self.to_s = "tos" }; [c.instance_method(:zz)]"####,
        r####"class String; def zzz = super; end; "a".zzz"####,
        r####"c = Class.new(Struct.new(:a)) { def self.name = "sname" }; c.new(1).zork"####,
    ]);
}

/// Named after the error was made; `name` called once per reading;
/// a `name` that raises or answers a non-String.
#[test]
fn name_is_read_when_the_message_is() {
    probe(&[
        r####"c = Class.new; e = (c.instance_method(:zz) rescue $!); Object.const_set(:NamedLater1, c); e.message"####,
        r####"c = Class.new; e = (c.zork rescue $!); Object.const_set(:NamedLater2, c); e.message"####,
        r####"c = Class.new; e = (c.new.zork rescue $!); Object.const_set(:NamedLater3, c); e.message"####,
        r####"$n = 0; c = Class.new { def self.name = ($n += 1; "nm") }; e = (c.zork rescue $!); a = $n; e.message; e.message; [a, $n]"####,
        r####"c = Class.new { def self.name = raise("boom") }; c.zork"####,
        r####"c = Class.new { def self.name = 42 }; c.zork"####,
        r####"c = Class.new { def self.name = nil; def self.inspect = 42 }; c.zork"####,
    ]);
}

/// `UncaughtThrowError.new` and its `to_s`.
#[test]
fn uncaught_throw_error() {
    probe(&[
        r####"UncaughtThrowError.new(1, 2).to_s"####,
        r####"UncaughtThrowError.new(1, 2, "x %p y").to_s"####,
        r####"[UncaughtThrowError.new(1, 2, "m").tag, UncaughtThrowError.new(1, 2, "m").value, UncaughtThrowError.new(1, 2, "m").message]"####,
        r####"UncaughtThrowError.new"####,
        r####"UncaughtThrowError.new(1)"####,
        r####"(throw :zz rescue $!).to_s"####,
    ]);
}

/// What the report of an uncaught exception shows: `#detailed_message`,
/// so a format message is applied to its tag there too, and a
/// `message` / `to_s` / `detailed_message` the class defines is what is
/// printed. Spawns the binary, since the in-process harness never
/// prints the report. The expected texts are CRuby 4.0.9's.
#[test]
fn uncaught_report_uses_detailed_message() {
    let cases = [
        (
            "throw :foo",
            "-e:1:in 'Kernel#throw': uncaught throw :foo (UncaughtThrowError)\n\tfrom -e:1:in '<main>'\n",
        ),
        (
            r#"class E < StandardError; def detailed_message(highlight: false, **) = "custom detail"; end; raise E"#,
            "-e:1:in '<main>': custom detail\n",
        ),
        (
            r#"class E < StandardError; def message = "custom message"; end; raise E"#,
            "-e:1:in '<main>': custom message (E)\n",
        ),
        (
            r#"class E < StandardError; def to_s = "custom to_s"; end; raise E, "given""#,
            "-e:1:in '<main>': custom to_s (E)\n",
        ),
        (
            r#"c = Class.new { def self.name = "custom_name" }; c.zork"#,
            "-e:1:in '<main>': undefined method 'zork' for class custom_name (NoMethodError)\n",
        ),
        (
            r#"raise UncaughtThrowError.new(:t, 1, "tag is %p")"#,
            "-e:1:in '<main>': tag is :t (UncaughtThrowError)\n",
        ),
        (
            r#"begin; raise "inner"; rescue; raise TypeError, "outer"; end"#,
            "-e:1:in '<main>': outer (TypeError)\n-e:1:in '<main>': inner (RuntimeError)\n",
        ),
    ];
    for (script, expected) in cases {
        let out = std::process::Command::new(env!("CARGO_BIN_EXE_monoruby"))
            .env_remove("RUBYOPT")
            .env_remove("RUBYLIB")
            .args(["--disable=gems", "-e", script])
            .output()
            .expect("spawn monoruby");
        assert_eq!(String::from_utf8_lossy(&out.stderr), expected, "{script}");
    }
}

/// A thread that dies of an exception reports it the same way
/// (`report_on_exception`): the message line is `#detailed_message`'s.
/// The first line carries the thread's address and is left out.
#[test]
fn thread_report_uses_detailed_message() {
    let cases = [
        (
            "Thread.new { throw :foo }.join rescue 1",
            "-e:1:in 'Kernel#throw': uncaught throw :foo (UncaughtThrowError)\n\tfrom -e:1:in 'block in <main>'\n",
        ),
        (
            r#"class E < StandardError; def detailed_message(**) = "DM"; end; Thread.new { raise E, "x" }.join rescue 1"#,
            "-e:1:in 'block in <main>': DM\n",
        ),
        (
            r#"Thread.new { raise "boom" }.join rescue 1"#,
            "-e:1:in 'block in <main>': boom (RuntimeError)\n",
        ),
    ];
    for (script, expected) in cases {
        let out = std::process::Command::new(env!("CARGO_BIN_EXE_monoruby"))
            .env_remove("RUBYOPT")
            .env_remove("RUBYLIB")
            .args(["--disable=gems", "-e", script])
            .output()
            .expect("spawn monoruby");
        let err = String::from_utf8_lossy(&out.stderr).into_owned();
        let (head, rest) = err.split_once('\n').unwrap_or((&err, ""));
        assert!(
            head.ends_with("terminated with exception (report_on_exception is true):"),
            "{script}: {err}"
        );
        assert_eq!(rest, expected, "{script}");
    }
}
