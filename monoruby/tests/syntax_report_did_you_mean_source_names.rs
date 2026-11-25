//! Four things CRuby decides for a process started on a file, none of
//! them visible to an in-process `run_test` (which runs with gems off
//! and compiles from a string):
//!
//! - the message of the SyntaxError of a source that does not parse
//!   (#2230): "FILE:LINE: syntax error(s) found" and the source around
//!   every error, raised by whoever asked for the parse (`eval`,
//!   `require`, the interpreter for its script) with `#path` naming the
//!   source;
//! - did_you_mean (#2238): `NameError#local_variables`, the
//!   corrections of a NameError / KeyError / LoadError /
//!   NoMatchingPatternKeyError and the "Did you mean?" of their
//!   reports. CRuby requires the gem at boot; monoruby registers
//!   `DidYouMean` as an autoload and loads it for the first exception
//!   that is asked for one of them;
//! - a feature whose file name is not UTF-8 (#2235): `require`, `load`,
//!   `autoload`, `$LOADED_FEATURES`, `__FILE__` and backtraces keep the
//!   bytes and the encoding of the name they were given;
//! - an argument of `spawn` / `system` / `exec` / `IO.popen` in an
//!   encoding that is not ASCII-compatible (#2231), which CRuby hands
//!   on as the C string its bytes are.
//!
//! The expectations are differential: each case runs the same script
//! on the reference CRuby, in the same directory.

use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

fn apply(cmd: &mut Command) {
    cmd.env_remove("RUBYOPT")
        .env_remove("RUBYLIB")
        .env_remove("RUBYPATH")
        .env_remove("NO_COLOR");
}

fn monoruby(flags: &[&str]) -> Command {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_monoruby"));
    apply(&mut cmd);
    cmd.args(flags);
    cmd
}

fn cruby(flags: &[&str]) -> Command {
    let mut cmd = Command::new(monoruby::tests::ruby_path());
    apply(&mut cmd);
    cmd.args(flags);
    cmd
}

/// Exit status, stdout and stderr of `cmd` run in `dir` on `stdin`.
fn run(mut cmd: Command, dir: &Path, stdin: &[u8]) -> String {
    cmd.current_dir(dir)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    let mut child = cmd.spawn().expect("failed to spawn");
    child
        .stdin
        .take()
        .unwrap()
        .write_all(stdin)
        .expect("write stdin");
    let out = child.wait_with_output().expect("wait");
    format!(
        "exit: {:?}\n{}--- stderr ---\n{}",
        out.status.code(),
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    )
}

fn assert_same(what: &str, expected: &str, actual: &str) {
    if expected != actual {
        let first_diff = expected
            .lines()
            .zip(actual.lines())
            .find(|(e, a)| e != a)
            .map(|(e, a)| format!("\n  CRuby:    {e}\n  monoruby: {a}"))
            .unwrap_or_else(|| "\n  (one output is a prefix of the other)".to_string());
        panic!(
            "{what}: first differing line:{first_diff}\n\
             --- CRuby ---\n{expected}\n--- monoruby ---\n{actual}"
        );
    }
}

/// Run `args` on both Rubies in `dir` — CRuby with `cruby_flags` in
/// front, monoruby with `mono_flags` — and require the same exit
/// status, stdout and stderr.
fn compare(
    what: &str,
    dir: &Path,
    cruby_flags: &[&str],
    mono_flags: &[&str],
    args: &[&str],
    stdin: &[u8],
) {
    let mut c = cruby(cruby_flags);
    c.args(args);
    let mut m = monoruby(mono_flags);
    m.args(args);
    let expected = run(c, dir, stdin);
    let actual = run(m, dir, stdin);
    assert_same(&format!("{what} ({args:?})"), &expected, &actual);
}

/// A private directory for one test's fixtures.
struct Fixtures(PathBuf);

impl Fixtures {
    fn new(name: &str) -> Self {
        let dir = std::env::temp_dir().join(format!("mr_report_{name}_{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).expect("create fixture dir");
        Fixtures(dir)
    }

    fn write(&self, name: &str, body: impl AsRef<[u8]>) {
        std::fs::write(self.0.join(name), body).expect("write fixture");
    }
}

impl Drop for Fixtures {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

// ---------------------------------------------------------------------
// #2230: the SyntaxError of a source that does not parse
// ---------------------------------------------------------------------

/// CRuby's `syntax_suggest` adds a block of its own in front of the
/// report of a *file* that does not parse; monoruby does not ship it.
/// (`--disable=gems` keeps rubygems, and with it did_you_mean and
/// error_highlight, out of both.)
const SYNTAX_CRUBY: &[&str] = &["--disable=gems,syntax_suggest"];
const SYNTAX_MONO: &[&str] = &["--disable=gems"];

/// Sources whose diagnostics prism 2.x (monoruby) and prism 1.8
/// (CRuby 4.0) agree on.
const EVAL_SYNTAX: &str = r##"
srcs = ["a = 1; )", "x = \ny = (1 +", "def f\n  1 +\nend\nfoo(",
        "あい = [1,\n  \"うえ\" 2]", "if true\n  p 1\n" + "# c\n" * 12 + "en d d",
        "x" * 200 + " = )", "a = 1; \u3042\u3044 )) ;" + "y" * 150, "\t\tx = )",
        "1 +* 2 // x", "class A; def self; end", "$1 = 2; @1 = 3", "p(**)",
        "def f(a, a); end", "x = <<~EOS\n  abc\n", "case 1\nin [a, a]\nend",
        "foo(&b, 1)", "a, b += 1", "%w[", "/(/", "?", "0b", "1.e", "p 08",
        "\xff = 1".b, "x = 'a\xffb' )".b, "break", "class A; return; end",
        "def f; A = 1; end", "retry", "def f; BEGIN { }; end",
        # A regexp literal that does not compile is the compilation's
        # error too, in a form of its own: no source is quoted.
        "/[/", "/\\p{Foo}/", "x = 1\n/(?<a>x)\\k<b>/", "def f = /[/", "/\#{1}[/", "/[/o",
        "/(?<n>a)(?<n>b)\\g<n>*)/"]
srcs.each_with_index do |src, i|
  begin
    eval(src, nil, "t.rb", 1)
    puts "#{i}: ok"
  rescue SyntaxError => e
    puts "#{i}: #{e.message.b.inspect}"
    puts e.message
    p e.message.encoding, e.path, e.backtrace.first, e.backtrace_locations.first.to_s
  rescue Exception => e
    p [i, e.class, e.message]
  end
end
def t(n) = begin; yield; rescue SyntaxError => e; puts "#{n}: #{e.message}"; p e.path, e.backtrace; end
t(:line) { eval("a = )", nil, "t.rb", 10) }
t(:negative_line) { eval("a = \n)", binding, "t.rb", -3) }
t(:no_name) { eval("a = )") }
t(:instance_eval) { instance_eval("a = )") }
t(:class_eval) { Class.new.class_eval("a = )", "c.rb", 7) }
t(:binding) { binding.eval("a = )") }
t(:proc_binding) { proc {}.binding.eval("a = )", "b.rb") }
p SyntaxError.new("x").path
p (SyntaxError.new("x", path: "q.rb").path rescue $!.class)
begin; eval("# -*- coding: foo -*-\n1"); rescue Exception => e; p e.class, e.message; end
begin; eval("# -*- coding: euc-jp -*-\nx = )"); rescue Exception => e; p e.class, e.message, e.message.encoding; end
begin; eval("x = )".encode("EUC-JP")); rescue Exception => e; p e.class, e.message, e.message.encoding; end
begin; eval("\n\ny = )".encode("Shift_JIS"), nil, "f.rb", 5); rescue Exception => e; p e.class, e.message, e.message.encoding; end
# A source that is US-ASCII and has other bytes in it is quoted as it
# is: the message is not valid in its encoding, and is still split into
# lines where the class name goes after the first.
begin
  eval("x = \"\xe3\x81\x82\" )".force_encoding("us-ascii"))
rescue SyntaxError => e
  p e.message, e.message.encoding, e.message.valid_encoding?, e.detailed_message
end
e = RuntimeError.new("a\nb\xff".force_encoding("us-ascii"))
p e.detailed_message, e.detailed_message(highlight: true)
# A SyntaxError is an exception like another once it is raised.
begin; eval("a = )", nil, "t.rb"); rescue ScriptError => e; p e.class.ancestors.take(3), e.full_message(highlight: false).lines.size; end
p((eval("a = )") rescue 1)) rescue p $!.class
"##;

#[test]
fn eval_syntax_error_messages() {
    let dir = Fixtures::new("eval_syntax");
    dir.write("ev.rb", EVAL_SYNTAX);
    compare("eval", &dir.0, SYNTAX_CRUBY, SYNTAX_MONO, &["ev.rb"], b"");
}

const REQUIRE_SYNTAX: &str = r##"
begin; require "./m1"; rescue SyntaxError => e; p e.message, e.path, e.backtrace, e.backtrace_locations&.size; end
begin; load "./m2.rb"; rescue SyntaxError => e; puts e.message; p e.path, e.backtrace; end
begin; load "./euc.rb"; rescue SyntaxError => e; p e.message, e.message.encoding, e.path.encoding; end
begin; require_relative "m2"; rescue SyntaxError => e; p e.message.lines.first, e.backtrace; end
def deep = require("./m1")
begin; deep; rescue SyntaxError => e; p e.backtrace; end
require "./m1"
"##;

/// The interpreter's own script, `-e`, standard input, and a file
/// reached by `require` / `load`: the report of the uncaught error,
/// what `-c` prints, and the exit status.
#[test]
fn script_syntax_error_reports() {
    let dir = Fixtures::new("script_syntax");
    dir.write("m1.rb", "a = 1; )\n");
    dir.write("m2.rb", "x = \ny = (1 +\n");
    // A source that is not UTF-8 is not quoted.
    dir.write(
        "euc.rb",
        b"# coding: euc-jp\nx = \"\xa4\xa2\"; \xa4\xa2 = )\n",
    );
    dir.write("r1.rb", REQUIRE_SYNTAX);
    dir.write("ascii.rb", b"# coding: us-ascii\nx = \"\xe3\x81\x82\"\n");
    // Regexp literals: at the top level, behind a `require`, and in a
    // method body (which is compiled with the file on CRuby and when
    // the `def` runs here: either way before anything is printed).
    dir.write("re1.rb", "x = 1\ny = /[/\n");
    dir.write("re2.rb", "require \"./re1\"\n");
    dir.write("re3.rb", "def f = /(?<a>x)\\k<b>/\np :defined\nf\n");
    for args in [
        &["m1.rb"][..],
        &["m2.rb"],
        &["euc.rb"],
        &["r1.rb"],
        &["ascii.rb"],
        &["re1.rb"],
        &["re2.rb"],
        &["re3.rb"],
        &["-e", "/[/"],
        &["-e", "a = ("],
        &["-e", "1 +\n2 +"],
        // The line a `-K` switch puts in front of the script is not
        // part of what is quoted.
        &["-Ke", "-e", "x = )"],
        &[
            "-Ke",
            "-e",
            "begin; eval('x = )'); rescue SyntaxError => e; p e.message, e.message.encoding; end",
        ],
        &["-w", "-e", "x = 1\ny = )"],
    ] {
        compare("script", &dir.0, SYNTAX_CRUBY, SYNTAX_MONO, args, b"");
    }
    compare(
        "stdin",
        &dir.0,
        SYNTAX_CRUBY,
        SYNTAX_MONO,
        &[],
        b"x = [1,\n",
    );

    // `-c`: nothing ran, so the report is the interpreter's
    // (`ruby: ` / `monoruby: `) rather than the script's.
    for (args, stdin) in [
        (&["-c", "m1.rb"][..], &b""[..]),
        (&["-c", "m2.rb"], b""),
        (&["-c", "re1.rb"], b""),
        (&["-c", "-e", "1 +"], b""),
        (&["-c"], b"x = [1,\n"),
    ] {
        let mut c = cruby(SYNTAX_CRUBY);
        c.args(args);
        let mut m = monoruby(SYNTAX_MONO);
        m.args(args);
        // CRuby names itself the way it was started.
        let ruby = format!("\n{}: ", monoruby::tests::ruby_path());
        let expected = run(c, &dir.0, stdin).replace(&ruby, "\nmonoruby: ");
        let actual = run(m, &dir.0, stdin);
        assert_same(&format!("-c ({args:?})"), &expected, &actual);
    }
}

// ---------------------------------------------------------------------
// #2238: did_you_mean and NameError#local_variables
// ---------------------------------------------------------------------

const DID_YOU_MEAN: &str = r##"
def t(n) = begin; p [n, yield]; rescue Exception; p [n, $!.class, $!.message]; end
begin
  foo_bar = 1
  foo_baz
rescue NameError => e
  t(:lv) { e.local_variables }
  t(:lv_same) { e.local_variables.equal?(e.local_variables) }
  t(:corr) { e.corrections }
  t(:dm) { e.detailed_message }
  t(:dmf) { e.detailed_message(highlight: false) }
  t(:dmt) { e.detailed_message(highlight: true) }
  t(:dmn) { e.detailed_message(did_you_mean: false) }
  t(:fm) { e.full_message(highlight: false).lines[1..] }
end
t(:new) { x = 1; NameError.new("x").local_variables }
t(:new_dm) { NameError.new("x").detailed_message }
t(:new_dm2) { NameError.new("x").detailed_message(highlight: false) }
t(:key_dm) { KeyError.new("x").detailed_message }
t(:load_dm) { LoadError.new("x").detailed_message }
t(:type_dm) { TypeError.new("x").detailed_message }
t(:rt_dm) { RuntimeError.new("x").detailed_message }
t(:nmpk_dm) { NoMatchingPatternKeyError.new("x").detailed_message }
def m(a, b = 2, *c, d:, **e, &f)
  zz = 1
  [1].each { |q| w = 2; return NameError.new("x").local_variables }
end
t(:blk) { m(1, d: 2) }
def m2(aa); bb = 1; aa.zork; end
t(:nm) { begin; m2(1); rescue NoMethodError => e; [e.local_variables, e.corrections]; end }
class Foo; def hello_world; end; end
t(:meth) { begin; Foo.new.hello_wrold; rescue NoMethodError => e; [e.corrections, e.detailed_message(highlight: false)]; end }
t(:key) { begin; {foo: 1}.fetch(:fooo); rescue KeyError => e; [e.corrections, e.detailed_message(highlight: false)]; end }
t(:sett) { begin; require "sett"; rescue LoadError => e; e.corrections; end }
t(:req) { begin; require "optpars"; rescue LoadError => e; [e.corrections, e.detailed_message(highlight: false)]; end }
t(:const) { begin; Strin; rescue NameError => e; [e.corrections, e.local_variables]; end }
t(:ivar) { @foo_bar = 1; begin; @foo_baz.zork; rescue NameError => e; [e.corrections]; end }
t(:case) { begin; case {foo: 1}; in {fooo:}; end; rescue NoMatchingPatternKeyError => e; [e.corrections, e.detailed_message(highlight: false)]; end }
t(:anc) { [NameError, KeyError, LoadError, NoMatchingPatternKeyError, NoMethodError, TypeError].map { |c| c.ancestors.take_while { |a| a != StandardError && a != ScriptError }.map(&:to_s) } }
t(:im) { [NameError, KeyError, NoMatchingPatternKeyError].map { |c| c.instance_methods(false).sort } }
t(:om) { NameError.new("zz").original_message }
t(:sc) { NameError.new("zz").spell_checker.class }
t(:ex) { e = NameError.new("q"); e2 = e.exception("r"); [e2.local_variables] }
t(:priv) { NameError.new("x").respond_to?(:local_variables) }
t(:arity) { NameError.instance_method(:local_variables).arity }
t(:eval) { eval("ev1 = 1; NameError.new('x').local_variables") }
t(:top) { NameError.new("x").local_variables.include?(:foo_bar) }
t(:raise_new) { v1 = 1; begin; raise NameError, "x"; rescue NameError => e; e.local_variables; end }
t(:raise_obj) { def mk; in_mk = 1; NameError.new("x"); end; v2 = 1; begin; raise mk; rescue NameError => e; e.local_variables; end }
t(:send) { v3 = 1; begin; send(:zork); rescue NameError => e; e.local_variables; end }
t(:mm) { v4 = 1; begin; Object.new.zork; rescue NameError => e; e.local_variables; end }
t(:cmiss) { v5 = 1; begin; Object.const_get(:Zork); rescue NameError => e; e.local_variables; end }
t(:ivg) { v6 = 1; begin; Object.new.instance_variable_get("zork"); rescue NameError => e; e.local_variables; end }
"##;

/// Which Ruby frame a NameError's `local_variables` are those of: the
/// one that failed for a missing method or variable, the one calling
/// it for an error built in place (CRuby's `name_err_init_attr`).
const LOCAL_VARIABLES: &str = r##"
def t(n) = begin; p [n, yield]; rescue Exception; p [n, $!.class, $!.message]; end
class Foo
  def cv; in_cv = 1; @@zork; end
  def co; in_co = 1; Zork; end
  def co2; in_co2 = 1; [1].each { |q| return Zork2 }; end
  def self.const_missing(n); in_cm = 1; super; end
  def co3; in_co3 = 1; Foo::Zork3; end
  def cg; in_cg = 1; Foo.const_get(:Zork4); end
  def lv; in_lv = 1; zork5; end
  def sc; in_sc = 1; ::Zork6; end
end
class Bar; def co; in_bar = 1; Zork7; end; end
def call(m, o = Foo) ; in_call = 1; o.new.send(m); end
%i[cv co co2 co3 cg lv sc].each do |m|
  t(m) { v = 1; begin; call(m); rescue NameError => e; [e.local_variables, e.backtrace[0][/in .*/]]; end }
end
t(:bar) { v = 1; begin; call(:co, Bar); rescue NameError => e; [e.local_variables, e.backtrace[0][/in .*/]]; end }
t(:direct) { v = 1; begin; Foo.new.co; rescue NameError => e; e.local_variables; end }
t(:direct_bar) { v = 1; begin; Bar.new.co; rescue NameError => e; e.local_variables; end }
t(:gv) { begin; raise NameError.new("a", :b); rescue NameError => e; e.local_variables; end }
t(:frozen) { e = NameError.new("x").freeze; e.local_variables }
t(:dup) { e = NameError.new("x"); a = e.local_variables; d = e.dup; [d.local_variables.equal?(a), e.instance_variables] }
t(:marshal) { e = NameError.new("x"); e.local_variables; Marshal.load(Marshal.dump(e)).local_variables }
t(:sub) { c = Class.new(NameError) { def initialize; end }; c.new.local_variables }
t(:sub2) { c = Class.new(NameError) { def initialize; sv = 1; super("z"); end }; c.new.local_variables }
t(:args) { NameError.new("x").local_variables(1) }
t(:alloc) { NameError.allocate.local_variables }
# `Marshal` writes the answer with the exception's other fields, once
# it has been asked for.
e = NameError.new("x", :nm); e.local_variables
p Marshal.dump(e)
e = (zork rescue $!); e.local_variables
p Marshal.dump(e).sub(/bt.*?\]/m, "BT")
e = (nil.zork rescue $!); e.local_variables
p Marshal.dump(e).sub(/bt.*?\]/m, "BT")
p Marshal.dump(NameError.new("x", :nm))
p Marshal.load(Marshal.dump(NameError.new("q"))).local_variables
p Marshal.load("\x04\bo:\x0ENameError\b:\tmesgI\"\x06x\x06:\x06ET:\abt0:\tname:\anm:\x14local_variables[\x06:\x06e").local_variables
# `initialize` and its relatives are private, in a builtin class as in
# any other.
p [NameError, KeyError, FrozenError, SystemExit, Exception, NoMethodError, IO, Thread, Array, String, Hash].map { |c|
  [c.private_instance_methods(false).include?(:initialize), c.public_instance_methods(false).include?(:initialize)]
}
p [Array, String, Hash].map { |c| c.private_method_defined?(:initialize_copy) }
p((NameError.new("x").initialize("y") rescue $!.class))
"##;

#[test]
fn did_you_mean_corrections() {
    let dir = Fixtures::new("did_you_mean");
    dir.write("d.rb", DID_YOU_MEAN);
    dir.write("l.rb", LOCAL_VARIABLES);
    dir.write("u.rb", "foo_bar = 1\np defined?(DidYouMean)\nfoo_baz\n");
    dir.write("k.rb", "h = {foo: 1, bar: 2}\nh.fetch(:fooo)\n");
    dir.write(
        "c.rb",
        "class Foo; def hello_world; end; end\nFoo.new.hello_wrold\n",
    );
    for args in [
        &["d.rb"][..],
        &["l.rb"],
        // The report of an uncaught exception.
        &["u.rb"],
        &["k.rb"],
        &["c.rb"],
        // Switched off: no corrections, and no `DidYouMean` at all.
        &["--disable=did_you_mean", "u.rb"],
        &["--disable-did_you_mean", "u.rb"],
        &[
            "--disable=did_you_mean",
            "-e",
            "p defined?(DidYouMean); foo_bar = 1; begin; foo_baz; rescue NameError => e; \
             p (e.corrections rescue $!.class), e.detailed_message(highlight: false), \
             e.local_variables; end",
        ],
        &[
            "--disable=gems",
            "-e",
            "p defined?(DidYouMean); p (NameError.new('x').corrections rescue $!.class)",
        ],
    ] {
        compare("did_you_mean", &dir.0, &[], &[], args, b"");
    }
}

/// CRuby requires did_you_mean at boot. monoruby only registers the
/// autoload: a program that never asks an exception for a correction
/// never loads it, and the first that does finds everything in place.
#[test]
fn did_you_mean_is_loaded_lazily() {
    let dir = Fixtures::new("did_you_mean_lazy");
    let loaded = "$LOADED_FEATURES.any? { |f| f.end_with?('/did_you_mean.rb') }";
    let out = run(
        {
            let mut m = monoruby(&[]);
            m.args([
                "-e",
                &format!(
                    "p {loaded}; p Object.autoload?(:DidYouMean); \
                     begin; raise NameError, 'x'; rescue NameError => e; e.message; end; \
                     begin; {{}}.fetch(:a); rescue KeyError => e; e.message; end; p {loaded}; \
                     p NameError.new('x').corrections; p {loaded}; \
                     p NameError.ancestors.include?(DidYouMean::Correctable); \
                     p NameError.instance_method(:corrections).owner"
                ),
            ]);
            m
        },
        &dir.0,
        b"",
    );
    assert_eq!(
        out,
        "exit: Some(0)\nfalse\n\"did_you_mean\"\nfalse\n[]\ntrue\ntrue\nDidYouMean::Correctable\n\
         --- stderr ---\n"
    );
    // Naming the constant loads it, as does prepending to one of the
    // classes it prepends to (the order of the ancestors is CRuby's).
    for code in [
        "DidYouMean::SpellChecker; p NameError.ancestors.first(2)",
        "m = Module.new; NameError.prepend(m); p NameError.ancestors.first(3).map(&:name)",
        "require 'did_you_mean'; p NameError.ancestors.first(2), KeyError.ancestors.first(2)",
    ] {
        compare("lazy load", &dir.0, &[], &[], &["-e", code], b"");
    }
}

// ---------------------------------------------------------------------
// #2235: a feature whose file name is not UTF-8
// ---------------------------------------------------------------------

/// The fixtures are files whose names are EUC-JP and Shift_JIS bytes,
/// which only a file system that takes any bytes for a name can hold:
/// macOS refuses them (and CRuby transcodes a path to UTF-8 there).
#[cfg(target_os = "linux")]
const NON_UTF8_FEATURES: &str = r##"
e = "\xC6\xFC\xCB\xDC.rb".dup.force_encoding("EUC-JP")
File.binwrite(e.b, "$x = __FILE__.encoding; $n = ($n || 0) + 1\n")
def t(n) = begin; p [n, yield]; rescue Exception; p [n, $!.class, $!.message.b, $!.message.encoding, (($!.path.b rescue nil) if LoadError === $!)]; end
t(:read)    { File.read(e).size }
t(:exist)   { File.exist?(e) }
t(:require) { [require("./".encode("EUC-JP") + e), $x] }
t(:require2) { [require("./".encode("EUC-JP") + e), $x, $n] }
t(:load)    { load e; [$x, $n] }
t(:load_b)  { load e.b; [$x, $n] }
t(:load_abs)  { load File.expand_path(e); [$x, $n] }
t(:feat) { $LOADED_FEATURES.select { |f| f.b.include?("\xC6\xFC".b) }.map { |f| [f.encoding, f.b.end_with?(e.b)] } }
t(:require_noext) { [require("./".encode("EUC-JP") + e.sub(/\.rb\z/, "")), $n] }
t(:require_b) { [require("./".b + e.b), $n] }
t(:require_rel) { [require_relative(e), $n] }
File.binwrite("\xC6\xFC9.rb".b, "Zz = __FILE__.encoding\n")
f = File.expand_path("\xC6\xFC9.rb".dup.force_encoding("EUC-JP"))
Object.autoload(:Zz, f)
t(:autoload_q) { a = Object.autoload?(:Zz); [a == f, a.bytes == f.bytes, a.encoding] }
t(:autoload_ref) { Zz }
t(:autoload_feat) { $LOADED_FEATURES.select { |x| x.b.include?("\xC6\xFC9".b) }.map { |x| x.encoding } }
t(:autoload_rel) { File.binwrite("\xC6\xFC8.rb".b, "Zy = 8\n"); Object.autoload(:Zy, "./".encode("EUC-JP") + "\xC6\xFC8".dup.force_encoding("EUC-JP")); [Object.autoload?(:Zy).encoding, Zy] }
t(:autoload_wide) { Object.autoload(:Zw, "/tmp/zw".encode("UTF-16LE")) }
t(:autoload_nul) { Object.autoload(:Zn, "/tmp/z\0w") }
t(:missing) { require("./".encode("EUC-JP") + "\xA4\xA2".dup.force_encoding("EUC-JP")) }
t(:missing_load) { load "\xA4\xA2".dup.force_encoding("EUC-JP") }
t(:missing_b) { require "./\xff\xfe".b }
t(:missing_u8) { require "./\u3042zz" }
t(:missing_load_u8) { load "\u3042zz" }
t(:lp) { $LOAD_PATH.unshift(Dir.pwd); File.binwrite("\xC6\xFC2.rb".b, "$y = 7\n"); [require("\xC6\xFC2".dup.force_encoding("EUC-JP")), $y] }
t(:sjis) { File.binwrite("\x93\xFA.rb".b, "$z = __FILE__.encoding\n"); load "\x93\xFA.rb".dup.force_encoding("Shift_JIS"); $z }
t(:wrap) { load e, true; $n }
t(:caller) { File.binwrite("\xC6\xFC3.rb".b, "$c = caller_locations(0,1)[0].path.then { [_1.encoding, _1.b] }\n$d = [__dir__.encoding, __FILE__.b]\n"); load "\xC6\xFC3.rb".dup.force_encoding("EUC-JP"); [$c, $d[0], $d[1]] }
t(:err) { File.binwrite("\xC6\xFC4.rb".b, "raise 'boom'\n"); begin; load "\xC6\xFC4.rb".dup.force_encoding("EUC-JP"); rescue => x; [x.backtrace[0].b, x.backtrace[0].encoding]; end }
t(:syntax) { File.binwrite("\xC6\xFC5.rb".b, "1 +\n"); begin; load "\xC6\xFC5.rb".dup.force_encoding("EUC-JP"); rescue SyntaxError => x; [x.message.b.lines[0], x.path&.encoding, x.path&.b]; end }
t(:wide) { require "/tmp".encode("UTF-16LE") }
t(:wide_load) { load "/tmp".encode("UTF-16LE") }
t(:wide_rr) { require_relative "tmp".encode("UTF-16LE") }
t(:nul) { require "a\0b" }
t(:nul_load) { load "a\0b" }
"##;

/// What `load` / `require` / `autoload` make of a name that is no
/// path: an encoding that is not ASCII-compatible, a NUL, an object
/// that does not convert.
#[cfg(target_os = "linux")]
const FEATURE_NAME_CHECKS: &str = r##"
def t(n) = begin; p [n, yield]; rescue Exception; p [n, $!.class, $!.message.b]; end
%w[tmp ./tmp /tmp ~/tmp].each do |s|
  %w[UTF-16LE UTF-16BE UTF-32LE UTF-16 ].each do |e|
    t([s, e, :load]) { load s.encode(e) }
    t([s, e, :req]) { require s.encode(e) }
  end
end
o = Object.new; def o.to_path = "/tmp".encode("UTF-16LE")
t(:to_path_load) { load o }
t(:to_path_req) { require o }
t(:int) { load 1 }
t(:int_r) { require 1 }
t(:nil_r) { require nil }
t(:rr_int) { require_relative 1 }
o2 = Object.new; def o2.to_path = 1
t(:to_path_int) { load o2 }
t(:to_path_int_r) { require o2 }
o3 = Object.new; def o3.to_str = "zzznone"
t(:to_str) { load o3 }
t(:to_str_r) { require o3 }
t(:empty_load) { load "" }
t(:empty_req) { require "" }
t(:nul_rr) { require_relative "a\0b" }
t(:nul_wide) { load "a\0b".encode("UTF-16LE") }
t(:dummy) { load "abc".dup.force_encoding("UTF-7") }
t(:dummy_r) { require "abc".dup.force_encoding("UTF-7") }
t(:al_int) { Object.autoload(:Qq, 1) }
t(:al_empty) { Object.autoload(:Qq, "") }
t(:al_to_path) { o4 = Object.new; def o4.to_path = "/nonexistent/qq"; Object.autoload(:Qq, o4); Object.autoload?(:Qq) }
# A path in an encoding that is not ASCII-compatible, wherever a path
# is taken.
w = "/tmp".encode("UTF-16LE")
t(1) { File.open(w) }
t(2) { File.exist?(w) }
t(3) { Dir.chdir(w) }
t(4) { File.stat(w) }
t(5) { File.read(w) }
t(6) { Dir.entries(w) }
t(7) { File.expand_path(w) }
t(8) { File.basename(w) }
t(11) { File.join(w, "a") }
t(12) { IO.popen(["pwd", chdir: w]).read }
t(13) { system("echo", "q", out: w) }
t(14) { system("echo", "q", out: [w, "w"]) }
t(15) { File.open("/tmp".encode("ISO-2022-JP")) }
t(16) { File.exist?("/tmp\0".encode("UTF-16LE")) }
t(18) { File.directory?("/tmp".force_encoding("UTF-16")) }
"##;

#[cfg(target_os = "linux")]
#[test]
fn features_with_names_that_are_not_utf8() {
    let dir = Fixtures::new("non_utf8_features");
    dir.write("t.rb", NON_UTF8_FEATURES);
    dir.write("l.rb", FEATURE_NAME_CHECKS);
    let gems_off = &["--disable=gems"];
    compare(
        "non-UTF-8 names",
        &dir.0,
        gems_off,
        gems_off,
        &["t.rb"],
        b"",
    );
    compare("name checks", &dir.0, gems_off, gems_off, &["l.rb"], b"");
}

// ---------------------------------------------------------------------
// #2231: process arguments in an encoding that is not ASCII-compatible
// ---------------------------------------------------------------------

#[cfg(target_os = "linux")]
const WIDE_ARGUMENTS: &str = r##"
def t(label)
  r = yield
  r = :pid if Integer === r
  puts "#{label}: #{r.inspect}"
rescue Exception => e
  puts "#{label}: #{e.class}: #{e.message}"
end
$stdout.sync = true
W = %w[UTF-16LE UTF-16BE UTF-32LE UTF-32BE]
W.each do |enc|
  puts "== #{enc}"
  t("system") { system("echo", "ab".encode(enc)) }
  t("system, a real NUL") { system("echo", "a\0b".encode(enc)) }
  t("system, one string") { system("echo ab".encode(enc)) }
  t("spawn") { Process.wait spawn("echo", "ab".encode(enc)); $?.exitstatus }
  t("IO.popen") { IO.popen(["echo", "ab".encode(enc)]).read }
  t("backtick") { `#{"echo ab".encode(enc)}` rescue $!.class }
  t("prog wide") { system("echo".encode(enc), "x") }
  t("prog wide argv0") { system(["echo".encode(enc), "zz"], "x") }
  t("argv0 wide") { system(["echo", "zz".encode(enc)], "x") }
  t("printf") { IO.popen(["printf", "[%s]", "ab".encode(enc), "c"]).read }
  t("odd") { IO.popen(["printf", "[%s]", "ab".encode(enc).b[0, 3].force_encoding(enc), "c"]).read }
  t("nul unaligned") { IO.popen(["printf", "[%s]", "\x01\x00\x00\x01\x01\x00\x00\x01".dup.force_encoding(enc), "c"]).read }
  t("empty") { IO.popen(["printf", "[%s]", "".encode(enc), "c"]).read }
  t("exec") { pid = fork { exec("printf", "[%s]", "ab".encode(enc)) }; Process.wait pid; puts; $?.exitstatus }
  t("env val") { IO.popen([{"X" => "ab".encode(enc)}, "sh", "-c", 'printf "[%s]" "$X"']).read }
  t("env key") { IO.popen([{"X".encode(enc) => "ab"}, "sh", "-c", 'printf "[%s]" "$X"']).read }
  t("chdir") { IO.popen(["pwd", chdir: "/tmp".encode(enc)]).read }
  t("redirect") { system("echo", "q", out: "/tmp/qq".encode(enc)) }
end
t("binary nul") { system("echo", "a\0b".b) }
t("utf8 nul") { system("echo", "a\0b") }
t("sjis nul") { system("echo", "a\0b".encode("Shift_JIS")) }
t("dummy utf16") { IO.popen(["printf", "[%s]", "ab".encode("UTF-16LE").force_encoding("UTF-16"), "c"]).read }
t("iso2022") { IO.popen(["printf", "[%s]", "ab".encode("ISO-2022-JP"), "c"]).read }
"##;

#[cfg(target_os = "linux")]
#[test]
fn process_arguments_in_wide_encodings() {
    let dir = Fixtures::new("wide_arguments");
    dir.write("w.rb", WIDE_ARGUMENTS);
    let gems_off = &["--disable=gems"];
    compare("wide arguments", &dir.0, gems_off, gems_off, &["w.rb"], b"");
}

// ---------------------------------------------------------------------
// #2220: lines of a wide stream, through ARGF and `Kernel#gets`
// ---------------------------------------------------------------------

/// The streams an in-process test cannot replace: the rest of #2220 is
/// in `wide_lines_nil_conversions.rs`.
#[test]
fn wide_lines_from_argf_and_stdin() {
    let dir = Fixtures::new("wide_argf");
    dir.write("w.txt", b"a\x00\r\x00\n\x00b\x00\n\x00\n\x00c\x00");
    dir.write("e.txt", b"\xa4\xa2x\xa4\xa4y\n");
    dir.write(
        "argf.rb",
        "ARGF.binmode; ARGF.set_encoding('utf-16le')\n\
         p ARGF.readlines.map(&:b), ARGF.lineno\n",
    );
    dir.write(
        "argf2.rb",
        "ARGF.binmode; ARGF.set_encoding('utf-16le')\n\
         p ARGF.external_encoding, ARGF.binmode?\n\
         p ARGF.gets(chomp: false).b, ARGF.readline(chomp: true).b, ARGF.each_line.map(&:b)\n",
    );
    dir.write(
        "stdin.rb",
        "$stdin = File.open('e.txt', 'r:euc-jp'); $/ = \"\u{3044}\"\n\
         p((gets rescue [$!.class, $!.message]))\n\
         $/ = $/.encode('euc-jp'); p gets.b, $_.encoding, gets.b, gets\n",
    );
    // (`ARGF.gets` with no argument is left out: CRuby's `argf_getline`
    // then skips the conversion of the default separator and raises
    // "invalid byte sequence in UTF-16LE" on a valid stream.)
    //
    // Without the mode the encoding is refused where it is set, and
    // is not what the next file is read in.
    dir.write(
        "argf3.rb",
        "p((ARGF.set_encoding('utf-16le') rescue $!.message))\n\
         p ARGF.external_encoding, ARGF.gets.b, ARGF.file.external_encoding\n\
         ARGF.skip; p ARGF.gets, ARGF.file.external_encoding\n\
         ARGF.read; p((ARGF.set_encoding('utf-8') rescue $!.message))\n",
    );
    let gems_off = &["--disable=gems"];
    for args in [
        &["argf.rb", "w.txt"][..],
        &["argf.rb", "w.txt", "w.txt"],
        &["argf2.rb", "w.txt"],
        &["argf3.rb", "w.txt", "e.txt"],
        &["stdin.rb"],
    ] {
        compare("wide lines", &dir.0, gems_off, gems_off, args, b"");
    }
    // Standard input itself, read as UTF-16LE.
    compare(
        "wide stdin",
        &dir.0,
        gems_off,
        gems_off,
        &[
            "-e",
            "$stdin.set_encoding('utf-16le'); $stdin.binmode; p $stdin.readlines.map(&:b), \
             $stdin.lineno",
        ],
        b"a\x00\n\x00b\x00\n\x00",
    );
}
