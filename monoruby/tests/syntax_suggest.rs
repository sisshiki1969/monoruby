//! syntax_suggest (#2245): the block CRuby puts in front of the report
//! of a *file* that does not parse — "Unmatched keyword, missing `end'
//! ?" and the lines concerned.
//!
//! CRuby's gem_prelude requires `syntax_suggest/core_ext` at boot,
//! which prepends an anonymous module to SyntaxError; its
//! `detailed_message` loads the library proper the first time it is
//! called. monoruby vendors the library unmodified and registers
//! `SyntaxSuggest` as an autoload of that file instead
//! (`builtins/gem_prelude.rb`), run by the first SyntaxError that is
//! asked for its message (`Exception#detailed_message`) and by the
//! first `prepend` to SyntaxError.
//!
//! None of it is visible to an in-process `run_test`, which runs with
//! gems off and compiles from a string. The expectations are
//! differential: each case runs the same script on the reference
//! CRuby, in the same directory.

use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

fn apply(cmd: &mut Command) {
    cmd.env_remove("RUBYOPT")
        .env_remove("RUBYLIB")
        .env_remove("RUBYPATH")
        .env_remove("NO_COLOR")
        .env_remove("SYNTAX_SUGGEST_DEBUG")
        .env_remove("SYNTAX_SUGGEST_DISABLE_PRISM")
        .env_remove("DEBUG")
        // The library gives its search one second and says "Search
        // timed out" in place of its diagnosis after that; a debug
        // build under a loaded test runner must not get there.
        .env("SYNTAX_SUGGEST_TIMEOUT", "120");
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

/// Run `args` on both Rubies in `dir` and require the same exit
/// status, stdout and stderr. Returns what they printed.
fn compare(what: &str, dir: &Path, args: &[&str], stdin: &[u8]) -> String {
    let mut c = cruby(&[]);
    c.args(args);
    let mut m = monoruby(&[]);
    m.args(args);
    let expected = run(c, dir, stdin);
    let actual = run(m, dir, stdin);
    assert_same(&format!("{what} ({args:?})"), &expected, &actual);
    actual
}

/// A private directory for one test's fixtures.
struct Fixtures(PathBuf);

impl Fixtures {
    fn new(name: &str) -> Self {
        let dir = std::env::temp_dir().join(format!("mr_suggest_{name}_{}", std::process::id()));
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

/// The issue's example: a `def` whose `if` is never closed.
const MISSING_END: &str = "def foo\n  if x\n    1\nend\n";

/// Sources that do not parse, one per kind of diagnosis the library
/// makes (a missing `end`, a missing keyword, each unmatched bracket,
/// and the ones it has nothing to add to), on which prism 2.x
/// (monoruby) and prism 1.8 (CRuby 4.0) report the same errors.
const SOURCES: &[(&str, &str)] = &[
    ("missing_end", MISSING_END),
    (
        "missing_do_end",
        "class Foo\n  def a\n    1\n  end\n\n  def b\n    [1,2].map do |x|\n      x\n  end\nend\n",
    ),
    ("extra_end", "def a\nend\nend\n"),
    (
        "nested",
        "def a\n  1\nend\n\nclass B\n  def c\n    if x\n  end\nend\n",
    ),
    ("brace", "h = {\n  a: 1,\n  b: [1, 2\n}\nputs h\n"),
    ("pipe", "[1].each do |x\n  p x\nend\n"),
    ("paren", "foo(1,\n  2\n\nbar(3\n"),
    ("curly_block", "foo {\n  bar do\n    1\n  }\n"),
    ("operator", "a = 1 +\n\nb = 2 *\n"),
    ("heredoc", "x = <<~EOS\n  abc\nputs 1\n"),
    ("string", "puts \"abc\nputs 1\n"),
    ("rescue", "begin\n  1\nrescue => e\n  2\nensure\n"),
    ("case", "case x\nwhen 1\n  2\n"),
    ("tabs", "def a\n\tif x\n\t\t1\nend\n"),
    ("utf8", "def あ\n  puts \"い\"\n  [1, 2\nend\n"),
    // Not a matter of pairing: the library finds nothing to point at.
    ("return_in_class", "class A; return; end\n"),
    ("duplicate_argument", "def f(a, a); end\n"),
];

/// A longer source: methods around the one that lacks its `end`, so
/// that the search has blocks to expand and to discard.
fn long_source() -> String {
    let mut s = String::from("# frozen_string_literal: true\n\nmodule Outer\n  class Inner\n");
    for i in 0..40 {
        s += &format!(
            "    # Method number {i}.\n    def m{i}(a, b = {i})\n      if a > b\n        \
             [a, b].each do |x|\n          puts x\n        end\n      else\n        \
             {{ a: a, b: b }}\n      end\n"
        );
        // The twentieth method is never closed.
        if i != 20 {
            s += "    end\n";
        }
        s += "\n";
    }
    s += "  end\nend\n";
    s
}

/// The interpreter's own script: the whole report, on standard error,
/// and the exit status.
#[test]
fn report_of_a_script_that_does_not_parse() {
    let dir = Fixtures::new("script");
    for (name, src) in SOURCES {
        dir.write(&format!("{name}.rb"), src);
    }
    dir.write("long.rb", long_source());
    let shown = compare("script", &dir.0, &["missing_end.rb"], b"");
    // What the issue asked for, spelled out once.
    assert!(
        shown.contains(
            "missing_end.rb: --> missing_end.rb\n\n\
             Unmatched keyword, missing `end' ?\n\n\
             > 1  def foo\n> 2    if x\n> 4  end\n\n\
             missing_end.rb:4: syntax errors found (SyntaxError)\n"
        ),
        "{shown}"
    );
    for name in [
        "missing_do_end",
        "extra_end",
        "brace",
        "pipe",
        "utf8",
        "duplicate_argument",
        "long",
    ] {
        compare("script", &dir.0, &[&format!("{name}.rb")], b"");
    }
    // Named by another path, the file is shown under that one.
    compare("script", &dir.0, &["./missing_end.rb"], b"");
    let abs = dir.0.join("missing_end.rb");
    compare("script", &dir.0, &[abs.to_str().unwrap()], b"");
    // A script that is not a file has no block: `-e`, standard input.
    compare("-e", &dir.0, &["-e", "def foo\n  if x\nend"], b"");
    compare("stdin", &dir.0, &[], MISSING_END.as_bytes());
}

const MESSAGES: &str = r##"
def show(e)
  puts e.detailed_message(highlight: false).gsub(Dir.pwd, "PWD")
rescue Exception => err
  p [:raised, err.class, err.message]
end
# Every kind of source, through `load`.
Dir["src/*.rb"].sort.each do |f|
  puts "== #{f}"
  begin
    load f
    puts "loaded"
  rescue SyntaxError => e
    p e.path
    show e
  end
end
# `require` / `require_relative` name the file by its full path.
begin
  require "./src/missing_end"
rescue SyntaxError => e
  p e.path == File.expand_path("src/missing_end.rb")
  puts e.message.gsub(Dir.pwd, "PWD")
  show e
  # The keywords: `syntax_suggest: false` leaves the block out,
  # `highlight:` (true when not given) is handed on to the library and
  # to the message, anything else goes on to `Exception`.
  puts e.detailed_message(highlight: false, syntax_suggest: false).gsub(Dir.pwd, "PWD")
  p e.detailed_message(highlight: true).gsub(Dir.pwd, "PWD")
  p e.detailed_message.gsub(Dir.pwd, "PWD")
  p e.detailed_message(highlight: false, other: 1).gsub(Dir.pwd, "PWD") == e.detailed_message(highlight: false).gsub(Dir.pwd, "PWD")
  puts e.full_message(highlight: false).gsub(Dir.pwd, "PWD")
  puts e.full_message(highlight: false, order: :bottom).gsub(Dir.pwd, "PWD")
  p e.full_message(highlight: true).gsub(Dir.pwd, "PWD")
end
begin
  require_relative "src/brace"
rescue SyntaxError => e
  show e
end
# `eval` under a file's name reads that file; under its own name (or
# one that does not exist) there is nothing to read and no block.
begin; eval("def foo\n  if x\n    1\nend\n", nil, "src/missing_end.rb"); rescue SyntaxError => e; p e.path; show e; end
begin; eval("def foo\n  if x\n    1\nend\n"); rescue SyntaxError => e; p e.path; show e; end
begin; eval("def foo\n  if x\n    1\nend\n", nil, "nowhere.rb"); rescue SyntaxError => e; p e.path; show e; end
begin; eval("x = )", nil, "src"); rescue SyntaxError => e; p e.path; show e; end
# A SyntaxError that was made rather than raised by a parse.
e = SyntaxError.new("x")
p e.path, e.detailed_message, e.detailed_message(highlight: false), e.detailed_message(highlight: true)
p e.detailed_message(syntax_suggest: false), e.detailed_message(foo: 1)
p (e.detailed_message(highlight: 1) rescue $!.class)
p e.full_message(highlight: false).sub(/\A.*?: /, "")
p SyntaxError.new("frozen").freeze.detailed_message(highlight: false)
p SyntaxError.new("").detailed_message(highlight: false)
# ... and whose `path` is its own.
class Sub < SyntaxError; def path = "src/extra_end.rb"; end
show Sub.new("boo")
e = SyntaxError.new("singleton")
def e.path = "src/pipe.rb"
show e
class Odd < SyntaxError; def path = 1; end
show Odd.new("odd")
# No message of its own: "compile error" (#2251).
p SyntaxError.new.message, SyntaxError.exception.message, Class.new(SyntaxError).new.message
m = SyntaxError.new.message
p m.frozen?, m.encoding, m.equal?(SyntaxError.new.message), SyntaxError.new(nil).message.frozen?
p SyntaxError.new(nil).message, SyntaxError.new(3).message, SyntaxError.new("a").message
p SyntaxError.instance_method(:initialize).owner
p SyntaxError.new.detailed_message(highlight: false)
p (SyntaxError.new("a", "b") rescue $!.message)
p (SyntaxError.new("a", path: "x") rescue $!.message)
begin; raise SyntaxError; rescue SyntaxError => e; p e.message, e.path; end
begin; raise SyntaxError, "given"; rescue SyntaxError => e; p e.message; end
# The state CRuby has from boot, reached by now.
anc = SyntaxError.ancestors
p anc.take(2).map(&:class), anc[0].name, anc[0].instance_methods(false)
p SyntaxError.instance_method(:detailed_message).owner == anc[0]
p SyntaxError.instance_method(:detailed_message).parameters
p SyntaxSuggest.singleton_methods.include?(:module_for_detailed_message)
p defined?(SyntaxSuggest::DEFAULT_VALUE), SyntaxSuggest::VERSION
# The library's own interface.
require "syntax_suggest"
require "syntax_suggest/api"
require "stringio"
p SyntaxSuggest.valid?("def a; end"), SyntaxSuggest.invalid?("def a"), SyntaxSuggest.valid?(["def a\n", "end\n"])
io = StringIO.new
SyntaxSuggest.call(io: io, source: "def a\n  [1\nend\n", filename: "x.rb", terminal: false)
puts io.string
io = StringIO.new
SyntaxSuggest.call(io: io, source: "class A\n  def b\n    1\n\n  def c\n  end\nend\n", terminal: true)
p io.string
begin
  load "src/nested.rb"
rescue SyntaxError => e
  io = StringIO.new
  SyntaxSuggest.handle_error(e, re_raise: false, io: io)
  puts io.string
  # It raises the error again unless told not to.
  begin
    SyntaxSuggest.handle_error(e, io: StringIO.new)
  rescue SyntaxError => again
    p again.equal?(e)
  end
end
"##;

/// `SyntaxError#detailed_message` and `#full_message` in a running
/// program: every kind of source through `load`, the files `require` /
/// `eval` name, the method's keywords, SyntaxErrors made by hand, and
/// the library called directly.
#[test]
fn messages_of_a_syntax_error() {
    let dir = Fixtures::new("messages");
    std::fs::create_dir_all(dir.0.join("src")).unwrap();
    for (name, src) in SOURCES {
        dir.write(&format!("src/{name}.rb"), src);
    }
    dir.write("src/long.rb", long_source());
    dir.write("src/fine.rb", "X = 1\n");
    dir.write("m.rb", MESSAGES);
    compare("messages", &dir.0, &["m.rb"], b"");
}

/// A file reached by `require` from a method, by `load` in a thread
/// that reports its exception, and under a `rescue` that re-raises:
/// the report of the uncaught error with its backtrace.
#[test]
fn reports_with_a_backtrace() {
    let dir = Fixtures::new("backtrace");
    dir.write("bad.rb", MISSING_END);
    dir.write("req.rb", "def run\n  require_relative \"bad\"\nend\nrun\n");
    dir.write(
        "thr.rb",
        "t = Thread.new { load \"bad.rb\" }\n\
         t.report_on_exception = true\n\
         begin; t.join; rescue SyntaxError => e; puts \"joined: #{e.class}\"; end\n",
    );
    dir.write(
        "cause.rb",
        "begin\n  load \"bad.rb\"\nrescue SyntaxError\n  raise \"wrapped\"\nend\n",
    );
    for script in ["req.rb", "cause.rb"] {
        compare("backtrace", &dir.0, &[script], b"");
    }
    // The thread's report names the Thread object by its address.
    let strip = |s: String| {
        let (head, tail) = s.split_once("#<Thread:0x").expect("thread report");
        format!("{head}{}", tail.split_once(' ').unwrap().1)
    };
    let mut c = cruby(&[]);
    c.arg("thr.rb");
    let mut m = monoruby(&[]);
    m.arg("thr.rb");
    assert_same(
        "thread report",
        &strip(run(c, &dir.0, b"")),
        &strip(run(m, &dir.0, b"")),
    );
}

/// Modules prepended to SyntaxError by a program sit *above* the
/// library's, which CRuby has had there since boot: the first prepend
/// brings the library in underneath.
#[test]
fn prepend_goes_above_the_library() {
    let dir = Fixtures::new("prepend");
    dir.write("bad.rb", MISSING_END);
    dir.write(
        "p.rb",
        r##"
module M; def detailed_message(**kw) = "M[" + super + "]"; end
SyntaxError.prepend(M)
anc = SyntaxError.ancestors.take(3)
p anc.map { |m| m.name || m.class }
# core_ext alone: the library proper waits for a message.
p defined?(SyntaxSuggest::DEFAULT_VALUE)
begin; load "bad.rb"; rescue SyntaxError => e; puts e.detailed_message(highlight: false); end
p defined?(SyntaxSuggest::DEFAULT_VALUE)
module N; end
SyntaxError.prepend(N)
p SyntaxError.ancestors.take(4).map { |m| m.name || m.class }
load "bad.rb"
"##,
    );
    compare("prepend", &dir.0, &["p.rb"], b"");
    // A subclass's own modules, and a singleton method, are above
    // SyntaxError in any case; the library is loaded underneath them by
    // the message.
    dir.write(
        "s.rb",
        r##"
class Sub < SyntaxError
  prepend(Module.new { def detailed_message(**kw) = "sub[" + super + "]" })
  def path = "bad.rb"
end
puts Sub.new("boo").detailed_message(highlight: false)
p Sub.ancestors.take(4).map(&:class)
e = SyntaxError.new("x")
def e.detailed_message(**kw) = "single[" + super + "]"
p e.detailed_message(highlight: false), e.detailed_message
"##,
    );
    compare("subclass", &dir.0, &["s.rb"], b"");
}

/// `--disable-syntax_suggest`, `--disable-gems` and `--disable=all`
/// leave `SyntaxSuggest` undefined and the report without its block;
/// the library can still be required by name.
#[test]
fn feature_switch() {
    let dir = Fixtures::new("switch");
    dir.write("bad.rb", MISSING_END);
    let probe = "p defined?(SyntaxSuggest), SyntaxError.ancestors.take(2).map(&:class)";
    for flags in [
        &["--disable-syntax_suggest"][..],
        &["--disable=syntax_suggest"],
        &["--disable-gems"],
        &["--disable=all"],
        &["--disable=all", "--enable=syntax_suggest"],
        &["--disable=all", "--enable=gems"],
        &["--disable-syntax_suggest", "--enable-syntax_suggest"],
        &["--enable=all"],
        &["--disable-did_you_mean", "--disable-error_highlight"],
    ] {
        let mut args = flags.to_vec();
        args.push("bad.rb");
        compare("switch", &dir.0, &args, b"");
        // (A message first: monoruby has not loaded the library until
        // one is asked for, see `loaded_by_the_first_message`.)
        let code = format!("SyntaxError.new('x').detailed_message; {probe}");
        let mut args = flags.to_vec();
        args.extend(["-e", &code]);
        compare("switch", &dir.0, &args, b"");
    }
    compare(
        "required by name",
        &dir.0,
        &[
            "--disable-syntax_suggest",
            "-e",
            "p defined?(SyntaxSuggest); require 'syntax_suggest'; \
             p SyntaxError.ancestors[0].class; load 'bad.rb'",
        ],
        b"",
    );
    // What both say about the constant itself, loaded or not.
    compare(
        "constant",
        &dir.0,
        &[
            "-e",
            "p defined?(SyntaxSuggest), Object.constants.include?(:SyntaxSuggest), \
             Object.const_defined?(:SyntaxSuggest); p SyntaxSuggest.class, SyntaxSuggest.constants",
        ],
        b"",
    );
}

/// monoruby's own arrangement: nothing of the library is read until a
/// SyntaxError is asked for its message.
#[test]
fn loaded_by_the_first_message() {
    let dir = Fixtures::new("lazy");
    let code = r##"
loaded = -> { $LOADED_FEATURES.grep(/syntax_suggest/).map { |f| f[/syntax_suggest.*/] } }
p Object.autoload?(:SyntaxSuggest), loaded.()
e = begin; eval("x = )"); rescue SyntaxError => err; err; end
p e.class, e.message.empty?, e.path.class
p Object.autoload?(:SyntaxSuggest), loaded.(), SyntaxError.ancestors[0]
# Another exception's message does not load it.
RuntimeError.new("x").detailed_message
p Object.autoload?(:SyntaxSuggest)
e.detailed_message
p Object.autoload?(:SyntaxSuggest), loaded.().include?("syntax_suggest/core_ext.rb")
p SyntaxError.ancestors[0].class, defined?(SyntaxSuggest::DEFAULT_VALUE)
"##;
    let out = run(monoruby(&["-e", code]), &dir.0, b"");
    assert_eq!(
        out,
        "exit: Some(0)\n\
         \"syntax_suggest/core_ext\"\n[]\n\
         SyntaxError\nfalse\nString\n\
         \"syntax_suggest/core_ext\"\n[]\nSyntaxError\n\
         \"syntax_suggest/core_ext\"\n\
         nil\ntrue\n\
         Module\n\"constant\"\n\
         --- stderr ---\n"
    );
    // A reference to the constant loads it too, as any autoload.
    let out = run(
        monoruby(&[
            "-e",
            "SyntaxSuggest; p Object.autoload?(:SyntaxSuggest), SyntaxError.ancestors[0].class",
        ]),
        &dir.0,
        b"",
    );
    assert_eq!(out, "exit: Some(0)\nnil\nModule\n--- stderr ---\n");
}
