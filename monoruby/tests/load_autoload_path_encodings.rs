//! Three things CRuby decides where it turns a file name into running
//! code, all of them invisible to an in-process `run_test`:
//!
//! - which file `Kernel#load` runs for a name and what path that file
//!   then reports (#2194): a name that is absolute or starts with `./`
//!   / `../` is expanded, any other is searched for on `$LOAD_PATH`
//!   and, failing that, read relative to the working directory under
//!   the name *as it was written*;
//! - what the other constants registered onto a file look like while an
//!   autoload is running that file, and where the "Expected … to define
//!   …" warning says it happened (#2128);
//! - the encoding of the Strings a source path ends up in — `__FILE__`,
//!   `__dir__`, a `Location`'s path and label, `$0`, `autoload?`
//!   (#2096).
//!
//! Everything here spawns the real binary, in a directory of its own:
//! the working directory, the locale and `$0` are process-level. The
//! expectations are differential — each case asks the reference CRuby
//! what it answers in the same directory under the same environment —
//! because the answers carry temporary paths and, for the encodings,
//! follow the locale.

use std::io::Write;
use std::process::{Command, Stdio};

/// `(label, LC_ALL)`: one locale whose encoding is US-ASCII and one
/// whose encoding is UTF-8. Both `LANG` and `LC_CTYPE` are cleared.
const LOCALES: &[(&str, &str)] = &[("LC_ALL=C", "C"), ("LC_ALL=C.UTF-8", "C.UTF-8")];

fn apply(cmd: &mut Command, locale: Option<&str>) {
    cmd.env_remove("RUBYOPT")
        .env_remove("RUBYLIB")
        .env_remove("RUBYPATH");
    if let Some(locale) = locale {
        cmd.env_remove("LANG")
            .env_remove("LC_CTYPE")
            .env("LC_ALL", locale);
    }
}

fn monoruby(locale: Option<&str>) -> Command {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_monoruby"));
    apply(&mut cmd, locale);
    cmd.arg("--disable=gems");
    cmd
}

fn cruby(locale: Option<&str>) -> Command {
    let mut cmd = Command::new(monoruby::tests::ruby_path());
    apply(&mut cmd, locale);
    cmd.arg("--disable=gems,rubyopt");
    cmd
}

/// stdout followed by stderr of `cmd` run in `dir`, with `stdin` (when
/// there is one) as its standard input.
fn run(mut cmd: Command, dir: &std::path::Path, stdin: Option<&str>) -> String {
    cmd.current_dir(dir)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    let mut child = cmd.spawn().expect("failed to spawn");
    child
        .stdin
        .take()
        .unwrap()
        .write_all(stdin.unwrap_or_default().as_bytes())
        .expect("write stdin");
    let out = child.wait_with_output().expect("wait");
    assert!(
        out.status.success(),
        "{:?} exited with {:?}\nstderr: {}",
        cmd,
        out.status,
        String::from_utf8_lossy(&out.stderr)
    );
    format!(
        "{}--- stderr ---\n{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    )
}

/// Run `args` on both Rubies in `dir` and require the same output.
fn compare(what: &str, dir: &std::path::Path, locale: Option<&str>, args: &[&str]) {
    compare_stdin(what, dir, locale, args, None)
}

fn compare_stdin(
    what: &str,
    dir: &std::path::Path,
    locale: Option<&str>,
    args: &[&str],
    stdin: Option<&str>,
) {
    let mut c = cruby(locale);
    c.args(args);
    let mut m = monoruby(locale);
    m.args(args);
    let expected = run(c, dir, stdin);
    let actual = run(m, dir, stdin);
    if expected != actual {
        let first_diff = expected
            .lines()
            .zip(actual.lines())
            .find(|(e, a)| e != a)
            .map(|(e, a)| format!("\n  CRuby:    {e}\n  monoruby: {a}"))
            .unwrap_or_else(|| "\n  (one output is a prefix of the other)".to_string());
        panic!(
            "{what} ({locale:?}, {args:?}): first differing line:{first_diff}\n\
             --- CRuby ---\n{expected}\n--- monoruby ---\n{actual}"
        );
    }
}

/// A private directory for one test's fixtures: the tests run in
/// parallel, and each compares what it reads back from its own files.
fn fixture_dir(name: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir().join(format!("mr_srcpath_{name}_{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("create fixture dir");
    dir
}

fn write(dir: &std::path::Path, name: &str, body: &str) {
    let path = dir.join(name);
    std::fs::create_dir_all(path.parent().unwrap()).expect("create fixture subdirectory");
    std::fs::write(&path, body).expect("write fixture");
}

// ---------------------------------------------------------------------
// #2194: the file `load` runs, and the path it runs it under
// ---------------------------------------------------------------------

/// What a loaded file can say about where it is.
const WHERE: &str = r##"p [__FILE__, __dir__, caller_locations(0, 1)[0].path, caller_locations(0, 1)[0].absolute_path, (raise "x" rescue $!.backtrace[0])]
"##;

const LOAD_MAIN: &str = r##"
def t(*a)
  print a.inspect, " => "
  load(*a)
rescue LoadError => e
  p [e.class, e.message, e.path]
end
d = Dir.pwd
# Explicitly relative, and absolute: expanded and normalized.
t "./f.rb"
t "./sub//g.rb"
t "./sub/../f.rb"
t "../#{File.basename(d)}/f.rb"
t "#{d}/./f.rb"
t "#{d}/sub/../f.rb"
t "#{d}//f.rb"
# Neither, and not on $LOAD_PATH: relative to the working directory,
# under the name as written.
t "f.rb"
t "sub/g.rb"
t "sub//g.rb"
t "sub/../f.rb"
# A symbolic link is not resolved in the path, only in `__dir__` and
# `absolute_path`.
t "fl.rb"
t "./fl.rb"
t "lnk/h.rb"
t "./lnk/h.rb"
# On $LOAD_PATH: the entry's real path, the name normalized under it.
$:.unshift "./sub"
t "g.rb"
t "../f.rb"
$:.shift
$:.unshift "lnk"
t "h.rb"
t "in/../h.rb"
t "in//i.rb"
$:.shift
# A file that is not there is named the way the caller named it.
t "nonexist.rb"
t "./nonexist.rb"
t "sub/../nonexist.rb"
t "#{d}/sub/../nonexist.rb"
t "sub"
t "./sub"
t "~/mr_srcpath_nonexist_zz.rb"
Dir.chdir("sub") { t "g.rb"; t "../f.rb"; t "./g.rb" }
# `load` with a wrap module follows the same rule.
t "f.rb", true
Wrap = Module.new
t "./sub/../f.rb", Wrap
# `require` for comparison: always expanded.
def r(a)
  print "require ", a.inspect, " => "
  require a
end
r "./rq.rb"; $".pop
r "./sub/../rq.rb"; $".pop
r "#{d}/./rq.rb"; $".pop
$:.unshift "lnk"
r "rq2"; $".pop
r "in/../rq2"; $".pop
"##;

/// The String a `load`ed file's path is in follows the name it was
/// loaded by: an expanded name takes the name's encoding with US-ASCII
/// and BINARY standing for the filesystem encoding, a name read as
/// written keeps its own while the real path of it is BINARY.
const LOAD_ENCODINGS: &str = r##"
load "e.rb".b
load "e.rb".force_encoding("US-ASCII")
load "e.rb".encode("EUC-JP")
load "e.rb"
load "./e.rb".b
load "./e.rb".force_encoding("US-ASCII")
load "./e.rb".encode("EUC-JP")
load "#{Dir.pwd}/e.rb".b
$:.unshift "."
load "e.rb".b
load "e.rb".encode("EUC-JP")
$:.unshift Dir.pwd.encode("EUC-JP")
load "e.rb".b
load "e.rb"
"##;

fn load_fixture(name: &str) -> std::path::PathBuf {
    let dir = fixture_dir(name);
    write(&dir, "main.rb", LOAD_MAIN);
    write(&dir, "f.rb", WHERE);
    write(&dir, "sub/g.rb", WHERE);
    write(&dir, "lp/h.rb", WHERE);
    write(&dir, "lp/in/i.rb", WHERE);
    write(&dir, "rq.rb", "p __FILE__\n");
    write(&dir, "lp/rq2.rb", "p __FILE__\n");
    write(
        &dir,
        "e.rb",
        "p [__FILE__, __FILE__.encoding, caller_locations(0)[0].absolute_path.encoding, __dir__.encoding]\n",
    );
    write(&dir, "e_main.rb", LOAD_ENCODINGS);
    std::os::unix::fs::symlink("f.rb", dir.join("fl.rb")).expect("symlink fl.rb");
    std::os::unix::fs::symlink("lp", dir.join("lnk")).expect("symlink lnk");
    dir
}

#[test]
fn load_resolves_and_reports_paths_as_cruby() {
    let dir = load_fixture("load");
    compare("load path rule", &dir, None, &["main.rb"]);
    // The main script named two other ways.
    compare("load path rule", &dir, None, &["./main.rb"]);
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn loaded_file_path_encodings_follow_the_name() {
    let dir = load_fixture("load_enc");
    for (_, locale) in LOCALES {
        compare("load path encodings", &dir, Some(locale), &["e_main.rb"]);
        compare(
            "load path encodings",
            &dir,
            Some(locale),
            &["-E", "UTF-8", "e_main.rb"],
        );
    }
    let _ = std::fs::remove_dir_all(&dir);
}

// ---------------------------------------------------------------------
// #2128: the constants registered onto a file an autoload is running
// ---------------------------------------------------------------------

/// The autoloaded file: it defines `P::BV` and `P::V`, never `P::W` or
/// `TopV`, and looks at all of them from the inside.
const AUTOLOADED: &str = r##"
module P
  class BV; end
  p [:in_file, defined?(V), P.autoload?(:V), P.const_defined?(:V), P.constants.sort]
  p [:in_file_w, defined?(W), P.autoload?(:W), P.const_defined?(:W)]
  p [:in_file_q, defined?(P::V), defined?(::P::W), P.const_defined?(:W, false), P.autoload?(:W, false)]
  class V < BV; end
  p [:after_v, defined?(V), P.autoload?(:V), P.const_defined?(:V)]
end
p [:top_in_file, defined?(TopV), autoload?(:TopV), Object.const_defined?(:TopV), defined?(::TopV)]
p [:sub, defined?(Sub::V), Sub.autoload?(:V), Sub.const_defined?(:V), Sub.autoload?(:W), Sub.const_defined?(:W)] if defined?(Sub)
p [:thread, Thread.new { [P.autoload?(:W), P.const_defined?(:W)] }.value] if $from_thread
"##;

/// The file runs because one of its constants was read.
const AUTOLOAD_BY_READ: &str = r##"
$LOAD_PATH.unshift __dir__
module P; autoload :BV, "v"; autoload :V, "v"; autoload :W, "v"; end
autoload :TopV, "v"
class Sub; include P; end
P::BV
p [:after, P.autoload?(:W), P.const_defined?(:W), defined?(P::W), autoload?(:TopV), defined?(TopV), P.constants.sort]
begin; P::W; rescue NameError => e; p e.message; end
p [:after2, P.autoload?(:W), P.const_defined?(:W), defined?(P::W), P.constants.sort]
begin
  TopV
rescue NameError => e
  p e.message
end
def reader = P::W
begin; reader; rescue NameError => e; p e.message; end
p [1].map { (P::W rescue :undefined) }
"##;

/// The file runs because it was required.
const AUTOLOAD_BY_REQUIRE: &str = r##"
$LOAD_PATH.unshift __dir__
module P; autoload :V, "v"; autoload :W, "v"; end
autoload :TopV, "v"
class Sub; include P; end
require "v"
p [:after, P.autoload?(:W), P.const_defined?(:W), defined?(P::W), autoload?(:TopV), defined?(TopV), P.constants.sort]
begin; P::W; rescue NameError => e; p e.message; end
begin; TopV; rescue NameError => e; p e.message; end
"##;

/// The file is `load`ed: that provides no feature, so the entries stay
/// what they were while it runs, and the first read requires it again.
const AUTOLOAD_BY_LOAD: &str = r##"
$LOAD_PATH.unshift __dir__
module P; autoload :V, "v"; autoload :W, "v"; end
autoload :TopV, "v"
load "v.rb"
p [:after, P.autoload?(:W), P.const_defined?(:W), defined?(P::W), autoload?(:TopV), defined?(TopV), P.constants.sort]
"##;

/// Another thread goes on seeing the registered autoload.
const AUTOLOAD_OTHER_THREAD: &str = r##"
$LOAD_PATH.unshift __dir__
$from_thread = true
module P; autoload :BV, "v"; autoload :V, "v"; autoload :W, "v"; end
P::V
"##;

/// An entry registered to the same file under *another spelling* of
/// the feature is not left pending by the autoload: it is consumed, as
/// by a direct `require`.
const AUTOLOADED_SPELLED: &str = r##"
module Q
  class V; end
  p [:in_file, Q.autoload?(:W), Q.const_defined?(:W), Q.autoload?(:X), Q.const_defined?(:X), Q.autoload?(:Y), Q.const_defined?(:Y)]
end
"##;

const AUTOLOAD_BY_SPELLING: &str = r##"
$LOAD_PATH.unshift __dir__
module Q; autoload :V, "sp"; autoload :W, "sp.rb"; autoload :X, "#{__dir__}/sp.rb"; autoload :Y, "sp"; end
Q::V
p [:after, Q.autoload?(:W), Q.const_defined?(:W), Q.autoload?(:X), Q.const_defined?(:X), Q.autoload?(:Y), Q.const_defined?(:Y), Q.constants.sort]
begin; Q::W; rescue NameError => e; p e.message; end
begin; Q::X; rescue NameError => e; p e.message; end
begin; Q::Y; rescue NameError => e; p e.message; end
p [:after2, Q.autoload?(:W), Q.const_defined?(:W), Q.autoload?(:X), Q.const_defined?(:X), Q.autoload?(:Y), Q.const_defined?(:Y), Q.constants.sort]
# Once the feature is gone the consumed entries have a file to load again.
$LOADED_FEATURES.delete_if { |f| f.end_with?("/sp.rb") }
p [:unloaded, Q.autoload?(:W), Q.const_defined?(:W), Q.autoload?(:X), Q.const_defined?(:X)]
"##;

/// The other warning a constant read can raise carries the same
/// `file:line:` of the reference.
const DEPRECATED_CONSTANT: &str = r##"
class A; B = 1; deprecate_constant :B; end
p A::B
def f = A::B
f
[1].each { A::B }
p A.const_get(:B)
class A; def self.g = B; C = 2; deprecate_constant :C; end
A.g
def h = A::C
3.times { h }
p defined?(A::B)
A.const_defined?(:B)
Object.const_get("A::B")
class D < A; B; end
x = A
x::B
"##;

#[test]
fn constants_of_a_file_being_autoloaded_match_cruby() {
    let dir = fixture_dir("autoload");
    write(&dir, "v.rb", AUTOLOADED);
    write(&dir, "by_read.rb", AUTOLOAD_BY_READ);
    write(&dir, "by_require.rb", AUTOLOAD_BY_REQUIRE);
    write(&dir, "by_load.rb", AUTOLOAD_BY_LOAD);
    write(&dir, "other_thread.rb", AUTOLOAD_OTHER_THREAD);
    write(&dir, "sp.rb", AUTOLOADED_SPELLED);
    write(&dir, "by_spelling.rb", AUTOLOAD_BY_SPELLING);
    write(&dir, "deprecated.rb", DEPRECATED_CONSTANT);
    for script in [
        "by_read.rb",
        "by_require.rb",
        "by_load.rb",
        "other_thread.rb",
        "by_spelling.rb",
        "deprecated.rb",
    ] {
        // `-w`: the "Expected v to define P::W but it didn't" warning
        // and the `file:line:` it is reported at are part of the answer.
        compare("autoload in flight", &dir, None, &["-w", script]);
    }
    let _ = std::fs::remove_dir_all(&dir);
}

// ---------------------------------------------------------------------
// #2096: the encoding of a source path's Strings
// ---------------------------------------------------------------------

/// What a file knows of its own path, as encodings.
const SELF_ENCODINGS: &str = "def self.g = [__FILE__.encoding, __dir__.encoding, caller_locations(0)[0].absolute_path.encoding, caller_locations(0)[0].path.encoding, caller(0)[0].encoding]";

const PATH_ENCODINGS: &str = r##"
D = __dir__
# A feature String's encoding is what `autoload?` hands back, and what
# the autoloaded file's path is in (US-ASCII and BINARY standing for
# the filesystem encoding, as for `require`).
autoload :AlMod, "#{D}/al.rb".b
autoload :AlMod2, "#{D}/al2.rb"
autoload :AlMod3, "#{D}/al3.rb".encode("EUC-JP")
autoload :AlMod4, "#{D}/al4.rb".force_encoding("US-ASCII")
p [:autoload?, Object.autoload?(:AlMod).encoding, Object.autoload?(:AlMod2).encoding, Object.autoload?(:AlMod3).encoding, Object.autoload?(:AlMod4).encoding]
def m = caller_locations(0)[0]
loc = m
p [:file, __FILE__.encoding, __dir__.encoding, $0.encoding, $PROGRAM_NAME.encoding]
p [:loc, loc.label.encoding, loc.base_label.encoding, loc.path.encoding, loc.to_s.encoding, loc.absolute_path.encoding]
p [:al, AlMod.g, AlMod2.g, AlMod3.g, AlMod4.g]
p [:autoload_done, Object.autoload?(:AlMod), Object.autoload?(:AlMod3)]
p [:eval, eval("__FILE__").encoding, eval("__FILE__", nil, "foo.rb").encoding, eval("__FILE__", nil, "foo.rb".encode("EUC-JP")).encoding, eval("__FILE__", nil, "foo.rb".b).encoding, eval("__dir__", nil, "/a/foo.rb".b).encoding, eval("__dir__", nil, "/a/foo.rb").encoding]
p [:eval_loc, eval("caller_locations(0)[0].then{[_1.path.encoding,_1.label.encoding,_1.base_label.encoding,_1.absolute_path]}")]
p [:eval_named, eval("caller_locations(0)[0].path.encoding", nil, "foo.rb".encode("EUC-JP")), binding.eval("__FILE__", "b.rb".b).encoding]
p [:instance_eval, Object.new.instance_eval("__FILE__", "ie.rb".encode("EUC-JP")).encoding, Object.new.instance_eval("__FILE__").encoding]
p [:class_eval, Class.new.class_eval("__FILE__", "ce.rb".b).encoding, Class.new.module_eval("__FILE__", "me.rb".encode("EUC-JP")).encoding]
class Foo
  L0 = caller_locations(0)[0]
  def im = caller_locations(0)[0]
  def self.sm = caller_locations(0)[0]
  def blk = [1].map { caller_locations(0)[0] }[0]
  def blk2 = [1].map { [1].map { caller_locations(0)[0] }[0] }[0]
  def あ = caller_locations(0)[0]
  def resc
    raise "x"
  rescue
    caller_locations(0)[0]
  end
  def ens
    return 1
  ensure
    $ens = caller_locations(0)[0]
  end
  define_method(:dm) { caller_locations(0)[0] }
  class << self
    L1 = caller_locations(0)[0]
  end
end
module Mあ
  L = caller_locations(0)[0]
  def self.f = caller_locations(0)[0]
end
module Outer
  module Inner
    L = caller_locations(0)[0]
    def self.f = caller_locations(0)[0]
  end
end
anon = Class.new { def am = caller_locations(0)[0] }
def show(tag, l) = p([tag, l.label, l.label.encoding, l.base_label, l.base_label.encoding, l.to_s.encoding])
show :main, caller_locations(0)[0]
show :top_m, m
show :class, Foo::L0
show :im, Foo.new.im
show :sm, Foo.sm
show :blk, Foo.new.blk
show :blk2, Foo.new.blk2
show :nonascii, Foo.new.あ
show :resc, Foo.new.resc
Foo.new.ens; show :ens, $ens
show :dm, Foo.new.dm
show :sclass, Foo.singleton_class::L1
show :namod, Mあ::L
show :namod_f, Mあ.f
show :nested, Outer::Inner::L
show :nested_f, Outer::Inner.f
show :anon, anon.new.am
show :topblk, [1].map { caller_locations(0)[0] }[0]
show :lambda, ->{ caller_locations(0)[0] }.call
show :cfunc, [1].map { caller_locations(1)[0] }[0]
# Which of these Strings is made per call and which is the iseq's own.
l = m
p [:made, l.label.equal?(l.label), l.label.frozen?, l.base_label.equal?(l.base_label), l.base_label.frozen?, l.label << "x", l.label]
l = caller_locations(0)[0]
p [:own, l.label.equal?(l.label), l.label.frozen?, l.base_label.equal?(l.base_label), l.base_label.frozen?, l.label.equal?(l.base_label)]
p [:path, l.path.equal?(l.path), l.path.frozen?, l.absolute_path.equal?(l.absolute_path), l.absolute_path.frozen?, l.to_s.equal?(l.to_s), l.to_s.frozen?]
l = [1].map { caller_locations(0)[0] }[0]
p [:block, l.label.equal?(l.label), l.label.frozen?, l.base_label.equal?(l.base_label), l.base_label.frozen?]
p [:caller, caller(0)[0].encoding, Foo.new.method(:あ).unbind.bind_call(Foo.new).to_s.encoding]
begin; raise "x"; rescue => e; p [:bt, e.backtrace[0].encoding, e.backtrace_locations[0].label.encoding, e.backtrace_locations[0].base_label.encoding, e.backtrace_locations[0].absolute_path.encoding]; end
t = Thread.new { Thread.current.backtrace_locations(0)[0] }.value
show :thread, t
# The locations of an exception are parsed back from its backtrace.
class Foo
  def raiser = raise("x")
  def self.sraiser = raise("x")
  def braiser = [1].each { [2].each { raise "x" } }
  BT = (raise "x" rescue $!.backtrace_locations[0])
  def あraiser = raise("x")
end
show :bt_main, (raise "x" rescue $!.backtrace_locations[0])
show :bt_im, (Foo.new.raiser rescue $!.backtrace_locations[0])
show :bt_sm, (Foo.sraiser rescue $!.backtrace_locations[0])
show :bt_blk, (Foo.new.braiser rescue $!.backtrace_locations[0])
show :bt_class, Foo::BT
show :bt_nonascii, (Foo.new.あraiser rescue $!.backtrace_locations[0])
show :bt_cfunc, (Integer("z") rescue $!.backtrace_locations[0])
l = (Foo.new.raiser rescue $!.backtrace_locations[0])
p [:bt_made, l.label.equal?(l.label), l.label.frozen?, l.base_label.equal?(l.base_label), l.base_label.frozen?]
l = (raise "x" rescue $!.backtrace_locations[0])
p [:bt_own, l.label.equal?(l.label), l.label.frozen?, l.base_label.equal?(l.base_label), l.base_label.frozen?, l.label.equal?(l.base_label)]
require "#{D}/rq1.rb"; p [:require, $rq]
load "#{D}/rq1.rb".b; p [:load_binary, $rq]
load "#{D}/rq1.rb".encode("EUC-JP"); p [:load_eucjp, $rq]
require_relative "rr1"; p [:require_relative, $rq]
"##;

/// `$0`: a frozen String in the locale encoding; assigning one stores a
/// frozen copy of what `to_str` gives.
const PROGRAM_NAME: &str = r##"
s = "abc".dup.force_encoding("EUC-JP")
p [$0.frozen?, $0.encoding, $PROGRAM_NAME.frozen?]
$0 = s
p [$0.frozen?, $0.equal?(s), s.frozen?, $0.encoding, $PROGRAM_NAME.equal?($0), $0.equal?($0)]
s << "d"
p $0
p Process.argv0, Process.argv0.frozen?
alias $prog $0
p $prog
begin; $0 = 1; rescue => e; p e.class, e.message; end
begin; $0 = nil; rescue => e; p e.class, e.message; end
o = Object.new; def o.to_str = "zzz"
$0 = o
p $0, $0.frozen?
$PROGRAM_NAME = "via_name"
p $0, $PROGRAM_NAME.frozen?
begin; $0 << "x"; rescue => e; p e.class; end
"##;

/// `-e` and standard input name no file: `__FILE__` is CRuby's own
/// String for them, not the locale's.
const NO_FILE: &str = r#"p [__FILE__, __FILE__.encoding, $0, $0.encoding, $0.frozen?, __dir__, __dir__.encoding, caller_locations(0)[0].path.encoding, caller_locations(0)[0].absolute_path, eval("__FILE__", nil, "-e").encoding, eval("__FILE__", nil, "-").encoding, eval("__FILE__").encoding]"#;

#[test]
fn source_path_string_encodings_match_cruby() {
    let dir = fixture_dir("enc");
    write(&dir, "t.rb", PATH_ENCODINGS);
    for (i, name) in ["al", "al2", "al3", "al4"].iter().enumerate() {
        let module = if i == 0 {
            "AlMod".to_string()
        } else {
            format!("AlMod{}", i + 1)
        };
        write(
            &dir,
            &format!("{name}.rb"),
            &format!("module {module}; {SELF_ENCODINGS}; end\n"),
        );
    }
    let rq = "$rq = [__FILE__.encoding, __dir__.encoding, caller_locations(0)[0].absolute_path.encoding, caller_locations(0)[0].label.encoding, caller_locations(0)[0].base_label.encoding]\n";
    write(&dir, "rq1.rb", rq);
    write(&dir, "rr1.rb", rq);
    write(&dir, "z0.rb", PROGRAM_NAME);
    for (_, locale) in LOCALES {
        let l = Some(*locale);
        compare("path encodings", &dir, l, &["t.rb"]);
        compare("path encodings", &dir, l, &["./t.rb"]);
        compare("path encodings", &dir, l, &["-E", "UTF-8", "t.rb"]);
        compare("path encodings", &dir, l, &["-E", "EUC-JP", "t.rb"]);
        compare("$0", &dir, l, &["z0.rb"]);
        compare("-e", &dir, l, &["-e", NO_FILE]);
        compare_stdin("stdin script", &dir, l, &[], Some(NO_FILE));
        compare_stdin("stdin script", &dir, l, &["-"], Some(NO_FILE));
    }
    let _ = std::fs::remove_dir_all(&dir);
}
