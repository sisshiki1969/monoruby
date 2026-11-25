//! error_highlight (#1893): the source line and the caret under the
//! report of a NameError, a TypeError or an ArgumentError.
//!
//! CRuby requires the gem at boot. monoruby registers `ErrorHighlight`
//! as an autoload (`builtins/gem_prelude.rb`) and loads it when
//! `Exception#detailed_message` is first asked about an exception of
//! one of those classes; the gem then finds the node a backtrace
//! location was executing through
//! `RubyVM::AbstractSyntaxTree.node_id_for_backtrace_location`, which
//! monoruby answers from the extent each instruction carries
//! (`Thread::Backtrace::Location#__node_id`).
//!
//! All of it is a property of a process started on a file — the
//! in-process `run_test` interpreters run with gems disabled and
//! compile from a string — so everything here spawns the binary.

use std::path::PathBuf;
use std::process::{Command, Output};

fn monoruby() -> Command {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_monoruby"));
    cmd.env_remove("RUBYOPT")
        .env_remove("RUBYLIB")
        .env_remove("RUBYPATH");
    cmd
}

fn fixtures() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/error_highlight")
}

fn stdout(out: &Output) -> String {
    String::from_utf8_lossy(&out.stdout).into_owned()
}

fn stderr(out: &Output) -> String {
    String::from_utf8_lossy(&out.stderr).into_owned()
}

/// `-e code`, which must succeed; its stdout.
fn eval(args: &[&str], code: &str) -> String {
    let out = monoruby()
        .args(args)
        .arg("-e")
        .arg(code)
        .output()
        .expect("failed to spawn monoruby");
    assert!(out.status.success(), "{out:?}");
    stdout(&out).trim().to_string()
}

/// A script in a directory of its own under the temp directory, removed
/// when dropped. Run from that directory by its bare name, so reports
/// read `name.rb:1:in ...`.
struct Script {
    dir: PathBuf,
    name: String,
}

impl Script {
    fn new(name: &str, source: &str) -> Self {
        let dir = std::env::temp_dir().join(format!("i1893_{name}_{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let name = format!("{name}.rb");
        std::fs::write(dir.join(&name), source).unwrap();
        Script { dir, name }
    }

    fn run(&self, args: &[&str]) -> Output {
        monoruby()
            .args(args)
            .arg(&self.name)
            .current_dir(&self.dir)
            .output()
            .expect("failed to spawn monoruby")
    }
}

impl Drop for Script {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.dir);
    }
}

/// The 121 cases of tests/error_highlight/cases.rb answer what CRuby
/// 4.0.9 answers (cases.expected): every node kind the gem spots, the
/// caller / callee form of an arity error, frames in a second file, in
/// eval'd code and in JIT-compiled code, `ErrorHighlight.spot`, the
/// formatter.
#[test]
fn detailed_messages_match_cruby() {
    let dir = fixtures();
    let out = monoruby()
        .arg("cases.rb")
        .current_dir(&dir)
        .output()
        .expect("failed to spawn monoruby");
    assert!(out.status.success(), "{out:?}");
    let expected = std::fs::read_to_string(dir.join("cases.expected"))
        .unwrap()
        .replace("@DIR@", dir.to_str().unwrap());
    let got = stdout(&out);
    for (i, (e, g)) in expected.lines().zip(got.lines()).enumerate() {
        assert_eq!(e, g, "line {} of the output", i + 1);
    }
    assert_eq!(expected.lines().count(), got.lines().count());
}

/// The reports of the issue: an uncaught error's snippet goes between
/// the header and the `from` lines.
#[test]
fn uncaught_error_reports_carry_the_snippet() {
    let script = Script::new("raise", "def m(a); raise ArgumentError, \"x\"; end\nm(1)\n");
    let out = script.run(&[]);
    assert!(!out.status.success());
    assert_eq!(
        stderr(&out),
        "raise.rb:1:in 'Object#m': x (ArgumentError)\n\
         \n\
         def m(a); raise ArgumentError, \"x\"; end\n\
         \x20               ^^^^^^^^^^^^^^^^^^\n\
         \tfrom raise.rb:2:in '<main>'\n"
    );

    let script = Script::new("arity", "def m(a)\nend\nm\n");
    let out = script.run(&[]);
    assert!(!out.status.success());
    assert_eq!(
        stderr(&out),
        "arity.rb:1:in 'Object#m': wrong number of arguments (given 0, expected 1) (ArgumentError)\n\
         \n\
         \x20   caller: arity.rb:3\n\
         \x20   | m\n\
         \x20     ^\n\
         \x20   callee: arity.rb:1\n\
         \x20   | def m(a)\n\
         \x20         ^\n\
         \tfrom arity.rb:3:in '<main>'\n"
    );

    let script = Script::new("nomethod", "x = nil\nx.foo(1)\n");
    let out = script.run(&[]);
    assert_eq!(
        stderr(&out),
        "nomethod.rb:2:in '<main>': undefined method 'foo' for nil (NoMethodError)\n\
         \n\
         x.foo(1)\n\
         \x20^^^^\n"
    );
}

/// A `-e` program has no file to quote: an arity error still names the
/// two places, any other error is the bare header.
#[test]
fn a_program_without_a_file_has_no_snippet() {
    let out = monoruby().args(["-e", "nil.foo"]).output().unwrap();
    assert_eq!(
        stderr(&out),
        "-e:1:in '<main>': undefined method 'foo' for nil (NoMethodError)\n"
    );
    let out = monoruby()
        .args(["-e", "def m(a); end; m"])
        .output()
        .unwrap();
    assert_eq!(
        stderr(&out),
        "-e:1:in 'Object#m': wrong number of arguments (given 0, expected 1) (ArgumentError)\n\
         \n\
         \x20   caller: -e:1\n\
         \x20   callee: -e:1\n\
         \tfrom -e:1:in '<main>'\n"
    );
}

const LOADED: &str = r#"$LOADED_FEATURES.any? { |f| f.end_with?("/error_highlight.rb") }"#;

/// The constant is there from the start, as in CRuby, but the gem is
/// read only by the first message that needs it — or the first
/// reference to the constant.
#[test]
fn error_highlight_is_loaded_on_first_use() {
    let got = eval(
        &[],
        &format!(
            r#"p [defined?(ErrorHighlight), Object.autoload?(:ErrorHighlight), {LOADED},
                  NameError.ancestors.first]"#
        ),
    );
    assert_eq!(got, r#"["constant", "error_highlight", false, NameError]"#);

    // Messages that are never decorated do not load it: a rescued
    // error's `message`, and the detailed message of another class.
    let got = eval(
        &[],
        &format!(
            r#"begin; nil.foo; rescue => e; e.message; end
               RuntimeError.new("x").detailed_message
               p {LOADED}"#
        ),
    );
    assert_eq!(got, "false");

    let got = eval(
        &[],
        &format!(
            r#"begin; nil.foo; rescue => e; m = e.detailed_message; end
               p [m, {LOADED}, Object.autoload?(:ErrorHighlight),
                  NameError.ancestors.first, TypeError.ancestors.first,
                  ArgumentError.ancestors.first]"#
        ),
    );
    assert_eq!(
        got,
        "[\"undefined method 'foo' for nil (NoMethodError)\", true, nil, \
         ErrorHighlight::CoreExt, ErrorHighlight::CoreExt, ErrorHighlight::CoreExt]"
    );

    let got = eval(
        &[],
        &format!(r#"v = ErrorHighlight::VERSION; p [v, {LOADED}, require("error_highlight")]"#),
    );
    assert_eq!(got, r#"["0.7.2", true, false]"#);

    // An explicit require while the autoload is pending is the direct
    // load of the autoload's own file.
    let got = eval(
        &[],
        r#"p [require("error_highlight"), ErrorHighlight.respond_to?(:spot),
              Object.autoload?(:ErrorHighlight)]"#,
    );
    assert_eq!(got, "[true, true, nil]");
}

/// The message that triggers the load is itself decorated, also when it
/// was reached through a subclass's own `detailed_message`.
#[test]
fn the_message_that_loads_it_is_decorated() {
    let script = Script::new(
        "first",
        "class Mine < NameError\n\
         \x20 def detailed_message(**) = \"<\" + super + \">\"\n\
         end\n\
         begin\n\
         \x20 raise Mine, \"mine\"\n\
         rescue => e\n\
         \x20 puts e.detailed_message\n\
         \x20 puts e.detailed_message\n\
         end\n",
    );
    let out = script.run(&[]);
    assert!(out.status.success(), "{out:?}");
    let once = "<mine (Mine)\n\n  raise Mine, \"mine\"\n  ^^^^^>\n";
    assert_eq!(stdout(&out), format!("{once}{once}"));
}

/// `--disable-error_highlight` and `--disable-gems` leave the constant
/// undefined and the report bare, as in CRuby.
/// On CRuby error_highlight is there from boot, so anything a program
/// prepends to one of the three classes sits above `CoreExt`. Here the
/// first such prepend loads it first (`Module#prepend`), which is also
/// what puts did_you_mean's suggestions below the snippet.
#[test]
fn a_module_prepended_before_the_load_goes_on_top() {
    let s = Script::new(
        "prepended",
        r#"module Deco
  def detailed_message(**) = "<#{super}>"
end
before = Object.autoload?(:ErrorHighlight)
TypeError.prepend(Deco)
p [before, Object.autoload?(:ErrorHighlight), TypeError.ancestors.take(3),
   NameError.ancestors.take(2)]
begin
  1 + nil
rescue => e
  puts e.detailed_message
end
"#,
    );
    let out = s.run(&[]);
    assert!(out.status.success(), "{out:?}");
    assert_eq!(
        stdout(&out),
        "[\"error_highlight\", nil, [Deco, ErrorHighlight::CoreExt, TypeError], \
         [ErrorHighlight::CoreExt, NameError]]\n\
         <nil can't be coerced into Integer (TypeError)\n\
         \n\
         \x20 1 + nil\n\
         \x20     ^^^>\n"
    );

    let got = eval(
        &[],
        &format!(
            r#"m = Module.new
               RuntimeError.prepend(m); NoMethodError.prepend(m)
               NameError.singleton_class.prepend(m); NameError.include(m)
               p {LOADED}"#
        ),
    );
    assert_eq!(got, "false");

    // did_you_mean, loaded after boot, lands on top as on CRuby.
    let got = eval(
        &[],
        r#"require "did_you_mean"
           p NameError.ancestors.take(3)"#,
    );
    assert_eq!(
        got,
        "[DidYouMean::Correctable, ErrorHighlight::CoreExt, NameError]"
    );

    // Without the library there is nothing to load.
    let got = eval(
        &["--disable-error_highlight"],
        "m = Module.new; NameError.prepend(m); p NameError.ancestors.first == m",
    );
    assert_eq!(got, "true");
}

#[test]
fn disabling_the_feature() {
    for flag in [
        "--disable-error_highlight",
        "--disable=error_highlight",
        "--disable-gems",
    ] {
        let got = eval(&[flag], "p defined?(ErrorHighlight)");
        assert_eq!(got, "nil", "{flag}");
        let script = Script::new("off", "x = nil\nx.foo(1)\n");
        let out = script.run(&[flag]);
        assert_eq!(
            stderr(&out),
            "off.rb:2:in '<main>': undefined method 'foo' for nil (NoMethodError)\n",
            "{flag}"
        );
    }
    // It can still be required by hand.
    let got = eval(
        &["--disable-error_highlight"],
        r#"require "error_highlight"; p NameError.ancestors.first"#,
    );
    assert_eq!(got, "ErrorHighlight::CoreExt");
}

/// What the gem asks `RubyVM::AbstractSyntaxTree` is answered by a
/// private constant of `ErrorHighlight`: no top-level `RubyVM` appears,
/// which libraries take to mean CRuby's VM.
#[test]
fn ruby_vm_stays_private_to_error_highlight() {
    let script = Script::new(
        "rubyvm",
        r#"p defined?(RubyVM)
ErrorHighlight
p defined?(RubyVM), ErrorHighlight.constants.include?(:RubyVM)
p((ErrorHighlight::RubyVM rescue $!.class))
ast = ErrorHighlight.module_eval("RubyVM::AbstractSyntaxTree")
loc = caller_locations(0).first
p((ast.of(loc, keep_script_lines: true) rescue [$!.class, $!.message]))
p((ast.node_id_for_backtrace_location(1) rescue [$!.class, $!.message]))
def f = caller_locations(1).first
here = f
id = ast.node_id_for_backtrace_location(here)
require "prism"
node = Prism.parse_file(__FILE__).value.breadth_first_search { |n| n.node_id == id }
p [node.class, node.name, node.location.start_line]
p ast.node_id_for_backtrace_location(eval("caller_locations(0).first"))
p ast.node_id_for_backtrace_location(Thread.current.backtrace_locations.first)
"#,
    );
    let out = script.run(&[]);
    assert!(out.status.success(), "{out:?}");
    assert_eq!(
        stdout(&out),
        "nil\n\
         nil\n\
         false\n\
         NameError\n\
         [RuntimeError, \"cannot get AST for ISEQ compiled by prism\"]\n\
         [TypeError, \"Thread::Backtrace::Location object expected\"]\n\
         [Prism::CallNode, :f, 10]\n\
         -1\n\
         -1\n"
    );
}

/// An exception's locations know the path their file was loaded from,
/// as `caller_locations` do: unchanged by a `chdir`, nil for eval'd
/// code even when it was given a file name.
#[test]
fn exception_locations_keep_the_loaded_path() {
    let script = Script::new(
        "abs",
        r#"here = File.realpath(__FILE__)
begin
  raise "a"
rescue => e
  Dir.chdir("/") { p e.backtrace_locations[0].absolute_path == here }
end
begin
  eval("raise 'b'", nil, "/nonexistent/named.rb")
rescue => e
  l = e.backtrace_locations[0]
  p [l.path, l.absolute_path]
end
begin
  eval("raise 'c'")
rescue => e
  p e.backtrace_locations[0].absolute_path
end
"#,
    );
    let out = script.run(&[]);
    assert!(out.status.success(), "{out:?}");
    assert_eq!(
        stdout(&out),
        "true\n[\"/nonexistent/named.rb\", nil]\nnil\n"
    );
}
