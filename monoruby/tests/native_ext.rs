//! The native extension ABI (`monoruby_ext_sys`, `src/ext.rs`): a C
//! extension built here with the system C compiler against
//! `include/monoruby_ext.h`, loaded by the `monoruby` binary through
//! `require "hello_ext.so"`, and driven through every entry the table
//! offers — arity checking, errors in both directions, blocks, variadic
//! methods, native objects with a marked payload across a collection,
//! `dup`, a blocking call on the pool, an error stashed and re-raised.
//!
//! No CRuby oracle: nothing on the CRuby side can load this library, so
//! the expected output is spelled out.

use std::path::PathBuf;
use std::process::Command;

/// Compile the fixture into the test binary's own directory, which is
/// on the extension search path (`ext::search_dirs`), and answer it.
/// `None` when no C compiler is available.
fn build_fixture() -> Option<PathBuf> {
    let src = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/native_ext/hello_ext.c");
    let include = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../monoruby_ext_sys/include");
    let out_dir = std::env::current_exe()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf();
    let ext = if cfg!(target_os = "macos") {
        "dylib"
    } else {
        "so"
    };
    let out = out_dir.join(format!("libhello_ext.{ext}"));
    let cc = std::env::var("CC").unwrap_or_else(|_| "cc".to_string());
    let status = Command::new(cc)
        .args(["-shared", "-fPIC", "-O1", "-Wall", "-I"])
        .arg(&include)
        .arg("-o")
        .arg(&out)
        .arg(&src)
        .status()
        .ok()?;
    status.success().then_some(out_dir)
}

#[test]
fn native_extension_round_trip() {
    let Some(dir) = build_fixture() else {
        eprintln!("skipped: no C compiler");
        return;
    };
    let script = r#"
p require "hello_ext.so"
p require "hello_ext.so"
p $LOADED_FEATURES.grep(/hello_ext/).map { |f| File.basename(f) }
p Hello::VERSION == RUBY_VERSION
p Hello.add(2, 40)
p Hello.add(2**40, 2**40)
p Hello.greet("world")
p (begin; Hello.greet(1); rescue TypeError => e; [e.class, e.message]; end)
p (begin; Hello.boom("bang"); rescue Hello::Error => e; [e.class, e.message, e.class.ancestors.include?(StandardError)]; end)
p Hello.each3 { |i| i * 10 }
p (begin; Hello.each3 { |i| raise "in block #{i}" if i == 1 }; rescue => e; [e.class, e.message]; end)
p Hello.variadic
p Hello.variadic(1, 2, 3)
p (begin; Hello.add(1); rescue ArgumentError => e; e.message; end)
class Foo; def bar = :bar_called; private def priv = :priv_called; def bad = raise(IOError, "io"); end
p Hello.call_it(Foo.new, :bar)
p Hello.call_it(Foo.new, :priv)
p (begin; Hello.call_it(Foo.new, :bad); rescue IOError => e; e.message; end)
p (begin; Hello.call_it(Foo.new, :nope); rescue NoMethodError => e; e.message[0, 30]; end)
c = Hello::Counter.new("t" * 3)
p [c.incr, c.incr, c.incr, c.tag]
d = c.dup
p [d.incr, c.incr, d.tag.equal?(c.tag)]
e = Hello::Counter.allocate
p (begin; e.incr; rescue TypeError => e; e.message; end)
p Hello.blocking(100000)
p (begin; Hello.stash { raise KeyError, "stashed" }; rescue KeyError => e; [e.class, e.message]; end)
p Hello.stash { :fine }
cs = 200.times.map { |i| Hello::Counter.new("tag#{i}") }
GC.start
p cs.each_with_index.all? { |x, i| x.tag == "tag#{i}" }
s = 0
1000.times { |i| s += Hello.add(i, 1) }
p s
p (begin; require "nosuch.so"; rescue LoadError => e; e.message; end)
"#;
    let expected = r#"true
false
["libhello_ext.so"]
true
42
2199023255552
"hello, world"
[TypeError, "no implicit conversion of Integer into String"]
[Hello::Error, "bang", true]
[0, 10, 20]
[RuntimeError, "in block 1"]
{argc: 0, f: 1.5}
{argc: 3, f: 1.5}
"wrong number of arguments (given 1, expected 2)"
:bar_called
:priv_called
"io"
"undefined method 'nope' for an"
[1, 2, 3, "ttt"]
[4, 4, true]
"not a counter"
4999950000
[KeyError, "stashed"]
:fine
true
500500
"cannot load such file -- nosuch.so"
"#;
    let expected = if cfg!(target_os = "macos") {
        expected.replace("libhello_ext.so", "libhello_ext.dylib")
    } else {
        expected.to_string()
    };
    let out = Command::new(env!("CARGO_BIN_EXE_monoruby"))
        .env("MONORUBY_EXT_PATH", &dir)
        .arg("-e")
        .arg(script)
        .output()
        .unwrap();
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        out.status.success(),
        "monoruby failed:\n{stdout}\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert_eq!(stdout, expected);
}
