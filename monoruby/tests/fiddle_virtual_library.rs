//! `Fiddle.register_virtual_library`: a pure-Ruby stub standing in for the
//! C extension it replaced.
//!
//! monoruby serves several C-extension gems from pure-Ruby stubs, so the
//! `.so` / `.bundle` those gems would have installed is not on disk. A
//! program that reaches past the gem's Ruby API and `dlopen`s the
//! extension itself — to call a symbol the Ruby API never exposed — would
//! otherwise get a `DLError` for a file that, from its point of view, has
//! to be there.
extern crate monoruby;

fn run_with(args: &[&str], code: &str) -> String {
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_monoruby"))
        .args(args)
        .arg("-e")
        .arg(code)
        .output()
        .unwrap();
    assert!(
        out.status.success(),
        "monoruby failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    String::from_utf8_lossy(&out.stdout).into_owned()
}

fn run(code: &str) -> String {
    run_with(&["--disable=gems"], code)
}

/// A registered path resolves, serves the symbols its resolver answers
/// for, and reports every other name as missing — a stub is expected to
/// export what the real extension exported and nothing else, so `sym?`
/// still tells the truth. An unregistered path keeps raising.
#[test]
fn a_virtual_library_stands_in_for_a_missing_extension() {
    let got = run(r#"
        require "fiddle"
        fake   = File.join(Dir.pwd, "no_such_extension.bundle")
        global = Fiddle.dlopen(nil)
        Fiddle.register_virtual_library(fake) { |n| n == "answer" ? global.sym?("abs") : nil }

        h = Fiddle.dlopen(fake)
        raise "unresolved" if h["answer"].to_i == 0
        raise "leaked a symbol the virtual library does not export" unless h.sym?("abs").nil?
        begin
          h["abs"]
          raise "expected DLError"
        rescue Fiddle::DLError
        end
        begin
          Fiddle.dlopen(File.join(Dir.pwd, "unregistered.bundle"))
          raise "expected DLError for an unregistered path"
        rescue Fiddle::DLError
        end
        f = Fiddle::Function.new(h["answer"], [Fiddle::TYPE_INT], Fiddle::TYPE_INT)
        print f.call(-42)
        "#);
    assert_eq!(got, "42");
}
