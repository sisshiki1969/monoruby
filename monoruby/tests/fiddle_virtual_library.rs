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
    let got = run(
        r#"
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
        "#,
    );
    assert_eq!(got, "42");
}

/// What the gosu stub uses it for: `Gem.loaded_specs["gosu"]` names the
/// stub tree, and the `gosu.<dlext>` under it opens and exports the two
/// symbols an app needs for a keyboard grab (Gosu never put either on the
/// Ruby side, so apps `dlopen` the extension for them).
#[test]
fn the_gosu_stub_completes_its_native_half() {
    // There is only a stub to complete if it loads at all, and it needs
    // the host's `ffi` gem plus a libSDL2 for that — neither of which a
    // CI runner has. Let the spawned process report that itself: which of
    // the two is missing, and what it raises, varies by host, so matching
    // on the message from here would be guesswork.
    //
    // Runs *with* rubygems (every other spawn here passes
    // `--disable=gems`): what the stub publishes into `Gem.loaded_specs`
    // is half the subject.
    let got = run_with(
        &[],
        r#"
        begin
          require "gosu"
        rescue LoadError, StandardError => e
          print "skip: #{e.class}: #{e.message}"
          exit 0
        end
        require "fiddle"
        spec = Gem.loaded_specs["gosu"]
        raise "no spec" unless spec
        bundle = File.join(spec.full_gem_path, "lib", "gosu.#{RbConfig::CONFIG["DLEXT"]}")
        lib = Fiddle.dlopen(bundle)
        raise "shared_window" if lib["_ZN4Gosu13shared_windowEv"].to_i == 0
        raise "kb grab"       if lib["SDL_SetWindowKeyboardGrab"].to_i == 0
        raise "not a C++ ABI dump" unless lib.sym?("_ZN4Gosu6nosuchEv").nil?
        print "ok"
        "#,
    );
    if let Some(reason) = got.strip_prefix("skip: ") {
        eprintln!("skipped: the gosu stub does not load here ({reason})");
        return;
    }
    assert_eq!(got, "ok");
}
