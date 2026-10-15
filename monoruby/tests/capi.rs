//! The CRuby-compatible C API (`monoruby_capi/`, `doc/c_api.md`): one C
//! extension source, written against CRuby's `ruby.h` with nothing
//! monoruby-specific in it, built twice — against the host CRuby's
//! headers and against `monoruby_capi/include` plus the layer's
//! `capi.c` — and driven by the same script on both interpreters. The
//! outputs must be identical.

use std::path::{Path, PathBuf};
use std::process::Command;

fn manifest_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn cc() -> String {
    std::env::var("CC").unwrap_or_else(|_| "cc".to_string())
}

fn out_dir(flavor: &str) -> PathBuf {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR"))
        .join("capi")
        .join(flavor);
    std::fs::create_dir_all(&dir).unwrap();
    dir
}

/// The extension sources run on both interpreters, each with its
/// script: the stage 1 surface and the stage 2 one (TypedData, alloc
/// functions, blocks from C, `rb_call_super`, Struct).
const FIXTURES: [&str; 3] = ["capi_hello", "capi_data", "capi_st"];

/// The fixtures built against monoruby's headers, in one directory (to
/// be put on `$LOAD_PATH`). `None` when there is no C compiler. Built
/// once per test binary: the tests run in parallel.
fn build_for_monoruby() -> Option<PathBuf> {
    static DIR: std::sync::OnceLock<Option<PathBuf>> = std::sync::OnceLock::new();
    DIR.get_or_init(build_for_monoruby_once).clone()
}

fn build_for_monoruby_once() -> Option<PathBuf> {
    let root = manifest_dir().join("..");
    let dir = out_dir("monoruby");
    for name in FIXTURES {
        let status = Command::new(cc())
            .args(["-shared", "-fPIC", "-O1", "-Wall", "-Werror"])
            .arg("-I")
            .arg(root.join("monoruby_capi/include"))
            .arg("-I")
            .arg(root.join("monoruby_ext_sys/include"))
            .arg("-o")
            .arg(dir.join(format!("{name}.so")))
            .arg(manifest_dir().join(format!("tests/capi/{name}.c")))
            .arg(root.join("monoruby_capi/src/capi.c"))
            .arg(root.join("monoruby_capi/src/st.c"))
            .status()
            .ok()?;
        assert!(status.success(), "building {name}.c against monoruby_capi failed");
    }
    Some(dir)
}

fn ruby_config(key: &str) -> String {
    let out = Command::new("ruby")
        .args(["-e", &format!("print RbConfig::CONFIG[{key:?}]")])
        .output()
        .expect("a host `ruby` is needed to build the CRuby side of the comparison");
    String::from_utf8(out.stdout).unwrap()
}

/// The same source built against the host CRuby, as `extconf.rb` would.
fn build_for_cruby() -> PathBuf {
    static DIR: std::sync::OnceLock<PathBuf> = std::sync::OnceLock::new();
    DIR.get_or_init(build_for_cruby_once).clone()
}

fn build_for_cruby_once() -> PathBuf {
    let dir = out_dir("cruby");
    let dlext = ruby_config("DLEXT");
    for name in FIXTURES {
        let mut cmd = Command::new(cc());
        cmd.args(["-shared", "-fPIC", "-O1", "-w"])
            .arg("-I")
            .arg(ruby_config("rubyhdrdir"))
            .arg("-I")
            .arg(ruby_config("rubyarchhdrdir"));
        if cfg!(target_os = "macos") {
            cmd.args(["-undefined", "dynamic_lookup"]);
        }
        let status = cmd
            .arg("-o")
            .arg(dir.join(format!("{name}.{dlext}")))
            .arg(manifest_dir().join(format!("tests/capi/{name}.c")))
            .status()
            .unwrap();
        assert!(status.success(), "building {name}.c against CRuby failed");
    }
    dir
}

fn run(cmd: &mut Command) -> String {
    let out = cmd.output().unwrap();
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(
        out.status.success(),
        "{cmd:?} failed\n--- stdout\n{stdout}\n--- stderr\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
    stdout
}

fn monoruby(lib: &Path) -> Command {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_monoruby"));
    cmd.arg("-I").arg(lib);
    cmd
}

#[test]
fn capi_extension_matches_cruby() {
    let Some(mr_dir) = build_for_monoruby() else {
        eprintln!("skipped: no C compiler");
        return;
    };
    let cr_dir = build_for_cruby();
    for name in FIXTURES {
        let script = manifest_dir().join(format!("tests/capi/{name}.rb"));
        let expected = run(Command::new("ruby").arg("-I").arg(&cr_dir).arg(&script));
        for args in [&[][..], &["--no-jit"][..]] {
            let actual = run(monoruby(&mr_dir).args(args).arg(&script));
            if actual != expected {
                let diff: Vec<String> = expected
                    .lines()
                    .zip(actual.lines())
                    .enumerate()
                    .filter(|(_, (e, a))| e != a)
                    .map(|(i, (e, a))| format!("line {}:\n  cruby:    {e}\n  monoruby: {a}", i + 1))
                    .collect();
                panic!(
                    "{name}: monoruby {args:?} differs from CRuby ({} vs {} lines)\n{}",
                    expected.lines().count(),
                    actual.lines().count(),
                    diff.join("\n")
                );
            }
        }
    }
}

/// A library compiled against CRuby's headers is refused with a
/// LoadError (its `rb_*` symbols do not resolve in monoruby), not loaded
/// and run with the wrong value layout.
#[test]
fn capi_cruby_build_is_refused() {
    if build_for_monoruby().is_none() {
        eprintln!("skipped: no C compiler");
        return;
    }
    let cr_dir = build_for_cruby();
    let out = run(monoruby(&cr_dir).args([
        "-e",
        r#"begin; require "capi_hello"; rescue LoadError => e; p e.message.include?("monoruby's C API headers"); end
p defined?(CapiHello)"#,
    ]));
    // On macOS CRuby's DLEXT is `bundle`; `require` looks for that too
    // there, and refuses it the same way.
    assert_eq!(out, "true\nnil\n");
}

/// Loaded by path, under the name a gem's `require "x/x"` gives, and
/// required once only.
#[test]
fn capi_extension_by_path() {
    let Some(mr_dir) = build_for_monoruby() else {
        eprintln!("skipped: no C compiler");
        return;
    };
    let so = mr_dir.join("capi_hello.so");
    let out = run(Command::new(env!("CARGO_BIN_EXE_monoruby")).args([
        "-e",
        &format!(
            r#"p require({so:?}); p require({so:?}); p CapiHello.add(1, 2); p $LOADED_FEATURES.grep(/capi_hello/).size"#,
            so = so.display().to_string()
        ),
    ]));
    assert_eq!(out, "true\nfalse\n3\n1\n");
}

/// `tests/capi/capi_errors.c` built as `<dir>/<name>.so`, with `defines`
/// and, unless `layer` is false, the layer's `capi.c`. `None` when there
/// is no C compiler.
fn build_errors(name: &str, defines: &[&str], layer: bool) -> Option<PathBuf> {
    let root = manifest_dir().join("..");
    let dir = out_dir(&format!("errors/{name}"));
    let mut cmd = Command::new(cc());
    cmd.args(["-shared", "-fPIC", "-O1", "-Wall", "-Werror"])
        .arg("-I")
        .arg(root.join("monoruby_capi/include"))
        .arg("-I")
        .arg(root.join("monoruby_ext_sys/include"))
        .args(defines.iter().map(|d| format!("-D{d}")))
        .arg("-o")
        .arg(dir.join(format!("{name}.so")))
        .arg(manifest_dir().join("tests/capi/capi_errors.c"));
    if layer {
        cmd.arg(root.join("monoruby_capi/src/capi.c"))
            .arg(root.join("monoruby_capi/src/st.c"));
    }
    let status = cmd.status().ok()?;
    assert!(status.success(), "building {name} failed");
    Some(dir)
}

/// The API's error paths that CRuby leaves undefined: each one raises
/// instead of crashing, and a registered address is a GC root until it
/// is unregistered.
#[test]
fn capi_api_errors() {
    let Some(dir) = build_errors("capi_errors", &[], true) else {
        eprintln!("skipped: no C compiler");
        return;
    };
    let out = run(monoruby(&dir).args([
        "-e",
        r#"require "capi_errors"
E = CapiErrors
def t = (yield; :ok) rescue [$!.class, $!.message]
p t { E.rstring_ptr(1) }
p t { E.resize(1) }
p t { E.resize("ab".freeze) }
p t { E.cat(nil) }
p t { E.ary_len(1) }
p t { E.ary_push([].freeze) }
p t { E.hash_aset({}.freeze) }
p t { E.define_on("str") }
p E.register
class Str < String; end
p t { E.alloc_on(Str) }
p t { E.alloc_on("str") }
p t { E.undef_alloc_on(1) }
p t { E.data_ptr("str") }
p t { E.data_ptr(E::Bad.new) }
p E.typed?(Object.new), E.typed?(nil)"#,
    ]));
    assert_eq!(
        out,
        r#"[TypeError, "no implicit conversion of Integer into String"]
[TypeError, "no implicit conversion of Integer into String"]
[FrozenError, "can't modify frozen String: \"ab\""]
[TypeError, "no implicit conversion of nil into String"]
[TypeError, "Array expected"]
[FrozenError, "can't modify frozen Array: []"]
[FrozenError, "can't modify frozen Hash: {}"]
[TypeError, "define_method: class/module expected"]
"registered"
[TypeError, "instances of Str cannot carry a native payload"]
[TypeError, "class_set_alloc: class expected"]
[TypeError, "class_set_alloc: class expected"]
[TypeError, "wrong argument type String (expected Data)"]
[TypeError, "allocator of CapiErrors::Bad did not return a Data object"]
false
false
"#
    );
}

/// Loader failures: no `Init_<name>`, an `Init_` that raises, and an entry
/// point reporting a newer layer or an unknown status. Each is a
/// LoadError (or the initializer's own exception), never a crash.
#[test]
fn capi_load_errors() {
    let cases: [(&str, &[&str], bool); 5] = [
        ("libcapi_noinit", &["CAPI_NO_INIT"], true),
        ("capi_raises", &["CAPI_INIT_RAISES", "CAPI_INIT=Init_capi_raises"], true),
        ("capi_super", &["CAPI_INIT_SUPER", "CAPI_INIT=Init_capi_super"], true),
        ("capi_newer", &["CAPI_RC=-2", "CAPI_INIT=Init_capi_newer"], false),
        ("capi_status", &["CAPI_RC=5", "CAPI_INIT=Init_capi_status"], false),
    ];
    let mut expected = vec![];
    for (name, defines, layer) in cases {
        let Some(dir) = build_errors(name, defines, layer) else {
            eprintln!("skipped: no C compiler");
            return;
        };
        let so = dir.join(format!("{name}.so")).display().to_string();
        let out = run(monoruby(&dir).args([
            "-e",
            &format!(r##"begin; require {name:?}; rescue ScriptError, StandardError => e; puts "#{{e.class}}: #{{e.message.sub({so:?}, "SO")}}"; end"##),
        ]));
        expected.push(out);
    }
    assert_eq!(
        expected.concat(),
        "LoadError: SO is not a Ruby extension: no Init_libcapi_noinit
RuntimeError: Init failed
RuntimeError: super called outside of method
LoadError: SO was built against a newer monoruby C API than this interpreter provides
LoadError: SO returned 5 from Init_capi_status
"
    );
}
