//! Building a C extension *for* monoruby the way one is built for CRuby
//! (`doc/c_api.md` §3.10): `RbConfig` names the C API layer's headers and
//! archive, so `extconf.rb` + `make` produce an extension of monoruby, and
//! `gem install` of a gem with a C extension builds it the same way and
//! installs it into monoruby's own gem home. `tests/capi/capi_gem/` is the
//! gem. The `gem build` + `gem install` round trip on both interpreters,
//! and the rebuild of a gem the *host* CRuby installed, are `bin/test-gems`
//! (run by `bin/test`), not cargo tests: they depend on the host's `gem`
//! and gem cache and failed on developer machines for reasons outside the
//! interpreter.

use std::path::{Path, PathBuf};
use std::process::Command;

fn manifest_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn fixture() -> PathBuf {
    manifest_dir().join("tests/capi/capi_gem")
}

fn monoruby() -> Command {
    Command::new(env!("CARGO_BIN_EXE_monoruby"))
}

fn run(cmd: &mut Command) -> String {
    let out = cmd.output().unwrap();
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
    if !out.status.success() {
        // mkmf's own log says which compile or link an `extconf.rb` failed
        // on; the failure message alone ("checking for ... no") does not.
        let logs: Vec<String> = cmd
            .get_current_dir()
            .into_iter()
            .flat_map(|d| [d.join("mkmf.log"), d.join("home")])
            .flat_map(|p| {
                if p.is_file() {
                    vec![p]
                } else {
                    glob_logs(&p)
                }
            })
            .map(|p| format!("--- {}\n{}", p.display(), std::fs::read_to_string(&p).unwrap_or_default()))
            .collect();
        panic!(
            "{cmd:?} failed\n--- stdout\n{stdout}\n--- stderr\n{}\n{}",
            String::from_utf8_lossy(&out.stderr),
            logs.join("\n")
        );
    }
    stdout
}

/// Every `mkmf.log` / `gem_make.out` under `dir` (a gem home, after a
/// failed `gem install`).
fn glob_logs(dir: &Path) -> Vec<PathBuf> {
    let mut found = Vec::new();
    let Ok(entries) = std::fs::read_dir(dir) else {
        return found;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            found.extend(glob_logs(&path));
        } else if matches!(
            path.file_name().and_then(|n| n.to_str()),
            Some("mkmf.log" | "gem_make.out")
        ) {
            found.push(path);
        }
    }
    found
}

fn has(cmd: &str, arg: &str) -> bool {
    Command::new(cmd)
        .arg(arg)
        .output()
        .is_ok_and(|o| o.status.success())
}

/// A C compiler and `make` are what mkmf needs; a host `ruby` is what the
/// CRuby side of the comparison needs.
fn have_build_tools() -> bool {
    let cc = std::env::var("CC").unwrap_or_else(|_| "cc".to_string());
    has(&cc, "--version") && has("make", "--version")
}

fn have_cruby() -> bool {
    // The harness's Ruby, and the `gem` script next to it (not `gem` on
    // PATH, which may belong to another Ruby or be a version manager's shim).
    let ruby = monoruby::tests::ruby_path();
    has(ruby, "--version") && Path::new(ruby).with_file_name("gem").is_file()
}

/// A fresh directory under the target's tmp dir.
fn work_dir(name: &str) -> PathBuf {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("mkmf").join(name);
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    dir
}

fn copy_dir(src: &Path, dst: &Path) {
    std::fs::create_dir_all(dst).unwrap();
    for entry in std::fs::read_dir(src).unwrap() {
        let entry = entry.unwrap();
        let to = dst.join(entry.file_name());
        if entry.file_type().unwrap().is_dir() {
            copy_dir(&entry.path(), &to);
        } else {
            std::fs::copy(entry.path(), &to).unwrap();
        }
    }
}

/// `RbConfig::CONFIG[key]` as monoruby reports it.
fn config(key: &str) -> String {
    run(monoruby().args(["-e", &format!("print RbConfig::CONFIG[{key:?}]")]))
}

/// The install root, from the header directory RbConfig names.
fn install_root() -> PathBuf {
    PathBuf::from(config("rubyhdrdir")).parent().unwrap().to_path_buf()
}

/// A CRuby API function the layer does not implement: the extension that
/// references it still builds and loads (the stubs archive on the
/// extension's link line, via EXTDLDFLAGS, defines it), calling it raises
/// NotImplementedError naming the function, and `have_func` -- whose
/// conftest links the layer's archive alone -- still answers "no", so an
/// extension's `#ifdef HAVE_RB_FOO` fallbacks stay in force.
#[test]
fn unimplemented_api_loads_and_raises_not_implemented() {
    if !have_build_tools() {
        eprintln!("skipped: no C compiler or make");
        return;
    }
    // The first name of the API list the layer neither defines (found the
    // way build.rs finds it: the name at column 0, then `(`) nor declares
    // in its headers -- the test source declares it itself -- so the test
    // follows the layer as it grows.
    let capi = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../monoruby_capi");
    let defined: std::collections::HashSet<String> = ["src/capi.c", "src/st.c"]
        .iter()
        .flat_map(|f| std::fs::read_to_string(capi.join(f)).unwrap().lines().map(String::from).collect::<Vec<_>>())
        .filter_map(|l| {
            let (name, _) = l.split_once('(')?;
            (!name.is_empty()
                && name.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
                && !name.starts_with(|c: char| c.is_ascii_digit()))
            .then(|| name.to_string())
        })
        .collect();
    let mut headers = String::new();
    let mut dirs = vec![capi.join("include")];
    while let Some(d) = dirs.pop() {
        for e in std::fs::read_dir(d).unwrap() {
            let p = e.unwrap().path();
            if p.is_dir() {
                dirs.push(p);
            } else {
                headers.push_str(&std::fs::read_to_string(p).unwrap());
            }
        }
    }
    let list = std::fs::read_to_string(capi.join("src/cruby_api.txt")).unwrap();
    let missing = list
        .lines()
        .map(str::trim)
        .find(|n| n.starts_with("rb_") && !defined.contains(*n) && !headers.contains(*n))
        .expect("the layer implements and declares the whole API list?");
    let dir = work_dir("unimplemented");
    let c_src = r#"#include <ruby.h>
void NAME(void);
static VALUE call_it(VALUE self) { (void)self; NAME(); return Qnil; }
void Init_unimpl(void) {
    VALUE m = rb_define_module("Unimpl");
    rb_define_singleton_method(m, "call_it", call_it, 0);
}
"#;
    std::fs::write(dir.join("unimpl.c"), c_src.replace("NAME", missing)).unwrap();
    std::fs::write(
        dir.join("extconf.rb"),
        format!(
            "require 'mkmf'
             have_func('rb_str_new_cstr')
             have_func('{missing}')
             create_makefile('unimpl')
"
        ),
    )
    .unwrap();
    run(monoruby().current_dir(&dir).arg("extconf.rb"));
    let log = std::fs::read_to_string(dir.join("mkmf.log")).unwrap();
    assert!(log.contains("checking for rb_str_new_cstr()... -------------------- yes"), "{log}");
    assert!(log.contains(&format!("checking for {missing}()... -------------------- no")), "{log}");
    let makefile = std::fs::read_to_string(dir.join("Makefile")).unwrap();
    assert!(makefile.contains("-lmonoruby_capi_stubs"), "{makefile}");
    run(Command::new("make").current_dir(&dir));
    let so = dir.join(format!("unimpl.{}", config("DLEXT")));
    assert!(so.is_file(), "{} was not built", so.display());
    let out = run(monoruby().args([
        "-e",
        &format!(
            r#"require {so:?}; begin; Unimpl.call_it; rescue NotImplementedError => e; puts e.message; end"#
        ),
    ]));
    assert_eq!(out.trim(), format!("{missing}() is not implemented in monoruby's C API"), "{out}");
}

/// `libdirname` names the key of the directory that holds LIBRUBY, and
/// that directory is where the archive is: ruby/spec's C API helper
/// stats `CONFIG[CONFIG['libdirname']]/LIBRUBY`, and mkmf puts
/// `-L$(<libdirname>)` on every link line, so both must reach capi/
/// while `libdir` (and the gem dirs derived from it) stay in lib/.
#[test]
fn rbconfig_libdirname_locates_the_archive() {
    let root = install_root();
    let out = run(monoruby().args([
        "-e",
        r#"c = RbConfig::CONFIG
puts c["libdirname"], c[c["libdirname"]], File.file?(File.join(c[c["libdirname"]], c["LIBRUBY"])), c["libdir"]
puts RbConfig::MAKEFILE_CONFIG["capidir"]"#,
    ]));
    let lines: Vec<&str> = out.lines().collect();
    let capi = root.join("capi");
    assert_eq!(lines[0], "capidir", "{out}");
    assert_eq!(lines[1], capi.to_str().unwrap(), "{out}");
    assert_eq!(lines[2], "true", "{out}");
    // `libdir` is the snapshot host's (whatever that was); it is not
    // redirected, only `libdirname` points elsewhere.
    assert_ne!(lines[3], capi.to_str().unwrap(), "{out}");
    assert!(lines[3].ends_with("/lib"), "{out}");
    assert_eq!(lines[4], capi.to_str().unwrap(), "{out}");
}

/// `RbConfig` names the layer: its headers, its archive, and a gem home
/// that is monoruby's own.
#[test]
fn rbconfig_names_the_layer() {
    let root = install_root();
    let out = run(monoruby().args([
        "-e",
        r#"puts RbConfig::CONFIG.values_at("rubyhdrdir", "rubyarchhdrdir", "sitehdrdir", "LIBRUBYARG", "LIBRUBYARG_STATIC", "LIBRUBY", "ruby_version")
puts RbConfig::MAKEFILE_CONFIG.values_at("LIBRUBYARG_SHARED", "rubyarchhdrdir")
require "rubygems"
puts Gem.default_dir, Gem.user_dir, Gem.default_specifications_dir, Gem.path.include?(Gem.default_dir)
require "mkmf"
puts $hdrdir, $arch_hdrdir, $LIBRUBYARG"#,
    ]));
    let lines: Vec<&str> = out.lines().collect();
    let include = root.join("include");
    let include = include.to_str().unwrap();
    let libarg = format!("-L{} -lmonoruby_capi", root.join("capi").display());
    assert_eq!(lines[0], include, "{out}");
    assert_eq!(lines[1], include, "{out}");
    assert_eq!(lines[2], format!("{include}/site_ruby"), "{out}");
    assert_eq!(lines[3], libarg, "{out}");
    assert_eq!(lines[4], libarg, "{out}");
    assert_eq!(lines[5], "libmonoruby_capi.a", "{out}");
    let api_version = lines[6];
    assert_eq!(lines[7], libarg, "{out}");
    assert_eq!(lines[8], include, "{out}");
    // The gem home is monoruby's, under ~/.monoruby, and on Gem.path.
    assert!(
        lines[9].ends_with(&format!("/.monoruby/gems/{api_version}")),
        "{out}"
    );
    assert!(lines[10].ends_with(&format!("/monoruby/{api_version}")), "{out}");
    assert!(lines[11].ends_with("/specifications/default"), "{out}");
    assert_eq!(lines[12], "true", "{out}");
    // mkmf picked them up.
    assert_eq!(lines[13], include, "{out}");
    assert_eq!(lines[14], include, "{out}");
    assert_eq!(lines[15], libarg, "{out}");
    // What they name exists: the headers mkmf makes every object depend
    // on, and the archive build.rs compiled.
    for h in [
        "ruby.h",
        "ruby/ruby.h",
        "ruby/config.h",
        "ruby/backward.h",
        "ruby/intern.h",
        "ruby/st.h",
        "ruby/subst.h",
        "ruby/defines.h",
        "ruby/missing.h",
    ] {
        assert!(root.join("include").join(h).is_file(), "missing include/{h}");
    }
    assert!(
        root.join("capi/libmonoruby_capi.a").is_file(),
        "capi/libmonoruby_capi.a was not installed"
    );
    assert!(root.join("bin/gem").is_file(), "bin/gem was not installed");
    // Not the host CRuby's gem home.
    if have_cruby() {
        let host = run(Command::new(monoruby::tests::ruby_path()).args(["-e", "print Gem.default_dir"]));
        assert_ne!(lines[9], host, "{out}");
    }
}

/// `extconf.rb` + `make` by hand: the probes answer for the layer, the
/// build links it, and the result loads — by path and, after
/// `make install`, from `$LOAD_PATH`.
#[test]
fn extconf_and_make_build_an_extension() {
    if !have_build_tools() {
        eprintln!("skipped: no C compiler or make");
        return;
    }
    let dir = work_dir("extconf");
    copy_dir(&fixture().join("ext/capi_gem"), &dir);
    run(monoruby().current_dir(&dir).arg("extconf.rb"));
    let log = std::fs::read_to_string(dir.join("mkmf.log")).unwrap();
    assert!(dir.join("Makefile").is_file());
    assert!(
        log.contains("have_func: checking for rb_str_new_cstr() in ruby.h... -------------------- yes"),
        "{log}"
    );
    assert!(
        log.contains("have_func: checking for rb_no_such_function_xyzzy() in ruby.h... -------------------- no"),
        "{log}"
    );
    assert!(
        log.contains("have_type: checking for rb_data_type_t in ruby.h... -------------------- yes"),
        "{log}"
    );
    let makefile = std::fs::read_to_string(dir.join("Makefile")).unwrap();
    let root = install_root();
    assert!(
        makefile.contains(&format!("LIBRUBYARG_SHARED = -L{} -lmonoruby_capi", root.join("capi").display())),
        "{makefile}"
    );
    assert!(
        makefile.contains(&format!("hdrdir = {}", root.join("include").display()))
            || makefile.contains(&format!("topdir = {}", root.join("include").display())),
        "{makefile}"
    );
    run(Command::new("make").current_dir(&dir));
    let dlext = config("DLEXT");
    let so = dir.join(format!("capi_gem.{dlext}"));
    assert!(so.is_file(), "{} was not built", so.display());
    let out = run(monoruby().args([
        "-e",
        &format!(
            r#"require {so:?}; p CapiGem::BUILT_WITH, CapiGem.greet("make"), CapiGem::Counter.new.incr"#,
            so = so.display().to_string()
        ),
    ]));
    assert_eq!(
        out,
        "\"monoruby\"\n\"hello, make (sizeof(long) = 8)\"\n1\n"
    );
    // `make install` as rubygems drives it: into a site dir of its choosing.
    let inst = dir.join("inst");
    run(Command::new("make")
        .current_dir(&dir)
        .arg("install")
        .arg(format!("sitearchdir={}", inst.display()))
        .arg(format!("sitelibdir={}", inst.display())));
    assert!(inst.join("capi_gem").join(format!("capi_gem.{dlext}")).is_file());
    let out = run(monoruby()
        .arg("-I")
        .arg(&inst)
        .args(["-e", r#"require "capi_gem/capi_gem"; p CapiGem.sum(1, 2) { |s| s * 7 }"#]));
    assert_eq!(out, "21\n");
}
