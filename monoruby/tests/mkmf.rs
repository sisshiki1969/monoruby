//! Building a C extension *for* monoruby the way one is built for CRuby
//! (`doc/c_api.md` §3.10): `RbConfig` names the C API layer's headers and
//! archive, so `extconf.rb` + `make` produce an extension of monoruby, and
//! `gem install` of a gem with a C extension builds it the same way and
//! installs it into monoruby's own gem home. `tests/capi/capi_gem/` is the
//! gem; it is also built and installed on the host CRuby, and the same
//! script must print the same thing on both.

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
    has("ruby", "--version") && has("gem", "--version")
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
        let host = run(Command::new("ruby").args(["-e", "print Gem.default_dir"]));
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

/// `gem build` + `gem install` of the fixture gem, on monoruby (through
/// the installed `bin/gem`) and on the host CRuby (through its `gem`);
/// the extension is built by each for itself, and the same script prints
/// the same thing on both.
#[test]
fn gem_install_builds_the_extension() {
    if !have_build_tools() {
        eprintln!("skipped: no C compiler or make");
        return;
    }
    let mono_gem = install_root().join("bin/gem");
    let mono = |dir: &Path| {
        let mut c = monoruby();
        c.current_dir(dir);
        (c, vec![mono_gem.to_str().unwrap().to_string()])
    };
    let cruby = |dir: &Path| {
        let mut c = Command::new("ruby");
        c.current_dir(dir);
        (c, vec!["-S".to_string(), "gem".to_string()])
    };
    let on_monoruby = build_and_install("monoruby", &mono);
    assert!(on_monoruby.contains("\"hello, gem (sizeof(long) = 8)\""), "{on_monoruby}");
    assert!(on_monoruby.ends_with("[\"ext/capi_gem/extconf.rb\"]\n1\n"), "{on_monoruby}");
    if !have_cruby() {
        eprintln!("no host ruby/gem: the CRuby side of the comparison was skipped");
        return;
    }
    let on_cruby = build_and_install("cruby", &cruby);
    assert_eq!(on_monoruby, on_cruby);
}

/// Copies the fixture gem to a work dir, builds the `.gem` and installs
/// it into a gem home of its own with the given interpreter (`gem_cmd`
/// yields the interpreter command and the arguments that reach its
/// `gem`), then runs `run.rb` against that home and returns its output.
fn build_and_install(flavor: &str, gem_cmd: &dyn Fn(&Path) -> (Command, Vec<String>)) -> String {
    let dir = work_dir(&format!("gem/{flavor}"));
    let src = dir.join("src");
    copy_dir(&fixture(), &src);
    let home = dir.join("home");
    let gem = |args: &[&str]| {
        let (mut cmd, mut argv) = gem_cmd(&src);
        argv.extend(args.iter().map(|s| s.to_string()));
        cmd.args(argv)
            .env("GEM_HOME", &home)
            .env("GEM_PATH", &home)
            .env_remove("RUBYOPT")
            .env_remove("BUNDLE_GEMFILE");
        cmd
    };
    let out = run(&mut gem(&["build", "capi_gem.gemspec"]));
    assert!(src.join("capi_gem-0.1.0.gem").is_file(), "{out}");
    let out = run(&mut gem(&[
        "install",
        "--local",
        "--no-document",
        "--ignore-dependencies",
        "--install-dir",
        home.to_str().unwrap(),
        "capi_gem-0.1.0.gem",
    ]));
    assert!(out.contains("Building native extensions"), "{out}");
    assert!(out.contains("1 gem installed"), "{out}");
    // rubygems marks the build done, and keeps a copy of the extension
    // next to the gem's Ruby (`Gem.install_extension_in_lib`).
    let gem_dir = home.join("gems/capi_gem-0.1.0");
    assert!(gem_dir.join("lib/capi_gem.rb").is_file());
    let built: Vec<PathBuf> = std::fs::read_dir(home.join("extensions"))
        .unwrap()
        .flat_map(|plat| std::fs::read_dir(plat.unwrap().path()).unwrap())
        .map(|api| api.unwrap().path().join("capi_gem-0.1.0"))
        .collect();
    assert_eq!(built.len(), 1, "{built:?}");
    assert!(built[0].join("gem.build_complete").is_file(), "{built:?}");
    let (mut cmd, _) = gem_cmd(&src);
    run(cmd
        .arg(fixture().join("run.rb"))
        .env("GEM_HOME", &home)
        .env("GEM_PATH", &home)
        .env_remove("RUBYOPT")
        .env_remove("BUNDLE_GEMFILE"))
}
