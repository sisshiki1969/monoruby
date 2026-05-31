use std::path::{Path, PathBuf};
use std::process::Command;
use std::{fs, io};

/// Minimum CRuby version whose stdlib monoruby is willing to wire up at
/// build time. Older Rubies (Debian's system 3.0, for example) lack APIs
/// monoruby relies on and have a different Hash inspect form.
const MIN_RUBY_VERSION: (u32, u32) = (4, 0);

fn ruby_version_ok(ruby_cmd: &str) -> bool {
    let Ok(output) = Command::new(ruby_cmd)
        .args(["-e", "puts RUBY_VERSION"])
        .output()
    else {
        return false;
    };
    if !output.status.success() {
        return false;
    }
    let s = String::from_utf8_lossy(&output.stdout);
    let mut parts = s.trim().split('.').map(|p| p.parse::<u32>().ok());
    let major = parts.next().flatten();
    let minor = parts.next().flatten();
    match (major, minor) {
        (Some(maj), Some(min)) => (maj, min) >= MIN_RUBY_VERSION,
        _ => false,
    }
}

/// Pick a `ruby` executable that is at least `MIN_RUBY_VERSION`. Returns
/// `None` if no suitable Ruby is found, in which case build.rs leaves the
/// cached library_path / ruby_version files untouched and prints a warning.
fn find_ruby() -> Option<String> {
    if ruby_version_ok("ruby") {
        return Some("ruby".to_string());
    }
    if let Some(home) = std::env::var_os("HOME") {
        let shim = PathBuf::from(home).join(".rbenv/shims/ruby");
        if let Some(s) = shim.to_str()
            && ruby_version_ok(s)
        {
            return Some(s.to_string());
        }
    }
    if let Some(home) = std::env::var_os("HOME") {
        let rvm = PathBuf::from(home).join(".rvm/bin/ruby");
        if let Some(s) = rvm.to_str()
            && ruby_version_ok(s)
        {
            return Some(s.to_string());
        }
    }
    None
}

fn main() {
    // JIT cfgs. `jit` gates the JIT front-end (bytecode→TraceIR→AsmIR,
    // abstract state, register-allocation planning) — arch-neutral, intended
    // to compile on any JIT-capable arch. `jit_emit` gates the x86-64
    // machine-code emission: the AsmIR→native lowering AND the builtins
    // inline-method generators (`inline_gen!`). On aarch64 the plan is to
    // enable `jit` (front-end) with `jit_emit` off — inlined methods fall back
    // to a plain call, and the AsmIR lowering deopts to the VM on anything not
    // yet supported (see doc/aarch64-jitgen-plan.md). Today both are set
    // together (x86-64 && !no-jit), so this is behavior-neutral.
    // Downstream gates: `#[cfg(jit)]`/`#[cfg(not(jit))]` for the front-end,
    // `#[cfg(jit_emit)]`/`#[cfg(not(jit_emit))]` for emission.
    println!("cargo::rustc-check-cfg=cfg(jit)");
    println!("cargo::rustc-check-cfg=cfg(jit_emit)");
    let target_x86 = std::env::var("CARGO_CFG_TARGET_ARCH").as_deref() == Ok("x86_64");
    let no_jit = std::env::var_os("CARGO_FEATURE_NO_JIT").is_some();
    if target_x86 && !no_jit {
        println!("cargo::rustc-cfg=jit");
        println!("cargo::rustc-cfg=jit_emit");
    }

    let lib_path = dirs::home_dir().unwrap().join(".monoruby");

    let lib_dir = lib_path.join("lib");
    let builtins_dir = lib_path.join("builtins");
    // `stub` holds *only* monoruby's own C-extension replacement stubs
    // (the `stdlib/` and `gem/` trees). The require resolver
    // (`search_lib`) pins this directory ahead of `$LOAD_PATH` so those
    // stubs win even after rubygems/bundler activates a host gem and
    // unshifts its lib dir to the front of `$LOAD_PATH`. It deliberately
    // excludes the vendored CRuby snapshot (bundler / rubygems), whose
    // code version must stay in lockstep with the activated gem spec.
    let stub_dir = lib_path.join("stub");
    // Order matters: the checked-in CRuby stdlib snapshot is laid down
    // first, then monoruby's own builtins/stdlib/gem stubs overwrite any
    // name clash so monoruby's host-independent implementations of
    // C-extension-backed libraries stay authoritative. No `ruby` is
    // invoked here — the snapshot is produced offline by
    // bin/vendor-ruby-stdlib, so the build works without CRuby. The
    // `stdlib/` and `gem/` trees are additionally laid down into `stub`
    // (the pinned resolution root; see `search_lib`).
    let sources = [
        (PathBuf::from("vendor/ruby-stdlib"), lib_dir.clone()),
        (PathBuf::from("builtins"), builtins_dir.clone()),
        (PathBuf::from("stdlib"), lib_dir.clone()),
        (PathBuf::from("gem"), lib_dir.clone()),
        (PathBuf::from("stdlib"), stub_dir.clone()),
        (PathBuf::from("gem"), stub_dir.clone()),
    ];

    // Re-run when the installed sources change so edits to the vendored
    // stdlib or monoruby's stubs reinstall on the next build.
    for (src, _) in &sources {
        println!("cargo:rerun-if-changed={}", src.display());
    }
    // Self-heal: track a stamp file we write into ~/.monoruby. If the
    // user deletes ~/.monoruby, the stamp disappears, Cargo sees a
    // tracked input change and re-runs this script, repopulating the
    // directory. Without this, a plain `cargo build` after `rm -rf
    // ~/.monoruby` would NOT recreate it (the build script is cached).
    let stamp = lib_path.join(".build-stamp");
    println!("cargo:rerun-if-changed={}", stamp.display());

    if !lib_path.exists() {
        fs::create_dir(&lib_path).unwrap();
    }
    for p in [&lib_dir, &builtins_dir, &stub_dir] {
        if p.exists() {
            fs::remove_dir_all(p).unwrap();
        }
        fs::create_dir(p).unwrap();
    }

    match find_ruby() {
        Some(ruby) => {
            match Command::new(&ruby).args(["-e", "puts($:)"]).output() {
                Ok(output) => {
                    let dest_path = lib_path.join("library_path");
                    let mut load_path =
                        std::str::from_utf8(&output.stdout).unwrap().to_string();
                    // Per the decoupling policy: the pure-Ruby standard
                    // library and *default* gems are vendored
                    // (monoruby/vendor/ruby-stdlib, copied to
                    // ~/.monoruby/lib which is prepended to $LOAD_PATH so
                    // it shadows the host). *Non-default* gems
                    // (ostruct/base64/Rails/… — anything `gem install`ed)
                    // are intentionally still resolved from the host gem
                    // directory at runtime, so capture every installed
                    // gem's require paths and append them. Default-gem
                    // entries here are harmless: the vendored copies take
                    // priority. rubygems normally adds these lazily on
                    // `require`; the vendored rubygems falls back to
                    // $LOAD_PATH, so they must be present up front.
                    if let Ok(g) = Command::new(&ruby)
                        .args([
                            "-e",
                            "Gem::Specification.latest_specs(true).each{|s| \
                             s.require_paths.each{|p| d=File.join(s.full_gem_path,p); \
                             puts d if Dir.exist?(d)}}",
                        ])
                        .output()
                        && g.status.success()
                    {
                        if !load_path.ends_with('\n') {
                            load_path.push('\n');
                        }
                        load_path.push_str(std::str::from_utf8(&g.stdout).unwrap());
                    }
                    fs::write(dest_path, load_path).unwrap();

                    // Capture host CRuby's `Gem.paths.path` (the gem-root
                    // directories that contain `specifications/*.gemspec`)
                    // so the vendored rubygems can resolve host-installed
                    // gems via `Gem::Specification.find_by_name` — adding
                    // just the require paths to $LOAD_PATH (above) lets
                    // `require` succeed but leaves the rubygems metadata
                    // index empty. Written as a colon-joined string so it
                    // can be propagated verbatim through the GEM_PATH
                    // env-var at runtime.
                    if let Ok(p) = Command::new(&ruby)
                        .args(["-e", "print Gem.paths.path.join(':')"])
                        .output()
                        && p.status.success()
                    {
                        let gem_path = std::str::from_utf8(&p.stdout).unwrap().trim();
                        if !gem_path.is_empty() {
                            fs::write(lib_path.join("gem_path"), gem_path).unwrap();
                        }
                    }
                }
                Err(_) => {
                    println!("cargo:warning=failed to read ruby library path from {ruby}");
                }
            }

            // Bake the build-host Ruby version into the binary as a
            // compile-time env var instead of a runtime file. This keeps
            // differential-test parity with the host Ruby when one is
            // present at build time, while removing any runtime file
            // dependency (and the startup panic when it is absent).
            match Command::new(&ruby)
                .args(["-e", "print(RUBY_VERSION)"])
                .output()
            {
                Ok(output) if output.status.success() => {
                    let ver = String::from_utf8_lossy(&output.stdout);
                    let ver = ver.trim();
                    if !ver.is_empty() {
                        println!("cargo:rustc-env=MONORUBY_RUBY_VERSION={ver}");
                        // Older monoruby builds wrote `~/.monoruby/ruby_version`
                        // and read it back at runtime. The runtime now uses
                        // the baked-in env var instead, but the file lingers
                        // and confuses anyone inspecting it (e.g. `cat
                        // ~/.monoruby/ruby_version` showing stale 3.4.0 long
                        // after upgrading to 4.0). Keep it as a human-
                        // facing breadcrumb of what host Ruby this build
                        // was paired with, refreshed every cargo build.
                        let _ = fs::write(lib_path.join("ruby_version"), ver);
                    }
                }
                _ => {
                    println!("cargo:warning=failed to read ruby version from {ruby}");
                }
            }

            // Bake the build-host Ruby's RUBY_PLATFORM in the same way.
            // The runtime reports it as `RUBY_PLATFORM`, and startup.rb
            // derives `RbConfig::CONFIG["arch"]` from it. On macOS that
            // string carries the Darwin major version (e.g.
            // `arm64-darwin23`); rubygems keys each gem's built-extension
            // directory on it (`extensions/<arch>/<api>/<gem>`), so without
            // the version monoruby looks in the wrong dir and warns that
            // every C-extension gem is unbuilt.
            match Command::new(&ruby)
                .args(["-e", "print(RUBY_PLATFORM)"])
                .output()
            {
                Ok(output) if output.status.success() => {
                    let plat = String::from_utf8_lossy(&output.stdout);
                    let plat = plat.trim();
                    if !plat.is_empty() {
                        println!("cargo:rustc-env=MONORUBY_RUBY_PLATFORM={plat}");
                        // Human-facing breadcrumb, refreshed every build
                        // (mirrors ruby_version above); the runtime reads
                        // the baked-in env var, not this file.
                        let _ = fs::write(lib_path.join("ruby_platform"), plat);
                    }
                }
                _ => {
                    println!("cargo:warning=failed to read ruby platform from {ruby}");
                }
            }
        }
        None => {
            println!(
                "cargo:warning=no Ruby >= {}.{} found on PATH or in rbenv/rvm; \
                 ~/.monoruby/library_path and ruby_version were not refreshed",
                MIN_RUBY_VERSION.0, MIN_RUBY_VERSION.1
            );
        }
    }

    for (src, dst) in &sources {
        copy_dir_all(src, dst).unwrap();
    }

    // Write the stamp last so its presence means a complete install.
    // Its path is tracked above via cargo:rerun-if-changed, so deleting
    // ~/.monoruby makes the next build re-run this script.
    fs::write(&stamp, env!("CARGO_PKG_VERSION")).unwrap();
}

fn copy_dir_all(src: &Path, dst: &Path) -> io::Result<()> {
    if !fs::exists(dst)? {
        fs::create_dir(dst)?;
    }
    for entry in fs::read_dir(src)? {
        let entry = entry?;
        let from = entry.path();
        let to = dst.join(entry.file_name());
        if from.is_dir() {
            copy_dir_all(&from, &to)?;
        } else {
            fs::copy(&from, &to)?;
        }
    }
    Ok(())
}
