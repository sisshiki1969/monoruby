//! Integration coverage for the deferred rubygems boot
//! (`builtins/gem_prelude.rb`). Whether rubygems was *loaded* is a
//! process-level fact (`$LOADED_FEATURES` of a fresh interpreter), so
//! everything here spawns the real binary; the in-process `run_test`
//! interpreters run with gems disabled.

use std::process::Command;

fn monoruby() -> Command {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_monoruby"));
    cmd.env_remove("RUBYOPT")
        .env_remove("RUBYLIB")
        .env_remove("RUBYPATH");
    cmd
}

fn run(args: &[&str], code: &str) -> String {
    let out = monoruby()
        .args(args)
        .arg("-e")
        .arg(code)
        .output()
        .expect("failed to spawn monoruby");
    assert!(
        out.status.success(),
        "monoruby exited with {:?}\nstderr: {}",
        out.status,
        String::from_utf8_lossy(&out.stderr)
    );
    String::from_utf8_lossy(&out.stdout).trim().to_string()
}

const RUBYGEMS_LOADED: &str = r#"$LOADED_FEATURES.any? { |f| f.end_with?("/rubygems.rb") }"#;

#[test]
fn a_program_that_never_touches_gem_never_loads_rubygems() {
    // The constant is there (defined, listed, autoload-registered) but
    // rubygems itself has not been read.
    let got = run(
        &[],
        &format!(
            r#"p [{RUBYGEMS_LOADED}, defined?(Gem), Object.const_defined?(:Gem),
                  Object.constants.include?(:Gem), Object.autoload?(:Gem)]"#
        ),
    );
    assert_eq!(got, r#"[false, "constant", true, true, "rubygems"]"#);
}

#[test]
fn the_first_reference_to_gem_boots_rubygems() {
    let got = run(
        &[],
        &format!(
            r#"before = {RUBYGEMS_LOADED}
               v = Gem::VERSION
               p [before, v.class, {RUBYGEMS_LOADED}, Object.autoload?(:Gem), Gem.respond_to?(:loaded_specs)]"#
        ),
    );
    assert_eq!(got, "[false, String, true, nil, true]");
}

#[test]
fn an_explicit_require_of_rubygems_still_works() {
    // `require "rubygems"` while the autoload is pending is the direct
    // load of the autoload's own file: it defines `Gem` and consumes the
    // registration, and a later reference does not load it again.
    let got = run(
        &[],
        &format!(
            r#"r = require "rubygems"
               p [r, {RUBYGEMS_LOADED}, Gem::Specification.name, require("rubygems")]"#
        ),
    );
    assert_eq!(got, r#"[true, true, "Gem::Specification", false]"#);
}

#[test]
fn kernel_gem_boots_rubygems_and_dispatches() {
    // The stub `Kernel#gem` boots rubygems and hands over to the real
    // one, which raises rubygems' own error (with its own message) for
    // an unknown gem. Both the stub and the real method are private, as
    // in CRuby.
    let got = run(
        &[],
        &format!(
            r#"priv = Kernel.private_method_defined?(:gem)
               begin
                 gem "monoruby-no-such-gem-xyz"
               rescue Gem::MissingSpecError => e
                 kind = e.class
                 msg = e.message.start_with?("Could not find 'monoruby-no-such-gem-xyz' (>= 0)")
               end
               p [priv, kind, msg, {RUBYGEMS_LOADED}, Kernel.private_method_defined?(:gem)]"#
        ),
    );
    assert_eq!(got, "[true, Gem::MissingSpecError, true, true, true]");
}

#[test]
fn a_stub_library_does_not_boot_rubygems() {
    // `json` and `set` resolve from monoruby's stub directory; requiring
    // them must not drag rubygems in (the activation hook only asks a
    // rubygems that is already loaded).
    let got = run(
        &[],
        &format!(
            r#"require "json"
               require "set"
               p [JSON.generate([1]), Set.new([1]).size, {RUBYGEMS_LOADED}]"#
        ),
    );
    assert_eq!(got, r#"["[1]", 1, false]"#);
}

#[test]
fn a_host_gem_boots_rubygems_and_is_activated() {
    // A file served from a host gem's directory (`/gems/`) is proof the
    // program uses host gems: the activation hook boots rubygems through
    // the autoload and activates the gem, so `Gem.loaded_specs` fills in
    // as in CRuby. `rake` is a bundled gem of every Ruby, but only
    // reachable if a host Ruby was found; let the process report that.
    let got = run(
        &[],
        &format!(
            r#"before = {RUBYGEMS_LOADED}
               begin
                 require "rake"
               rescue LoadError => e
                 print "skip: #{{e.message}}"
                 exit 0
               end
               p [before, {RUBYGEMS_LOADED}, Gem.loaded_specs.key?("rake"),
                  $LOADED_FEATURES.grep(%r{{/rake\.rb\z}}).first&.include?("/gems/")]"#
        ),
    );
    if let Some(reason) = got.strip_prefix("skip: ") {
        eprintln!("skipped: no host rake gem here ({reason})");
        return;
    }
    assert_eq!(got, "[false, true, true, true]");
}

#[test]
fn disable_gems_leaves_gem_undefined() {
    let got = run(
        &["--disable=gems"],
        &format!(r#"p [defined?(Gem), Object.autoload?(:Gem), {RUBYGEMS_LOADED}]"#),
    );
    assert_eq!(got, "[nil, nil, false]");
}

/// A host gem directory monoruby started with stays on `Gem.path` after
/// the program clears `GEM_PATH` itself. ruby-bench's harness does that
/// before it has the host's `bundle install` put gems there; since
/// `Gem.default_dir` is monoruby's own gem home, the fallback `Gem.path`
/// has to bring the host directories along (`MONORUBY_HOST_GEM_PATH`,
/// `rubygems/defaults/operating_system.rb`).
#[test]
fn clearing_gem_path_keeps_the_host_gem_dirs() {
    let root =
        std::env::temp_dir().join(format!("monoruby-gem-path-fallback-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&root);
    let host = root.join("hostgems");
    let home = root.join("home");
    std::fs::create_dir_all(host.join("specifications")).unwrap();
    std::fs::create_dir_all(host.join("gems/mrbfake-1.0/lib")).unwrap();
    std::fs::create_dir_all(&home).unwrap();
    std::fs::write(
        host.join("specifications/mrbfake-1.0.gemspec"),
        r#"Gem::Specification.new do |s|
  s.name = "mrbfake"
  s.version = "1.0"
  s.summary = "fixture"
  s.authors = ["monoruby"]
  s.files = ["lib/mrbfake.rb"]
  s.require_paths = ["lib"]
end
"#,
    )
    .unwrap();
    std::fs::write(
        host.join("gems/mrbfake-1.0/lib/mrbfake.rb"),
        "MRBFAKE = :loaded\n",
    )
    .unwrap();
    // rubygems realpaths every `Gem.path` entry (`Gem::PathSupport#expand`),
    // and macOS's temp dir sits behind the `/var` -> `/private/var` symlink.
    let host = host.canonicalize().unwrap();
    let host_s = host.to_str().unwrap();
    let cases = [
        // Cleared before rubygems boots, as the harness does.
        r#"ENV.merge!("GEM_HOME" => nil, "GEM_PATH" => nil)
           gem "mrbfake"; require "mrbfake"
           p [MRBFAKE, Gem.path.include?(HOST)]"#,
        // Cleared after it booted, then the paths recomputed.
        r#"Gem::VERSION
           ENV["GEM_PATH"] = nil; Gem.clear_paths
           require "mrbfake"
           p [MRBFAKE, Gem.path.include?(HOST)]"#,
    ];
    for code in cases {
        let out = monoruby()
            .env("HOME", &home)
            .env("GEM_PATH", host_s)
            .env_remove("GEM_HOME")
            .env_remove("MONORUBY_GEM_PATH")
            .env_remove("MONORUBY_HOST_GEM_PATH")
            .arg("-e")
            .arg(code.replace("HOST", &format!("{host_s:?}")))
            .output()
            .expect("failed to spawn monoruby");
        assert!(
            out.status.success(),
            "monoruby exited with {:?}\nstderr: {}",
            out.status,
            String::from_utf8_lossy(&out.stderr)
        );
        assert_eq!(
            String::from_utf8_lossy(&out.stdout).trim(),
            "[:loaded, true]"
        );
    }
    let _ = std::fs::remove_dir_all(&root);
}
