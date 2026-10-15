//! A gem bundled from a git source, checked out by the host CRuby's
//! `bundle install`, is found by `Bundler.setup` on monoruby. Bundler looks
//! for the checkout under `Gem.dir/bundler/gems`, and `Gem.dir` is
//! monoruby's own gem home, so `monoruby_hooks/bundler_source_git.rb`
//! (loaded right after `bundler/source/git.rb`) falls back to the same
//! checkout under a host gem directory. ruby-bench's liquid-*, rubyboy and
//! fluentd benchmarks depend on it. Offline: the checkout and the lockfile
//! are fixtures, so Bundler never runs `git`.

use std::process::Command;

fn monoruby() -> Command {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_monoruby"));
    cmd.env_remove("RUBYOPT")
        .env_remove("RUBYLIB")
        .env_remove("RUBYPATH")
        .env_remove("GEM_HOME")
        .env_remove("BUNDLE_GEMFILE")
        .env_remove("BUNDLER_SETUP");
    cmd
}

// Linux only: the fixture lockfile names its platforms, and a host whose
// platform it does not list would make Bundler re-resolve (and go to the
// network) before it ever looks for the checkout.
#[cfg(target_os = "linux")]
#[test]
fn a_git_checkout_under_a_host_gem_dir_is_found() {
    // The host gem directories the runtime probe found; without a host
    // Ruby there is no bundler to set up, so there is nothing to test.
    let probed = monoruby()
        .env_remove("GEM_PATH")
        .args(["-e", r#"print ENV["MONORUBY_HOST_GEM_PATH"]"#])
        .output()
        .expect("failed to spawn monoruby");
    let probed = String::from_utf8_lossy(&probed.stdout).to_string();
    if probed.is_empty() {
        eprintln!("skipped: no host Ruby gem path");
        return;
    }

    let root = std::env::temp_dir().join(format!(
        "monoruby-bundler-host-checkout-{}",
        std::process::id()
    ));
    let _ = std::fs::remove_dir_all(&root);
    let checkout = root.join("host/bundler/gems/mrbgit-0123456789ab");
    let app = root.join("app");
    std::fs::create_dir_all(checkout.join("lib")).unwrap();
    std::fs::create_dir_all(&app).unwrap();
    std::fs::write(
        checkout.join("mrbgit.gemspec"),
        r#"Gem::Specification.new do |s|
  s.name = "mrbgit"
  s.version = "1.0"
  s.summary = "fixture"
  s.authors = ["monoruby"]
  s.files = ["lib/mrbgit.rb"]
  s.require_paths = ["lib"]
end
"#,
    )
    .unwrap();
    std::fs::write(checkout.join("lib/mrbgit.rb"), "MRBGIT = :loaded\n").unwrap();
    std::fs::write(
        app.join("Gemfile"),
        "source \"https://rubygems.org\"\ngem \"mrbgit\", git: \"https://example.invalid/mrbgit.git\"\n",
    )
    .unwrap();
    std::fs::write(
        app.join("Gemfile.lock"),
        "GIT\n  remote: https://example.invalid/mrbgit.git\n  \
         revision: 0123456789abcdef0123456789abcdef01234567\n  specs:\n    mrbgit (1.0)\n\n\
         GEM\n  remote: https://rubygems.org/\n  specs:\n\n\
         PLATFORMS\n  ruby\n  x86_64-linux\n  aarch64-linux\n  arm64-darwin\n\n\
         DEPENDENCIES\n  mrbgit!\n",
    )
    .unwrap();

    let host = root.join("host");
    let out = monoruby()
        .current_dir(&app)
        .env("GEM_PATH", format!("{}:{probed}", host.display()))
        .env("BUNDLE_GEMFILE", app.join("Gemfile"))
        .args([
            "-e",
            // What ruby-bench's harness does around its `bundle install`.
            r#"ENV.merge!("GEM_HOME" => nil, "GEM_PATH" => nil)
               require "bundler"
               Bundler.setup
               require "mrbgit"
               p [MRBGIT, $LOADED_FEATURES.grep(/mrbgit/).first.start_with?(ARGV[0])]"#,
            host.to_str().unwrap(),
        ])
        .output()
        .expect("failed to spawn monoruby");
    let _ = std::fs::remove_dir_all(&root);
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
