//! `Encoding.locale_charmap` / `Encoding.default_external` follow the
//! locale, as CRuby's do (#1433).
//!
//! Everything here spawns the real binary: the locale is read once per
//! process, from the environment the process was started with, so an
//! in-process `run_test` cannot reach it — and `Globals::new_test` pins
//! `default_external` to UTF-8 anyway, matching the `-E UTF-8` the
//! differential harness spawns its reference CRuby with.
//!
//! The expectations are differential rather than literal. The codeset's
//! spelling is the platform's (`ANSI_X3.4-1968` on glibc, `US-ASCII` on
//! macOS), `C.UTF-8` exists on some hosts and not others, and a CI image
//! may or may not have `en_US.UTF-8` generated — so each case asks the
//! reference CRuby what it answers under the same environment and
//! requires the same answer, which is the property the issue is about.

use std::process::Command;

/// The locale environments worth pinning. Each is a list of
/// `(variable, Some(value) | None)`, applied on top of an environment
/// with both `LANG` and `LC_ALL` cleared.
const LOCALES: &[(&str, &[(&str, Option<&str>)])] = &[
    ("no locale at all", &[]),
    ("LC_ALL=C", &[("LC_ALL", Some("C"))]),
    ("LC_ALL=POSIX", &[("LC_ALL", Some("POSIX"))]),
    ("LANG=C.UTF-8", &[("LANG", Some("C.UTF-8"))]),
    ("LANG=en_US.UTF-8", &[("LANG", Some("en_US.UTF-8"))]),
    // `LC_ALL` wins over `LANG`, as it does for `setlocale`.
    (
        "LC_ALL=C over LANG=C.UTF-8",
        &[("LANG", Some("C.UTF-8")), ("LC_ALL", Some("C"))],
    ),
];

fn apply(cmd: &mut Command, env: &[(&str, Option<&str>)]) {
    cmd.env_remove("RUBYOPT")
        .env_remove("RUBYLIB")
        .env_remove("RUBYPATH")
        .env_remove("LANG")
        .env_remove("LC_ALL")
        .env_remove("LC_CTYPE");
    for (key, val) in env {
        match val {
            Some(v) => cmd.env(key, v),
            None => cmd.env_remove(key),
        };
    }
}

fn monoruby(env: &[(&str, Option<&str>)]) -> Command {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_monoruby"));
    apply(&mut cmd, env);
    cmd.arg("--disable=gems");
    cmd
}

fn cruby(env: &[(&str, Option<&str>)]) -> Command {
    let mut cmd = Command::new(monoruby::tests::ruby_path());
    apply(&mut cmd, env);
    cmd.arg("--disable=gems,rubyopt");
    cmd
}

/// stdout of running `body` as a script file, panicking on a non-zero
/// exit with whatever went to stderr.
fn run_script(mut cmd: Command, path: &std::path::Path) -> String {
    let out = cmd.arg(path).output().expect("failed to spawn");
    assert!(
        out.status.success(),
        "{:?} exited with {:?}\nstderr: {}",
        cmd,
        out.status,
        String::from_utf8_lossy(&out.stderr)
    );
    String::from_utf8_lossy(&out.stdout).into_owned()
}

fn write_script(name: &str, body: &str) -> std::path::PathBuf {
    let path = std::env::temp_dir().join(name);
    std::fs::write(&path, body).expect("write script");
    path
}

/// Both halves of the issue, in one script: what the charmap is, what
/// `default_external` became, and the `#inspect` escaping that follows
/// from it.
const PROBE: &str = r##"
puts Encoding.locale_charmap
puts Encoding.default_external
puts Encoding.find("locale")
puts Encoding.find("external")
puts Encoding.find("filesystem")
p Encoding.default_internal
p "\u3044"
p "caf\u00e9"
p "plain ascii"
p :"\u3042"
p ["\u3044", :"\u3042", {"\u304D" => :"\u304F"}]
p({"\u3042": 1, "\u3044" => "\u3046"})
p "\u3044".inspect.encoding
p "abc".inspect.encoding
p :"\u3042".inspect.encoding
p ["\u3044"].inspect.encoding
p({"\u3044" => 1}.inspect.encoding)
p "\u3044".encoding, "\u3044".size, "\u3044".bytes.size
# `puts`, `to_s` and interpolation are not `#inspect` and never escape.
puts "\u3044"
puts ["\u3044", :"\u3042"].to_s
puts "#{"\u3044"}!"
# An `#inspect` of the object's own making is escaped by `p` (CRuby's
# `rb_inspect`), not by the method itself.
class Own; def inspect; "\u3046own"; end; end
p Own.new
class Priv; private; def inspect; "\u3048priv"; end; end
p Priv.new
begin; raise ArgumentError, "\u3044"; rescue => e; p e, e.message; end
# A recursive container still renders, and an exotic result encoding
# tags the rendering rather than defaulting to US-ASCII.
h = {}; h[:a] = h; p h
Encoding.default_external = Encoding::EUC_JP
p "\u3044", "abc".inspect.encoding, ["\u3044"].inspect.encoding
p({"\u3044" => 1}.inspect.encoding)
p :"\u3042".inspect.encoding
"##;

#[test]
fn locale_charmap_and_default_external_match_cruby() {
    let path = write_script("mr_enc_locale_probe.rb", PROBE);
    for (label, env) in LOCALES {
        let expected = run_script(cruby(env), &path);
        let actual = run_script(monoruby(env), &path);
        // Name the first line that differs, and say which platform arm
        // this build compiled in: `Encoding.find("filesystem")` is the
        // one answer here that is per-platform rather than per-locale
        // (UTF-8 on macOS, the locale encoding elsewhere), so a bare
        // blob-vs-blob failure on a runner we cannot reproduce says
        // very little on its own.
        if expected != actual {
            let first_diff = expected
                .lines()
                .zip(actual.lines())
                .enumerate()
                .find(|(_, (e, a))| e != a)
                .map(|(i, (e, a))| format!("line {}: cruby {e:?} vs monoruby {a:?}", i + 1))
                .unwrap_or_else(|| "line counts differ".to_string());
            panic!(
                "locale-derived encoding behaviour differs under {label}\n\
                 first difference: {first_diff}\n\
                 built with target_os = macos: {}\n\
                 expected (cruby):\n{expected}\n\
                 actual (monoruby):\n{actual}",
                cfg!(target_os = "macos")
            );
        }
    }
}

/// `default_external` *is* the charmap's encoding, not a fixed UTF-8 —
/// stated without reference to CRuby, so a failure here is unambiguous
/// even if the reference Ruby is missing the machine's locale.
#[test]
fn default_external_is_the_charmap_encoding() {
    let path = write_script(
        "mr_enc_locale_selfconsistent.rb",
        r#"
p Encoding.default_external == Encoding.find(Encoding.locale_charmap)
p Encoding.default_external == Encoding.find("locale")
"#,
    );
    for (label, env) in LOCALES {
        let out = run_script(monoruby(env), &path);
        assert_eq!(out, "true\ntrue\n", "under {label}");
    }
}

/// A `C` locale starts the process on US-ASCII; a UTF-8 one on UTF-8.
/// The spelling of the charmap is the platform's, so match on the
/// encoding rather than the name.
#[test]
fn c_locale_is_us_ascii_and_utf8_locale_is_utf8() {
    let path = write_script(
        "mr_enc_locale_named.rb",
        r#"puts Encoding.default_external"#,
    );
    let c = run_script(monoruby(&[("LC_ALL", Some("C"))]), &path);
    assert_eq!(c.trim(), "US-ASCII");
    // Only assert the UTF-8 direction when the reference CRuby agrees
    // the host has such a locale — a machine with no UTF-8 locale
    // generated legitimately answers US-ASCII here too.
    let env: &[(&str, Option<&str>)] = &[("LANG", Some("C.UTF-8"))];
    if run_script(cruby(env), &path).trim() == "UTF-8" {
        assert_eq!(run_script(monoruby(env), &path).trim(), "UTF-8");
    }
}

/// `Encoding.default_external=` overrides the locale, and moves the
/// `#inspect` escaping with it — the setter path, which the startup
/// path must not have baked anything past.
#[test]
fn explicit_assignment_overrides_the_locale() {
    let path = write_script(
        "mr_enc_locale_override.rb",
        r#"
p "\u3044"
Encoding.default_external = Encoding::UTF_8
p "\u3044", Encoding.default_external
Encoding.default_external = Encoding::US_ASCII
p "\u3044", Encoding.default_external
Encoding.default_internal = Encoding::UTF_8
p "\u3044"
Encoding.default_internal = nil
p "\u3044"
"#,
    );
    for (label, env) in LOCALES {
        let expected = run_script(cruby(env), &path);
        let actual = run_script(monoruby(env), &path);
        assert_eq!(expected, actual, "under {label}");
    }
}

/// `-E` pins `default_external` whatever the locale says. This is the
/// switch the differential test harness leans on, so it has to keep
/// working from a `C` locale.
#[test]
fn dash_e_encoding_switch_beats_the_locale() {
    let path = write_script(
        "mr_enc_locale_dashE.rb",
        r#"puts Encoding.default_external; p "\u3044""#,
    );
    let env: &[(&str, Option<&str>)] = &[("LC_ALL", Some("C"))];
    let mut cmd = monoruby(env);
    cmd.arg("-E").arg("UTF-8");
    let out = run_script(cmd, &path);
    assert_eq!(out, "UTF-8\n\"\u{3044}\"\n");
    let mut cmd = cruby(env);
    cmd.arg("-E").arg("UTF-8");
    let expected = run_script(cmd, &path);
    assert_eq!(expected, out);
}

/// A `-e` script is read in the locale's encoding, so a `C` locale
/// makes a multibyte character in it a SyntaxError — CRuby's third,
/// downstream consequence. The two implementations word the error
/// differently; what is pinned is that it is rejected, on the right
/// line, for the right reason.
#[test]
fn dash_e_script_is_read_in_the_locale_encoding() {
    let env: &[(&str, Option<&str>)] = &[("LC_ALL", Some("C"))];

    let out = monoruby(env)
        .arg("-e")
        .arg("p \"\u{3044}\"")
        .output()
        .expect("spawn");
    let err = String::from_utf8_lossy(&out.stderr);
    assert!(!out.status.success(), "expected a SyntaxError, got {out:?}");
    assert!(
        err.contains("invalid multibyte character"),
        "unexpected stderr: {err}"
    );
    assert!(err.contains("-e:1"), "unexpected stderr: {err}");
    // CRuby rejects it in the same terms.
    let cout = cruby(env)
        .arg("-e")
        .arg("p \"\u{3044}\"")
        .output()
        .expect("spawn");
    assert!(!cout.status.success());
    assert!(
        String::from_utf8_lossy(&cout.stderr).contains("invalid multibyte character"),
        "the reference CRuby no longer rejects this; the case has moved"
    );

    // `__ENCODING__` is that same locale encoding, and `-E` does not
    // move it — it sets the external encoding, not the source's.
    for extra in [vec![], vec!["-E", "UTF-8"]] {
        let mut cmd = monoruby(env);
        cmd.args(&extra);
        let out = cmd.arg("-e").arg("p __ENCODING__").output().expect("spawn");
        assert!(out.status.success());
        assert_eq!(
            String::from_utf8_lossy(&out.stdout).trim(),
            "#<Encoding:US-ASCII>",
            "with {extra:?}"
        );
    }

    // `-K` does move it, and a magic comment in the script wins over
    // everything — both let the multibyte character through.
    let out = monoruby(env)
        .arg("-Ku")
        .arg("-e")
        .arg("p \"\u{3044}\", __ENCODING__")
        .output()
        .expect("spawn");
    assert!(
        out.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "\"\u{3044}\"\n#<Encoding:UTF-8>\n"
    );

    let out = monoruby(env)
        .arg("-e")
        .arg("# encoding: utf-8\np \"\u{3044}\", __ENCODING__")
        .output()
        .expect("spawn");
    assert!(
        out.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "\"\\u3044\"\n#<Encoding:UTF-8>\n"
    );

    // The injected encoding comment must not shift what the script
    // reports about itself.
    let out = monoruby(env)
        .arg("-e")
        .arg("\np __LINE__, __FILE__")
        .output()
        .expect("spawn");
    assert!(
        out.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&out.stdout), "2\n\"-e\"\n");
}

/// A script *file* is UTF-8 whatever the locale is (CRuby has read
/// source files as UTF-8 by default since 2.0) — only `-e` follows the
/// locale. The escaping of what it prints still does.
#[test]
fn a_script_file_is_utf8_whatever_the_locale() {
    let path = write_script(
        "mr_enc_locale_file_source.rb",
        "p __ENCODING__\np \"\u{3044}\".size\n",
    );
    for (label, env) in LOCALES {
        let expected = run_script(cruby(env), &path);
        let actual = run_script(monoruby(env), &path);
        assert_eq!(expected, actual, "under {label}");
        assert!(actual.starts_with("#<Encoding:UTF-8>\n"), "under {label}");
    }
}
