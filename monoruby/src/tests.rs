use crate::*;
use std::{io::Write, path::PathBuf};
use tempfile::NamedTempFile;

use std::sync::LazyLock;

// Note: static items do not call [`Drop`] on program termination, so this won't be deallocated.
// this is fine, as the OS can deallocate the terminated program faster than we can free memory
// but tools like valgrind might report "memory leaks" as it isn't obvious this is intentional.
static RUBY: LazyLock<String> = LazyLock::new(|| {
    // M3 Ultra takes about 16 million years in --release config
    find_ruby()
});

pub fn run_test(code: &str) {
    let wrapped = format!(
        r##"
      __res = ({0})
      for __i in 0..24 do
          __res2 = ({0})
          __assert(__res, __res2)
      end
      ({0})
  "##,
        code
    );
    eprintln!("{}", wrapped);
    let mut globals = Globals::new_test();
    let interp_val = run_test_main(&mut globals, &wrapped);
    let ruby_res = run_ruby(&mut globals, code, interp_val);

    Value::assert_eq(&globals, interp_val, ruby_res);
}

/// Like [`run_test`], but always verifies against a live CRuby and never
/// touches the snapshot oracle. Use for environment-dependent expressions
/// (e.g. `Dir.home`, `Dir.pwd`) whose expected value varies per host, so a
/// recorded snapshot would only be correct on the machine that recorded it.
pub fn run_test_live(code: &str) {
    let wrapped = format!(
        r##"
      __res = ({0})
      for __i in 0..24 do
          __res2 = ({0})
          __assert(__res, __res2)
      end
      ({0})
  "##,
        code
    );
    eprintln!("{}", wrapped);
    let mut globals = Globals::new_test();
    let interp_val = run_test_main(&mut globals, &wrapped);
    let ruby_res = run_ruby_live(&mut globals, code);

    Value::assert_eq(&globals, interp_val, ruby_res);
}

pub fn run_test_once(code: &str) {
    let wrapped = format!(
        r##"
      ({0})
  "##,
        code
    );
    eprintln!("{}", wrapped);
    let mut globals = Globals::new_test();
    let interp_val = run_test_main(&mut globals, &wrapped);
    let ruby_res = run_ruby(&mut globals, code, interp_val);

    Value::assert_eq(&globals, interp_val, ruby_res);
}

/// Like [`run_test_once`], but always verifies against a live CRuby and
/// never touches the snapshot oracle. See [`run_test_live`].
pub fn run_test_once_live(code: &str) {
    let wrapped = format!(
        r##"
      ({0})
  "##,
        code
    );
    eprintln!("{}", wrapped);
    let mut globals = Globals::new_test();
    let interp_val = run_test_main(&mut globals, &wrapped);
    let ruby_res = run_ruby_live(&mut globals, code);

    Value::assert_eq(&globals, interp_val, ruby_res);
}

pub fn run_tests<S: AsRef<str>>(codes: &[S]) {
    let mut code = "__a = [];".to_string();
    for c in codes {
        let c = c.as_ref();
        code += &format!("__a << ({c});");
    }
    code += "__a";
    let wrapped = format!(
        r##"
      __res = ({0})
      for __i in 0..24 do
          __res2 = ({0})
          __assert(__res, __res2)
      end
      ({0})
  "##,
        code
    );
    eprintln!("{}", wrapped);
    let mut globals = Globals::new_test();
    let interp_val = run_test_main(&mut globals, &wrapped).as_array();
    let ruby_res = run_ruby_live(&mut globals, &code).as_array();

    for i in 0..codes.len() {
        let interp_elem = interp_val.get(i).unwrap();
        let ruby_elem = ruby_res.get(i).unwrap();
        eprintln!("{}", codes[i].as_ref());
        Value::assert_eq(&globals, *interp_elem, *ruby_elem);
    }
    //Value::assert_eq(&globals, interp_val, ruby_res);
}

pub fn run_binop_tests(lhs: &[&str], op: &[&str], rhs: &[&str]) {
    let mut test = vec![];
    for lhs in lhs {
        for rhs in rhs {
            for op in op {
                test.extend_from_slice(&[
                    format!("({lhs}) {op} ({rhs})"),
                    format!("({lhs}).{op}({rhs})"),
                    format!("({lhs}) {op} (-({rhs}))"),
                    format!("-({lhs}) {op} ({rhs})"),
                    format!("-({lhs}) {op} -({rhs})"),
                    format!("@a = ({lhs}); @a {op} ({rhs})"),
                    format!("@a = ({lhs}); @a.{op}({rhs})"),
                    format!("@a = ({lhs}); @a {op} (-({rhs}))"),
                    format!("@a = -({lhs}); @a {op} ({rhs})"),
                    format!("@a = -({lhs}); @a {op} -({rhs})"),
                ]);
            }
        }
    }
    run_tests(&test);
}

pub fn run_binop_tests2(lhs: &[&str], op: &[&str], rhs: &[&str]) {
    let mut test = vec![];
    for lhs in lhs {
        for rhs in rhs {
            for op in op {
                test.extend_from_slice(&[format!("({lhs}) {op} ({rhs})")]);
                test.extend_from_slice(&[format!("({lhs}).{op}({rhs})")]);
            }
        }
    }
    run_tests(&test);
}

pub fn run_unop_tests(op: &[&str], rhs: &[&str]) {
    let mut test = vec![];
    for rhs in rhs {
        for op in op {
            test.extend_from_slice(&[format!("{op} ({rhs})"), format!("{op} (-{rhs})")]);
        }
    }
    run_tests(&test);
}

pub fn run_test_with_prelude(code: &str, prelude: &str) {
    let wrapped = format!(
        r##"
      {prelude}
      res = ({code})
      for __i in 0..100 do
          res2 = ({code})
          __assert(res, res2)
      end
      ({code})
  "##
    );
    eprintln!("{}", wrapped);
    let mut globals = Globals::new_test();
    let interp_val = run_test_main(&mut globals, &wrapped);
    let ruby_res = run_ruby(&mut globals, &format!("{prelude}\n{code}"), interp_val);

    Value::assert_eq(&globals, interp_val, ruby_res);
}

pub fn run_test2(code: &str) {
    let mut globals = Globals::new_test();
    let interp_val = run_test_main(&mut globals, code);
    let ruby_res = run_ruby(&mut globals, code, interp_val);

    Value::assert_eq(&globals, interp_val, ruby_res);
}

/// Batched form of [`run_test2`]: concatenates `codes` into a single
/// `[..]` literal so the whole slice costs one CRuby spawn instead of
/// one per element. Like `run_test2` (and unlike [`run_tests`]) it runs
/// the code only once — no 25-iteration JIT-warmup loop.
pub fn run_tests2<S: AsRef<str>>(codes: &[S]) {
    let mut code = "__a = [];".to_string();
    for c in codes {
        let c = c.as_ref();
        code += &format!("__a << ({c});");
    }
    code += "__a";
    eprintln!("{code}");
    let mut globals = Globals::new_test();
    let interp_val = run_test_main(&mut globals, &code).as_array();
    let ruby_res = run_ruby_live(&mut globals, &code).as_array();

    for i in 0..codes.len() {
        let interp_elem = interp_val.get(i).unwrap();
        let ruby_elem = ruby_res.get(i).unwrap();
        eprintln!("{}", codes[i].as_ref());
        Value::assert_eq(&globals, *interp_elem, *ruby_elem);
    }
}

pub fn run_test_no_result_check(code: &str) -> Value {
    eprintln!("{code}");
    let mut globals = Globals::new_test();
    run_test_main(&mut globals, code)
}

pub fn run_test_error(code: &str) {
    eprintln!("{code}");
    let mut globals = Globals::new_test();
    match globals.run(code, std::path::Path::new(".")) {
        Ok(v) => {
            eprintln!("{}", v.inspect(&globals.store));
            panic!()
        }
        Err(err) => err.show_error_message_and_all_loc(&globals.store),
    }
}

fn run_test_main(globals: &mut Globals, code: &str) -> Value {
    let res = match globals.run(code, std::path::Path::new(".")) {
        Ok(res) => res,
        Err(err) => {
            if let MonorubyErrKind::SystemExit(status) = &err.kind {
                if *status == 0 {
                    return Value::nil();
                }
            }
            err.show_error_message_and_all_loc(&globals.store);
            panic!();
        }
    };

    let jit_str = res.inspect(&globals.store);
    eprintln!("monoruby:  {jit_str}");

    res
}

/// Reference result for `code`, served from the checked-in snapshot oracle
/// (`tests/ruby_oracle.tsv`) when possible. A live CRuby is spawned only on
/// a cache miss (the fresh entry is then recorded into the oracle file), or
/// always when `MONORUBY_TEST_ORACLE=ruby` is exported — the mode to use
/// after a CRuby version bump to re-verify / refresh the stored snapshots.
///
/// **Heal path**: when a *cached* entry disagrees with monoruby's own result
/// (`interp_val`), the answer is re-derived from a live CRuby before
/// declaring failure. Environment-dependent expressions (e.g. `Dir.home`,
/// whose value differs between the recording host and CI runners) thus
/// degrade to the old one-spawn behavior instead of failing spuriously,
/// while a genuine monoruby regression still fails — the live CRuby agrees
/// with the snapshot. The healed value is deliberately **not** recorded:
/// it is host-specific, so rewriting the entry would make the checked-in
/// file flip-flop between hosts.
fn run_ruby(globals: &mut Globals, code: &str, interp_val: Value) -> Value {
    let (inspect, from_cache) = oracle::expected_inspect(code, || spawn_ruby(code));
    let label = if from_cache { "ruby(oracle)" } else { "ruby" };
    let ruby_res = value_from_inspect(globals, &inspect, label);
    if from_cache && !Value::test_eq(&globals.store, interp_val, ruby_res) {
        eprintln!("oracle: snapshot mismatch — re-verifying against a live ruby");
        let live = spawn_ruby(code);
        return value_from_inspect(globals, &live, "ruby(live)");
    }
    ruby_res
}

/// Reference result for `code`, always obtained from a live CRuby process.
/// Used by the batched helpers ([`run_tests`], [`run_tests2`]): their
/// auto-generated concatenated code strings are huge and churn with every
/// tweak of the combination tables, so snapshotting them would bloat the
/// oracle file for no stable benefit.
fn run_ruby_live(globals: &mut Globals, code: &str) -> Value {
    let inspect = spawn_ruby(code);
    value_from_inspect(globals, &inspect, "ruby")
}

/// Reconstruct a [`Value`] from a CRuby `p` output line (either freshly
/// captured or replayed from the oracle file).
fn value_from_inspect(globals: &mut Globals, inspect: &str, label: &str) -> Value {
    let nodes = crate::parser::parse_program(inspect.to_string(), PathBuf::new())
        .unwrap()
        .node;

    let res = Value::from_ast(&nodes, globals);

    eprintln!("{label}: {}", res.inspect(&globals.store));
    res
}

/// Run `code` under the reference CRuby and return the final `p` output
/// line (the single-line `inspect` of the expression's value).
fn spawn_ruby(code: &str) -> String {
    let code = format!(
        r#"
            ____a = ({});
            puts;
            p(____a)
        "#,
        code
    );
    let mut tmpfile = NamedTempFile::new().unwrap();
    tmpfile.write_all(code.as_bytes()).unwrap();

    // Force the reference CRuby's default external encoding to UTF-8 with
    // `-E UTF-8`. monoruby always treats source/strings as UTF-8, but
    // CRuby derives `Encoding.default_external` from the locale, and in a
    // non-UTF-8 / empty-`LANG` environment (which is how `cargo nextest`
    // spawns these subprocesses) `String#inspect` escapes non-ASCII to
    // `\uXXXX` — so `"café".inspect` would diff against monoruby's literal
    // `"café"`. `-E UTF-8` pins the encoding independent of locale, making
    // the differential comparison reproducible across hosts and runners.
    let res = match std::process::Command::new(&*RUBY)
        .arg("-E")
        .arg("UTF-8")
        // Skip rubygems boot (~5x faster startup across the differential
        // spawns) and ignore any ambient RUBYOPT for hermeticity.
        .arg("--disable=gems,rubyopt")
        .arg(tmpfile.path())
        .output()
    {
        Ok(output) => String::from_utf8(output.stdout).unwrap(),
        Err(err) => {
            panic!("failed to invoke ruby ({}). {}", *RUBY, err);
        }
    };

    res.trim_end().split('\n').last().unwrap().to_string()
}

/// Minimum CRuby version the test harness compares output against.
/// monoruby's startup files mirror Ruby 4.x semantics (e.g. the new Hash
/// inspect form), so older Rubies on PATH would produce spurious diffs.
const MIN_RUBY_VERSION: (u32, u32) = (4, 0);

/// Returns true when `ruby_cmd` exists and reports a version `>=
/// MIN_RUBY_VERSION`.
fn ruby_version_ok(ruby_cmd: &str) -> bool {
    let Ok(output) = std::process::Command::new(ruby_cmd)
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

/// Locate a `ruby` executable that is at least `MIN_RUBY_VERSION`.
/// Tries the system PATH first, then falls back to common rbenv/rvm shim
/// paths. A too-old Ruby on PATH (e.g. system 3.0 on Debian) is skipped
/// so tests run against the rbenv-managed Ruby instead.
fn find_ruby() -> String {
    // Already on PATH and recent enough?
    if ruby_version_ok("ruby") {
        return "ruby".to_string();
    }
    // rbenv shim — defers to the version selected by ~/.rbenv/version.
    if let Some(home) = std::env::var_os("HOME") {
        let shim = std::path::PathBuf::from(home).join(".rbenv/shims/ruby");
        if let Some(shim_str) = shim.to_str() {
            if ruby_version_ok(shim_str) {
                return shim_str.to_string();
            }
        }
    }
    // rvm
    if let Some(home) = std::env::var_os("HOME") {
        let rvm = std::path::PathBuf::from(home).join(".rvm/bin/ruby");
        if let Some(rvm_str) = rvm.to_str() {
            if ruby_version_ok(rvm_str) {
                return rvm_str.to_string();
            }
        }
    }
    "ruby".to_string() // last resort — will fail with a clear error message
}

/// Snapshot oracle: a checked-in cache of CRuby reference outputs.
///
/// Spawning a CRuby subprocess for every `run_test`-family call dominates
/// suite wall-clock and memory, yet the answer for a given code string never
/// changes for a given CRuby version. This module memoizes the mapping
/// `code → last p-output line` in `monoruby/tests/ruby_oracle.tsv` so the
/// suite compares monoruby against the recorded string instead of a live
/// process.
///
/// Modes, selected by the `MONORUBY_TEST_ORACLE` environment variable:
///
/// - unset / `snapshot` (default): serve hits from the file with **no** Ruby
///   spawn; on a miss spawn CRuby once and record the fresh entry back into
///   the file (so `rm tests/ruby_oracle.tsv && cargo test` regenerates it
///   from scratch, dropping any orphaned entries).
/// - `ruby`: always spawn CRuby and use its live answer, refreshing any
///   stale stored entry. Use this after bumping the reference CRuby version
///   to re-verify the whole suite against the new Ruby and rewrite the
///   snapshots in place.
///
/// File format: one entry per line, `key TAB output TAB snippet`, where
/// `key` is a truncated SHA-256 of the code string, `output` is the escaped
/// `p` line, and `snippet` is an escaped prefix of the code (human context
/// only — ignored on load). Entries are kept key-sorted so regeneration and
/// incremental additions produce clean diffs. Concurrent writers (nextest
/// runs one process per test) serialize on an flock'd side file and each
/// re-reads + rewrites the file atomically, so parallel misses merge instead
/// of clobbering each other.
mod oracle {
    use std::collections::{BTreeMap, HashMap};
    use std::io::Write;
    use std::os::fd::AsRawFd;
    use std::path::PathBuf;
    use std::sync::{LazyLock, Mutex};

    const HEADER: &str = "# monoruby ruby-oracle v1 — CRuby reference outputs for the run_test harness.\n\
                          # Maintenance: `rm` this file and run `cargo test` to regenerate from a live Ruby;\n\
                          # `MONORUBY_TEST_ORACLE=ruby cargo test` re-verifies/refreshes entries in place.";

    #[derive(Clone, Copy, PartialEq, Eq)]
    enum Mode {
        /// Default: replay stored entries; spawn + record only on miss.
        Snapshot,
        /// Always spawn CRuby; refresh stale entries (version-bump mode).
        Ruby,
    }

    fn mode() -> Mode {
        match std::env::var("MONORUBY_TEST_ORACLE") {
            Err(_) => Mode::Snapshot,
            Ok(s) if s == "snapshot" => Mode::Snapshot,
            Ok(s) if s == "ruby" => Mode::Ruby,
            Ok(s) => panic!("MONORUBY_TEST_ORACLE={s:?} — expected \"snapshot\" or \"ruby\""),
        }
    }

    fn oracle_path() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/ruby_oracle.tsv")
    }

    /// In-process view of the oracle file, loaded once. Misses recorded by
    /// other concurrent processes after this load are simply re-derived
    /// live (and re-recorded, idempotently) — correctness never depends on
    /// seeing them.
    static CACHE: LazyLock<Mutex<HashMap<String, String>>> = LazyLock::new(|| {
        let mut map = HashMap::new();
        if let Ok(text) = std::fs::read_to_string(oracle_path()) {
            for (key, output_esc, _) in parse_lines(&text) {
                map.insert(key.to_string(), unescape(output_esc));
            }
        }
        Mutex::new(map)
    });

    /// Returns the expected `p` output line for `code` and whether it came
    /// from the snapshot cache (`true`) or a live CRuby spawn (`false`).
    pub(super) fn expected_inspect(code: &str, spawn: impl FnOnce() -> String) -> (String, bool) {
        let key = key_of(code);
        match mode() {
            Mode::Snapshot => {
                if let Some(hit) = CACHE.lock().unwrap().get(&key).cloned() {
                    return (hit, true);
                }
                let live = spawn();
                record(&key, code, &live);
                (live, false)
            }
            Mode::Ruby => {
                let live = spawn();
                let stale = CACHE.lock().unwrap().get(&key) != Some(&live);
                if stale {
                    eprintln!("oracle: refreshing entry {key}");
                    record(&key, code, &live);
                }
                (live, false)
            }
        }
    }

    /// 128-bit truncated SHA-256 of the code string. The `v1\n` prefix ties
    /// the key to the harness wrapper (`____a = (..); puts; p(____a)` +
    /// CRuby flags): if that wrapper ever changes in an output-affecting
    /// way, bump the prefix to invalidate every stored entry at once.
    fn key_of(code: &str) -> String {
        use sha2::{Digest, Sha256};
        hex::encode(&Sha256::digest(format!("v1\n{code}").as_bytes())[..16])
    }

    /// Merge one entry into the oracle file (read-modify-rewrite under an
    /// exclusive flock so concurrent test processes don't drop each other's
    /// additions). Best-effort: a read-only checkout only loses the caching,
    /// not the test.
    fn record(key: &str, code: &str, output: &str) {
        CACHE
            .lock()
            .unwrap()
            .insert(key.to_string(), output.to_string());
        if let Err(err) = try_rewrite(key, code, output) {
            eprintln!("oracle: failed to record entry (non-fatal): {err}");
        }
    }

    fn try_rewrite(key: &str, code: &str, output: &str) -> std::io::Result<()> {
        let path = oracle_path();
        let lock_path = path.with_extension("tsv.lock");
        let lock = std::fs::OpenOptions::new()
            .create(true)
            .write(true)
            .open(&lock_path)?;
        // SAFETY: flock on an fd we own; released when `lock` drops (close).
        let rc = unsafe { libc::flock(lock.as_raw_fd(), libc::LOCK_EX) };
        if rc != 0 {
            return Err(std::io::Error::last_os_error());
        }

        // Re-read under the lock: another process may have appended entries
        // since our startup load.
        let mut entries: BTreeMap<String, (String, String)> = BTreeMap::new();
        if let Ok(text) = std::fs::read_to_string(&path) {
            for (k, out_esc, snip_esc) in parse_lines(&text) {
                entries.insert(k.to_string(), (out_esc.to_string(), snip_esc.to_string()));
            }
        }
        let snippet: String = code.chars().take(80).collect();
        entries.insert(
            key.to_string(),
            (escape(output), escape(snippet.trim_start())),
        );

        let tmp_path = path.with_extension("tsv.tmp");
        {
            let mut tmp = std::fs::File::create(&tmp_path)?;
            writeln!(tmp, "{HEADER}")?;
            for (k, (out_esc, snip_esc)) in &entries {
                writeln!(tmp, "{k}\t{out_esc}\t{snip_esc}")?;
            }
            tmp.sync_all()?;
        }
        std::fs::rename(&tmp_path, &path)
    }

    /// Yields `(key, escaped_output, escaped_snippet)` for each entry line,
    /// skipping comments/blank lines and tolerating a missing snippet field.
    fn parse_lines(text: &str) -> impl Iterator<Item = (&str, &str, &str)> {
        text.lines().filter_map(|line| {
            if line.is_empty() || line.starts_with('#') {
                return None;
            }
            let mut fields = line.splitn(3, '\t');
            let key = fields.next()?;
            let output = fields.next()?;
            let snippet = fields.next().unwrap_or("");
            Some((key, output, snippet))
        })
    }

    /// The stored output is a single `p` line, but escape defensively so the
    /// TSV framing can never be broken by exotic inspect output.
    pub(super) fn escape(s: &str) -> String {
        let mut out = String::with_capacity(s.len());
        for c in s.chars() {
            match c {
                '\\' => out.push_str("\\\\"),
                '\t' => out.push_str("\\t"),
                '\n' => out.push_str("\\n"),
                '\r' => out.push_str("\\r"),
                c => out.push(c),
            }
        }
        out
    }

    pub(super) fn unescape(s: &str) -> String {
        let mut out = String::with_capacity(s.len());
        let mut chars = s.chars();
        while let Some(c) = chars.next() {
            if c != '\\' {
                out.push(c);
                continue;
            }
            match chars.next() {
                Some('\\') => out.push('\\'),
                Some('t') => out.push('\t'),
                Some('n') => out.push('\n'),
                Some('r') => out.push('\r'),
                Some(other) => {
                    out.push('\\');
                    out.push(other);
                }
                None => out.push('\\'),
            }
        }
        out
    }
}

#[cfg(test)]
mod harness_tests {
    use super::*;

    #[test]
    fn oracle_escape_roundtrip() {
        // The TSV framing must survive any inspect output: tabs/newlines/CRs
        // are escaped on write and restored on load, and escaped forms never
        // contain a literal field or line separator.
        for s in [
            "",
            "plain",
            "tab\there",
            "line\nbreak\r\n",
            r"back\slash",
            "mixed\\t not-a-tab \\n literal\t\n\\",
            "日本語\tと\\改行\n",
        ] {
            let esc = oracle::escape(s);
            assert!(!esc.contains('\t') && !esc.contains('\n') && !esc.contains('\r'));
            assert_eq!(oracle::unescape(&esc), s);
        }
    }

    #[test]
    fn nonfinite_floats_reconstruct() {
        // issue #930: CRuby's `p` prints non-finite Floats as the bare
        // words Infinity / -Infinity / NaN; reconstructing the expected
        // value from that output used to panic on the unresolved
        // "constant" instead of comparing.
        run_test_once(r#"[1.0, 1.0 / 0.0]"#);
        run_test_once(r#"[1.0, -1.0 / 0.0]"#);
        run_test_once(r#"[1.0, 0.0 / 0.0]"#);
        run_test_once(r#"[Float::INFINITY, -Float::INFINITY]"#);
        run_test_once(r#"{ a: 1.0 / 0.0, b: -1.0 / 0.0 }"#);
        run_test_once(r#"(1.0 / 0.0)..(1.0 / 0.0)"#);
        run_test_once(r#"[[Float::INFINITY], { x: -Float::INFINITY }]"#);
    }
}
