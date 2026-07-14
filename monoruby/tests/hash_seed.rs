//! Integration coverage for per-process hash-seed randomization
//! (CVE-2011-4815): Ruby-visible `#hash` digests must differ between
//! processes (hash-flooding hardening) while staying consistent within
//! one. Cross-process comparison requires spawning the real binary twice —
//! the in-process `run_test` harness can only observe a single seed.

use std::process::{Command, Output};

fn monoruby(script: &str) -> Output {
    Command::new(env!("CARGO_BIN_EXE_monoruby"))
        .env_remove("RUBYOPT")
        .env_remove("RUBYLIB")
        .arg("-e")
        .arg(script)
        .output()
        .expect("failed to spawn monoruby")
}

fn stdout(out: &Output) -> String {
    String::from_utf8_lossy(&out.stdout).into_owned()
}

/// One `#hash` digest per line for every class the CVE-2011-4815 specs
/// cover (small/large Integer, Float, Rational, Complex, String, Symbol,
/// Array, Hash) plus nil.
const DIGESTS: &str = r##"[14, 10**30, 3.14, Rational(1, 2), Complex(1, 2),
 "abc", :a, [1, 2, 3], {a: 1, b: 2}, nil].each { |v| puts v.hash }"##;

#[test]
fn hash_digests_differ_between_processes() {
    let a = monoruby(DIGESTS);
    let b = monoruby(DIGESTS);
    assert!(a.status.success() && b.status.success());
    let (a, b) = (stdout(&a), stdout(&b));
    let pairs: Vec<(&str, &str)> = a.lines().zip(b.lines()).collect();
    assert_eq!(pairs.len(), 10);
    // Every single digest must differ — one colliding class would mean its
    // `#hash` bypasses the seeded hasher (astronomically unlikely to
    // collide by chance: 10 independent 1-in-2^64 events).
    for (i, (x, y)) in pairs.iter().enumerate() {
        assert_ne!(x, y, "digest #{i} identical across processes");
    }
}

#[test]
fn hash_digests_consistent_within_process() {
    // eql?-equal values must hash equal within one process, including the
    // heap-allocated representations (Bignum, subnormal Float) that used
    // to fall back to address-based Kernel#hash.
    let out = monoruby(
        r##"checks = {
             fixnum: 14.hash == (7 + 7).hash,
             bignum: (10**30).hash == (10**30).hash,
             heap_float: (1e-320).hash == (1e-320).hash,
             negative_zero: 0.0.hash == (-0.0).hash,
             string: "abc".hash == "ab#{:c}".hash,
             symbol: :a.hash == "a".to_sym.hash,
             bignum_key: { 10**30 => 1 }[10**30] == 1,
           }
           bad = checks.reject { |_, v| v }
           puts bad.empty? ? "ok" : "failed: #{bad.keys.inspect}""##,
    );
    assert_eq!(stdout(&out), "ok\n");
    assert!(out.status.success());
}
