//! 64-bit typed reads and writes through `Fiddle.___read` / `Fiddle.___write`.
//!
//! monoruby's Fixnum is an i63, so the 64-bit Fiddle types are the only ones
//! whose value may not fit in one: a `long long` outside `[-2^62, 2^62)`, or
//! an `unsigned long long` at or above `2^62`, has to be boxed as a Bignum —
//! and, on the way in, a Bignum has to be accepted and narrowed the way C
//! would. Both the builtin and the JIT's inline load/store are exercised
//! here, and their answers are required to agree.
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

/// Run `code` twice — once with the JIT, once without — and require both
/// tiers to produce the same output. The JIT's inline `___read`/`___write`
/// specializations only fire in the first run, so any disagreement is a bug
/// in the emitted code.
fn run_both_tiers(code: &str) -> String {
    let jit = run_with(&["--disable=gems"], code);
    let vm = run_with(&["--disable=gems", "--no-jit"], code);
    assert_eq!(jit, vm, "JIT and interpreter disagree");
    jit
}

const PROLOGUE: &str = r#"
    require "fiddle"
    ptr = Fiddle::Pointer.new(Fiddle.malloc(8))
"#;

/// Signed 64-bit values survive a round trip at every interesting boundary,
/// including the ones that must be boxed as a Bignum on the way out and
/// narrowed from one on the way in. The loop runs the accessors well past
/// the JIT thresholds so the inline path is what answers.
#[test]
fn int64_round_trips_across_the_fixnum_boundary() {
    let got = run_both_tiers(&format!(
        r##"{PROLOGUE}
        values = [0, 1, -1, 2**31, -(2**31) - 1,
                  2**62 - 1, 2**62, -(2**62), -(2**62) - 1,
                  2**63 - 1, -(2**63)]
        200.times do
          values.each do |v|
            ptr.write_int64(v)
            r = ptr.read_int64
            raise "int64 #{{v}} -> #{{r}}" unless r == v
            raise "#{{v}} class #{{r.class}}" unless r.is_a?(Integer)
          end
        end
        print values.map {{ |v| ptr.write_int64(v); ptr.read_int64 }}.join(",")
        "##
    ));
    assert_eq!(
        got,
        "0,1,-1,2147483648,-2147483649,\
         4611686018427387903,4611686018427387904,\
         -4611686018427387904,-4611686018427387905,\
         9223372036854775807,-9223372036854775808"
    );
}

/// The unsigned door reports the full `[0, 2^64)` range: reinterpreting the
/// loaded word as an i64 would turn every value with bit 63 set into a
/// negative Integer, and refusing a Bignum argument would reject values the
/// C type holds exactly.
#[test]
fn uint64_round_trips_over_the_whole_unsigned_range() {
    let got = run_both_tiers(&format!(
        r##"{PROLOGUE}
        values = [0, 1, 2**62 - 1, 2**62, 2**62 + 1,
                  2**63 - 1, 2**63, 2**64 - 1]
        200.times do
          values.each do |v|
            ptr.write_uint64(v)
            r = ptr.read_uint64
            raise "uint64 #{{v}} -> #{{r}}" unless r == v
          end
        end
        print values.map {{ |v| ptr.write_uint64(v); ptr.read_uint64 }}.join(",")
        "##
    ));
    assert_eq!(
        got,
        "0,1,4611686018427387903,4611686018427387904,4611686018427387905,\
         9223372036854775807,9223372036854775808,18446744073709551615"
    );
}

/// A negative written through the unsigned door wraps, and the same bits read
/// back signed are that negative again — C's own conversion, and what a gem
/// storing `-1` into a `size_t` field expects.
#[test]
fn the_two_doors_see_the_same_bits() {
    let got = run_both_tiers(&format!(
        r##"{PROLOGUE}
        200.times do
          ptr.write_uint64(-1)
          raise "unsigned" unless ptr.read_uint64 == 2**64 - 1
          raise "signed" unless ptr.read_int64 == -1
          ptr.write_int64(-(2**63))
          raise "wrapped" unless ptr.read_uint64 == 2**63
        end
        ptr.write_uint64(-1)
        print ptr.read_uint64, " ", ptr.read_int64
        "##
    ));
    assert_eq!(got, "18446744073709551615 -1");
}

/// `VOIDP` is a 64-bit unsigned load/store too, so an address with its top
/// bit set has to survive `write_pointer` / `read_pointer`.
#[test]
fn a_pointer_word_survives_its_top_bits() {
    let got = run_both_tiers(&format!(
        r##"{PROLOGUE}
        addrs = [0x1000, 2**62, 2**63, 2**64 - 8]
        200.times do
          addrs.each do |a|
            ptr.write_pointer(Fiddle::Pointer.new(a))
            r = ptr.read_pointer.to_i
            raise "voidp #{{a}} -> #{{r}}" unless r == a
          end
        end
        print addrs.map {{ |a| ptr.write_pointer(Fiddle::Pointer.new(a)); ptr.read_pointer.to_i }}.join(",")
        "##
    ));
    assert_eq!(got, "4096,4611686018427387904,9223372036854775808,18446744073709551608");
}

/// A NULL pointer still raises rather than faulting, JIT-warm or not — the
/// inline path deopts to the builtin, which is where the check lives.
#[test]
fn a_null_pointer_raises_from_either_tier() {
    let got = run_both_tiers(&format!(
        r##"{PROLOGUE}
        null = Fiddle::Pointer.new(0)
        n = 0
        200.times do
          ptr.write_int64(1)
          begin
            null.write_int64(1)
          rescue RuntimeError
            n += 1
          end
          begin
            null.read_uint64
          rescue RuntimeError
            n += 1
          end
        end
        print n
        "##
    ));
    assert_eq!(got, "400");
}
