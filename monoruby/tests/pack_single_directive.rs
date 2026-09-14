//! A template of one fixed-width numeric directive is answered without
//! the general template machinery. Every case here pins the two paths
//! together: a trailing space is skipped as whitespace by the template
//! parser, so `pack("E ")` is the same operation taken the general way,
//! and the two must agree byte for byte and error for error.
extern crate monoruby;
use monoruby::tests::*;

const DIRS: &str = r#"%w[c C s S v n i I l L V N q Q j J e f F g E d D G]"#;

/// Every directive the fast path covers, over values spanning each
/// width's signed and unsigned edges.
#[test]
fn every_covered_directive_matches_the_general_path() {
    run_test(&format!(
        r##"
        dirs = {DIRS}
        vals = [0, 1, -1, 127, 128, 255, -128, 32767, -32768, 65535,
                0x7fff_ffff, -0x8000_0000, 0xffff_ffff,
                0x7fff_ffff_ffff_ffff, -0x8000_0000_0000_0000,
                0xffff_ffff_ffff_ffff, 1 << 70, -(1 << 70),
                1.5, -1.5, 0.0, -0.0, 1e300, -1e300, 1e-300]
        def try(a, t) = (a.pack(t).bytes rescue [$!.class.to_s, $!.message])
        dirs.all? {{ |d| vals.all? {{ |v| try([v], d) == try([v], d + " ") }} }}
        "##
    ));
}

/// The same, spelled out rather than reduced to a boolean, so a
/// mismatch names the directive and value that broke.
#[test]
fn the_packed_bytes_themselves() {
    run_test(&format!(
        r##"
        dirs = {DIRS}
        vals = [0, 1, -1, 255, -128, 65535, 0xffff_ffff, 1.5, -1.5]
        dirs.flat_map {{ |d| vals.map {{ |v| [v].pack(d + " ").bytes rescue $!.class.to_s }} }}
        "##
    ));
}

/// The `<` and `>` modifiers, which flip the byte order the directive
/// would otherwise pick, including the byte directives that reject them.
#[test]
fn endian_modifiers_match_the_general_path() {
    run_test(&format!(
        r##"
        dirs = {DIRS}
        vals = [1, -1, 0x0102_0304, 0x0102_0304_0506_0708, 1.5, -1.5]
        def try(a, t) = (a.pack(t).bytes rescue [$!.class.to_s, $!.message])
        dirs.all? {{ |d|
          ["<", ">"].all? {{ |m| vals.all? {{ |v| try([v], d + m) == try([v], d + m + " ") }} }}
        }}
        "##
    ));
}

/// Shapes just outside the fast path, which take the general one
/// either way: a repeat count, two directives, leading whitespace, a
/// native-size modifier, the empty template, and non-numeric
/// directives.
#[test]
fn shapes_that_are_not_a_single_numeric_directive() {
    run_test(
        r##"
        def try(a, t) = (a.pack(t).bytes rescue [$!.class.to_s, $!.message])
        [try([1], "C2"), try([1, 2], "CC"), try([1], " C"), try([1], "C "),
         try([1], "C*"), try([], ""), try(["a"], "a"), try([65], "U"),
         try([1], "x"), try([1], "@2"), try([1.5], "E2")]
        "##,
    );
}

/// Argument-count edges: the fast path takes exactly one value, so zero
/// and two must behave as the general path does.
#[test]
fn argument_counts_around_one() {
    run_test(&format!(
        r##"
        dirs = {DIRS}
        def try(a, t) = (a.pack(t).bytes rescue [$!.class.to_s, $!.message])
        dirs.all? {{ |d|
          [[], [1], [1, 2], [1, 2, 3]].all? {{ |a| try(a, d) == try(a, d + " ") }}
        }}
        "##
    ));
}

/// A value that is not already an Integer or Float goes through the
/// same coercion, including ones that raise.
#[test]
fn coercion_of_the_packed_value() {
    run_test(
        r##"
        class ToInt
          def to_int = 65
        end
        class ToF
          def to_f = 1.5
        end
        class Boom
          def to_int = raise("boom")
        end
        def try(a, t) = (a.pack(t).bytes rescue [$!.class.to_s, $!.message])
        [ToInt.new, ToF.new, Boom.new, Object.new, nil, true, "x", 1 << 70].all? { |v|
          %w[C q Q E e].all? { |d| try([v], d) == try([v], d + " ") }
        }
        "##,
    );
}

/// The result's encoding and code range, which the fast path sets
/// directly instead of letting the general path resolve them.
#[test]
fn the_result_string_encoding() {
    run_test(&format!(
        r##"
        dirs = {DIRS}
        def desc(s) = [s.encoding.to_s, s.valid_encoding?, s.ascii_only?, s.bytesize]
        dirs.all? {{ |d|
          [0, 65, 200, 255, 1.5].all? {{ |v|
            (desc([v].pack(d)) == desc([v].pack(d + " ")) rescue true)
          }}
        }}
        "##
    ));
}

/// `unpack1` of the same directives, over strings shorter than the
/// directive (which yield nil), exactly its width, and longer.
#[test]
fn unpack1_matches_the_general_path() {
    run_test(&format!(
        r##"
        dirs = {DIRS}
        strs = ["", "\x00", "\xff", "\x7f\x80", "\x01\x02\x03",
                "\x01\x02\x03\x04", "\x01\x02\x03\x04\x05\x06\x07",
                "\x01\x02\x03\x04\x05\x06\x07\x08", "\xff" * 8, "\x00" * 8,
                "\xff" * 9, "\x80" * 8]
        def try(s, t) = (s.b.unpack1(t).inspect rescue [$!.class.to_s, $!.message])
        dirs.all? {{ |d|
          ["", "<", ">"].all? {{ |m|
            strs.all? {{ |s| try(s, d + m) == try(s, d + m + " ") }}
          }}
        }}
        "##
    ));
}

/// The unpacked values themselves, so a mismatch names them.
#[test]
fn the_unpacked_values_themselves() {
    run_test(&format!(
        r##"
        dirs = {DIRS}
        strs = ["", "\xff", "\x01\x02\x03\x04", "\xff" * 8, "\x80" * 8]
        dirs.flat_map {{ |d| strs.map {{ |s| s.b.unpack1(d + " ").inspect rescue $!.class.to_s }} }}
        "##
    ));
}

/// `unpack1` with the `offset:` keyword, which moves the window the
/// directive reads from, including the offsets that raise.
#[test]
fn unpack1_with_an_offset() {
    run_test(&format!(
        r##"
        dirs = {DIRS}
        s = "abcdefghijkl".b
        def try(s, t, o) = (s.unpack1(t, offset: o).inspect rescue [$!.class.to_s, $!.message])
        dirs.all? {{ |d|
          [0, 1, 4, 8, 11, 12, 13, -1].all? {{ |o| try(s, d, o) == try(s, d + " ", o) }}
        }}
        "##
    ));
}

/// `unpack` (not `unpack1`) of a single directive still returns an
/// Array, so it must not take the `unpack1` path.
#[test]
fn unpack_returns_an_array() {
    run_test(
        r##"
        s = ("\x01\x02\x03\x04\x05\x06\x07\x08" * 2).b
        [s.unpack("Q<"), s.unpack("Q<2"), s.unpack("Q<*"), s.unpack("C"),
         s.unpack("E"), "".b.unpack("C"), "".b.unpack("Q<")]
        "##,
    );
}

/// The float bit patterns dewasm-generated code round-trips through
/// `pack`/`unpack1`, where a payload or a sign bit going astray would
/// change the value.
#[test]
fn float_bit_round_trips() {
    run_test(
        r##"
        bits = [0x0000_0000_0000_0000, 0x8000_0000_0000_0000,
                0x3ff0_0000_0000_0000, 0xbff0_0000_0000_0000,
                0x7ff0_0000_0000_0000, 0xfff0_0000_0000_0000,
                0x7ff8_0000_0000_0000, 0x7ff0_0000_0000_0001,
                0xfff8_0000_0000_1234, 0x000f_ffff_ffff_ffff,
                0x7fef_ffff_ffff_ffff, 0x0010_0000_0000_0000]
        bits.map { |b|
          f = [b].pack("Q<").unpack1("E")
          [f.nan?, [f].pack("E").unpack1("Q<") == b]
        }
        "##,
    );
}

/// `pack` with a `buffer:` keyword appends into the buffer, which the
/// fast path must not take over.
#[test]
fn pack_with_a_buffer_keyword() {
    run_test(
        r##"
        buf = "seed".b
        r1 = [65].pack("C", buffer: buf)
        r2 = [1.5].pack("E", buffer: buf)
        r3 = [1].pack("N", buffer: buf)
        [r1.equal?(buf), r2.equal?(buf), r3.equal?(buf), buf.bytes, buf.encoding.to_s]
        "##,
    );
}

/// A hot loop, so the JIT-compiled call sites reach the fast path too.
#[test]
fn a_hot_pack_unpack_loop() {
    run_test(
        r##"
        acc = 0
        f = 0.0
        i = 0
        while i < 30000
          b = [i * 2654435761].pack("Q<").unpack1("Q<")
          f = [b].pack("Q<").unpack1("E")
          acc = (acc + (f.nan? ? 1 : 0)) & 0xffff_ffff
          i += 1
        end
        [acc, f.nan?]
        "##,
    );
}
