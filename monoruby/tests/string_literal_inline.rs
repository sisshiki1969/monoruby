//! A String literal short enough to live in the copy's own inline
//! buffer is built by emitted code rather than `value_deep_copy`, so
//! each case here is a property of literal instantiation the two paths
//! have to agree on. A 200-byte literal exceeds the inline buffer and
//! still takes the general path, which is why the long cases are here
//! alongside the short ones.
extern crate monoruby;
use monoruby::tests::*;

/// Lengths on both sides of the inline buffer's 32-byte capacity, and
/// on both sides of each 8-byte word the emitted copy writes.
#[test]
fn lengths_around_the_inline_capacity() {
    run_test(
        r##"
        (0..40).to_a.push(64, 100, 200).map { |n|
          s = eval('"' + ("a" * n) + '"')
          [s.bytesize, s.length, s == "a" * n, s.encoding.to_s, s.frozen?]
        }
        "##,
    );
}

/// Each evaluation yields a fresh, independently mutable object.
#[test]
fn each_evaluation_is_a_fresh_object() {
    run_test(
        r##"
        a = []
        i = 0
        while i < 3000
          a = []
          3.times { a << "xy" }
          a[0] << "z"
          i += 1
        end
        [a, a.map(&:object_id).uniq.size, a[1], a[2]]
        "##,
    );
}

/// Mutation of the fresh copy must not reach the template: a literal
/// in a loop has to read the same every time.
#[test]
fn mutating_a_copy_leaves_the_template_alone() {
    run_test(
        r##"
        res = []
        i = 0
        while i < 30000
          s = "hello"
          res = [s.dup]
          s << "!"
          s[0] = "J"
          res << s
          i += 1
        end
        res
        "##,
    );
}

/// Encodings the inline path handles, and ones it declines.
#[test]
fn encodings_and_code_ranges() {
    run_test(
        r##"
        strs = ["", "a", "abc", "\xff".b, "あ", "あいうえお", "é",
                "a" * 31, "a" * 32, "a" * 33, "日本語テキストです"]
        strs.map { |s|
          [s.bytesize, s.length, s.encoding.to_s, s.valid_encoding?, s.ascii_only?]
        }
        "##,
    );
}

/// The code range is a cache: asking for it before and after a
/// mutation has to give the same answers either way round.
#[test]
fn the_code_range_cache() {
    run_test(
        r##"
        res = []
        i = 0
        while i < 3000
          a = "abc"
          b = "abc"
          b.ascii_only?
          b << "\xe3\x81\x82"
          c = "abc"
          c << "\xe3\x81\x82"
          res = [a.ascii_only?, b.ascii_only?, b.valid_encoding?, b.length,
                 c.ascii_only?, c.valid_encoding?, c.length]
          i += 1
        end
        res
        "##,
    );
}

/// `freeze` / `dup` / `+@` / `-@` on a literal, and the frozen literal
/// spelling, which takes a different bytecode op entirely.
#[test]
fn freezing_and_duplicating() {
    run_test(
        r##"
        res = []
        i = 0
        while i < 3000
          res = ["fz".freeze.frozen?, "fz".frozen?, (+"pl").frozen?, (-"mn").frozen?,
                 "dp".dup.frozen?, "cl".clone.frozen?, "cl".clone(freeze: true).frozen?]
          i += 1
        end
        res
        "##,
    );
}

/// A literal that is never mutated but is compared, hashed and used as
/// a Hash key — the paths that read the encoding and code range.
#[test]
fn comparison_and_hashing() {
    run_test(
        r##"
        h = {}
        i = 0
        while i < 3000
          h = {}
          h["key"] = 1
          h["key"] += 1
          i += 1
        end
        [h, h["key"], "key".hash == "key".hash, "key".eql?("key"),
         "key" <=> "kez", "a" * 40 == "a" * 40]
        "##,
    );
}

/// Interpolation and adjacent-literal concatenation, which build
/// strings out of literal pieces.
#[test]
fn interpolation_and_concatenation() {
    run_test(
        r##"
        x = 5
        res = nil
        i = 0
        while i < 3000
          res = ["v#{x}w", "a" "b", "c" + "d", "e" * 3, "#{x}" + "y", "long #{"z" * 40} tail"]
          i += 1
        end
        res
        "##,
    );
}

/// Literals reaching the methods that read bytes directly, which the
/// JIT also has inline paths for.
#[test]
fn byte_level_reads_of_a_literal() {
    run_test(
        r##"
        res = nil
        i = 0
        while i < 3000
          s = "abcdefgh"
          res = [s.bytesize, s.getbyte(0), s.getbyte(7), s.getbyte(8),
                 s.bytes, s[0], s[-1], s.b.encoding.to_s]
          i += 1
        end
        res
        "##,
    );
}

/// An empty literal, the degenerate case: no bytes to write at all.
#[test]
fn the_empty_literal() {
    run_test(
        r##"
        res = nil
        i = 0
        while i < 30000
          s = ""
          s << "a"
          res = [s, "".bytesize, "".empty?, "".encoding.to_s, "".frozen?, "".ascii_only?]
          i += 1
        end
        res
        "##,
    );
}

/// A literal used as a `pack` / `unpack1` template, the shape that
/// motivated this: the copy is created and immediately read as bytes.
#[test]
fn a_literal_as_a_pack_template() {
    run_test(
        r##"
        acc = 0
        i = 0
        while i < 30000
          b = [i * 2654435761].pack("Q<").unpack1("Q<")
          acc = (acc + (b & 0xffff)) & 0xffff_ffff
          i += 1
        end
        acc
        "##,
    );
}
