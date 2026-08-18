extern crate monoruby;
use monoruby::tests::*;

// The JIT's inline String#setbyte detaches a shared (copy-on-write)
// receiver in place (runtime::str_detach) instead of deopting — the
// `lit.dup` + `setbyte` shape made the shared miss chronic (the ruby-xor
// regression: one escalated deopt per dup'd string). These pin the
// detach path's correctness under the warmed JIT.

#[test]
fn setbyte_on_shared_string_detaches() {
    // The write must land in a detached buffer: the dup source, a second
    // sharer, and the frozen literal all keep their original bytes.
    run_test(
        r#"
        src = "hello world"
        out = []
        20.times do |k|
          a = src.dup
          b = src.dup
          a.setbyte(0, 72 + (k & 1))
          out << [a.getbyte(0), b.getbyte(0), src.getbyte(0)]
        end
        out.uniq
        "#,
    );
}

#[test]
fn setbyte_xor_loop_shape() {
    // The ruby-bench ruby-xor kernel: every call dups a frozen literal and
    // rewrites it byte-by-byte through the inline fast path.
    run_test(
        r#"
        def bxor!(a, b)
          l = a.bytesize
          lb = b.bytesize
          l = lb if lb < l
          i = 0
          while i < l
            a.setbyte(i, a.getbyte(i) ^ b.getbyte(i))
            i = i.succ
          end
          a
        end
        s = "this is a long string with no useful contents".freeze
        t = "this is also a long string with no useful cont".freeze
        r = nil
        30.times { r = bxor!(s.dup, t) }
        r.bytes
        "#,
    );
}

#[test]
fn setbyte_edge_cases_still_deopt_correctly() {
    // Frozen receivers, negative indices, out-of-range indices and the
    // code-range downgrade all still behave like CRuby after the
    // detach-and-retry rewrite (errors go through the deopt path).
    run_test(
        r#"
        r = []
        20.times do
          s = "abcdef".dup
          s.setbyte(-2, 90)
          r << s
          begin; "xyz".freeze.setbyte(0, 65); rescue => e; r << e.class; end
          begin; s2 = "abc".dup; s2.setbyte(5, 65); rescue => e; r << e.class << e.message; end
          begin; s3 = "abc".dup; s3.setbyte(-4, 65); rescue => e; r << e.class; end
          u = "plain".dup
          u.setbyte(0, 0xE3)
          r << u.valid_encoding? << u.bytesize
        end
        r.uniq
        "#,
    );
}
