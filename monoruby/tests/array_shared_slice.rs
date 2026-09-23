extern crate monoruby;
use monoruby::tests::*;

/// A long slice of a heap-buffered array is a view of a shared root; a
/// write to either side copies that side out first, so the parent, each
/// slice, and a slice of a slice keep their own contents.
#[test]
fn slices_are_isolated_views() {
    run_test(
        r#"
        a = (0...100).to_a
        b = a[10..-1]; c = a[20, 30]; d = b[5..-1]; e = a[0, 8]
        r = [b, c, d, e]
        b[0] = :x; r << a[10] << b[0] << d[0]
        a[11] = :y; r << b[1] << c[0]
        c << :z; r << c.size << a.size << d.size
        a << :w; r << a.size << b.size
        f = a[50..]; a.clear; r << f.size << f[0]
        g = f.dup; g.push(1); r << f.size << g.size
        GC.start
        r << f.map { |x| x }
        big2 = (0...30).to_a; sl = big2[1..]; big2.replace([1, 2, 3]); r << sl.size << big2
        q = (0...50).to_a[10..30]; q.sort!; q.reverse!; r << q[0]
        w = (0...50).to_a[10..30]; w.unshift(5); r << w[0] << w[1]
        p1 = (0...60).to_a[0..50]; p2 = p1[0..50]; p1.map! { |x| x + 1 }; r << p2[0] << p1[0]
        r
        "#,
    );
}

/// A frozen array is its own root: a slice of it, and a slice of that,
/// read it in place, and writing the slice never touches it.
#[test]
fn slices_of_a_frozen_array() {
    run_test(
        r#"
        fr = (0...40).to_a.freeze
        s1 = fr[3..]; s2 = s1[2..]
        r = [s2, fr.frozen?, s1.frozen?]
        s2[0] = 9; r << fr[5] << s1[2] << s2[0]
        GC.start
        r << fr[10..20] << fr[38..] << fr[39, 5] << fr[40..] << fr[41..]
        r
        "#,
    );
}

/// The JIT's inline `[]=`, `<<` and slice-assignment fast paths must
/// take the generic path for a view rather than write into the root.
#[test]
fn jit_stores_never_write_a_shared_root() {
    run_test(
        r#"
        res = []
        200.times do |it|
          a = (0...64).to_a
          b = a[8..]
          b[0] = it
          a[9] = -it
          b << 1
          a << 2
          c = a[16, 32]
          c[3, 2] = [7, 8]
          res = [a[8], a[9], b[0], b[1], b.size, a.size, c[3], c[4], a[19], a[20], c.size]
        end
        d = (0...100).to_a
        s = 0
        300.times { |i| t = d[i % 50..]; s += t[0] + t.size }
        res << s
        big = Array.new(1000) { |i| i * 2 }
        1000.times { |i| t = big[i..]; t[0] = -1 if t.size > 0 }
        res << big[999] << big[500..].sum
        res
        "#,
    );
}

/// Short slices, and slices of an array whose elements are still inline,
/// are plain copies; the boundary must not change what comes back.
#[test]
fn short_slices_are_copies() {
    run_test(
        r#"
        a = [1, 2, 3, 4, 5]
        b = a[1..]
        b[0] = 9
        c = (0...20).to_a
        d = c[2, 5]; d << 0
        e = c[0, 16]; e[0] = 7
        [a, b, c, d, e, c[15..], c[16, 100], c[20..], c[21..], c[-3..], c[-30..]]
        "#,
    );
}
