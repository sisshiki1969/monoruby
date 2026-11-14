//! A loop head takes its back edge's unboxed float for a local that is
//! dead at the head, whatever the entries bring.
//!
//! A local first assigned inside a loop is `nil` on the way in: a method
//! JIT enters the loop with it as the constant `nil`, which no bridge
//! turns into a float, so the head kept it boxed (`nil` or a `Float`)
//! and the back edge boxed the float it carried on every iteration —
//! for a value nobody reads, since the body assigns the local before
//! reading it. When the local is dead at the head (written before it is
//! read on every path from there, those that leave the loop included),
//! the head now holds it in a float register and the entry brings
//! nothing: what the register holds on the first iteration is never
//! read.
//!
//! The other half of these tests is the shapes where the local is *not*
//! dead and the `nil` must survive: assigned on some paths only, read
//! after a loop that may not run, read by a block or through a binding,
//! read by a `rescue` clause, or reachable through a `Proc` that
//! captured the frame before the local's first write — whatever the
//! block touches, its `binding` reaches every local. A float in the
//! register there would be read as garbage.
extern crate monoruby;
use monoruby::tests::*;

/// The local is assigned before it is read on every path, and not read
/// after the loop: the head holds it unboxed.
#[test]
fn floats_first_assigned_in_the_loop() {
    run_test(
        r#"
        def f(n)
          i = 0
          s = 0.0
          while i < n
            x = i * 1.5
            s += x * x
            i += 1
          end
          s
        end
        # Two of them, one computed from the other.
        def f2(n)
          i = 0
          s = 0.0
          while i < n
            x = i * 1.5
            y = x - 0.25
            s += x * y
            i += 1
          end
          s
        end
        # An inner loop's locals: nil on the first trip round the outer
        # loop, floats on the later ones.
        def nb(n)
          i = 0
          e = 0.0
          while i < n
            j = i + 1
            while j < n
              dx = i * 0.5 - j * 0.25
              dy = dx * dx + 1.0
              e += dy
              j += 1
            end
            i += 1
          end
          e
        end
        # Assigned on both arms of a branch.
        def br(n)
          i = 0
          s = 0.0
          while i < n
            if i.even?
              x = i * 1.5
            else
              x = i * 0.5
            end
            s += x
            i += 1
          end
          s
        end
        r = []
        12.times { r << f(7) << f(0) << f(1) << f2(6) << nb(5) << nb(0) << nb(1) << br(6) }
        r.uniq
        "#,
    );
}

/// The local holds something else before the loop — an Integer, a
/// String, a Float — and the loop overwrites it before reading it.
#[test]
fn locals_set_to_something_else_before_the_loop() {
    run_test(
        r#"
        def m(n)
          i = 0
          s = 0.0
          x = 1
          while i < n
            x = i * 1.5
            s += x * x
            i += 1
          end
          s
        end
        def m2(n)
          i = 0
          s = 0.0
          x = "a"
          while i < n
            x = i * 1.5
            s += x * x
            i += 1
          end
          s
        end
        def m3(n, z)
          i = 0
          s = 0.0
          x = z
          while i < n
            x = i * 1.5
            s += x * x
            i += 1
          end
          s
        end
        r = []
        12.times { r << m(5) << m(0) << m2(5) << m2(0) << m3(5, nil) << m3(5, :a) << m3(5, 2.5) << m3(0, [1]) }
        r.uniq
        "#,
    );
}

/// Not dead at the head: the value the entry brings is read on some path.
#[test]
fn locals_read_before_they_are_written() {
    run_test(
        r#"
        # Assigned on some trips only; read on all of them.
        def g(n)
          i = 0
          y = nil
          while i < n
            x = i * 1.5 if i > 0
            y = x
            i += 1
          end
          [x, y]
        end
        # Read after a loop that may not run at all.
        def h(n)
          i = 0
          while i < n
            x = i * 1.5
            i += 1
          end
          x
        end
        # Read before the write on a path a flag opens.
        def k(n, f)
          i = 0
          s = 0.0
          while i < n
            x = i * 1.5 if f || i > 1
            s += x if x
            x = i * 2.5
            s += x
            i += 1
          end
          s
        end
        # Another type before the loop, read after it.
        def q(n)
          i = 0
          s = 0.0
          x = "a"
          while i < n
            x = i * 1.5
            s += x * x
            i += 1
          end
          [s, x]
        end
        # Carried from one trip to the next.
        def c(n)
          i = 0
          s = 0.0
          prev = nil
          while i < n
            s += prev if prev
            prev = i * 1.5
            i += 1
          end
          s
        end
        r = []
        12.times do
          r << g(3) << g(0) << g(1) << h(0) << h(2) << k(3, true) << k(3, false) << k(0, true)
          r << q(3) << q(0) << c(4) << c(0) << c(1)
        end
        r.uniq
        "#,
    );
}

/// Read by something the bytecode of the loop does not show: a block
/// literal, a binding, a `rescue` clause.
#[test]
fn locals_read_through_a_block_a_binding_or_a_handler() {
    run_test(
        r#"
        def blk(n)
          i = 0
          s = 0.0
          while i < n
            x = i * 1.5 if i > 5
            [1].each { s += x.to_f }
            x = i * 2.5
            s += x
            i += 1
          end
          s
        end
        def bnd(n)
          i = 0
          r = []
          while i < n
            r << binding.local_variable_get(:x)
            x = i * 1.5
            r << x
            i += 1
          end
          r
        end
        def hnd(n)
          i = 0
          s = 0.0
          begin
            while i < n
              Integer("z") if i == n - 1
              x = i * 1.5
              s += x
              i += 1
            end
          rescue ArgumentError
            return [s, x]
          end
          nil
        end
        r = []
        12.times { r << blk(4) << blk(0) << bnd(3) << bnd(0) << hnd(1) << hnd(3) << hnd(0) }
        r.uniq
        "#,
    );
}

/// A frame captured between the head and the local's first write, or
/// after a loop that may not run, by a block that does not touch the
/// local: the `Proc`'s binding shows it all the same, read from another
/// method.
#[test]
fn locals_a_captured_frame_shows() {
    run_test(
        r#"
        def peek(pr) = pr.binding.local_variable_get(:x)
        def c1(n)
          i = 0; s = 0.0; r = []
          pr = nil
          while i < n
            pr ||= proc { 1 }
            r << peek(pr)
            x = i * 1.5
            s += x
            i += 1
          end
          [s, r]
        end
        def c2(n)
          i = 0; s = 0.0; r = []
          while i < n
            pr = proc { 1 }
            r << peek(pr)
            x = i * 1.5
            s += x
            i += 1
          end
          [s, r]
        end
        # Kept by the callee.
        $keep = nil
        def keep(&b) = ($keep = b)
        def c3(n)
          i = 0; s = 0.0; r = []
          while i < n
            keep { i }
            r << $keep.binding.local_variable_get(:x)
            x = i * 1.5
            s += x
            i += 1
          end
          [s, r]
        end
        # Captured after a loop that may not run.
        def c4(n)
          i = 0; s = 0.0
          while i < n
            x = i * 1.5
            s += x
            i += 1
          end
          pr = proc { 1 }
          [s, peek(pr)]
        end
        # The same behind an inner loop, with a block and with a lambda
        # literal.
        def c5(n, m)
          i = 0; s = 0.0
          while i < n
            x = nil
            j = 0
            while j < m
              x = j * 1.5
              s += x
              j += 1
            end
            i += 1
          end
          pr = proc { 1 }
          [s, peek(pr)]
        end
        def c6(n, m)
          i = 0; s = 0.0
          while i < n
            x = nil
            j = 0
            while j < m
              x = j * 1.5
              s += x
              j += 1
            end
            i += 1
          end
          pr = -> { 1 }
          [s, peek(pr)]
        end
        # Captured on a later trip, after a side exit.
        def c7(n, k)
          i = 0; s = 0.0; r = []
          while i < n
            t = k + i
            pr = proc { 1 } if i == 1
            r << peek(pr) if pr
            x = t * 1.5
            s += x
            i += 1
          end
          [s, r]
        end
        # A block the loop runs before the write, inlined or not.
        def c8(n)
          i = 0; s = 0.0
          while i < n
            [1].each { |v| s += v }
            x = i * 1.5
            s += x
            i += 1
          end
          s
        end
        r = []
        20.times do
          r << c1(4) << c1(0) << c2(4) << c3(3) << c4(3) << c4(0)
          r << c5(2, 2) << c5(2, 0) << c5(0, 0) << c6(2, 2) << c6(2, 0) << c6(0, 0)
          r << c7(3, 1) << c8(3)
        end
        r << c7(3, 0.5) << c7(3, 2**70)
        r.uniq
        "#,
    );
}

/// A side exit between the head and the local's first write: the
/// interpreter takes over with whatever the register held boxed into
/// the slot, and overwrites it before reading it.
#[test]
fn a_side_exit_before_the_first_write() {
    run_test(
        r#"
        def d(n, k)
          i = 0
          s = 0.0
          while i < n
            t = k + i
            x = t * 1.5
            s += x
            i += 1
          end
          s
        end
        r = []
        20.times { r << d(6, 1) }
        # `k + i` was Integer + Integer until now.
        r << d(6, 0.5) << d(6, 1r) << d(6, 2**70) << d(0, nil) << d(6, 1)
        r.uniq
        "#,
    );
}

/// More such locals than float registers, with a float carried across
/// the loop as well: some of them take a spill slot at the head, and an
/// entry's register may already be taken.
#[test]
fn more_locals_than_float_registers() {
    run_test(
        r#"
        def w(n, z)
          i = 0
          s = z
          t = z * 2.0
          while i < n
            a0 = i * 0.5
            a1 = a0 + 1.0
            a2 = a1 + a0
            a3 = a2 * 0.5
            a4 = a3 - a1
            a5 = a4 + a2
            a6 = a5 * 0.25
            a7 = a6 + a3
            a8 = a7 - a4
            a9 = a8 + a5
            b0 = a9 * 0.5
            b1 = b0 + a6
            b2 = b1 - a7
            b3 = b2 + a8
            b4 = b3 * 0.125
            b5 = b4 + a9
            b6 = b5 - b0
            b7 = b6 + b1
            s += a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9
            t += b0 + b1 + b2 + b3 + b4 + b5 + b6 + b7
            i += 1
          end
          [s, t]
        end
        r = []
        12.times { r << w(6, 0.5) << w(0, 1.5) << w(1, 2.5) }
        r.uniq
        "#,
    );
}

/// The register the head gives such a local is another local's on the
/// way in: a float computed before the loop that the loop turns into
/// something else (so the head has it boxed and its register is free
/// there). The entry has to move that one out of the way first, and
/// must not hand it a spill slot the head has already given away.
#[test]
fn a_register_the_entry_already_uses() {
    run_test(
        r#"
        def g7(n, z)
          t = z * 2.0
          i = 0
          s = t
          while i < n
            x = i * 1.5
            s += x
            t = i
            i += 1
          end
          [s, t]
        end
        def g8(n, z)
          t = z * 2.0
          u = z + 0.25
          v = t * u
          i = 0
          s = 0.0
          while i < n
            x = i * 1.5
            y = x + 0.5
            w = y * x
            s += x + y + w
            t = i
            u = nil
            v = :a
            i += 1
          end
          [s, t, u, v]
        end
        # The dead locals come first in the frame.
        def h1(n, z)
          x = nil
          y = nil
          t = z * 2.0
          u = z + 0.25
          v = t * u
          i = 0
          s = 0.0
          while i < n
            x = i * 1.5
            y = x + 0.5
            s += x + y
            t = i
            u = nil
            v = :a
            i += 1
          end
          [s, t, u, v]
        end
        # Floats that stay floats and are read after the loop, next to
        # retyped ones.
        def h2(n, z)
          a = z * 2.0
          b = z + 0.25
          c = a * b
          d = c - a
          i = 0
          s = 0.0
          while i < n
            x = i * 1.5
            y = x + a
            w = y * b
            s += x + y + w
            c = i
            d = nil
            i += 1
          end
          [s, a, b, c, d]
        end
        # One the loop reads, one it retypes, one it carries.
        def h3(n, z)
          a = z * 2.0
          b = z + 0.25
          e = a + b
          i = 0
          s = z
          while i < n
            x = i * a
            y = x + 0.5
            s += x * y
            e = y
            b = i.to_s
            i += 1
          end
          [s, a, b, e]
        end
        # The inner loop's dead locals meet registers the outer body
        # filled.
        def h4(n, z)
          i = 0
          s = 0.0
          p = q = o = nil
          while i < n
            p = i * z
            q = p + z
            o = p * q
            j = 0
            while j < 3
              u = j * 1.5
              v = u + 0.25
              w = u * v
              s += u + v + w
              p = j
              q = nil
              j += 1
            end
            s += o
            i += 1
          end
          [s, p, q, o]
        end
        # Many of each.
        def h5(n, z)
          a0 = z * 1.5
          a1 = a0 + z
          a2 = a1 * z
          a3 = a2 - a0
          a4 = a3 + a1
          a5 = a4 * a2
          i = 0
          s = 0.0
          while i < n
            b0 = i * 0.5
            b1 = b0 + 1.0
            b2 = b1 * b0
            b3 = b2 - b1
            b4 = b3 + b2
            b5 = b4 * 0.5
            b6 = b5 + b3
            b7 = b6 - b4
            s += b0 + b1 + b2 + b3 + b4 + b5 + b6 + b7
            a0 = i
            a2 = nil
            a4 = :k
            i += 1
          end
          [s, a0, a1, a2, a3, a4, a5]
        end
        # A boxed float entry (a parameter) the loop accumulates into.
        def h9(n, z, q)
          t = z * 2.0
          u = z + 0.25
          i = 0
          while i < n
            x = i * 1.5
            k = x + 0.5
            q += x + k
            t = i
            u = nil
            i += 1
          end
          [q, t, u]
        end
        r = []
        12.times do
          [0, 1, 5].each do |n|
            r << g7(n, 0.5) << g8(n, 0.5) << h1(n, 0.5) << h2(n, 0.5) << h3(n, 0.5)
            r << h4(n, 0.5) << h5(n, 0.5) << h9(n, 0.5, 0.125)
            r << g7(n, 1.25) << g8(n, 1.25) << h1(n, 1.25) << h2(n, 1.25) << h3(n, 1.25)
            r << h5(n, 1.25) << h9(n, 1.5, 2.0)
          end
        end
        r.uniq
        "#,
    );
}

/// The same next to a local that is a float literal on the way in:
/// its bridge loads the literal into the head's register, which the
/// move above may just have given to the displaced float.
#[test]
fn a_float_literal_entry_beside_a_displaced_float() {
    run_test(
        r#"
        def m3(n, z, q)
          d2 = 0.5
          d1 = 1
          p4 = z * 0.25
          d4 = 1
          i = 0
          s = q
          while i < n
            d0 = i * 0.125
            d1 = d0 * 0.125
            d2 = d0 - 1.5
            d4 = d0 + 0.25
            s += d0
            p4 = nil
            i += 1
          end
          [s, p4, d4]
        end
        # Literal accumulators, with retyped floats before or after them.
        def h7(n, z)
          y = 1.0
          w = 2.5
          t = z * 2.0
          u = z + 0.25
          i = 0
          while i < n
            x = i * 1.5
            k = x + 0.5
            y += x
            w += k
            t = i
            u = nil
            i += 1
          end
          [y, w, t, u]
        end
        def h8(n, z)
          t = z * 2.0
          u = z + 0.25
          y = 1.0
          w = 2.5
          i = 0
          while i < n
            x = i * 1.5
            k = x + 0.5
            y += x
            w += k
            t = i
            u = nil
            i += 1
          end
          [y, w, t, u]
        end
        r = []
        12.times do
          [0, 1, 4].each do |n|
            r << m3(n, 0.5, 1.25) << m3(n, 1.5, 0.75)
            r << h7(n, 0.5) << h8(n, 0.5) << h7(n, 1.5) << h8(n, 1.5)
          end
        end
        r.uniq
        "#,
    );
}

/// Locals that share a register on the way in (a copy takes its
/// source's), with different registers at the head. Moving one of them
/// by swapping registers takes the others along, and the swap for the
/// next one brings the first back out of its place (#1913). And with
/// no register free, the head must not leave a local and its copy on
/// one register when the loop writes only one of them (#1915; `m3`
/// with two float registers, `m4` with the usual fourteen).
#[test]
fn copies_that_share_a_register_at_the_entry() {
    run_test(
        r#"
        def m(n, z)
          a = z - 0.5
          d = z
          s = z
          i = 0
          while i < n
            d = i * 3.0
            a = nil
            i += 1
          end
          [s, a, d]
        end
        def m2(n, z, q)
          p1 = z - 0.5
          p4 = z + 0.25
          d3 = z
          i = 0
          s = z
          while i < n
            d0 = i * 2.0
            d1 = d0 + 0.125
            d3 = i * 3.0
            s += d1
            p1 = i.to_s
            p4 = i.to_s
            i += 1
          end
          [s, p1, p4, d3]
        end
        # Three copies, each rewritten by the loop at a different point.
        def m3(n, z)
          a = z * 2.0
          b = a
          c = a
          t = z + 1.0
          i = 0
          while i < n
            b = i * 0.5
            t = nil
            c = b + a
            i += 1
          end
          [a, b, c, t]
        end
        def m4(n, z)
          f0 = z + 0.5; f1 = z + 1.5; f2 = z + 2.5; f3 = z + 3.5; f4 = z + 4.5
          f5 = z + 5.5; f6 = z + 6.5; f7 = z + 7.5; f8 = z + 8.5; f9 = z + 9.5
          g0 = z + 10.5; g1 = z + 11.5; g2 = z + 12.5; g3 = z + 13.5
          a = z * 2.0
          b = a
          i = 0
          while i < n
            b = i * 0.5
            i += 1
          end
          [a, b, f0 + f1 + f2 + f3 + f4 + f5 + f6 + f7 + f8 + f9 + g0 + g1 + g2 + g3]
        end
        r = []
        12.times do
          [0, 1, 4].each do |n|
            r << m(n, 0.5) << m(n, 1.5) << m2(n, 0.5, 1.25) << m2(n, 1.5, 0.75)
            r << m3(n, 0.5) << m3(n, 1.5) << m4(n, 0.5) << m4(n, 1.5)
          end
        end
        r.uniq
        "#,
    );
}

/// Every float register taken at the head, so that the loop body drops
/// the float view of an integer it copied (`p3 = i`) to make room. The
/// back edge then has that local boxed where the head, built from an
/// earlier walk's back edge, keeps the view, and its bridge has to
/// convert the integer again (#1916: `m` once the locals the loop
/// assigns first are in registers too, `m2` on its own). And a bridge
/// that needs a temporary register with none free must not take one by
/// dropping another local's view — in `m3`, with two float registers,
/// the one it is moving a value out of.
#[test]
fn no_float_register_free_at_a_bridge() {
    run_test(
        r#"
        def m(n, z, q)
          p2 = z - 0.5
          p3 = p2
          p1 = q
          p5 = p1 * z
          p4 = q
          p0 = p2 * z
          i = 0
          s = 0.0
          while i < n
            d0 = i * 2.0
            d1 = i * 1.5 + p4
            d2 = d1 * 2.0
            d3 = d2 - 1.5
            d5 = d2 * 0.25
            d6 = d5
            s += d2
            p3 = i
            p1 += d6
            i += 1
          end
          [s, p0, p1, p2, p3, p4, p5, d3]
        end
        def m2(n, z, q)
          p2 = z - 0.5
          p3 = p2
          p1 = q
          p5 = p1 * z
          p4 = q
          p0 = p2 * z
          d0 = z + 1.0
          d1 = z + 2.0
          d2 = z + 3.0
          d5 = z + 4.0
          d6 = z + 5.0
          i = 0
          s = 0.0
          while i < n
            d0 = i * 2.0
            d1 = i * 1.5 + p4
            d2 = d1 * 2.0
            d3 = d2 - 1.5
            d5 = d2 * 0.25
            d6 = d5
            s += d2
            p3 = i
            p1 += d6
            i += 1
          end
          [s, p0, p1, p2, p3, p4, p5, d3]
        end
        def m3(n, z, q)
          p1 = q
          p3 = p1 * z
          p5 = q
          p2 = p3
          d4 = 1
          p0 = "s"
          i = 0
          s = z
          while i < n
            j = 0
            while j < 2
              d0 = j * 1.5
              d2 = j * 2.0
              d3 = d2
              d4 = j * 0.125 + p1
              s += d3
              p4 = j
              p5 += d0
              j += 1
            end
            i += 1
          end
          [s, p0, p1, p2, p3, p4, p5, d4]
        end
        r = []
        12.times do
          [0, 1, 4].each do |n|
            r << m(n, 0.5, 1.25) << m(n, 1.5, 0.75)
            r << m2(n, 0.5, 1.25) << m2(n, 1.5, 0.75)
            r << m3(n, 0.5, 1.25) << m3(n, 1.5, 0.75)
          end
        end
        r.uniq
        "#,
    );
}
