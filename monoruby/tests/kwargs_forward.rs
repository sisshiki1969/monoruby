extern crate monoruby;
use monoruby::tests::*;

// K1: literal keyword arguments at a `X.new(...)`-style call site are
// source-routed through the pure forwarding trampoline (`Class#new`'s
// `def new(...)`) straight into the callee's declared keyword
// parameters, skipping the `**kwrest` Hash round trip. These tests pin
// the routed fast path, its static-veto fallbacks, and the deopt
// materialization against CRuby.

#[test]
fn new_with_literal_kwargs() {
    run_test(
        r#"
        class Node
          attr_reader :pos, :name, :dirs, :sels, :file, :src
          def initialize(pos: nil, name: "d", dirs: [], sels: nil, file: nil, src: :none)
            @pos = pos; @name = name; @dirs = dirs
            @sels = sels; @file = file; @src = src
          end
        end
        r = []
        30.times do |i|
          n = Node.new(pos: i, name: "x#{i}", sels: [i])
          r << [n.pos, n.name, n.dirs, n.sels, n.file, n.src]
        end
        30.times do |i|
          n = Node.new
          m = Node.new(src: i, pos: 0)
          r << [n.pos, n.src, m.src, m.pos]
        end
        r
        "#,
    );
}

#[test]
fn new_kwargs_with_positionals_and_required() {
    run_test(
        r#"
        class Mix
          attr_reader :v
          def initialize(x, y = 7, k: :k, j: nil) = @v = [x, y, k, j]
        end
        class Req
          attr_reader :v
          def initialize(a:, b: 2) = @v = [a, b]
        end
        r = []
        30.times do |i|
          r << Mix.new(i, k: i).v << Mix.new(i, 2, k: 3, j: 4).v << Mix.new(i).v
          r << Req.new(a: i).v << Req.new(b: i, a: -1).v
        end
        r
        "#,
    );
}

#[test]
fn new_kwargs_error_cases_stay_generic() {
    // Missing required / unknown keyword must still raise ArgumentError
    // (the static veto keeps these on the generic runtime path).
    run_test(
        r#"
        class Req
          def initialize(a:, b: 2) = @x = [a, b]
        end
        r = []
        30.times do
          begin
            Req.new(b: 1)
          rescue ArgumentError => e
            r << e.message
          end
          begin
            Req.new(a: 1, z: 9)
          rescue ArgumentError => e
            r << e.message
          end
        end
        r
        "#,
    );
}

#[test]
fn new_kwargs_kwrest_callee_vetoed() {
    // A callee with `**rest` collects the leftovers — not routable, must
    // keep last-one-wins Hash semantics via the generic path.
    run_test(
        r#"
        class KwR
          attr_reader :h
          def initialize(a: 1, **h) = @h = [a, h]
        end
        r = []
        30.times do |i|
          r << KwR.new(a: i, z: 9, w: i).h << KwR.new.h
        end
        r
        "#,
    );
}

#[test]
fn new_kwargs_hash_splat_vetoed() {
    // `X.new(**h)` carries a dynamic hash-splat: no deferral, generic
    // path, same results.
    run_test(
        r#"
        class N
          attr_reader :v
          def initialize(a: 0, b: 1) = @v = [a, b]
        end
        r = []
        h = {a: 5, b: 6}
        30.times do |i|
          r << N.new(**h).v << N.new(a: i, **{b: i}).v
        end
        r
        "#,
    );
}

#[test]
fn new_kwargs_redefinition_deopt() {
    // Redefining initialize mid-loop invalidates the specialized
    // trampoline; the transition (deopt + recompile) must keep binding
    // the routed keywords correctly.
    run_test_once(
        r#"
        class D
          attr_reader :v
          def initialize(k: 0) = @v = k
        end
        r = []
        40.times do |i|
          r << D.new(k: i).v
          if i == 25
            class D
              def initialize(k: 0) = @v = k + 100
            end
          end
        end
        r
        "#,
    );
}

#[test]
fn new_kwargs_heap_values_gc() {
    // Routed keyword values are heap objects; the deferral window (nil
    // kwrest, values only in caller slots) must keep them alive across
    // the callee-entry GC poll.
    run_test(
        r#"
        class H
          attr_reader :a, :b
          def initialize(a: nil, b: nil) = (@a = a; @b = b)
        end
        r = 0
        200.times do |i|
          o = H.new(a: "s#{i}" * 10, b: [i, i + 1])
          r += o.a.size + o.b.sum
        end
        r
        "#,
    );
}
