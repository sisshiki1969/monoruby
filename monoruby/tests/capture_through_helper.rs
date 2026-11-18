use monoruby::tests::*;

// A VM instruction that calls a frameless runtime helper (an operator's
// generic path, `[]` / `[]=`, a Hash literal's `hash`, multiple assignment's
// `to_ary`, ...) can reach `binding` / `eval` with the interpreted frame as
// the caller. The capture moves the frame to the heap; the instruction used
// to go on with the LFP register pointing at the abandoned stack copy, so
// its result and every later local went there, out of the binding's sight
// and out of the GC's (#2070). Each method here runs once, in the VM.

#[test]
fn capture_through_unary_and_binary_operators() {
    run_test_once(
        r##"
        class Neg
          alias_method :-@, :binding
          alias_method :+@, :binding
          alias_method :~, :binding
          alias_method :!, :binding
          alias_method :<, :eval
          alias_method :[]=, :eval
          public :-@, :+@, :~, :!, :<, :[]=
        end
        def u(o)
          s = 15
          a = -o; b = +o; c = ~o; d = !o
          e = (o < "s = 20")
          o["s += 1"] = nil
          s += 1
          [s, e, a.local_variable_get(:s), [b, c, d].map(&:class)]
        end
        u(Neg.new)
        "##,
    );
}

#[test]
fn capture_through_index() {
    run_test_once(
        r##"
        class Idx
          alias_method :[], :eval
          public :[]
        end
        def g(o)
          s = 15
          b = o["binding"]
          s = 17
          [s, b.local_variable_get(:s)]
        end
        g(Idx.new)
        "##,
    );
}

#[test]
fn capture_through_hash_literal_key() {
    run_test_once(
        r##"
        class Key
          alias_method :hash, :binding
          public :hash
        end
        def h
          x = 1
          begin
            {Key.new => 1}
          rescue TypeError => e
          end
          x += 1
          [x, e.class]
        end
        h
        "##,
    );
}

/// The locals written after the capture are Hash / String objects and a GC
/// runs before they are read: written to the abandoned stack copy, which is
/// no GC root, they were collected while still in use.
#[test]
fn locals_written_after_a_capture_stay_alive() {
    run_test_once(
        r##"
        class Neg
          alias_method :-@, :binding
          public :-@
        end
        class Arr
          alias_method :to_ary, :binding
          alias_method :to_a, :binding
          public :to_ary, :to_a
        end
        def f(o, q)
          b = -o
          h = {a: [1, 2]}
          GC.start
          begin; x, y = q; rescue TypeError => e1; end
          s = "s" * 3
          GC.start
          begin; z = [*q]; rescue TypeError => e2; end
          t = {s => h}
          GC.start
          [b.class, h, s, t, e1.class, e2.class, x, y, z]
        end
        f(Neg.new, Arr.new)
        "##,
    );
}
