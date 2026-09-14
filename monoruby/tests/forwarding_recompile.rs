extern crate monoruby;
use monoruby::tests::*;

// A specialized `Class#new` whose `__builtin_initialize__(...)` forward was
// source-routed from the caller's argument window (D1: the caller emits no
// rest Array) must not be recompiled standalone when a deopt counter inside
// it trips — the fresh body would bind `initialize` from the caller's
// never-materialized (nil) rest local and raise "wrong number of arguments
// (given 0, expected 1)". The recompile rebuilds the root unit instead.

#[test]
fn deferred_rest_specialized_body_recompiles_its_root() {
    run_test_once(
        r#"
        class VL
          attr_reader :name, :lookups, :flags
          def self.parse(m) = new(m)
          def initialize(m)
            lookups = m.scan(/[\w-]+/)
            @name = lookups.shift
            @lookups = lookups
            @flags = 0
            @lookups.each_index do |i|
              if %w[size first last].include?(lookups[i])
                @flags |= 1 << i
              end
            end
          end
        end
        def drive(m) = VL.parse(m)
        r = []
        300.times { r << drive("product.title").class }
        300.times { r << drive("product.images.first").flags }
        long = (("a".."z").to_a * 3).join(".") + ".size"
        300.times { r << drive(long).flags }
        r.uniq
        "#,
    );
}

// A caller that specialized a callee for its call site takes the callee's
// return state into its own: a `ReturnValue::Const` leaves it with no store
// at all, the folded value baked into its code. That is only true of the
// body it was compiled against, so replacing that body alone leaves the
// caller reading a constant nothing computes any more.
//
// Here `c(3)` folds to `3 * SCALE`. Redefining `SCALE` moves the constant
// version, the callee's `GuardConstVersion` fails, and the recompile has to
// rebuild the caller's unit too — otherwise the caller answers 6 forever,
// while CRuby (and monoruby --no-jit) answer 15.

#[test]
fn a_folded_return_survives_a_specialized_recompile() {
    run_test_once(
        r#"
        SCALE = 2
        class C
          def initialize(v); @v = v; end
          def hit = @v
          def c(k)
            hit
            k * SCALE
          end
        end
        def b(o)
          s = 0
          i = 0
          while i < 200_000
            s = o.c(3)
            i += 1
          end
          s
        end
        o = C.new(1)
        r1 = b(o)
        Object.send(:remove_const, :SCALE)
        Object.const_set(:SCALE, 5)
        [r1, b(o), b(o)]
        "#,
    );
}
