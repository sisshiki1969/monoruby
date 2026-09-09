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
