extern crate monoruby;
use monoruby::tests::*;

#[test]
fn string_concat_does_not_dispatch_through_shl() {
    // `String#concat` appends through a hidden primitive: a subclass that
    // aliases `<<` to its own `concat` and calls `super` from there
    // (ActiveSupport::SafeBuffer) must not recurse.
    run_test(
        r#"
        class SafeBuf < String
          attr_reader :log
          def concat(value)
            (@log ||= []) << value.to_s
            super(value.to_s.upcase)
          end
          alias << concat
        end
        b = SafeBuf.new("x")
        b << "a"
        b.concat("b")
        s = +"s"
        s.concat("t", "u", 118)
        s << "v"
        [b, b.log, s, "ab".concat, "q".concat("r", "q"), "".respond_to?(:append)]
        "#,
    );
}
