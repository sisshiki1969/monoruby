extern crate monoruby;
use monoruby::tests::*;

// Hot builtins compiled in line, or as a direct call to a runtime helper
// that skips the builtin's native wrapper and frame.

#[test]
fn inline_identity_eq_class_and_empty() {
    // `BasicObject#==` / `equal?`, `Module#==`, `Kernel#class`,
    // `Array#empty?` and `Hash#empty?` are compiled inline.
    run_test(
        r#"
        class LdcFoo; end
        class LdcBar < LdcFoo; end
        a = LdcFoo.new
        b = LdcBar.new
        o = Object.new
        def o.sing = 1
        h = {k: 1}
        h2 = {k: 1}; h2.delete(:k)
        arr = [1]
        [
          a.class, b.class, o.class, 1.class, nil.class, :s.class, 1.5.class, true.class,
          a == b, a.equal?(b), a.equal?(a), a == a, a != a,
          LdcFoo == LdcFoo, LdcFoo == LdcBar, LdcFoo == 1, Comparable == Comparable,
          arr.empty?, [].empty?, h.empty?, {}.empty?, h2.empty?,
          (a == a) ? 1 : 2, a.equal?(b) ? 1 : 2, arr.empty? ? 1 : 2, h2.empty? ? 1 : 2,
        ]
        "#,
    );
}

#[test]
fn inline_hash_include_iter_and_current() {
    run_test(
        r#"
        class BihKey
          def hash = 1
          def eql?(o) = o.is_a?(BihKey)
        end
        h = {a: 1, "s" => 2, BihKey.new => 3}
        cnt = 0
        h.each { |k, v| cnt += v }
        [
          h.include?(:a), h.key?(:b), h.member?(BihKey.new), h.has_key?("s"),
          {}.include?(:a), cnt,
          Thread.current.equal?(Thread.main), Fiber.current.class,
          Thread.current == Thread.current, Fiber.current.equal?(Fiber.current),
        ]
        "#,
    );
}

#[test]
fn inline_hash_include_raises_and_iter_guard() {
    run_test(
        r#"
        class BihBad
          def hash = raise("boom")
        end
        r = []
        begin
          {a: 1}.include?(BihBad.new)
        rescue => e
          r << e.message
        end
        begin
          h = {a: 1}
          h.each { h[:n] = 1 }
        rescue => e
          r << e.class
        end
        h = {a: 1}
        h.each { h[:a] = 2 }
        r << h
        "#,
    );
}
