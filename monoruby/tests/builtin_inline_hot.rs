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

// The JIT-compiled `Hash#include?` calls a helper that must not run Ruby:
// the code after it keeps the assumptions compiled before it. A key whose
// `#hash` is Ruby code goes back to the interpreter, so a method, constant
// or basic operator it redefines is seen by the rest of the caller.
#[test]
fn inline_hash_include_key_hash_redefines() {
    run_test_once(
        r#"
        class BihFoo; def bar = 1; end
        BIH_C = 1
        class BihKey
          def initialize(what) = @what = what
          def hash
            case @what
            when :method then BihFoo.class_eval { def bar = 2 }
            when :const
              Object.send(:remove_const, :BIH_C)
              Object.const_set(:BIH_C, 2)
            when :bop then Integer.class_eval { def -(o) = 999 }
            end
            1
          end
          def eql?(o) = true
        end
        def bih_m(h, k, o) = [h.include?(k), o.bar]
        def bih_c(h, k) = [h.key?(k), BIH_C]
        def bih_b(h, k, a) = [h.member?(k), a - 1]
        h = {BihKey.new(nil) => 1}
        k = BihKey.new(nil)
        o = BihFoo.new
        300.times { bih_m(h, k, o); bih_c(h, k); bih_b(h, k, 5) }
        r = [bih_m(h, BihKey.new(:method), o), bih_c(h, BihKey.new(:const))]
        Integer.alias_method(:__bih_orig_minus, :-)
        r << bih_b(h, BihKey.new(:bop), 5)
        Integer.alias_method(:-, :__bih_orig_minus)
        r
        "#,
    );
}

// An inline generator stands in for the builtin's body, which checks the
// argument count: compiled code must raise ArgumentError as the VM does.
#[test]
fn inline_gens_check_arity() {
    run_test_once(
        r#"
        def bih_arity(a, h, o)
          [
            (a.empty?(1) rescue $!.class), (h.empty?(1) rescue $!.class),
            (o.class(1) rescue $!.class), (a.size(1) rescue $!.class),
            (h.size(1) rescue $!.class), (o.nil?(1) rescue $!.class),
            (o.is_a? rescue $!.class), (a.clone(1, 2) rescue $!.class),
          ]
        end
        r = nil
        300.times { r = bih_arity([], {}, Object.new) }
        r
        "#,
    );
}

// `IO#reopen` moves the receiver to the other stream's class, so compiled
// code must not fold `#class` of an IO it has proven earlier.
#[test]
fn inline_class_of_reopened_io() {
    run_test_once(
        r#"
        def bih_reopen(io, other)
          c0 = io.class
          io.reopen(other)
          [c0, io.class]
        end
        200.times do
          rd, wr = IO.pipe
          r2, w2 = IO.pipe
          bih_reopen(rd, r2)
          [rd, wr, r2, w2].each(&:close)
        end
        path = "/tmp/monoruby_test_bih_reopen_#{Process.pid}"
        File.write(path, "x")
        rd, wr = IO.pipe
        f = File.open(path)
        r = bih_reopen(rd, f)
        [rd, wr, f].each { _1.close rescue nil }
        File.unlink(path)
        r
        "#,
    );
}
