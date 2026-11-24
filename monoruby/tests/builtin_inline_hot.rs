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

// The same for `Hash#[]` and `Hash#[]=` (#2156): their helpers answer
// only where the lookup runs no Ruby (a packed or plain String key, an
// identity Hash, no default proc on a miss), and the rest is the
// interpreter's call, in a frame.
#[test]
fn inline_hash_index_key_hash_redefines() {
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
        def bih_m(h, k, o) = [o.bar, h[k], o.bar]
        def bih_c(h, k) = [h[k], BIH_C]
        def bih_b(h, k, a) = [h[k], a - 1]
        def bih_sm(h, k, o) = [o.bar, h[k] = 7, o.bar]
        def bih_sb(h, k, a) = [h[k] = 7, a - 1]
        r = []
        [:method, :const, :bop, :set_method, :set_bop].each do |what|
          h = {BihKey.new(nil) => 1}
          k = BihKey.new(nil)
          o = BihFoo.new
          300.times { bih_m(h, k, o); bih_c(h, k); bih_b(h, k, 5); bih_sm(h, k, o); bih_sb(h, k, 5) }
          BihFoo.class_eval { def bar = 1 }
          case what
          when :method then r << bih_m(h, BihKey.new(:method), o)
          when :const then r << bih_c(h, BihKey.new(:const))
          when :set_method then r << bih_sm(h, BihKey.new(:method), o)
          else
            Integer.alias_method(:__bih_orig_minus, :-)
            r << (what == :bop ? bih_b(h, BihKey.new(:bop), 5) : bih_sb(h, BihKey.new(:bop), 5))
            Integer.alias_method(:-, :__bih_orig_minus)
          end
        end
        r
        "#,
    );
}

// A default proc is Ruby code too, and a key's `#hash` may change what an
// instance variable read before the lookup proved about its object.
#[test]
fn inline_hash_index_default_proc_and_ivar_type() {
    run_test_once(
        r#"
        class BihFoo; def bar = 1; end
        def bih_d(h, k, f) = [f.bar, h[k], f.bar]
        h = Hash.new { |_, kk| BihFoo.class_eval { def bar = 2 } if kk == :z; 0 }
        h[:a] = 5
        f = BihFoo.new
        300.times { bih_d(h, :a, f) }
        r = [bih_d(h, :z, f)]

        class BihP; def initialize(a) = @a = a; attr_reader :a; end
        class BihObj; def initialize = @p = BihP.new(10); attr_reader :p; end
        class BihIvKey
          def initialize(o) = @o = o
          def hash = (@o.instance_variable_set(:@p, "ABCDEFGH" * 2) if @o; 1)
          def eql?(o) = true
        end
        def bih_iv(h, k, o) = (y = o.p.a; x = h[k]; z = o.p.a; [x, y, z])
        h = {BihIvKey.new(nil) => 1}
        k = BihIvKey.new(nil)
        o = BihObj.new
        300.times { bih_iv(h, k, o) }
        o2 = BihObj.new
        r << (bih_iv(h, BihIvKey.new(o2), o2) rescue $!.class)

        # `transform_keys(hash)` probes with the same kind of helper.
        def bih_tk(h, m) = h.transform_keys(m)
        300.times { bih_tk({a: 1, b: 2}, {a: :x}) }
        class BihKey2; def hash = 1; def eql?(o) = BihKey2 === o; def inspect = "K2"; end
        r << bih_tk({BihKey2.new => 1, :b => 2}, {BihKey2.new => :y, :b => :z})
        r
        "#,
    );
}

// `ary[start, len] = obj` calls `obj.to_ary`: Ruby code again, so the
// helper behind the compiled splice leaves it to the interpreter.
#[test]
fn inline_array_slice_assign_to_ary_redefines() {
    run_test_once(
        r#"
        class BihFoo; def bar = 1; end
        class BihToAry
          def initialize(what) = @what = what
          def to_ary
            case @what
            when :method then BihFoo.class_eval { def bar = 2 }
            when :bop then Integer.class_eval { def -(o) = 999 }
            end
            [7]
          end
        end
        def bih_am(a, v, o) = [o.bar, (a[0, 1] = v).class, o.bar, a]
        def bih_ab(a, v, x) = [(a[0, 1] = v).class, x - 1, a]
        o = BihFoo.new
        t = BihToAry.new(nil)
        300.times { bih_am([1, 2, 3], t, o); bih_ab([1, 2, 3], t, 5) }
        300.times { bih_am([1, 2, 3], [4], o); bih_ab([1, 2, 3], [4], 5) }
        r = [bih_am([1, 2, 3], BihToAry.new(:method), o)]
        Integer.alias_method(:__bih_orig_minus, :-)
        r << bih_ab([1, 2, 3], BihToAry.new(:bop), 5)
        Integer.alias_method(:-, :__bih_orig_minus)
        r << bih_am([1, 2, 3], 9, o) << (bih_ab([1, 2, 3], [4], 5)) << (bih_ab([], [4], -5) rescue $!.class)
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
