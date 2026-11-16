//! `==` that is not the operator written at a call site: the one a builtin
//! calls (`rb_equal`, the reverse dispatch of `Integer#==` / `Float#==` /
//! `String#==`, the default `!=`). CRuby makes those calls with
//! `rb_funcall`, so they see no refinement, reach a private method, fall
//! back to `method_missing`, and leave the calling builtin's frame in the
//! backtrace of whatever the method raises (#1970, #1971, #1982).

extern crate monoruby;
use monoruby::tests::*;

/// A refinement of `==` is seen by the `==` written in its scope and by
/// nothing else — not by `Array#==`, `include?`, a number's reverse
/// dispatch, or the default `!=` (#1970).
#[test]
fn builtin_eq_sees_no_refinement() {
    run_test_once(
        r##"
        class Rev1970; end
        module R1970
          refine Rev1970 do
            def ==(o) = true
          end
        end
        using R1970
        r = Rev1970.new
        s = Struct.new(:a)
        [r == 1, r != 1, 1 == r, 1.5 == r, (2**70) == r, 1 != r, 1.5 != r,
         "a" == r, :a == r, nil == r, 1 === r, 1.5 === r, r === 1, "a" === r,
         [r] == [1], [1].include?(r), [r].include?(1), [r].index(1),
         [r].count(1), [r].delete(1), ([r] - [1]).size, [[r, 1]].assoc(1),
         [r].find_index(1), [r].rindex(1), {a: r} == {a: 1},
         s.new(r) == s.new(1), s.new(r) != s.new(1), (r..r) == (1..1),
         [r] <=> [1], Comparable.instance_method(:==).bind_call(r, 1)]
        "##,
    );
}

/// The same from compiled code: the operator sites of a method written
/// under the refinement keep seeing it, the builtins they call do not.
#[test]
fn builtin_eq_sees_no_refinement_jit() {
    run_test_once(
        r##"
        class Rev1970J; end
        module R1970J
          refine Rev1970J do
            def ==(o) = true
          end
        end
        using R1970J
        def f1970(r, s)
          [r == 1, r != 1, 1 == r, 1.5 == r, 1 != r, [r] == [1],
           [r].include?(1), s.new(r) == s.new(1), s.new(r) != s.new(1)]
        end
        r = Rev1970J.new
        s = Struct.new(:a)
        res = nil
        40.times { res = f1970(r, s) }
        res
        "##,
    );
}

/// A receiver with no `==` gets `method_missing`, from the operator and
/// from every builtin that compares (#1971).
#[test]
fn eq_without_a_method_is_method_missing() {
    run_test_once(
        r##"
        class Baz1971 < BasicObject
          undef_method :==
          def method_missing(name, *a) = name == :== ? :mm : super
        end
        b = Baz1971.new
        res = []
        res << ((b == 1) rescue $!.class)
        res << ((b != 1) rescue $!.class)
        res << (([b] == [1]) rescue $!.class)
        res << (([b].include?(1)) rescue $!.class)
        res << (([b].index(1)) rescue $!.class)
        res << ((1 == b) rescue $!.class)
        res << ((1.5 == b) rescue $!.class)
        res << ((1 != b) rescue $!.class)
        res << (("a" == b) rescue $!.class)
        res << ((:a == b) rescue $!.class)
        res << ((nil == b) rescue $!.class)
        res << ((1 === b) rescue $!.class)
        res << (({b => 1} == {b => 1}) rescue $!.class)
        res
        "##,
    );
}

/// Without a `method_missing` of its own the failure is the NoMethodError
/// of the `==` that was looked for.
#[test]
fn eq_without_a_method_raises_no_method_error() {
    run_test_once(
        r##"
        class Baz1971N < BasicObject
          undef_method :==
        end
        b = Baz1971N.new
        res = []
        res << ((b == 1) rescue [$!.class, $!.message])
        res << ((b != 1) rescue [$!.class, $!.message])
        res << (([b] == [1]) rescue [$!.class, $!.message])
        res << ((1 == b) rescue [$!.class, $!.message])
        res
        "##,
    );
}

/// A private `==`: the operator may not call it (and asks
/// `method_missing`), `rb_funcall` may.
#[test]
fn private_eq() {
    run_test_once(
        r##"
        class Priv1971
          private def ==(o) = :priv
          def method_missing(name, *a) = name == :== ? :mm : super
        end
        class Priv1971N
          private def ==(o) = :priv
        end
        res = []
        [Priv1971.new, Priv1971N.new].each do |pr|
          res << ((pr == 1) rescue [$!.class, $!.message])
          res << ((pr != 1) rescue [$!.class, $!.message])
          res << (([pr] == [1]) rescue [$!.class, $!.message])
          res << ((1 == pr) rescue [$!.class, $!.message])
          res << ((1 === pr) rescue [$!.class, $!.message])
        end
        res
        "##,
    );
}

/// The same dispatches from compiled code, where the operator sites have
/// gone through their inline caches.
#[test]
fn eq_method_missing_jit() {
    run_test_once(
        r##"
        class Baz1971J < BasicObject
          undef_method :==
          def method_missing(name, *a) = name == :== ? :mm : super
        end
        def f1971(b)
          [b == 1, b != 1, [b] == [1], 1 == b, 1.5 == b, 1 != b, "a" == b]
        end
        b = Baz1971J.new
        res = nil
        40.times { res = f1971(b) }
        res
        "##,
    );
}

/// An error raised under a default `!=` passes through `BasicObject#!=`,
/// and one raised under a builtin `==` through that builtin, each at the
/// line of the Ruby frame that called it (#1982).
#[test]
fn backtrace_through_default_ne_and_builtin_eq() {
    run_test_once(
        r##"
        strip = ->(bt) { bt.map { |l| l.sub(/\A.*:(\d+:in )/, '\1') } }
        s = Struct.new(:a)
        class Boom1982
          def ==(o) = raise("y")
        end
        res = []
        begin
          Boom1982.new != 1
        rescue => e
          res << strip.(e.backtrace)
        end
        begin
          s.new(Boom1982.new) != s.new(Boom1982.new)
        rescue => e
          res << strip.(e.backtrace)
        end
        begin
          s.new(Boom1982.new) == s.new(Boom1982.new)
        rescue => e
          res << strip.(e.backtrace)
        end
        begin
          1 == Boom1982.new
        rescue => e
          res << strip.(e.backtrace)
        end
        begin
          1.5 != Boom1982.new
        rescue => e
          res << strip.(e.backtrace)
        end
        begin
          (2**70) == Boom1982.new
        rescue => e
          res << strip.(e.backtrace)
        end
        begin
          [Boom1982.new] == [1]
        rescue => e
          res << strip.(e.backtrace)
        end
        begin
          [[Boom1982.new]] != [[1]]
        rescue => e
          res << strip.(e.backtrace)
        end
        res
        "##,
    );
}

/// The frames of the same backtraces from compiled code (the megamorphic
/// and the specialized path of `!=`).
#[test]
fn backtrace_through_default_ne_jit() {
    run_test_once(
        r##"
        strip = ->(bt) { bt.map { |l| l.sub(/\A.*:(\d+:in )/, '\1') } }
        class Quiet1982
          def ==(o) = false
        end
        class Boom1982J
          def ==(o) = $boom ? raise("y") : false
        end
        def ne1982(a, b)
          a != b
        end
        def eq1982(a, b)
          a == b
        end
        s = Struct.new(:a)
        $boom = false
        40.times do
          ne1982(Boom1982J.new, 1)
          ne1982(s.new(Boom1982J.new), s.new(1))
          ne1982(1, Boom1982J.new)
          ne1982(Quiet1982.new, 1)
          eq1982(1, Boom1982J.new)
          eq1982(1.5, Boom1982J.new)
        end
        $boom = true
        res = []
        pairs = [[Boom1982J.new, 1], [s.new(Boom1982J.new), s.new(Boom1982J.new)],
                 [1, Boom1982J.new]]
        # Not `each`: a builtin written in Ruby has a frame of its own
        # position here.
        i = 0
        while i < pairs.size
          a, b = pairs[i]
          begin
            ne1982(a, b)
          rescue => e
            res << strip.(e.backtrace)
          end
          begin
            eq1982(a, b)
          rescue => e
            res << strip.(e.backtrace)
          end
          i += 1
        end
        res
        "##,
    );
}

/// A `!=` site that saw one receiver class calls `BasicObject#!=` itself,
/// which runs a native `==` in its own frame: each of the two is in the
/// backtrace once.
#[test]
fn backtrace_through_default_ne_monomorphic_jit() {
    run_test_once(
        r##"
        strip = ->(bt) { bt.map { |l| l.sub(/\A.*:(\d+:in )/, '\1') } }
        class BoomM1982
          def ==(o) = $boom1982 ? raise("y") : false
        end
        def ne_s1982(a, b)
          a != b
        end
        def ne_a1982(a, b)
          a != b
        end
        def ne_o1982(a, b)
          a != b
        end
        def ne_m1982(a, b)
          a.!=(b)
        end
        s = Struct.new(:a)
        $boom1982 = false
        50.times do
          ne_s1982(s.new(BoomM1982.new), s.new(BoomM1982.new))
          ne_a1982([BoomM1982.new], [BoomM1982.new])
          ne_o1982(BoomM1982.new, 1)
          ne_m1982(s.new(BoomM1982.new), s.new(BoomM1982.new))
        end
        $boom1982 = true
        res = []
        begin
          ne_s1982(s.new(BoomM1982.new), s.new(BoomM1982.new))
        rescue => e
          res << strip.(e.backtrace)
        end
        begin
          ne_a1982([BoomM1982.new], [BoomM1982.new])
        rescue => e
          res << strip.(e.backtrace)
        end
        begin
          ne_o1982(BoomM1982.new, 1)
        rescue => e
          res << strip.(e.backtrace)
        end
        begin
          ne_m1982(s.new(BoomM1982.new), s.new(BoomM1982.new))
        rescue => e
          res << strip.(e.backtrace)
        end
        $boom1982 = false
        res << ne_s1982(s.new(BoomM1982.new), s.new(BoomM1982.new))
        res
        "##,
    );
}

/// `Comparable#==` under a default `!=`: the labels of the frames (the
/// position of a builtin written in Ruby is the interpreter's own file
/// here, the caller's in CRuby).
#[test]
fn backtrace_through_comparable_eq() {
    run_test_once(
        r##"
        class Cmp1982
          include Comparable
          def <=>(o) = raise("x")
        end
        begin
          Cmp1982.new != Cmp1982.new
        rescue => e
          e.backtrace.map { |l| l.sub(/\A.*:in /, '') }
        end
        "##,
    );
}

/// The caller's line as a method reached through a binary operator sees
/// it: the operator's, not the line the calling method starts at.
#[test]
fn caller_line_through_an_operator() {
    run_test_once(
        r##"
        strip = ->(bt) { bt.map { |l| l.sub(/\A.*:(\d+:in )/, '\1') } }
        class Callee1982
          def ==(o) = caller(0)
          def +(o) = caller(0)
          def -(o) = caller(0)
          def <(o) = caller(0)
          def <=>(o) = caller(0)
          def <<(o) = caller(0)
        end
        b = Callee1982.new
        res = []

        res << strip.(b == 1)

        res << (b != 1)

        res << strip.(b + 1)

        res << strip.(b - 1)

        res << strip.(b < 1)

        res << strip.(b <=> 1)

        res << strip.(b << 1)
        res
        "##,
    );
}

/// The same once the calling method is compiled, with the operator sites
/// both monomorphic and megamorphic.
#[test]
fn caller_line_through_an_operator_jit() {
    run_test_once(
        r##"
        strip = ->(bt) { bt.is_a?(Array) ? bt.map { |l| l.sub(/\A.*:(\d+:in )/, '\1') } : bt }
        class Callee1982J
          def ==(o) = caller(0)
          def +(o) = caller(0)
        end
        class Other1982A; def ==(o) = false; def +(o) = 1; end
        class Other1982B; def ==(o) = false; def +(o) = 2; end
        class Other1982C; def ==(o) = false; def +(o) = 3; end
        class Other1982D; def ==(o) = false; def +(o) = 4; end
        def ops1982(b)
          x = b == 1

          y = b != 1

          z = b + 1

          [x, y, z]
        end
        b = Callee1982J.new
        res = []
        40.times { ops1982(b) }
        res << ops1982(b).map(&strip)
        others = [Other1982A.new, Other1982B.new, Other1982C.new, Other1982D.new]
        40.times { |i| ops1982(others[i % 4]) }
        res << ops1982(b).map(&strip)
        res
        "##,
    );
}

/// Every shape a binary operator is compiled in — a value, a branch
/// condition, a loop condition, an immediate or a local operand, a number
/// on the left — in the interpreter, in a compiled method whose sites saw
/// one class, and in one whose sites saw many.
#[test]
fn caller_line_through_operator_shapes() {
    run_test_once(
        r##"
        class Shape1982
          def ==(o) = ($r1982 << caller(0)[1][/(\d+):in/, 1].to_i; true)
          def <(o) = ($r1982 << caller(0)[1][/(\d+):in/, 1].to_i; true)
          def >(o) = ($r1982 << caller(0)[1][/(\d+):in/, 1].to_i; false)
          def +(o) = ($r1982 << caller(0)[1][/(\d+):in/, 1].to_i; 1)
          def -(o) = ($r1982 << caller(0)[1][/(\d+):in/, 1].to_i; 1)
          def *(o) = ($r1982 << caller(0)[1][/(\d+):in/, 1].to_i; 1)
          def <=>(o) = ($r1982 << caller(0)[1][/(\d+):in/, 1].to_i; 0)
          def <<(o) = ($r1982 << caller(0)[1][/(\d+):in/, 1].to_i; true)
          def &(o) = ($r1982 << caller(0)[1][/(\d+):in/, 1].to_i; true)
          def %(o) = ($r1982 << caller(0)[1][/(\d+):in/, 1].to_i; true)
          def **(o) = ($r1982 << caller(0)[1][/(\d+):in/, 1].to_i; true)
          def /(o) = ($r1982 << caller(0)[1][/(\d+):in/, 1].to_i; true)
          def >>(o) = ($r1982 << caller(0)[1][/(\d+):in/, 1].to_i; true)
        end
        class Plain1982
          def ==(o) = false
          def <(o) = true
          def >(o) = false
          def +(o) = 1
          def -(o) = 1
          def *(o) = 1
          def <=>(o) = 0
          def <<(o) = 1
          def &(o) = 1
          def %(o) = 1
          def **(o) = 1
          def /(o) = 1
          def >>(o) = 1
        end
        def shapes1982(b)
          y = b + 1

          if b == 1
            y = b - 1
          end
          z = (b != 1 ? 1 :
               2)
          z = b * 2 unless b < 1

          while b > 1
          end
          z = 1 if b <=> 1

          z = b << 1
          z = b & 1
          z = b % 1
          z = b ** 1
          z = b / 1
          z = b >> 1
          z = 3 if 1 == b
          z = 3 if 1 != b

          z = 3 if 1.5 == b
          b.==(1)
          z = b.+ 3

          z = b + b
          z = b == b
          x = 5
          z = b + x
          z = b == x

          z = b == 1 && b == 2 &&
            b == 3
          z = [b] == [1]

          z = [b] != [1]
          z
        end
        b = Shape1982.new
        res = []
        $r1982 = []; shapes1982(b); res << $r1982.dup
        50.times { shapes1982(b) }
        $r1982 = []; shapes1982(b); res << $r1982.dup
        others = 5.times.map { Class.new(Plain1982).new }
        50.times { |i| shapes1982(others[i % 5]) }
        $r1982 = []; shapes1982(b); res << $r1982.dup
        res
        "##,
    );
}

/// An error rescued inside a method that an operator called: the frames
/// below it carry the operator's line.
#[test]
fn rescued_backtrace_line_through_an_operator() {
    run_test_once(
        r##"
        strip = ->(bt) { bt.map { |l| l.sub(/\A.*:(\d+:in )/, '\1') } }
        class Resc1982
          def ==(o)
            raise "x"
          rescue => e
            $bt1982 = e.backtrace
          end
          def *(o)
            raise "x"
          rescue => e
            $bt1982 = e.backtrace
          end
        end
        res = []

        Resc1982.new == 1
        res << strip.($bt1982)

        Resc1982.new * 1
        res << strip.($bt1982)
        res
        "##,
    );
}

/// `Comparable#==` does not rescue what `<=>` raises, and reads its
/// answer as `rb_cmpint` does.
#[test]
fn comparable_eq_propagates() {
    run_test_once(
        r##"
        res = []
        [->(o) { raise(ArgumentError, "ae") },
         ->(o) { raise(NoMethodError, "nme") },
         ->(o) { raise(TypeError, "te") },
         ->(o) { "x" }, ->(o) { nil }, ->(o) { 0.0 }, ->(o) { 1.5 },
         ->(o) { 2**70 }, ->(o) { 0 }, ->(o) { -1 }].each do |body|
          c = Class.new { include Comparable; define_method(:<=>, &body) }
          res << ((c.new == c.new) rescue [$!.class, $!.message])
          res << ((c.new != c.new) rescue [$!.class, $!.message])
        end
        a = Class.new { include Comparable; def <=>(o) = raise("never") }.new
        res << (a == a) << (a != a)
        res
        "##,
    );
}

/// `Integer#===` / `Float#===` are their `==`: an argument that is no
/// number is asked `arg == self`, from the operator, from `case`/`when`
/// and from the builtins that match with `===`.
#[test]
fn teq_reverse_dispatch() {
    run_test_once(
        r##"
        class TeqObj1982
          def ==(o) = $teq_boom1982 ? raise("y") : (o == 1 || o == 1.5 || o == 2**70)
        end
        o = TeqObj1982.new
        $teq_boom1982 = false
        res = []
        res << [o].grep(1).size << [o].grep(1.5).size << [o].grep(2**70).size << [o].grep(2).size
        res << [o].any?(1) << [o].all?(1.5) << [o].none?(2**70) << [o].one?(3)
        res << [o, o].any?(2.5) << [o].all?(3**70)
        res << (1 === o) << (1.5 === o) << ((2**70) === o) << (2 === o)
        res << (case o when 1 then :a else :b end) << (case o when 2.5 then :a else :b end)
        $teq_boom1982 = true
        # The frame between the two is `Integer#===` in CRuby and the
        # method it is an alias of here: only its presence is compared.
        begin
          1 === o
        rescue => e
          bt = e.backtrace.map { |l| l.sub(/\A.*:(\d+:in )/, '\1') }
          res << [bt.size, bt.first, bt.last]
        end
        res << (([o].grep(1.5) rescue $!.message)) << (([o].any?(2**70) rescue $!.message))
        res
        "##,
    );
}

/// A `throw` out of the `==` a default `!=` or a reverse dispatch called
/// is no exception: it has no backtrace to add a frame to.
#[test]
fn throw_through_elided_frames() {
    run_test_once(
        r##"
        class ThrowEq1982
          def ==(o) = throw(:eq1982, :thrown)
        end
        res = []
        res << catch(:eq1982) { ThrowEq1982.new != 1 }
        res << catch(:eq1982) { 1 == ThrowEq1982.new }
        res << catch(:eq1982) { 1 != ThrowEq1982.new }
        res << catch(:eq1982) { 1.5 == ThrowEq1982.new }
        res << catch(:eq1982) { [ThrowEq1982.new] != [1] }
        res
        "##,
    );
}

/// The body of `String#==` reached through a real call (`send`,
/// `Method#call`): an argument that responds to `to_str` is asked.
#[test]
fn string_eq_body_asks_a_to_str_argument() {
    run_test_once(
        r##"
        class StrLike1970
          def to_str = "a"
          def ==(o) = o == "a" ? :yes : nil
        end
        s = StrLike1970.new
        ["a".send(:==, s), "b".send(:==, s), "a".public_send(:==, s),
         "a".method(:==).call(s), "a" == s, "b" == s, "a" != s, "b" != s]
        "##,
    );
}

/// No `==` and no `method_missing` either: the NoMethodError of the `==`.
#[test]
fn eq_without_method_missing() {
    run_test_once(
        r##"
        class NoMM1971 < BasicObject
          undef_method :==
          undef_method :method_missing
        end
        n = NoMM1971.new
        [((n == 1) rescue $!.class), ((n != 1) rescue $!.class),
         (([n] == [1]) rescue $!.class), ((1 == n) rescue $!.class),
         (([n].include?(1)) rescue $!.class)]
        "##,
    );
}

/// `Complex#to_i` / `#to_f` / `#to_r` ask a custom imaginary part whether
/// it `== 0`.
#[test]
fn complex_conversions_ask_the_imaginary_part() {
    run_test_once(
        r##"
        class ImNum1970 < Numeric
          def initialize(v) = @v = v
          def ==(o) = @v == o
          def real? = true
          def coerce(o) = [o, @v]
          def to_s = "im#{@v}"
          def negative? = false
          def <(o) = @v < o
        end
        c0 = Complex(1, ImNum1970.new(0))
        c1 = Complex(1, ImNum1970.new(2))
        [c0, c1].flat_map do |c|
          [(c.to_i rescue $!.class), (c.to_f rescue $!.class), (c.to_r rescue $!.class)]
        end
        "##,
    );
}
