extern crate monoruby;
use monoruby::tests::*;

#[test]
fn comparable() {
    run_test_with_prelude(
        r##"
          res = []
          res << ( C.new(1) == C.new(2) )
          res << ( C.new(1) == C.new(1) )
          res << ( C.new(2) == C.new(1) )
          res << ( C.new(1) != C.new(2) )
          res << ( C.new(1) != C.new(1) )
          res << ( C.new(2) != C.new(1) )
          res << ( C.new(1) >= C.new(2) )
          res << ( C.new(1) >= C.new(1) )
          res << ( C.new(2) >= C.new(1) )
          res << ( C.new(1) > C.new(2) )
          res << ( C.new(1) > C.new(1) )
          res << ( C.new(2) > C.new(1) )
          res << ( C.new(1) <= C.new(2) )
          res << ( C.new(1) <= C.new(1) )
          res << ( C.new(2) <= C.new(1) )
          res << ( C.new(1) < C.new(2) )
          res << ( C.new(1) < C.new(1) )
          res << ( C.new(2) < C.new(1) )
          res
"##,
        r##"
          class C
            include Comparable
            attr_accessor :x
            def initialize(x)
              @x = x
            end
            def <=>(other)
              self.x <=> other.x
            end
          end
            "##,
    );
}

#[test]
fn comparable_non_standard_return() {
    // <=> can return any Numeric, not just -1/0/1
    run_test_with_prelude(
        r##"
          res = []
          res << ( W.new(1) == W.new(1) )
          res << ( W.new(1) == W.new(2) )
          res << ( W.new(1) < W.new(2) )
          res << ( W.new(2) > W.new(1) )
          res << ( W.new(1) <= W.new(2) )
          res << ( W.new(2) >= W.new(1) )
          res
"##,
        r##"
          class W
            include Comparable
            attr_accessor :x
            def initialize(x)
              @x = x
            end
            def <=>(other)
              (self.x - other.x) * 10
            end
          end
            "##,
    );
}

#[test]
fn comparable_float_return() {
    // <=> returning Float (e.g. 0.0) should work
    // Use run_test_once to avoid JIT optimization issues with Float comparison
    run_test_once(
        r##"
          class F
            include Comparable
            attr_accessor :x
            def initialize(x)
              @x = x
            end
            def <=>(other)
              (self.x - other.x).to_f
            end
          end
          res = []
          res << ( F.new(1) == F.new(1) )
          res << ( F.new(1) < F.new(2) )
          res << ( F.new(2) > F.new(1) )
          res << ( F.new(1) <= F.new(1) )
          res << ( F.new(1) >= F.new(1) )
          res
"##,
    );
}

#[test]
fn comparable_nil_return() {
    // <=> returning nil means comparison is not possible
    run_test_with_prelude(
        r##"
          res = []
          res << ( N.new(1) == N.new(2) )
          res << begin; N.new(1) < N.new(2); rescue ArgumentError; :arg_error; end
          res << begin; N.new(1) > N.new(2); rescue ArgumentError; :arg_error; end
          res << begin; N.new(1) <= N.new(2); rescue ArgumentError; :arg_error; end
          res << begin; N.new(1) >= N.new(2); rescue ArgumentError; :arg_error; end
          res
"##,
        r##"
          class N
            include Comparable
            attr_accessor :x
            def initialize(x)
              @x = x
            end
            def <=>(other)
              nil
            end
          end
            "##,
    );
}

#[test]
fn comparable_between() {
    run_test_with_prelude(
        r##"
          res = []
          res << C.new(3).between?(C.new(1), C.new(5))
          res << C.new(1).between?(C.new(1), C.new(5))
          res << C.new(5).between?(C.new(1), C.new(5))
          res << C.new(0).between?(C.new(1), C.new(5))
          res << C.new(6).between?(C.new(1), C.new(5))
          res
"##,
        r##"
          class C
            include Comparable
            attr_accessor :x
            def initialize(x)
              @x = x
            end
            def <=>(other)
              self.x <=> other.x
            end
          end
            "##,
    );
}

#[test]
fn comparable_clamp() {
    run_test_with_prelude(
        r##"
          res = []
          res << C.new(3).clamp(C.new(1), C.new(5)).x
          res << C.new(0).clamp(C.new(1), C.new(5)).x
          res << C.new(6).clamp(C.new(1), C.new(5)).x
          res << C.new(3).clamp(C.new(1)..C.new(5)).x
          res
"##,
        r##"
          class C
            include Comparable
            attr_accessor :x
            def initialize(x)
              @x = x
            end
            def <=>(other)
              self.x <=> other.x
            end
          end
            "##,
    );
}

#[test]
fn string_comparison_error_message() {
    run_test(
        r##"begin; "a" < 7; rescue ArgumentError => e; e.message; end"##,
    );
}

#[test]
fn comparable_has_no_ne_of_its_own() {
    // Comparable defines `==` but no `!=` (#1647): `!=` is
    // BasicObject's, which asks `==` — so a class that redefines `==`
    // gets an `!=` that agrees with it, a `<=>` answering nil makes
    // `!=` true rather than raising, and `Array#==` / `Struct#==` compare
    // elements with their own `==`.
    run_test_once(
        r##"
          class TT < Time; def ==(o) = true; end
          class V
            include Comparable
            attr_reader :v
            def initialize(v) = @v = v
            def <=>(o) = o.is_a?(V) ? v <=> o.v : nil
          end
          x = TT.at(0)
          s = Struct.new(:a)
          [Time.instance_method(:!=).owner, Comparable.instance_methods.sort,
           x != Time.at(1), [x] == [Time.at(1)], [x] != [Time.at(1)], [x].eql?([Time.at(1)]),
           Time.now != "x", Time.at(0) != Time.at(0), Time.at(0) != 1,
           V.new(1) != V.new(1), V.new(1) != V.new(2), V.new(1) != 3, V.new(1) == 3,
           s.new(x) == s.new(Time.at(1)), s.new(x) != s.new(Time.at(1)), s.new(1) != s.new(1.0), s.new(1) == s.new(1.0),
           ["a", 1.0, nil] == ["a", 1, nil], [1] != [1.0]]
"##,
    );
}
