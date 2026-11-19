extern crate monoruby;
use monoruby::tests::*;

#[test]
fn case0() {
    run_test(
        r#"
        foo = false
        bar = true
        quu = false
        
        case
        when foo then 'foo is true'
        when bar then 'bar is true'
        when quu then 'quu is true'
        end
        "#,
    );
}

#[test]
fn case1() {
    run_test(
        r#"
        foo = false
        bar = true
        quu = false
        
        case
        when foo then 'foo is true'
        when bar then 'bar is true'
        when quu then 'quu is true'
        end
        100
        "#,
    );
}

#[test]
fn case2() {
    run_test(
        r#"
        foo = false
        bar = true
        quu = false
        
        x = case
        when foo then 'foo is true'
        when bar then 'bar is true'
        when quu then 'quu is true'
        end
        "#,
    );
}

#[test]
fn case3() {
    run_test(
        r#"
        c = 5
        case
        when c == 3, c == 6, c == 9
          '10より小さな3の倍数'
        when c % 10 == 0
          '10の倍数'
        when c % 5 == 0
          '5の倍数'
        else
          'それ以外'
        end
        "#,
    )
}

#[test]
fn case4() {
    run_test(
        r#"
        case :symbol
        when Integer then 'integer'
        when Float then 'float'
        when Symbol then 'symbol'
        end
        "#,
    );
}

#[test]
fn case5() {
    run_test(
        r#"
        x = case :symbol
        when Integer then 'integer'
        when Float then 'float'
        when Symbol,String then 'symbol'
        end
        "#,
    );
}

#[test]
fn case6() {
    run_test(
        r#"
        a = []

        10.times do |x|
          case x
          when *[1,2],5
            a << "Matched 1"
          when 3,*[3,9,:a]
            a << "Matched 2"
          else
            a << "Not matched"
          end
        end
        a
        "#,
    );
}

#[test]
fn case_opt1() {
    run_test(
        r#"
        case 4
        when 0
          a = 10
        when 1
          a = 11
        when 2
          a = 12
        when 3, 4
          a = 13
        end
        a
        "#,
    );
}

#[test]
fn case_opt2() {
    run_test(
        r#"
        case 9
        when 0,4,8
          0
        when 1,5,9
          1
        when 2,6
          2
        when 3,7
          3
        else
          4
        end
        "#,
    );
}

#[test]
fn case_opt3() {
    run_test(
        r#"
        case 9
        when 0,4,8
          0
        when 1,5,9
          1
        when 2,6
          2
        when 3,5000
          3
        else
          4
        end
        "#,
    );
}

#[test]
fn case_opt4() {
    run_test(
        r#"
        case 9
        when 0,4,8
          0
        when 1,5,9
          1
        when 2,6
          2
        when 3,"5000"
          3
        else
          4
        end
        "#,
    );
}

#[test]
fn case_opt5() {
    run_test(
        r#"
      res = []
      50.times do |x|
        case x % 20
        when 0
          res << "zero"
        when 1
          res << "one"
        when 2
          res << "two"
        when 3,4,5,6
          res << "three-six"
        when 7
          res << "seven"
        when 8
          res << "eight"
        when 9
          res << "nine"
        when 10,11,12,13,14
          res << "ten-fourteen"
        when 15
          res << "fifteen"
        when 16
          res << "sixteen"
        when 17
          res << "seventeen"
        else
          res << "above eighteen"
        end
      end
      res
        "#,
    );
}

#[test]
fn case_when_splat() {
    // Subject-less `case` with a splat `when *arr`: matches when any
    // element of `arr` is truthy.
    run_tests(&[
        r#"
        def t(arr)
          case
          when *arr then "match"
          else "no"
          end
        end
        [t([false, nil]), t([false, 3]), t([]), t([nil]), t([true])]
        "#,
        // Splat combined with an ordinary condition (boolean disjunction).
        r#"
        def u(x)
          case
          when *[false], x then "y"
          else "n"
          end
        end
        [u(true), u(false)]
        "#,
        // Multiple splat `when` clauses pick the first truthy one.
        r#"
        case
        when *[false] then "a"
        when *[nil, 7] then "b"
        else "c"
        end
        "#,
        // With-subject splat still works (=== against each element).
        r#"
        case 3
        when *[1, 2] then "lo"
        when *[3, 4] then "hi"
        else "no"
        end
        "#,
    ]);
}

#[test]
fn case_calls_private_teq() {
    // case/when dispatches `===` with funcall semantics (a private
    // `===` is allowed), unlike an explicit `a === b`.
    run_test(
        r#"
        klass = Class.new do
          def ===(o); o == 1; end
          private :===
        end
        matcher = klass.new
        res = []
        3.times do |i|
          case i
          when matcher then res << "match #{i}"
          else res << "no #{i}"
          end
        end
        begin
          matcher === 1
          res << "no error"
        rescue NoMethodError
          res << "NoMethodError"
        end
        res
        "#,
    );
}

// Drive the case/when `===` dispatch through the JIT tier: the
// integration-test binaries use the production JIT thresholds, so the
// construct lives in a method called >20 times. The two matcher
// classes at the same call site make the comparison polymorphic,
// forcing the generic-cmp lowering (which must use funcall semantics
// for case/when: private `===` is still reachable).
#[test]
fn case_private_teq_jit() {
    run_test(
        r#"
        matcher_a = Class.new do
          def ===(x); x == :a; end
          private :===
        end.new
        matcher_b = Class.new do
          def ===(x); x == :b; end
        end.new

        def case_match(x, m)
          case x
          when m then "matcher"
          when Symbol then "symbol"
          else "other"
          end
        end

        res = []
        30.times do
          res << case_match(:a, matcher_a)
          res << case_match(:b, matcher_a)
          res << case_match(:b, matcher_b)
          res << case_match(1, matcher_b)
        end
        res
        "#,
    );
}

// Same for the splat form: `when *arr` goes through the array_teq
// runtime helper, which must also dispatch `===` with funcall
// semantics under the JIT.
#[test]
fn case_splat_teq_jit() {
    run_test(
        r#"
        private_matcher = Class.new do
          def ===(x); x == :hit; end
          private :===
        end.new

        def case_splat(x, arr)
          case x
          when *arr then "in"
          else "out"
          end
        end

        res = []
        list = [private_matcher, Integer]
        30.times do
          res << case_splat(:hit, list)
          res << case_splat(7, list)
          res << case_splat(:miss, list)
        end
        res
        "#,
    );
}

/// A `case` whose `when` values are all literals compares a key of a
/// literal's class by value, as CRuby's `opt_case_dispatch` does, and
/// calls no `===` — so a redefined `NilClass#==` (which `nil === 1`
/// calls) is not asked (#2033). A key of another class, a `when` value
/// that is no literal, a splat, and every `case` once the `===` of a
/// class the dispatch stands on was replaced (for good, as CRuby's
/// flag) go through `===`.
#[test]
fn case_literal_dispatch() {
    run_test_once(
        r##"
        def run
          r = []
          30.times { r << yield }
          r.uniq
        end
        def c1(x) = (case x; when nil then :a; else :b; end)
        def c2(x) = (case x; when nil then :a; when Integer then :i; else :b; end)
        def c3(x) = (case x; when nil, 2 then :a; else :b; end)
        def c4(x) = (case x; when true then :t; when false then :f; else :b; end)
        def c5(x) = (case x; when :z then :z; when "s" then :s; when 1.5 then :f; else :b; end)
        def c6(x) = (if nil === x then :a else :b end)
        def c7(x, v) = (case x; when v then :a; else :b; end)
        def c8(x) = (case x; when *[nil] then :a; else :b; end)
        class NilClass; def ==(o) = true; end
        class TrueClass; def ==(o) = true; end
        class FalseClass; def ==(o) = true; end
        res = []
        res << run { c1(1) } << run { c1(Object.new) } << run { c1(nil) }
        res << run { c1(1.0) } << run { c1("x") } << run { c2(1) }
        res << run { c3(1) } << run { c3(2) } << run { c3(2.0) }
        res << run { c4(1) } << run { c4(false) }
        res << run { c5(:q) } << run { c5(1.5) } << run { c5("s") }
        res << run { c6(1) } << run { c7(1, nil) } << run { c8(1) }
        class Symbol; def ==(o) = true; end
        res << run { c5(:q) }
        class Symbol; def ===(o) = true; end
        res << run { c5(:q) } << run { c1(1) }
        class Symbol; remove_method :===; end
        res << run { c1(1) }
        res
        "##,
    );
}

/// The table of a `case` with many Integer `when` values (`OptCase`)
/// answers a Fixnum key only: a Float, a Bignum or any other key, and
/// every key once `Integer#===` was replaced, goes through the `when`
/// values one by one.
#[test]
fn case_opt_table_fallback() {
    run_test_once(
        r##"
        def run
          r = []
          30.times { r << yield }
          r.uniq
        end
        def oc(x) = (case x; when 1 then :a; when 2 then :b; when 3 then :c; when 4 then :d; when 5 then :e; when 6 then :f; when 7 then :g; when 8 then :h; else :z; end)
        res = []
        res << run { oc(3) } << run { oc(3.0) } << run { oc(2**70) } << run { oc("3") }
        res << run { oc(3.5) } << run { oc(-0.0) } << run { oc(Float::INFINITY) } << run { oc(nil) }
        res << run { oc(:a) } << run { oc(0) } << run { oc(100) } << run { oc(-1) }
        o = Object.new; def o.==(x) = true
        res << run { oc(o) }
        class Integer; def ===(o) = o == 5; end
        res << run { oc(5) } << run { oc(3) }
        class Integer; remove_method :===; end
        res << run { oc(3) }
        res
        "##,
    );
}
