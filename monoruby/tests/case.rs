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
