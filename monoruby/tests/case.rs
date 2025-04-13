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
