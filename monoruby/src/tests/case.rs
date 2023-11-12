#[cfg(test)]
mod test {
    use crate::tests::*;

    #[ignore]
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

    #[ignore]
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

    #[ignore]
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
        when Symbol then 'symbol'
        end
        "#,
        );
    }
}
