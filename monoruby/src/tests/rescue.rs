#[cfg(test)]
mod test {
    use crate::tests::*;

    #[test]
    fn rescue1() {
        run_test(
            r#"
            # Use
            begin
              100
            end
        "#,
        );
    }

    #[test]
    fn rescue2() {
        run_test(
            r#"
            # NoUse
            begin
              100
            end
            nil
        "#,
        );
    }

    #[test]
    fn rescue3() {
        run_test_once(
            r#"
            # Ret
            begin
              100
            end
        "#,
        );
    }

    #[test]
    fn rescue4() {
        run_test_once(
            r#"
            #Ret
            begin
              100
            rescue
              200
            end
        "#,
        );
    }

    #[test]
    fn rescue5() {
        run_test(
            r#"
            begin
              100
            rescue
              200
            end
        "#,
        );
    }

    #[test]
    fn rescue6() {
        run_test(
            r#"
            begin
              100
            rescue
            else
              200
            end
        "#,
        );
    }

    #[test]
    fn rescue7() {
        run_test(
            r#"
            begin
              100
            rescue
            else
              200
            ensure
              300
            end
        "#,
        );
    }

    #[test]
    fn rescue8() {
        run_test(
            r#"
            $x = []
            begin
                begin
                    $x << 50
                    1/0
                    $x << 100
                rescue => c
                    $x << 150
                else
                    $x << 200
                ensure
                    $x << 250
                    1/0
                    $x << 300
                end
            rescue => d
                $x << d.to_s
            end
            $x
        "#,
        );
    }

    #[test]
    fn rescue_write_back1() {
        run_test(
            r#"
        res = []
        for i in 0..10
            begin
                x = 50.0 + i
                1/0
                x = 100.0
            rescue => c
                res << x
                x = 150.0
            else
                x = 200.0
            ensure
                x = 300.0
            end
        end
        res << x
        res
        "#,
        );
    }

    #[test]
    fn rescue_write_back2() {
        run_test(
            r#"
        res = []
        begin
            for i in 0..10
                x = 50.0 + 50/(9-i)
                res << x
            end
        rescue => c
            res << x
            x = 150.0
        else
            x = 200.0
        ensure
            x = 300.0
        end
        res << x
        "#,
        );
    }

    #[test]
    fn block_return_ensure() {
        run_test_with_prelude(
            r#"
            $x = []
            [foo, $x]
            "#,
            r#"
            def foo
              2.times do |i|
                2.times  do |j|
                  $x << [i,j]
                  return 3 if i == 1 && j == 0
                ensure
                  $x << ["j",j]
                end
              ensure
                $x << ["i",i]
              end
            ensure
              $x << "foo"
            end
            "#,
        );
    }
}
