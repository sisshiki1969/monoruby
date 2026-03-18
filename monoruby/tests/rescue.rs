extern crate monoruby;
use monoruby::tests::*;

#[test]
fn begin_use() {
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
fn begin_nouse() {
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
fn begin_ret() {
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
fn rescue_ret() {
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
fn rescue_use() {
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
fn rescue_else() {
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
fn rescue_else_ensure() {
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

#[test]
fn retry1() {
    run_test(
        r#"
            $x = 0
            begin
              $x += 1
              raise "err" if $x < 3
            rescue
              retry
            end
            $x
        "#,
    );
}

#[test]
fn retry2() {
    run_test(
        r#"
            x = 0.0
            $res = []
            begin
              x += 1.0
              $res << x
              raise "err" if x < 3.0
              $res << "done"
            rescue
              $res << "caught"
              retry
            end
            $res
        "#,
    );
}

#[test]
fn retry3() {
    run_test(
        r#"
            $x = 0
            begin
              $x += 1
              raise "err" if $x < 2
              $x
            rescue
              retry
            ensure
              $x += 10
            end
        "#,
    );
}

#[test]
fn return_in_ensure() {
    run_test_with_prelude(
        "foo",
        r#"
            def foo
              1
            ensure
              return 42
            end
        "#,
    );
    run_test_with_prelude(
        "bar",
        r#"
            def bar
              1
            rescue
              2
            ensure
              return 99
            end
        "#,
    );
    run_test_with_prelude(
        "[baz, $x]",
        r#"
            def baz
              $x = []
              $x << :body
              $x << :ensure_start
              return 100 unless $!
            ensure
              $x << :ensure
              return 200
            end
        "#,
    );
}

#[test]
fn retry_in_loop() {
    run_test(
        r#"
            res = []
            50.times do
              x = 0.0
              begin
                x += 1.0
                raise "err" if x < 3.0
              rescue
                i = 0
                while i < 50
                  retry if i == 49
                  i += 1
                end
              end
              res << x
            end
            res
        "#,
    );
}

#[test]
fn load_error_path() {
    run_test(
        r#"
            begin
              require "nonexistent_file_that_does_not_exist_12345"
            rescue LoadError => e
              [e.class.name, e.path]
            end
        "#,
    );
}
