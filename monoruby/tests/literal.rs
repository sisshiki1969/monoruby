extern crate monoruby;
use monoruby::tests::*;

#[test]
fn imaginary() {
    run_test(r#"5i"#);
    run_test(r#"4+5i"#);
}

#[test]
fn lambda() {
    run_test(
        r#"
        j = 42
        f = ->(x){x + j}
        f.call(3)
        "#,
    );
    run_test_with_prelude(
        r#"
            f.call(17)
            "#,
        r#"
            f = ->(x){x + 42}
            "#,
    );
    run_test_once(
        r#"
        j = 42
        a = []
        f = ->(x){x + j}
        for i in 0..30
            a << f.call(3)
        end
        a
        "#,
    );
    run_test_error(
        r#"
            f = ->(a, b){}
            f.call(1)
            "#,
    );
}

#[test]
fn regexp0() {
    run_test(
        r##"
        place = "東京都"
        /#{place}/.match?("Go to 東京都")
        "##,
    );
    run_test(
        r##"
        number = "(\\d+)"
        operator = "(\\+|-|\\*|/)"
        /#{number}#{operator}#{number}/.match?("43+291") 
        "##,
    );
}

#[test]
fn regexp_char_class() {
    run_test(
        r##"
    r = /[a-w&&[^c-g]e]/ # ([a-w] かつ ([^c-g] もしくは e)) つまり [abeh-w] と同じ
    [
        /[a-z[0-9]]/.match?("y"), # => #<MatchData "y">
        /[a-z[0-9]]/.match?("["), # => nil
        r.match?("b"), # => #<MatchData "b">
        r.match?("c"), # => nil
        r.match?("e"), # => #<MatchData "e">
        r.match?("g"), # => nil
        r.match?("h"), # => #<MatchData "h">
        r.match?("w"), # => #<MatchData "w">
        r.match?("z"), # => nil
    ]
        "##,
    );
}

#[test]
fn regexp_free_format() {
    run_test(
        r##"
    float_pat = /\A
      \d+ # 整数部
      (\. # 小数点
        \d+ # 小数部
      )?  # 小数点 + 小数部 はなくともよい
    \z/x
    float_pat.match?("3.14")
        "##,
    );
    run_test(r##"/x y/x.match?("x y")"##);
    run_test(r##"/x\ y/x.match?("x y")"##);
}
