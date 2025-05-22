extern crate monoruby;
use monoruby::tests::*;

#[test]
fn heredoc_default() {
    run_test(
        r##"
        <<EOF
        Hello, world!
EOF
        "##,
    );
    run_test(
        r##"
        <<-EOF
        Hello, world!
        EOF
        "##,
    );
    run_test(
        r##"
            <<~EOF
            Hello, world 1!
        Hello, world 2!
EOF
            "##,
    );
}

#[test]
fn heredoc_single() {
    run_test(
        r##"
        <<'EOF'
        Hello, world!
EOF
        "##,
    );
    run_test(
        r##"
        <<-'EOF'
        Hello, world!
        EOF
        "##,
    );
    run_test(
        r##"
        <<~'EOF'
        Hello, world 1!
    Hello, world 2!
EOF
        "##,
    );
    run_test(
        r##"
        <<~'EOF'
        Hello, world 1!


    Hello, world 2!
EOF
        "##,
    );
}

#[test]
fn heredoc_double() {
    run_test(
        r##"
        <<"EOF"
        Hello, world!
EOF
        "##,
    );
    run_test(
        r##"
        <<-"EOF"
        Hello, world!
        EOF
        "##,
    );
    run_test(
        r##"
        a = 77
        <<~"EOF"
        Hello, world 1!
   

            #{a * 2}
    Hello, world 2!
            EOF
        "##,
    );
    run_test(
        r##"
        a = 77
        <<~"EOF"
        Hello, world 1!
   

            #{a * 2}
    Hello, world 2!
      


            EOF
        "##,
    );
}

#[test]
fn percent_w() {
    run_test(r##"%w(abc def ghi)"##);
    run_test(
        r##"%w(abc   def 
    
    ghi)"##,
    );
    run_test(r##"%w(abc\ def \g\\hi)"##);
    run_test(
        r##"%w(abc\ de
    f \g\\h\ni)"##,
    );
}

#[ignore]
#[test]
fn percent_w_() {
    run_test(r##"%W(abc def ghi)"##);
    run_test(
        r##"%W(abc   def 
    
    ghi)"##,
    );
    run_test(
        r##"%W(abc\ def\ 
    
    ghi)"##,
    );
    run_test(
        r##"%W(abc\ def\ 
ghi)"##,
    );
    run_test(r##"%W(abc\ def \g\\hi)"##);
    run_test(
        r##"%W(abc\ de
    f \g\\h\ni)"##,
    );
}

#[test]
fn percent_i() {
    run_test(r##"%i(abc def ghi)"##);
    run_test(
        r##"%i(abc   def 
    
    ghi)"##,
    );
    run_test(r##"%i(abc\ def \g\\hi)"##);
    run_test(
        r##"%i(abc\ de
    f \g\\h\ni)"##,
    );
}

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
