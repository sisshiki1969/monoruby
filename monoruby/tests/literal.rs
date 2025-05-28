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
fn symbol_interpolation() {
    run_test(r##":"abc#{50*50}def""##);
}

#[test]
fn percent_q() {
    run_test(r##"%q(abcdefghi)"##);
}

#[test]
fn percent_q_() {
    run_test(r##"%Q(())"##);
    run_test(r##"%Q[[]]"##);
    run_test(r##"%Q{{}}"##);
    run_test(r##"%Q(abcdefghi)"##);
    run_test(r##"%Q(ab#{100*200}c#@a)"##);
    run_test(r##"class C; @@a = :Gquux; $res = %Q(ab#{100*200}c#@@a); end; $res"##);
    run_test(r##"%Q(ab#{100*200}c#$a)"##);
    run_test(r##"%Q(ab#{100*200}c#$1)"##);
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
    run_test(r##"%W(abc #{5*5}def#{4*4} ghi)"##);
    run_test(r##"%W(abc def#{5*5}#{4*4} ghi)"##);
    run_test(r##"%W(abc def#{5*5} #{4*4} ghi)"##);
    run_test(r##"%W(abc def #{5*5} #{4*4} ghi)"##);
    run_test(
        r##"%W(abc def #{5*5}
#{4*4} ghi)"##,
    );
}

#[test]
fn percent_r() {
    run_test(r##"%r(abcdefghi).to_s"##);
    run_test(r##"%r(abc[d-f[g-i]]).to_s"##);
    run_test(r##"%r[abc[d-f[g-i]]].to_s"##);
    run_test(r##"%r[a#{100}bc[d-f#{500}[g-i]]#{20*50}].to_s"##);
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
