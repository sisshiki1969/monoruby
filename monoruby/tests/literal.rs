extern crate monoruby;
use monoruby::tests::*;

#[test]
fn literal_heredoc_percentq() {
    run_tests(&[
        r##"
        <<EOF
        Hello, world!
EOF
        "##,
        r##"
        <<-EOF
        Hello, world!
        EOF
        "##,
        r##"
            <<~EOF
            Hello, world 1!
        Hello, world 2!
EOF
            "##,
        r##"
        <<'EOF'
        Hello, world!
EOF
        "##,
        r##"
        <<-'EOF'
        Hello, world!
        EOF
        "##,
        r##"
        <<~'EOF'
        Hello, world 1!
    Hello, world 2!
EOF
        "##,
        r##"
        <<~'EOF'
        Hello, world 1!


    Hello, world 2!
EOF
        "##,
        r##"
        <<"EOF"
        Hello, world!
EOF
        "##,
        r##"
        <<-"EOF"
        Hello, world!
        EOF
        "##,
        r##"
        a = 77
        <<~"EOF"
        Hello, world 1!


            #{a * 2}
    Hello, world 2!
            EOF
        "##,
        r##"
        a = 77
        <<~"EOF"
        Hello, world 1!


            #{a * 2}
    Hello, world 2!



            EOF
        "##,
        r##":"abc#{50*50}def""##,
        r##"%q(abcdefghi)"##,
    ]);
}

#[test]
fn percent_q_() {
    run_tests(&[
        r##"%Q(())"##,
        r##"%Q[[]]"##,
        r##"%Q{{}}"##,
        r##"%Q(abcdefghi)"##,
        r##"%Q(ab#{100*200}c#@a)"##,
    ]);
    run_test(r##"class C; @@a = :Gquux; $res = %Q(ab#{100*200}c#@@a); end; $res"##);
    run_test(r##"%Q(ab#{100*200}c#$a)"##);
    run_test(r##"%Q(ab#{100*200}c#$1)"##);
}

#[test]
fn literal_percent_range_rational() {
    run_tests(&[
        r##"%w(abc def ghi)"##,
        "%w(abc   def \n    \n    ghi)",
        r##"%w(abc\ def \g\\hi)"##,
        r##"%w(abc\ de
    f \g\\h\ni)"##,
        r##"%W(abc def ghi)"##,
        "%W(abc   def \n    \n    ghi)",
        "%W(abc\\ def\\ \n    \n    ghi)",
        "%W(abc\\ def\\ \nghi)",
        r##"%W(abc\ def \g\\hi)"##,
        r##"%W(abc\ de
    f \g\\h\ni)"##,
        r##"%W(abc #{5*5}def#{4*4} ghi)"##,
        r##"%W(abc def#{5*5}#{4*4} ghi)"##,
        r##"%W(abc def#{5*5} #{4*4} ghi)"##,
        r##"%W(abc def #{5*5} #{4*4} ghi)"##,
        r##"%W(abc def #{5*5}
#{4*4} ghi)"##,
        r##"%r(abcdefghi).to_s"##,
        r##"%r(abc[d-f[g-i]]).to_s"##,
        r##"%r[abc[d-f[g-i]]].to_s"##,
        r##"%r[a#{100}bc[d-f#{500}[g-i]]#{20*50}].to_s"##,
        r##"%i(abc def ghi)"##,
        "%i(abc   def \n    \n    ghi)",
        r##"%i(abc\ def \g\\hi)"##,
        r##"%i(abc\ de
    f \g\\h\ni)"##,
        r#"100..200"#,
        r#"..200"#,
        r#"100.."#,
        r#"100...200"#,
        r#"...200"#,
        r#"100..."#,
        r#"a = 100; b = 200; a..b"#,
        r#"a = 100; b = 200; ..b"#,
        r#"a = 100; b = 200; a.."#,
        r#"a = 100; b = 200; a...b"#,
        r#"a = 100; b = 200; ...b"#,
        r#"a = 100; b = 200; a..."#,
        r#"5i"#,
        r#"4+5i"#,
        r#"42r"#,
        r#"0r"#,
        r#"3.14r"#,
        r#"0.5r"#,
        r#"1r + 2r"#,
        r#"3r * 4r"#,
        r#"1r / 3r"#,
        r#"1r"#,
        r#"3.14r"#,
        r#"42r == Rational(42, 1)"#,
        r#"3.14r == Rational(314, 100)"#,
    ]);
}

#[test]
fn rational_imaginary_literal() {
    run_test_once(r#"42ri.class.to_s"#);
    run_test_once(r#"3.14ri.class.to_s"#);
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
fn literal_regexp_format() {
    run_tests(&[
        r##"
        place = "東京都"
        /#{place}/.match?("Go to 東京都")
        "##,
        r##"
        number = "(\\d+)"
        operator = "(\\+|-|\\*|/)"
        /#{number}#{operator}#{number}/.match?("43+291")
        "##,
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
        r##"
    float_pat = /\A
      \d+ # 整数部
      (\. # 小数点
        \d+ # 小数部
      )?  # 小数点 + 小数部 はなくともよい
    \z/x
    float_pat.match?("3.14")
        "##,
        r##"/x y/x.match?("x y")"##,
        r##"/x\ y/x.match?("x y")"##,
        // %d with integer
        r#""%d" % 42"#,
        // %d with float (truncates to integer)
        r#""%d" % 5.7"#,
        r#""%d" % -3.9"#,
        r#""%d" % 0.0"#,
        // %d with float exceeding i64 range
        r#""%d" % 1.0e19"#,
        r#""%d" % -1.0e19"#,
        // %d with format flags and float
        r#""%02d:%02d:%02d" % [1, 2, 5.784]"#,
        // %i (alias for %d) with float
        r#""%i" % 3.14"#,
        // %f with float
        r#""%f" % 3.14"#,
        // %s with string
        r#""%s" % "hello""#,
        // multiple format specifiers
        r#""%d %f %s" % [1.5, 2.5, "x"]"#,
        r#"format("%d", 42)"#,
        r#"sprintf("%d", 42)"#,
        r#"format("%s", "hello")"#,
        r#"format("%05d", 42)"#,
        r#"format("%x", 255)"#,
        r#"format("%X", 255)"#,
        r#"format("%b", 10)"#,
        r#"format("%f", 3.14)"#,
        r#"format("%.2f", 3.14159)"#,
        r#"format("%c", 65)"#,
        r#"format("%%")"#,
        r#"format("%d %s %f", 1, "hello", 3.14)"#,
        r#"format("Hello %s, you are %d years old", "Alice", 30)"#,
        r#"format("%02d:%02d:%02d", 1, 2, 3)"#,
    ]);
}
