//! The report of an uncaught exception goes through a replaced
//! `$stderr` (#2253); `RegexpError` messages carry the pattern's
//! encoding and CRuby's name for an unclosed `\p{` (#2254);
//! `Regexp#to_s` escapes a bare `/` and control characters (#2255);
//! a regexp literal with a backslash in it is marked whole in a syntax
//! error report (#2256).

use monoruby::tests::*;
use std::process::Command;

/// Run `script` on monoruby and on CRuby and compare stdout, stderr
/// and the exit status: the report is written at process exit, which
/// the in-process harness never reaches.
fn run_both(script: &str) {
    let mono = Command::new(env!("CARGO_BIN_EXE_monoruby"))
        .env_remove("RUBYOPT")
        .args(["--disable=gems", "-e", script])
        .output()
        .unwrap();
    let ruby = Command::new(ruby_path())
        .args(["--disable=gems,rubyopt", "-e", script])
        .output()
        .unwrap();
    assert_eq!(
        String::from_utf8_lossy(&mono.stdout),
        String::from_utf8_lossy(&ruby.stdout),
        "stdout mismatch for {script:?}"
    );
    assert_eq!(
        String::from_utf8_lossy(&mono.stderr),
        String::from_utf8_lossy(&ruby.stderr),
        "stderr mismatch for {script:?}"
    );
    assert_eq!(
        mono.status.code(),
        ruby.status.code(),
        "exit status mismatch for {script:?}"
    );
}

#[test]
fn uncaught_report_goes_to_a_replaced_stderr() {
    // Into a StringIO, after the at_exit handlers: nothing on fd 2.
    run_both(
        r#"
        require "stringio"
        $stderr = StringIO.new
        at_exit { STDOUT.puts "captured: #{$stderr.string.inspect}" }
        raise "boom"
        "#,
    );
    // Through an object's own `write`, the cause chain included.
    run_both(
        r#"
        o = Object.new
        def o.write(*a) = STDOUT.print("W:", *a.map(&:inspect), "\n")
        $stderr = o
        def f = raise("in f")
        begin; f; rescue => e; raise TypeError, "outer"; end
        "#,
    );
    // A dup of stdout, which here is a pipe.
    run_both(r#"$stderr = STDOUT.dup; raise ArgumentError, "x""#);
    // The original STDERR assigned back is fd 2 again.
    run_both(r#"$stderr = STDOUT; $stderr = STDERR; raise "plain""#);
    // An at_exit handler's own exit status still wins.
    run_both(
        r#"
        o = Object.new
        def o.write(*a) = STDOUT.print(*a)
        $stderr = o
        at_exit { exit 3 }
        raise "with status"
        "#,
    );
}

#[test]
fn regexp_error_message_encoding_and_property_name() {
    run_test_once(
        r##"
        t = -> (&b) { begin; b.call; :ok; rescue => e; [e.class, e.message, e.message.encoding.to_s]; end }
        [
          t.() { Regexp.new("(") },
          t.() { Regexp.new("あ(") },
          t.() { Regexp.new("(".encode("EUC-JP")) },
          t.() { Regexp.new("あ(".encode("EUC-JP")) },
          t.() { Regexp.new("あ(".encode("Shift_JIS")) },
          t.() { Regexp.new("(".b) },
          t.() { Regexp.new("\xff(".b) },
          t.() { Regexp.new("(".encode("US-ASCII")) },
          t.() { Regexp.new("\\xff".encode("EUC-JP")) },
          t.() { Regexp.new("\\u{110000}".encode("EUC-JP")) },
          t.() { Regexp.new("あ\\".encode("EUC-JP")) },
          t.() { Regexp.new("(?<a>b)\\k<c>".encode("EUC-JP")) },
          t.() { Regexp.new("a{2,1}".encode("EUC-JP")) },
          t.() { Regexp.new("\\xA4".force_encoding("EUC-JP")) },
          t.() { Regexp.new("\xA4(".force_encoding("EUC-JP")) },
          t.() { Regexp.new("(", Regexp::IGNORECASE) },
          t.() { Regexp.new("\\p{") },
          t.() { Regexp.new("\\p{".encode("EUC-JP")) },
          t.() { Regexp.new("\\p{Foo}") },
          t.() { Regexp.new("\\p{Foo".encode("EUC-JP")) },
          t.() { Regexp.new("\\p{Foo") },
          t.() { Regexp.new("\\P{Foo", Regexp::EXTENDED) },
          t.() { Regexp.new("\\p{^") },
          t.() { Regexp.new("\\p{^}") },
          t.() { Regexp.new("\\p{}") },
          t.() { Regexp.new("a\\p{L") },
          t.() { Regexp.new("\\p{Al(pha}") },
          t.() { Regexp.new("\\p{Al|") },
          t.() { Regexp.new("\\\\p{") },
          t.() { Regexp.new("[\\p{Alpha]") },
          t.() { Regexp.new("\\p{Alpha}\\p{Fo") },
        ]
        "##,
    );
}

#[test]
fn regexp_to_s_escapes_slash_and_controls() {
    run_test_once(
        r##"
        [
          Regexp.new("a\x01b").to_s, Regexp.new("あ/").to_s, Regexp.new("a\x7f").to_s,
          Regexp.new("a/b").to_s, Regexp.new("\t").to_s, Regexp.new("a\\/b").to_s,
          Regexp.new("(?i:a/b)").to_s, Regexp.new("a\nb\x0c\x1b").to_s, Regexp.new("あ\x00").to_s,
          Regexp.new("\xA4\xA2/".force_encoding("EUC-JP")).to_s.bytes,
          Regexp.new("\xA4\xA2/".force_encoding("EUC-JP")).to_s.encoding.to_s,
          Regexp.new("\xA4\xA2".force_encoding("EUC-JP")).to_s.bytes,
          Regexp.new("\xff/".b).to_s, Regexp.new("\xff/".b).to_s.encoding.to_s,
          /x#{Regexp.new("a/b")}y/.source, /x#{Regexp.new("a\x01")}/.source,
          "#{Regexp.new("a/\x02")}", Regexp.new("a\x01").inspect,
        ]
        "##,
    );
}

#[test]
fn regexp_literal_with_a_backslash_is_marked_whole() {
    run_test_once(
        r##"
        ['/a\/(/', '/\d(/', '/a\n(/', '/\\\\(/', '/a\/(/i', 'x = 1; /a\/(/m', '%r{a\/(}',
         '%r{a/(}', '%r{\{(}', '/(a\/)(/', "/a\n\\/(/", '/\//; /(/', '/a\/(/ + 1',
         'p(/\/(?<>a)/)', '/(/', '/a(/', 'x = /ab(c/i', "/a\n(/", 'if /a\/(/; end',
         '/\/*/', '/\/(?z)/', '/\/[]/'].map do |src|
          eval(src)
          :ok
        rescue SyntaxError => e
          e.message.lines.drop(1).join
        end
        "##,
    );
}
