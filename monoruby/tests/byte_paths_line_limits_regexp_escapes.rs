//! `Dir.glob` / `File.fnmatch` keep the bytes of a file name that is
//! not text of the pattern's encoding (#2243); `require` searches a
//! `$LOAD_PATH` entry whose bytes are not UTF-8 (#2244); a line limit
//! on a stream that converts counts bytes of the converted line, and a
//! line the limit cut does not count in `lineno` (#2247, #2261), and
//! `IO.readlines` sets `$.` (#2263); a
//! `\u` escape that names no character is a syntax error over its
//! digits (#2258); Onigmo's "invalid Unicode Property" and "without
//! escape" warnings print by default (#2259).

use monoruby::tests::*;

#[test]
#[cfg(target_os = "linux")]
fn glob_keeps_the_bytes_of_a_name() {
    run_test_once(
        r##"
        require "tmpdir"
        r = []
        Dir.mktmpdir do |d|
          File.binwrite(File.join(d, "\xA4\xA2.txt".b), "")
          File.binwrite(File.join(d, "b.txt"), "")
          Dir.mkdir(File.join(d, "\xA4\xA2dir".b))
          File.binwrite(File.join(d, "\xA4\xA2dir".b, "x\xA4.rb".b), "")
          Dir.chdir(d) do
            show = ->(a) { a.map { |n| [n.b, n.encoding.to_s, n.valid_encoding?] } }
            r << show.(Dir.glob("*"))
            r << Dir["*.txt"].map(&:b)
            r << show.(Dir.glob("\xA4\xA2*".b))
            r << show.(Dir.glob("\xA4\xA2*".dup.force_encoding("EUC-JP")))
            r << show.(Dir.glob("*".b))
            r << show.(Dir.glob("?.txt"))
            r << show.(Dir.glob("??.txt"))
            r << show.(Dir.glob("?.txt".dup.force_encoding("EUC-JP")))
            r << show.(Dir.glob("**/*.rb"))
            r << show.(Dir.glob("*/x?.rb"))
            r << show.(Dir.glob("[\xA4]*".b))
            r << show.(Dir.glob("{\xA4\xA2,b}.txt".b))
            r << show.(Dir.glob("*", base: "\xA4\xA2dir".b))
            r << show.(Dir.glob("\xA4\xA2dir/*".b))
            a = []; Dir.glob("*") { |n| a << n.b }; r << a
          end
        end
        r << File.fnmatch("\xA4\xA2*".b, "\xA4\xA2.txt".b)
        r << File.fnmatch("?.txt", "\xA4\xA2.txt".b)
        r << File.fnmatch("??.txt", "\xA4\xA2.txt".b)
        r << File.fnmatch("?.txt".dup.force_encoding("EUC-JP"), "\xA4\xA2.txt".b)
        r << File.fnmatch("?.txt".b, "あ.txt")
        r << File.fnmatch("???.txt".b, "あ.txt")
        r << File.fnmatch("\xA4\xA2*".b, "あ.txt")
        r << File.fnmatch("[あ-う].txt", "い.txt")
        r << File.fnmatch("*", "\xFF".b)
        r << File.fnmatch("Ä", "ä", File::FNM_CASEFOLD)
        r << File.fnmatch("[A-C]", "b", File::FNM_CASEFOLD)
        r
        "##,
    );
}

#[test]
#[cfg(target_os = "linux")]
fn require_searches_a_load_path_entry_that_is_not_utf8() {
    run_test_once(
        r##"
        require "tmpdir"
        r = []
        Dir.mktmpdir do |d|
          dir = File.join(d, "\xA4\xA2".b)
          Dir.mkdir(dir)
          File.write(File.join(dir, "feat_2244_b.rb"), "$loaded = __FILE__.b\n$n = ($n || 0) + 1\n")
          File.write(File.join(dir, "feat_2244_c.rb"), "$c = __FILE__.b\n")
          $LOAD_PATH.unshift(dir)
          begin
            r << require("feat_2244_b")
            r << $loaded.end_with?("\xA4\xA2/feat_2244_b.rb".b)
            r << require("feat_2244_b")
            r << $n
            r << $LOADED_FEATURES.last.b.end_with?("\xA4\xA2/feat_2244_b.rb".b)
            r << load("feat_2244_c.rb")
            r << $c.end_with?("\xA4\xA2/feat_2244_c.rb".b)
            k, v = $LOAD_PATH.resolve_feature_path("feat_2244_c")
            r << k << v.b.end_with?("feat_2244_c.rb")
          ensure
            $LOAD_PATH.delete(dir)
          end
        end
        r
        "##,
    );
}

#[test]
fn line_limit_counts_converted_bytes() {
    run_test_once(
        r##"
        require "tmpdir"
        r = []
        Dir.mktmpdir do |d|
          e = File.join(d, "e.txt")
          File.binwrite(e, "あいうえお\nabc\n".encode("EUC-JP"))
          File.open(e, "r:EUC-JP:UTF-8") { |f| r << [f.gets(3), f.gets(2), f.gets(7), f.gets(1), f.gets] }
          File.open(e, "r:EUC-JP:UTF-8") { |f| r << f.readlines(4) }
          File.open(e, "r:EUC-JP:UTF-8") { |f| r << f.each_line(5).to_a }
          File.open(e, "r:EUC-JP:UTF-8") { |f| r << f.each_line(5, chomp: true).to_a }
          File.open(e, "r:EUC-JP:UTF-8") { |f| r << f.each_line("う", 4).to_a }
          File.open(e, "r:EUC-JP:UTF-8") { |f| r << [f.gets(3), f.lineno, f.gets, f.lineno] }
          File.open(e, "r:EUC-JP:UTF-8") { |f| r << [f.gets(nil, 4), f.gets(nil, 4), f.read] }
          File.open(e, "r:EUC-JP:UTF-8") { |f| r << [f.gets(3), f.getc, f.gets(2), f.ungetc("x"), f.gets(4)] }
          File.open(e, "r:EUC-JP:UTF-8") { |f| r << [f.gets(0), f.gets(-1)] }
          r << IO.readlines(e, 4, encoding: "EUC-JP:UTF-8")
          a = []; IO.foreach(e, 4, encoding: "EUC-JP:UTF-8") { |l| a << [l, $.] }; r << a
          w = File.join(d, "w.txt")
          File.binwrite(w, "abcdef\nxyz\n".encode("UTF-16LE"))
          File.open(w, "rb:UTF-16LE:UTF-8") { |f| r << [f.gets(3), f.gets(1), f.gets(4), f.gets] }
          File.open(w, "rb:UTF-16LE:UTF-8") { |f| r << f.readlines(2) }
          pe = File.join(d, "pe.txt")
          File.binwrite(pe, "あい\n\n\nう\n".encode("EUC-JP"))
          File.open(pe, "r:EUC-JP:UTF-8") { |f| r << [f.gets("", 3), f.gets, f.gets("", 3), f.gets] }
          File.open(pe, "r:EUC-JP:UTF-8") { |f| r << [f.gets("", 6), f.gets, f.gets] }
          s = File.join(d, "s.txt")
          File.binwrite(s, "あいう".encode("Shift_JIS"))
          File.open(s, "r:Shift_JIS:UTF-8") { |f| r << [f.gets(1), f.gets(4), f.gets(100), f.gets] }
          u = File.join(d, "u.txt")
          File.binwrite(u, "あいう\n")
          File.open(u, "r:UTF-8:EUC-JP") { |f| r << [f.gets(1), f.gets(3), f.gets].map(&:b) }
          # Paragraph mode skips the line feeds after a line the limit
          # cut, on any stream.
          pa = File.join(d, "p.txt")
          File.binwrite(pa, "abc\n\n\ndef\n\n")
          File.open(pa) { |f| r << [f.gets("", 3), f.gets, f.gets] }
          File.open(pa) { |f| r << [f.gets("", 4), f.gets, f.gets] }
          File.open(pa, "rt") { |f| r << [f.gets("", 3), f.gets, f.gets] }
          File.open(pa, "r:EUC-JP:UTF-8") { |f| r << [f.gets("", 3), f.gets, f.gets] }
        end
        r
        "##,
    );
}

#[test]
fn a_line_the_limit_cut_does_not_count() {
    run_test_once(
        r##"
        require "tmpdir"
        r = []
        Dir.mktmpdir do |d|
          l = File.join(d, "l.txt")
          File.write(l, "abcdef\nxyz")
          File.open(l) do |f|
            a = []
            while (line = f.gets(4))
              a << [line, f.lineno, $.]
            end
            r << a
          end
          File.open(l) { |f| r << f.readlines(3).size << f.lineno }
          File.open(l) { |f| f.gets(nil, 2); r << f.lineno }
          File.open(l) { |f| f.gets(nil, 20); r << f.lineno }
          File.open(l) { |f| f.gets(7); r << f.lineno }
          File.open(l) { |f| f.gets("", 2); r << f.lineno }
          File.open(l) { |f| f.each_line(3) {}; r << f.lineno }
          a = []; IO.foreach(l, 4) { |x| a << [x, $.] }; r << a
          $. = 0; IO.readlines(l); r << $.
          $. = 0; File.readlines(l, 3); r << $.
          File.open(l, "r:UTF-8:UTF-16LE") { |f| f.gets(2); r << f.lineno; f.gets(30); r << f.lineno }
        end
        r
        "##,
    );
}

#[test]
fn out_of_range_unicode_escape_in_a_regexp_literal() {
    run_test_once(
        r##"
        srcs = ['/\u{110000}/', '/a\u{D800}/', '/\u{110000}#{1}/', '/\uD800/',
                '/\u{41 110000 DFFF}/', '/\\\\u{110000}/', '/[\u{110000}]/',
                '%r{\u{110000}}', '/\u{1100000}/', "/(?x) # \\u{110000}\n/",
                '/\u{110000}/n', 'if /\u{110000}/; end', '/\u{  110000 }/',
                '/\u{D800}/u', '/\u{0D800}/', '/\c\u{110000}/', '/\M-\u{110000}/',
                '/\u{0000041}/', '/\u{0000041}#{1}/', '/\u{110000/', '/\u{zz}/',
                '/\u{41 /', '/\u12/', '/a\u/', '/\u{}/', "/(?x)#\\u{zz}\n/",
                '/\u{41}\u{zz}/', '"\u{41"', '"\u{110000}"', '/\u{10FFFF}/']
        v = $VERBOSE
        $VERBOSE = nil
        r = srcs.map do |s|
          eval(s, nil, "t.rb")
          "ok"
        rescue SyntaxError => e
          e.message
        end
        $VERBOSE = v
        r
        "##,
    );
}

#[test]
fn onigmo_warnings_that_print_by_default() {
    run_test_once(
        r##"
        require "stringio"
        def cap; $stderr = StringIO.new; yield; s = $stderr.string; $stderr = STDERR; s; end
        strip = ->(s) { s.gsub(/^.*?warning/, "warning") }
        r = []
        r << strip.(cap { Regexp.new("\\p") })
        r << strip.(cap { Regexp.new("a\\P") })
        r << strip.(cap { Regexp.new("[\\p]") })
        r << strip.(cap { eval('/\p/') })
        r << strip.(cap { Regexp.new("[a-b-c]") })
        r << strip.(cap { Regexp.new("[a-]]") })
        r << strip.(cap { Regexp.new("a]") })
        r << strip.(cap { v = $VERBOSE; $VERBOSE = false; Regexp.new("b\\p"); $VERBOSE = v })
        r << cap { v = $VERBOSE; $VERBOSE = nil; Regexp.new("c\\p"); $VERBOSE = v }
        # Still verbose-only: duplicated ranges and unknown escapes.
        r << cap { Regexp.new("[aa]"); Regexp.new("\\y") }
        r << strip.(cap { v = $VERBOSE; $VERBOSE = true; Regexp.new("[bb]"); Regexp.new("\\z1"); $VERBOSE = v })
        r
        "##,
    );
}
