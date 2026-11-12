//! Reading through an internal encoding (#1852): what is read is
//! converted external -> internal with the stream's `invalid:` /
//! `undef:` / `replace:` options, and a conversion error is raised and
//! leaves the stream just past the offending bytes, as CRuby's
//! `fill_cbuf` does.

extern crate monoruby;
use monoruby::tests::*;

#[test]
fn read_conversion_errors_raise() {
    run_test_once(
        r##"
        path = "/tmp/mr_rdconv_err.txt"
        File.binwrite(path, "abc\na\xA4b\nxyz\n")
        r = []
        t = ->(&b) { begin; b.call; rescue => e; [e.class.name, e.message]; end }
        File.open(path, "r:UTF-8:EUC-JP") { |f| r << 5.times.map { t.() { f.gets } } }
        File.open(path, "r:UTF-8:EUC-JP") { |f| r << t.() { f.read } << f.read }
        File.open(path, "r:UTF-8:EUC-JP") { |f| r << 8.times.map { t.() { f.getc } } }
        File.open(path, "r:UTF-8:EUC-JP") { |f| r << [f.readline, t.() { f.readline }, f.readline] }
        File.open(path, "r:UTF-8:EUC-JP") { |f| r << [f.gets("b"), t.() { f.gets("b") }, f.gets("b")] }
        File.open(path, "r:UTF-8:EUC-JP") { |f| r << t.() { f.readlines } }
        File.open(path, "r:UTF-8:EUC-JP") { |f| r << t.() { f.each_line { } } }
        File.open(path, "r:UTF-8:EUC-JP") { |f| r << t.() { f.each_char { } } }
        File.open(path, "r:UTF-8:EUC-JP") { |f| r << t.() { f.gets(nil) } }
        File.open(path, "r:UTF-8:EUC-JP") { |f| f.gets; t.() { f.gets }; r << f.pos }
        # Sized and raw reads never convert.
        File.open(path, "r:UTF-8:EUC-JP") { |f| r << f.read(6) << f.readpartial(100) }
        r << t.() { IO.read(path, encoding: "UTF-8:EUC-JP") }
        r << t.() { File.read(path, mode: "r:UTF-8:EUC-JP") }
        r << t.() { IO.readlines(path, encoding: "UTF-8:EUC-JP") }
        r << t.() { IO.foreach(path, encoding: "UTF-8:EUC-JP") { } }
        # What was converted ahead of the error stays in a read buffer.
        File.open(path, "r:UTF-8:EUC-JP") { |f| b = +"zz"; t.() { f.read(nil, b) }; r << b << b.encoding.name }
        # The exception describes the conversion.
        File.open(path, "r:UTF-8:EUC-JP") do |f|
          f.gets
          begin
            f.gets
          rescue => e
            r << [e.error_bytes, e.readagain_bytes, e.source_encoding.name, e.destination_encoding.name]
          end
        end
        r
        "##,
    );
}

#[test]
fn read_conversion_incomplete_and_undefined() {
    run_test_once(
        r##"
        r = []
        t = ->(&b) { begin; b.call; rescue => e; [e.class.name, e.message]; end }
        File.binwrite("/tmp/mr_rdconv_inc.txt", "ab\xE3\x81\ncd\n")
        File.binwrite("/tmp/mr_rdconv_eof.txt", "ab\xE3\x81")
        File.binwrite("/tmp/mr_rdconv_undef.txt", "aé一\u{1F600}b\n")
        File.open("/tmp/mr_rdconv_inc.txt", "r:UTF-8:EUC-JP") { |f| r << t.() { f.read } << f.read }
        File.open("/tmp/mr_rdconv_inc.txt", "r:UTF-8:EUC-JP") { |f| r << [t.() { f.gets }, f.gets, f.gets, f.lineno] }
        File.open("/tmp/mr_rdconv_inc.txt", "r:UTF-8:EUC-JP") { |f| r << 7.times.map { t.() { f.getc } } }
        File.open("/tmp/mr_rdconv_eof.txt", "r:UTF-8:EUC-JP") { |f| r << t.() { f.read } }
        File.open("/tmp/mr_rdconv_eof.txt", "r:UTF-8:EUC-JP") { |f| r << 5.times.map { t.() { f.getc } } }
        File.open("/tmp/mr_rdconv_inc.txt", "r:UTF-8:EUC-JP") { |f| r << t.() { f.read } }
        begin
          File.open("/tmp/mr_rdconv_inc.txt", "r:UTF-8:EUC-JP") { |f| f.read }
        rescue => e
          r << [e.error_bytes, e.readagain_bytes, e.incomplete_input?]
        end
        r << t.() { File.open("/tmp/mr_rdconv_undef.txt", "r:UTF-8:EUC-JP") { |f| f.read } }
        # (`.b`: the EUC-JP inspect of a JIS X 0212 character does not
        # round-trip through the harness.)
        r << File.open("/tmp/mr_rdconv_undef.txt", "r:UTF-8:EUC-JP", undef: :replace) { |f| f.read.b }
        r << File.open("/tmp/mr_rdconv_undef.txt", "r:UTF-8:EUC-JP", replace: "*") { |f| f.read.b }
        r << File.open("/tmp/mr_rdconv_undef.txt", "r:UTF-8:US-ASCII", undef: :replace) { |f| f.read }
        r
        "##,
    );
}

#[test]
fn read_conversion_chomp_and_lineno() {
    run_test_once(
        r##"
        r = []
        t = ->(&b) { begin; b.call; rescue => e; [e.class.name, e.message]; end }
        File.binwrite("/tmp/mr_rdconv_chomp.txt", "abc\na\xA4b\nxyz\n")
        File.binwrite("/tmp/mr_rdconv_chomp2.txt", "ab\xE3\x81\ncd\n")
        File.open("/tmp/mr_rdconv_chomp.txt", "r:UTF-8:EUC-JP") do |f|
          r << [f.gets(chomp: true), t.() { f.gets(chomp: true) }, f.gets(chomp: true),
                f.gets(chomp: true), f.lineno, $.]
        end
        File.open("/tmp/mr_rdconv_chomp2.txt", "r:UTF-8:EUC-JP") do |f|
          r << [t.() { f.gets(chomp: true) }, f.gets(chomp: true), f.gets(chomp: true), f.lineno]
        end
        r << t.() { File.open("/tmp/mr_rdconv_chomp.txt", "r:UTF-8:EUC-JP") { |f| f.readlines(chomp: true) } }
        r << IO.readlines("/tmp/mr_rdconv_chomp.txt", chomp: true, encoding: "UTF-8:EUC-JP", invalid: :replace)
        r
        "##,
    );
}

#[test]
fn read_conversion_options() {
    run_test_once(
        r##"
        path = "/tmp/mr_rdconv_opts.txt"
        File.binwrite(path, "abc\na\xA4b\nxyz\n")
        r = []
        t = ->(&b) { begin; b.call; rescue => e; [e.class.name, e.message]; end }
        r << File.open(path, "r:UTF-8:EUC-JP", invalid: :replace) { |f| f.read }
        r << File.open(path, "r:UTF-8:EUC-JP", invalid: :replace, replace: "X") { |f| f.gets; f.gets }
        r << File.open(path, encoding: "UTF-8:EUC-JP", invalid: :replace) { |f| f.readlines }
        r << File.open(path) { |f| f.set_encoding("UTF-8", "EUC-JP", invalid: :replace); f.read }
        r << File.open(path) { |f| f.set_encoding("UTF-8:EUC-JP", invalid: :replace, replace: "R"); f.read }
        r << File.open(path) { |f| f.set_encoding(Encoding::UTF_8, Encoding::EUC_JP, invalid: :replace); f.read }
        # A later #set_encoding keeps `invalid: :replace` but not the
        # replacement string; a lone Encoding ignores the options.
        r << File.open(path, "r:UTF-8:EUC-JP", invalid: :replace, replace: "X") { |f| f.set_encoding("UTF-8", "EUC-JP"); f.read }
        r << File.open(path, invalid: :replace) { |f| f.set_encoding(nil); f.set_encoding("UTF-8", "EUC-JP"); f.read }
        r << t.() { File.open(path) { |f| f.set_encoding(Encoding::UTF_8, invalid: :replace); f.set_encoding("UTF-8:EUC-JP"); f.read } }
        r << File.open(path, "r:UTF-8:EUC-JP", invalid: :replace) { |f| g = f.dup; s = g.read; g.close; s }
        r << File.new(path, encoding: "UTF-8:EUC-JP", invalid: :replace).then { |f| s = f.read; f.close; s }
        fd = IO.sysopen(path)
        r << IO.new(fd, "r:UTF-8:EUC-JP", invalid: :replace).then { |io| s = io.read; io.close; s }
        fd = IO.sysopen(path)
        r << IO.open(fd, "r", external_encoding: "UTF-8", internal_encoding: "EUC-JP", invalid: :replace, replace: "Z") { |io| io.read }
        r << open(path, "r:UTF-8:EUC-JP", invalid: :replace) { |f| f.read }
        r << IO.read(path, encoding: "UTF-8:EUC-JP", invalid: :replace)
        r << IO.read(path, mode: "r:UTF-8:EUC-JP", invalid: :replace, replace: "#")
        r << IO.read(path, open_args: ["r:UTF-8:EUC-JP", {invalid: :replace}])
        r << t.() { IO.read(path, open_args: ["r:UTF-8:EUC-JP"], invalid: :replace) }
        r << IO.readlines(path, encoding: "UTF-8:EUC-JP", invalid: :replace)
        a = []; IO.foreach(path, encoding: "UTF-8:EUC-JP", invalid: :replace) { |l| a << l }; r << a
        rd, wr = IO.pipe("UTF-8:EUC-JP", invalid: :replace); wr.write "a\xA4b\n"; wr.close; r << rd.read; rd.close
        rd, wr = IO.pipe(Encoding::UTF_8, Encoding::EUC_JP, invalid: :replace); wr.write "a\xA4b\n"; wr.close; r << rd.read; rd.close
        rd, wr = IO.pipe; rd.set_encoding("UTF-8", "EUC-JP", invalid: :replace); wr.write "a\xA4b\n"; wr.close; r << rd.read; rd.close
        # Option values are checked as CRuby checks them.
        r << t.() { File.open(path, "r:UTF-8:EUC-JP", invalid: :foo) { } }
        r << t.() { File.open(path, "r:UTF-8:EUC-JP", undef: :foo) { } }
        r << t.() { File.open(path, "r:UTF-8:EUC-JP", replace: 1) { } }
        r << t.() { File.open("/tmp/mr_rdconv_missing/x.txt", "r", invalid: :foo) { } }
        r << t.() { File.open(path) { |f| f.set_encoding("UTF-8", "EUC-JP", invalid: :foo) } }
        r << t.() { File.open(path) { |f| f.set_encoding(Encoding::UTF_8, invalid: :foo); :ignored } }
        r << t.() { IO.read(path, invalid: :foo) }
        r << t.() { IO.readlines(path, invalid: :foo) }
        r << t.() { IO.pipe("UTF-8", "EUC-JP", invalid: :foo) }
        r
        "##,
    );
}

#[test]
fn read_conversion_wide_external() {
    run_test_once(
        r##"
        r = []
        s = "a\r\nb਀c\n\nd\ne".encode("UTF-16LE")
        File.binwrite("/tmp/mr_rdconv_w16.txt", s.b)
        File.binwrite("/tmp/mr_rdconv_w16bom.txt", "\xFF\xFE".b + s.b)
        File.binwrite("/tmp/mr_rdconv_w32.txt", "x\ny\n".encode("UTF-32BE").b)
        File.binwrite("/tmp/mr_rdconv_w16bad.txt", "a\x00\x00\xD8b\x00\n\x00".b)
        m = "rb:UTF-16LE:UTF-8"
        File.open("/tmp/mr_rdconv_w16.txt", m) { |f| a = []; while (l = f.gets); a << l; end; r << a << f.lineno }
        File.open("/tmp/mr_rdconv_w16.txt", m) { |f| a = []; while (l = f.gets(chomp: true)); a << l; end; r << a }
        File.open("/tmp/mr_rdconv_w16.txt", m) { |f| r << f.readlines(chomp: true) }
        File.open("/tmp/mr_rdconv_w16.txt", m) { |f| r << f.gets("") << [f.gets("b"), f.gets("b")] }
        File.open("/tmp/mr_rdconv_w16.txt", m) { |f| r << f.gets(nil) }
        r << IO.readlines("/tmp/mr_rdconv_w16.txt", mode: m)
        a = []; IO.foreach("/tmp/mr_rdconv_w16.txt", mode: m) { |l| a << l }; r << a
        File.open("/tmp/mr_rdconv_w16bom.txt", "rb:BOM|UTF-16LE:UTF-8") { |f| r << f.readlines << f.internal_encoding.name }
        File.open("/tmp/mr_rdconv_w32.txt", "rb:UTF-32BE:UTF-8") { |f| r << f.readlines }
        File.open("/tmp/mr_rdconv_w16bad.txt", m) do |f|
          r << [(f.gets rescue $!.message), f.gets]
        end
        r << File.open("/tmp/mr_rdconv_w16bad.txt", m, invalid: :replace) { |f| f.read }
        r
        "##,
    );
}

#[test]
fn class_readers_binmode_and_default_internal() {
    run_test_once(
        r##"
        path = "/tmp/mr_rdconv_cls.txt"
        File.binwrite(path, "abc\na\xA4b\nxyz\n")
        r = []
        t = ->(&b) { begin; b.call; rescue => e; [e.class.name, e.message]; end }
        r << File.read(path, mode: "rb").encoding.name
        r << File.read(path, mode: "rb:UTF-8").encoding.name
        r << File.read(path, binmode: true).encoding.name
        r << IO.readlines(path, mode: "rb")[0].encoding.name
        a = []; IO.foreach(path, mode: "rb") { |l| a << l.encoding.name }; r << a[0]
        begin
          Encoding.default_internal = "EUC-JP"
          r << t.() { File.read(path, encoding: "UTF-8") }
          r << IO.read(path, encoding: "UTF-8", invalid: :replace).then { |s| [s, s.encoding.name] }
          r << t.() { IO.readlines(path, encoding: "UTF-8") }
          r << t.() { File.open(path, "r:UTF-8") { |f| f.read } }
          r << File.open(path, "r:UTF-8", invalid: :replace) { |f| f.read }
          r << File.read(path, mode: "rb").encoding.name
        ensure
          Encoding.default_internal = nil
        end
        r
        "##,
    );
}
