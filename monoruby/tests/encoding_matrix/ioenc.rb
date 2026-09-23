require "tmpdir"; require "stringio"
def t(label) = (r = yield rescue "#{$!.class}: #{$!.message.lines.first.chomp}"; puts "#{label}\t#{r.inspect}")
def es(s) = s.is_a?(String) ? [s.b.unpack1("H*"), s.encoding.to_s, s.valid_encoding?] : s
Dir.mktmpdir do |d|
  path = File.join(d, "f.txt")
  euc = "日本語\n".encode("EUC-JP"); utf = "日本語\n"; bin = "\xFF\x00\x80".b
  File.binwrite(path, euc)
  t("defaults") { [Encoding.default_external, Encoding.default_internal, __ENCODING__, "".encoding, ARGV.map(&:encoding), ENV["PATH"].encoding, $stdout.external_encoding, $stdin.external_encoding, $stdin.internal_encoding] }
  t("File.read") { es(File.read(path)) }
  t("File.read enc:") { es(File.read(path, encoding: "EUC-JP")) }
  t("File.read enc:x:y") { es(File.read(path, encoding: "EUC-JP:UTF-8")) }
  t("File.read mode rb") { es(File.read(path, mode: "rb")) }
  t("File.binread") { es(File.binread(path)) }
  t("open r:EUC-JP") { File.open(path, "r:EUC-JP") { |f| [f.external_encoding, f.internal_encoding, es(f.read), es(f.gets)] } }
  t("open r:EUC-JP:UTF-8") { File.open(path, "r:EUC-JP:UTF-8") { |f| [f.external_encoding, f.internal_encoding, es(f.read)] } }
  t("open rb") { File.open(path, "rb") { |f| [f.external_encoding, f.internal_encoding, es(f.read)] } }
  t("open r + read(2)") { File.open(path, "r:EUC-JP") { |f| [es(f.read(2)), es(f.read(1)), es(f.read)] } }
  t("open r + getc") { File.open(path, "r:EUC-JP") { |f| [es(f.getc), es(f.getc), f.pos] } }
  t("open r + each_char") { File.open(path, "r:EUC-JP:UTF-8") { |f| f.each_char.map { |c| es(c) } } }
  t("open r + readpartial") { File.open(path, "r:EUC-JP:UTF-8") { |f| es(f.readpartial(100)) } }
  t("open r + ungetc") { File.open(path, "r:EUC-JP") { |f| f.ungetc("x"); es(f.read) } }
  t("open r + readlines") { File.open(path, "r:EUC-JP") { |f| f.readlines.map { |l| es(l) } } }
  t("set_encoding(SJIS)") { File.open(path) { |f| f.set_encoding("Shift_JIS"); [f.external_encoding, es(f.read)] } }
  t("set_encoding(EUC,UTF)") { File.open(path) { |f| f.set_encoding("EUC-JP", "UTF-8"); [f.external_encoding, f.internal_encoding, es(f.read)] } }
  t("set_encoding(enc obj)") { File.open(path) { |f| f.set_encoding(Encoding::EUC_JP); f.external_encoding } }
  t("set_encoding(BINARY)") { File.open(path) { |f| f.set_encoding("BINARY"); [f.external_encoding, es(f.read)] } }
  t("set_encoding(nil)") { File.open(path) { |f| f.set_encoding(nil); [f.external_encoding, f.internal_encoding] } }
  t("set_encoding(bad)") { File.open(path) { |f| f.set_encoding("NOPE"); f.external_encoding } }
  t("binmode") { File.open(path) { |f| f.binmode; [f.binmode?, f.external_encoding, es(f.read)] } }
  t("write w:EUC-JP") { File.open(path, "w:EUC-JP") { |f| f.write(utf); f.external_encoding }; es(File.binread(path)) }
  t("write w:EUC-JP:UTF-8") { File.open(path, "w:EUC-JP:UTF-8") { |f| f.write(utf) }; es(File.binread(path)) }
  t("write w + bin") { File.open(path, "w:EUC-JP") { |f| f.write(bin) }; es(File.binread(path)) }
  t("write w + sjis src") { File.open(path, "w:EUC-JP") { |f| f.write("日本".encode("Shift_JIS")) }; es(File.binread(path)) }
  t("write w:UTF-16LE") { File.open(path, "w:UTF-16LE") { |f| f.write("ab"); f.puts("c") }; es(File.binread(path)) }
  t("write wb + utf") { File.open(path, "wb") { |f| f.write(utf) }; es(File.binread(path)) }
  t("File.write enc:") { File.write(path, utf, encoding: "Shift_JIS"); es(File.binread(path)) }
  t("puts wide") { File.open(path, "w:UTF-16BE") { |f| f.puts("あ", 1, :sym, nil) }; es(File.binread(path)) }
  t("print wide") { File.open(path, "w:UTF-16BE") { |f| f.print("あ", 1) }; es(File.binread(path)) }
  t("<< wide") { File.open(path, "w:UTF-32LE") { |f| f << "あ" << 1 }; es(File.binread(path)) }
  t("printf wide") { File.open(path, "w:UTF-16BE") { |f| f.printf("%s-%d", "あ", 1) }; es(File.binread(path)) }
  t("write invalid src") { File.open(path, "w:EUC-JP") { |f| f.write("ab\xFFcd") }; es(File.binread(path)) }
  t("write undef src") { File.open(path, "w:US-ASCII") { |f| f.write("é") }; es(File.binread(path)) }
  t("write undef repl") { File.open(path, "w:US-ASCII", undef: :replace) { |f| f.write("é") }; es(File.binread(path)) }
  t("open invalid: opt") { File.binwrite(path, "a\xFFb"); File.open(path, "r:UTF-8:UTF-16LE", invalid: :replace) { |f| es(f.read) } }
  t("open undef: opt") { File.binwrite(path, "é"); File.open(path, "r:UTF-8:US-ASCII", undef: :replace, replace: "?") { |f| es(f.read) } }
  t("BOM|UTF-8") { File.binwrite(path, "\xEF\xBB\xBFabc"); File.open(path, "r:BOM|UTF-8") { |f| [f.external_encoding, es(f.read)] } }
  t("BOM|UTF-16") { File.binwrite(path, "\xFF\xFEa\x00"); File.open(path, "r:BOM|UTF-16") { |f| [f.external_encoding, es(f.read)] } }
  t("BOM absent") { File.binwrite(path, "abc"); File.open(path, "r:BOM|UTF-8") { |f| [f.external_encoding, es(f.read)] } }
  t("universal_newline") { File.binwrite(path, "a\r\nb\rc"); File.open(path, "r", universal_newline: true) { |f| es(f.read) } }
  t("rt mode") { File.binwrite(path, "a\r\nb"); File.open(path, "rt") { |f| es(f.read) } }
  t("mode: opt") { File.binwrite(path, euc); File.open(path, mode: "r:EUC-JP:UTF-8") { |f| es(f.read) } }
  t("external_encoding: opt") { File.open(path, external_encoding: "EUC-JP", internal_encoding: "UTF-8") { |f| [f.external_encoding, f.internal_encoding, es(f.read)] } }
  t("encoding: opt") { File.open(path, encoding: "EUC-JP:UTF-8") { |f| es(f.read) } }
  t("gets sep enc") { File.open(path, "r:EUC-JP") { |f| es(f.gets("本".encode("EUC-JP"))) } }
  t("gets sep incompat") { File.open(path, "r:EUC-JP") { |f| es(f.gets("本")) } }
  t("gets limit") { File.open(path, "r:EUC-JP:UTF-8") { |f| es(f.gets(nil, 2)) } }
  t("readline chomp") { File.open(path, "r:EUC-JP") { |f| es(f.readline(chomp: true)) } }
  t("each_line") { File.open(path, "r:EUC-JP") { |f| f.each_line.map { |l| es(l) } } }
  t("foreach") { File.foreach(path, encoding: "EUC-JP:UTF-8").map { |l| es(l) } }
  t("readlines") { File.readlines(path, encoding: "EUC-JP").map { |l| es(l) } }
  t("IO.read") { es(IO.read(path, encoding: "EUC-JP")) }
  t("pread") { File.open(path, "r:EUC-JP") { |f| es(f.pread(3, 0)) } }
  t("sysread") { File.open(path, "r:EUC-JP:UTF-8") { |f| es(f.sysread(3)) } }
  t("read_nonblock") { File.open(path, "r:EUC-JP:UTF-8") { |f| es(f.read_nonblock(100)) } }
  t("readchar") { File.open(path, "r:EUC-JP:UTF-8") { |f| es(f.readchar) } }
  t("readbyte") { File.open(path, "r:EUC-JP:UTF-8") { |f| f.readbyte } }
  t("each_byte") { File.open(path, "r:EUC-JP:UTF-8") { |f| f.each_byte.first(3) } }
  t("lineno/pos after gets") { File.open(path, "r:EUC-JP:UTF-8") { |f| f.gets; [f.lineno, f.pos, f.tell] } }
  t("seek then getc") { File.open(path, "r:EUC-JP:UTF-8") { |f| f.seek(2); es(f.getc) } }
  t("default_internal=") { begin; Encoding.default_internal = "UTF-8"; File.open(path, "r:EUC-JP") { |f| [f.internal_encoding, es(f.read)] }; ensure; Encoding.default_internal = nil; end }
  t("default_external=") { begin; Encoding.default_external = "EUC-JP"; [File.read(path).encoding, "".encoding, __ENCODING__, File.open(path) { |f| f.external_encoding }]; ensure; Encoding.default_external = "US-ASCII"; end }
  t("default_external=nil") { Encoding.default_external = nil }
  t("default_external=BINARY") { begin; Encoding.default_external = "BINARY"; es(File.read(path)); ensure; Encoding.default_external = "US-ASCII"; end }
  t("StringIO enc") { s = StringIO.new(euc.dup); [s.external_encoding, s.internal_encoding, es(s.read)] }
  t("StringIO set_encoding") { s = StringIO.new(euc.dup); s.set_encoding("UTF-8"); [s.external_encoding, es(s.read), es(s.string)] }
  t("StringIO write enc") { s = StringIO.new("".encode("EUC-JP")); s.write("日本"); es(s.string) }
  t("StringIO write bin") { s = StringIO.new("".b); s.write("日本"); es(s.string) }
  t("StringIO getc") { s = StringIO.new(euc.dup); [es(s.getc), s.pos] }
  t("StringIO gets sep") { s = StringIO.new(euc.dup); es(s.gets("本".encode("EUC-JP"))) }
  t("StringIO ungetc") { s = StringIO.new(euc.dup); s.ungetc("日".encode("EUC-JP")); es(s.read) }
  t("StringIO frozen") { s = StringIO.new("abc".freeze); s.write("x") }
  t("StringIO binmode") { s = StringIO.new(euc.dup); s.binmode; [s.external_encoding, es(s.read)] }
  t("pipe enc") { r, w = IO.pipe("EUC-JP", "UTF-8"); w.write(euc); w.close; [r.external_encoding, r.internal_encoding, es(r.read)] }
  t("pipe write transcode") { r, w = IO.pipe; w.set_encoding("EUC-JP"); w.write("日本"); w.close; es(r.read) }
  t("popen enc") { es(IO.popen(["ruby", "-e", "print 'x'"], "r:EUC-JP") { |io| io.read }) }
  t("Dir.entries enc") { Dir.entries(d).map(&:encoding).uniq }
  t("Dir.glob enc") { Dir.glob(File.join(d, "*")).map(&:encoding).uniq }
  t("File.basename enc") { File.basename("日本".encode("EUC-JP") + "/x").encoding }
  t("File.join enc") { File.join("a", "b").encoding }
  t("File.expand_path enc") { File.expand_path("x").encoding }
  t("__FILE__ enc") { __FILE__.encoding }
  t("Dir.pwd enc") { Dir.pwd.encoding }
  t("ENV enc") { ENV.to_h.first&.map(&:encoding) }
  t("Encoding.locale_charmap") { Encoding.locale_charmap }
  t("find(locale/external/filesystem)") { [Encoding.find("locale"), Encoding.find("external"), Encoding.find("filesystem"), (Encoding.find("internal") rescue $!.class)] }
  t("STDIN encodings") { [STDIN.external_encoding, STDIN.internal_encoding, STDOUT.external_encoding, STDERR.external_encoding] }
  t("IO#set_encoding_by_bom") { File.binwrite(path, "\xFF\xFEa\x00"); File.open(path, "rb") { |f| [f.set_encoding_by_bom, f.external_encoding, es(f.read)] } }
  t("Kernel#open mode") { File.binwrite(path, euc); open(path, "r:EUC-JP:UTF-8") { |f| es(f.read) } }
  t("File.new mode") { f = File.new(path, "r:EUC-JP"); r = [f.external_encoding, es(f.read)]; f.close; r }
  t("File.open perm+enc") { File.open(path, "w:UTF-8", 0644) { |f| f.write("x") }; es(File.binread(path)) }
  t("IO.write enc:") { IO.write(path, "日本", encoding: "EUC-JP"); es(File.binread(path)) }
  t("IO.binwrite") { IO.binwrite(path, "日本"); es(File.binread(path)) }
  t("Tempfile enc") { require "tempfile"; Tempfile.create(["x", ".txt"], encoding: "EUC-JP") { |f| f.write("日本"); f.rewind; [f.external_encoding, es(f.read)] } }
  t("String IO tty") { $stdout.tty? }
end
