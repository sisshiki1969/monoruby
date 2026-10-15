# Run on monoruby (the gem installed through its C API layer, over
# ruby/re.h) and on the host CRuby by tests/capi_gems.rs; both must print
# the same.
gem "strscan", ENV.fetch("CAPI_GEM_VERSION")
require "strscan"
puts $LOADED_FEATURES.grep(/\.(so|bundle)\z/).map { |f| File.basename(f, ".*") }.grep(/\Astrscan\z/).sort.inspect
s = StringScanner.new("test string 123")
p s.scan(/\w+/), s.scan(/\s+/), s.scan(/\d+/), s.scan(/(\w+)\s+(\d+)/), s.eos?, s.pos
p s.pre_match, s.matched, s.post_match, s[0], s[1], s[2]
s.reset
p s.scan_until(/str/), s.check(/i/), s.getch, s.peek(3), s.skip(/n/), s.unscan.pos, s.rest, s.rest_size
p s.exist?(/1/), s.match?(/ng/), s.search_full(/g/, true, true), s.scan_full(/\s/, false, true)
p s.scan(/(?<a>\d)(?<b>\d)/), s["a"], s[:b], s.captures, s.named_captures, s.values_at(0, 1, 2), s.size, s.charpos
p s.inspect, s.string, s.beginning_of_line?, s.terminate.eos?
u = StringScanner.new("あいう えお")
p u.scan(/\p{Hiragana}+/), u.pos, u.charpos, u.getch, u.get_byte, u.peek_byte, u.scan_byte, u.rest
u.pos = 3; p u.getch; u.string = "x"; p u.scan(/x/), u.fixed_anchor?
b = StringScanner.new("a\xffb".b); p b.scan(/\xff/n), b.scan(/a/), b.skip_until(/b/)
f = StringScanner.new("abc", fixed_anchor: true); f.pos = 1; p f.scan(/^b/), f.scan(/b/), f.fixed_anchor?
begin; StringScanner.new("x").scan("str" + 1.to_s); rescue => e; p e.class; end
begin; s.unscan; rescue => e; p e.class, e.message; end
p StringScanner::Version.class, StringScanner.must_C_version
s2 = StringScanner.new("foo bar"); p s2.scan("foo"), s2.scan(" "), s2.scan_until("r"), s2.skip("x"), s2.check_until(/z/)
s3 = StringScanner.new("abcabc"); p s3.scan_until(/c/), s3.pos, s3.pre_match, s3.matched_size, s3.matched?, s3.peek(10)
s4 = StringScanner.new("12 34"); p s4.scan_integer, s4.skip(/ /), s4.scan_integer(base: 16), s4.eos?
p s3.dup.pos, (s3 << "z").string, s3.concat("w").string
s5 = StringScanner.new("hello world"); s5.scan(/hello/); p s5.scan(/(?<=hello) world/), s5.pos
s6 = StringScanner.new("日本語テキスト".encode("EUC-JP")); p s6.scan(/\A../), s6.pos, s6.charpos
begin; s6.scan(/本/); rescue => e; p e.class; end
s7 = StringScanner.new(""); p s7.scan(/x/), s7.eos?, s7.scan(//), s7.matched
s8 = StringScanner.new("aaa"); p s8.scan(/a*?/), s8.matched_size, s8.scan(/a+/), s8.unscan.scan(/a/)
p s8.scan(/(a)|(b)/), s8.captures, s8[2], s8.values_at(0, 2, -1)
