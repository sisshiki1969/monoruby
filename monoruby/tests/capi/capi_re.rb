require "capi_re"

p CapiRe.run(/b+/, "aabbbcc", 0, false)
p CapiRe.run(/b+/, "aabbbcc", 2, true)
p CapiRe.run(/b+/, "aabbbcc", 0, true)
p CapiRe.run(/(\d+)-(\d+)?/, "x 12-34 5-", 3, false)
p CapiRe.run(/\z/, "abc", 0, false)
p CapiRe.run(/x/, "abc", 0, false)
# UTF-8 subject: positions are bytes
p CapiRe.run(/[い-う]+/, "あいうえお", 0, false)
p CapiRe.run(/./, "あいう", 3, true)
# A binary subject with 8-bit content, matched on its own bytes
p CapiRe.run(/\xff+/n, "a\xff\xffb".b, 0, false)
p CapiRe.run(/b/, "a\xff\xffb".b, 0, false)
# An ASCII-only subject in another ASCII-compatible encoding
p CapiRe.run(/l+/, "hello".encode("Shift_JIS"), 0, false)
p CapiRe.run(/本/, "日本語".encode("EUC-JP"), 0, false) rescue p $!.class
p CapiRe.run(Regexp.new("本".encode("EUC-JP")), "日本語".encode("EUC-JP"), 0, false)
p CapiRe.scan(/\w+/, "the quick  brown fox")
p CapiRe.scan(//, "ab")
p CapiRe.names(/(?<year>\d{4})-(?<mon>\d\d)(-(?<mon>\d\d))?/)
p CapiRe.names(/(a)(b)/)
p CapiRe.backref(/(?<a>x)(?<b>y)/, "xy", "b")
p CapiRe.backref(/(?<a>x)(?<b>y)/, "xy", "zz")
p CapiRe.region_copy(/(a)(b)?(c)/, "zac")
p CapiRe.memsearch("lo", "hello")
p CapiRe.memsearch("", "hello")
p CapiRe.memsearch("zz", "hello")
p CapiRe.memsearch("hello", "hello")
p CapiRe.memsearch("hello!", "hello")
p CapiRe.memsearch("l", "hello")
p CapiRe.misc("ab\\d+")
p CapiRe.mbclen("aé\u3042\u{1F600}")
p CapiRe.mbclen("a\xE3\x81".b.force_encoding("UTF-8"))
p CapiRe.mbclen("\xFF\xC0\x80z".b.force_encoding("UTF-8"))
p CapiRe.mbclen("\xE3\x81\x82".b)
begin
  CapiRe.long2int_overflow
rescue RangeError => e
  p e.message
end
begin
  CapiRe.run(/a/u, "\xff".b, 0, false)
rescue => e
  p e.class
end
begin
  CapiRe.run(/a/, "\xff\xfe".force_encoding("UTF-8"), 0, false)
rescue => e
  p [e.class, e.message]
end
