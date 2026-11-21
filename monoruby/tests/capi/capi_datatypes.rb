require "capi_datatypes"
D = CapiDatatypes
def t = (yield) rescue [$!.class, $!.message]

# String
s = "héllo wörld"
p D.subpos(s, 1, 4), D.subpos(s, -5, 3), D.subpos(s, 11, 2), D.subpos(s, 12, 1), D.subpos(s, -12, 1), D.subpos(s, 0, -1)
p D.subpos("abc", 1, 5), D.subpos("", 0, 1)
p D.sublen(s, 3), D.sublen(s, 0), D.sublen("abc", 2), D.sublen(s, 13)
p D.offset(s, 2), D.offset(s, 0), D.offset(s, 50)
p D.update(+"hello", 1, 2, "EY"), t { D.update(+"abc", 5, 1, "x") }, t { D.update(+"abc", 0, -1, "x") }
p D.str_misc("az", "b".encode("UTF-16LE"))
p D.str_misc("abc", "abc")
p D.ellipsize("hello world", 8), D.ellipsize("hello", 5), D.ellipsize("hello", 2), D.ellipsize("héllo wörld", 6)
p t { D.ellipsize("x", -1) }
p D.scrub("abc", nil), D.scrub("a\xffc", nil), D.scrub("a\xffc", "?")
p [0x41, 0xe9, 0x3042, 0x1f600].map { D.uv_to_utf8(_1) }, t { D.uv_to_utf8(0x80000000) }
p D.memcicmp("Hello", "hELLO"), D.memcicmp("abc", "abd"), D.memcicmp("b", "A")
p t { D.invalid_str("12x") }
x = "a\xFFc".dup.force_encoding("utf-8")
p D.encode(x, "us-ascii", Encoding::Converter::INVALID_REPLACE, nil)
p D.encode(x, "us-ascii", Encoding::Converter::INVALID_REPLACE, { replace: "b" }.freeze)
p D.encode("a\nb", "utf-8", Encoding::Converter::CRLF_NEWLINE_DECORATOR, nil)
p D.encode("<a>", "utf-8", Encoding::Converter::XML_TEXT_DECORATOR, nil)
y = "abc"
z = D.encode(y, "utf-8", 0, nil)
p z, z.equal?(y)
sub = Class.new(String)
n = D.new_with_class(sub.new("tmpl"))
p n, n.class == sub, n.encoding, D.new_with_class("plain").class
p D.cstr2inum("1234a", 10), D.cstr2inum("0x1f", 16), t { D.cstr2inum("1234a", 0) }, D.cstr2inum("017", 0), D.cstr2inum("0b101", 2), t { D.cstr2inum("0x1f", 0) }
a = "あれ".b
p D.external_str(a, Encoding::UTF_8).encoding, D.external_str("\xff".b, Encoding::US_ASCII).encoding
Encoding.default_internal = Encoding::EUC_JP
r = D.external_str(a, Encoding::UTF_8)
p r.encoding, r.bytes
p D.external_str("ascii", Encoding::UTF_8).encoding
Encoding.default_internal = nil
src = ["00ff"].pack("H*")
r = D.export_to_enc(src, Encoding::UTF_8)
p r[1], src.bytes
p D.export_to_enc("plain", Encoding::UTF_8)
p D.sprintf_value(8, 5, "Hello"), D.sprintf_value(0, 3, "Hello"), D.sprintf_value(0, 8, :sym)
p D.value_cstr("Hello".encode("UTF-16BE")), t { D.value_cstr("a\0b") }, t { D.value_cstr("a\0".encode("UTF-16LE")) }

# Array
p D.ary_misc([1, 2], [1, 3]), D.ary_misc([1, 2], 3)
a = [1, 2, 3, 4]
p D.rotate(a, 1), a, D.rotate(a, 4), D.rotate([1], 3), D.rotate(a, -1), D.rotate([], 1)
p t { D.rotate([1, 2].freeze, 1) }, t { D.modify([].freeze) }, D.modify([])
acc = []
p D.ary_each([1, 2]) { acc << _1 }, acc
e = D.ary_each([5, 6])
p e.class, e.size
p t { D.new_capa(-1) }, D.new_capa(10)

# Hash
p D.bulk_insert({ a: 1 }, [:b, 2, :c, 3])
h = { a: 1, b: 2, c: 3 }
p D.delete_if(h) { |k, v| v.odd? }, h, D.delete_if({ x: 1 }).class
p D.update_by({ a: 1, b: 2 }, { b: 3, c: 4 }, true), D.update_by({ a: 1 }, { a: 2 }, false)
p t { D.update_by({}.freeze, {}, false) }
o = Object.new
def o.hash = 2**64
q = Object.new
def q.hash = Struct.new(:to_int).new(12)
w = Object.new
def w.hash = "x"
p D.hash_of(o), D.hash_of(q), t { D.hash_of(w) }, D.hash_of("str")
p t { D.hash_capa(-1) }, D.hash_capa(4)

# Set
p D.set_api

# Struct / Data
k = D.data_define(nil)
p k.superclass, k.members, k.new(1, 2)
sup = Class.new(Data)
p D.data_define(sup).superclass == sup
p t { D.data_define([]) }, t { D.data_define_dup }
p D.struct_noaccessor

# Numbers
big = 2**70 + 12345
p D.num_misc(big)
p D.num2fix(3), t { D.num2fix(2**62) }, D.num2fix(4.7)
p D.num2long(1.9e3), t { D.num2long(Float::NAN) }, t { D.num2long(1e19) }, t { D.num2long(-Float::INFINITY) }
p D.num2ulong(1.5e19), t { D.num2ulong(Float::NAN) }, t { D.num2ulong(2.0e19) }, D.num2ulong(-1.0)
p D.num2dbl(3), t { D.num2dbl(nil) }, t { D.num2dbl(true) }, t { D.num2dbl(false) }, t { D.num2dbl("1") }, D.num2dbl(2**64)
p t { D.zerodiv }, t { D.out_of_int(2**40) }, t { D.out_of_int(-(2**40)) }
p t { D.cmperr(1, "a") }, t { D.cmperr(1, nil) }, t { D.cmperr(1, 2.5) }
p D.complex_api

# Symbols and IDs
%i[Foo @foo @@foo $foo $~ $1 $-w foo= foo foo? foo! + [] []= <=> Ünicode @1 $].each do |sym|
  p [sym, D.id_kinds(sym)]
end
p D.id_kinds(:"foo bar"), D.id_kinds(:"9a"), D.id_kinds(:"@foo=")
p D.attrset(:foo), D.attrset(:Foo), D.attrset(:@foo), D.attrset(:foo=), D.attrset(:[])
p t { D.attrset(:+) }, t { D.attrset(:foo?) }
p %w[foo Foo foo= foo? @a @@a $a + [] a\ b 9a].map { D.symname_p(_1) }
p D.symname_p(""), D.enc_symname_p("é"), D.enc_symname_p("a b")
p D.all_symbols_has(:capi_datatypes_marker)
p t { D.name_error(:nope) }, (D.name_error(:nope) rescue $!.name)
