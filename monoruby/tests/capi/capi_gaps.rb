require "capi_gaps"
G = CapiGaps

# Non-local exits from a block run under rb_protect / rb_ensure.
def ret_from(m) = G.send(m, 1) { |x| return [:returned, x] }
p ret_from(:protect_yield), ret_from(:ensure_yield)
p G.protect_yield(2) { |x| break [:broke, x] }
p G.ensure_yield(3) { |x| break [:broke, x] }
p catch(:tag) { G.protect_yield(4) { |x| throw :tag, [:thrown, x] } }
p G.protect_yield(5) { |x| x * 2 }
p [1, 2, 3].map { |i| G.protect_yield(i) { |x| next x + 10 } }
p (begin; G.protect_yield(6) { raise ArgumentError, "boom" }; rescue => e; [e.class, e.message]; end)
def iter = (yield; :done)
p iter { G.protect_yield(7) { |x| break } }
p((1..3).each { |i| G.ensure_yield(i) { |x| break x * 100 if x == 2 } })

# rb_integer_pack
le, be, comp, native = G.flags
[0, 1, -1, 255, 256, -256, 2**63 - 1, -(2**63), 2**63, 2**64 - 1, 2**64, -(2**64), -(2**64) - 1, 2**100].each do |v|
  p [v, G.pack(v, 1, 8, native | comp), G.pack(v, 1, 8, native), G.pack(v, 2, 2, be | comp), G.pack(v, 4, 1, le)]
end

# rb_range_beg_len
[[1..3, 5], [1...3, 5], [-2.., 5], [..-2, 5], [6..7, 5], [5..7, 5], [-6..1, 5], [2..1, 5], [nil..nil, 0]].each do |r, len|
  p [r, G.beg_len(r, len, 0), (G.beg_len(r, len, 1) rescue [$!.class, $!.message])]
end
p G.beg_len(3, 5, 0), G.beg_len("x", 5, 0)

p G.strings
[nil, {}, { external_encoding: "UTF-8" }, { encoding: "EUC-JP" }, { encoding: "UTF-8:EUC-JP" },
 { external_encoding: Encoding::Shift_JIS, internal_encoding: "UTF-8" },
 { external_encoding: "UTF-8", internal_encoding: "UTF-8" }].each { |o| p [o, G.extract(o)] }
o = Object.new
o.instance_variable_set(:@a, 1)
p G.hidden(o)
p G.misc
