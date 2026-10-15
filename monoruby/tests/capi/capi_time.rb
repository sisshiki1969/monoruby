require "capi_time"

def try
  yield
rescue => e
  [e.class, e.message]
end

t = CapiTime.nano_new(1700000000, 123456789)
p t.class, t.to_i, t.nsec, t.utc?, t.utc_offset == Time.at(1700000000).utc_offset
u = CapiTime.usec_new(1700000000, 123456)
p u.to_i, u.nsec, u.usec
p CapiTime.nano_new(-1, 5).to_i, CapiTime.nano_new(0, 1_500_000_000).to_a[0, 2]
# rb_time_timespec_new: local / UTC / fixed offsets / out of range
l = CapiTime.ts_new(1700000000, 42, 0)
p l.nsec, l.utc?, l.utc_offset == Time.at(1700000000).utc_offset
z = CapiTime.ts_new(1700000000, 42, 1)
p z.utc?, z.utc_offset, z.inspect
f = CapiTime.ts_new(1700000000, 999_999_999, 32400)
p f.utc?, f.utc_offset, f.inspect, f.zone
p CapiTime.ts_new(1700000000, 0, -86399).inspect, CapiTime.ts_new(1700000000, 0, -1).utc_offset
p try { CapiTime.ts_new(1700000000, 0, 86400) }, try { CapiTime.ts_new(1700000000, 0, -86400) }
# rb_time_num_new
p CapiTime.num_new(1700000000, nil).utc_offset == Time.at(1700000000).utc_offset
p CapiTime.num_new(1700000000.5, 3600).then { |t| [t.to_i, t.nsec, t.utc_offset] }, CapiTime.num_new(Rational(3, 2), "+09:00").inspect
p CapiTime.num_new(10, "UTC").utc?
# rb_time_timespec / rb_time_timespec_interval and the timeval forms
p CapiTime.timespec(Time.at(1700000000, 123456789, :nanosecond)), CapiTime.timespec(7), CapiTime.timespec(-7)
p CapiTime.timespec(1.5), CapiTime.timespec(-1.5), CapiTime.timespec(Rational(7, 4)), CapiTime.timespec(2.25)
p CapiTime.interval(1.5), CapiTime.interval(3), CapiTime.interval(Rational(1, 8)), CapiTime.interval(0)
p try { CapiTime.interval(-1) }, try { CapiTime.interval(-0.5) }, try { CapiTime.interval(Time.now) }
p try { CapiTime.timespec("1") }, try { CapiTime.interval(nil) }, try { CapiTime.timespec([]) }
p CapiTime.timeval(Time.at(1700000000, 123456789, :nanosecond)), CapiTime.timeval(2.5), CapiTime.timeval_interval(0.000001)
p try { CapiTime.timeval_interval(-2) }
# rb_time_utc_offset
p CapiTime.utc_offset(Time.at(0).utc), CapiTime.utc_offset(Time.at(0).localtime("+05:30")), CapiTime.utc_offset(Time.at(0).localtime(-3600))
p try { CapiTime.utc_offset(123) }, try { CapiTime.utc_offset(nil) }
# rb_timespec_now
p CapiTime.now_plausible
# rb_keyword_given_p: keywords reach a C method as a trailing Hash
p CapiTime.kw, CapiTime.kw(1), CapiTime.kw(1, a: 2), CapiTime.kw(a: 2), CapiTime.kw({a: 2}), CapiTime.kw(1, {a: 2})
h = {k: 1}
p CapiTime.kw(**h), CapiTime.kw(1, **h), CapiTime.kw(**{})
p CapiTime.kw2(1, a: 1), CapiTime.kw2(1, {a: 1}), CapiTime.kw2(1, 2)
p CapiTime.kw(*[1, 2], **h), CapiTime.send(:kw, a: 1), CapiTime.method(:kw).call(a: 1), CapiTime.public_send(:kw, {a: 1})
p CapiTime::KW_AT_INIT, CapiTime.kw0, CapiTime.kw_funcall({a: 1}), CapiTime.send(:send, :send, :kw, a: 1)
class CapiTimeSub < CapiTime::Base
  def kw(*args, **kw) = [super(*args), super(**kw), super(1, {a: 1}), super(a: 1), super(1)]
end
p CapiTimeSub.new.kw(1), CapiTimeSub.new.kw(a: 1), CapiTimeSub.new.kw({a: 1})
# rb_enc_codepoint_len across encodings
p CapiTime.codepoint("a"), CapiTime.codepoint("あい"), CapiTime.codepoint("\xff".b), CapiTime.codepoint("€".encode("ISO-8859-15"))
p CapiTime.codepoint("あ".encode("EUC-JP")), CapiTime.codepoint("あ".encode("Shift_JIS")), CapiTime.codepoint("ｱ".encode("Shift_JIS"))
p CapiTime.codepoint("あ".encode("UTF-16LE")), CapiTime.codepoint("😀".encode("UTF-16BE")), CapiTime.codepoint("a".encode("UTF-32LE"))
p try { CapiTime.codepoint("\xA4".force_encoding("EUC-JP") + "x") }, CapiTime.codepoint("日本".encode("Windows-31J"))
p try { CapiTime.codepoint("") }, try { CapiTime.codepoint("\xff") }, try { CapiTime.codepoint("\xA4".force_encoding("EUC-JP")) }
p try { CapiTime.codepoint("\xe3\x81".force_encoding("UTF-8")) }, try { CapiTime.codepoint("\x81".force_encoding("Shift_JIS")) }
# OBJ_INIT_COPY
o = Object.new
p CapiTime.init_copy(o, o), CapiTime.init_copy(Object.new, o), CapiTime.init_copy("a", "b")
p try { CapiTime.init_copy("a".freeze, "b") }, try { CapiTime.init_copy(Object.new, "b") }, try { CapiTime.init_copy(1, 2) }
# RB_INTEGER_TYPE_P / UNREACHABLE / time_t round trip
p CapiTime.classify(1), CapiTime.classify(2**70), CapiTime.classify(1.5), CapiTime.classify(nil), CapiTime.classify(Rational(1, 2))
p CapiTime.timet_roundtrip(0), CapiTime.timet_roundtrip(-1), CapiTime.timet_roundtrip(2**40), try { CapiTime.timet_roundtrip(2**70) }
