require "capi_value"
V = CapiValue

# CRuby's bit layout (64-bit, flonum): false 0, nil 8, true 0x14, undef 0x34
p V.constants_
samples = [nil, false, true, 0, -1, 2**40, 2**70, 1.5, 0.0, -0.0, 1e300, :sym, "str", [1], Object.new.class]
samples.each { |v| p [v, V.tests(v), V.type(v)] }
p V.sym(:hello), V.sym(:"with space"), V.sym(:a) == V.sym(:a)
[0, 1, -1, 2**62 - 1, -2**62, 2**62, 2**100, 1.5, -2.25, 0.0, 1e-310, 1e300, Float::INFINITY, Float::NAN].each { |n| r = V.num(n); p r.is_a?(Array) && r[1].is_a?(Float) && r[1].nan? ? [r[0], :nan] : r }
# a registered C global survives a collection whatever it holds
[nil, false, true, :s, 42, "kept alive"].each do |v|
  V.set_global(v)
  10_000.times { "garbage #{_1}" }
  GC.start
  p V.global
end
# RARRAY_CONST_PTR: one and two Arrays at once, with nil / false / Symbols in them
p V.ptr_sum([1, 2, 3, nil, 4]), V.ptr_sum([]), V.ptr_sum((1..300).to_a)
p V.ptr_zip([nil, false, :a, 1.5], [true, 2, "s", nil])
p V.from_values
# Qundef never reaches Ruby; the API's "absent" answers come back as such
h = { a: nil, b: false, c: 1 }
p V.lookup(h, :a), V.lookup(h, :b), V.lookup(h, :c), V.lookup(h, :d)
p V.scan(1), V.scan(1, nil), V.scan(1, false, k: nil), V.scan(nil, {})
# blocks, rb_protect, rb_ensure, rb_hash_foreach, rb_funcall carry nil / false unchanged
p V.yield_all { |*a| a.size == 1 ? a[0] : a }
p V.block_call([nil, false, 1], nil), V.block_call([:x], false)
p V.protect(nil, false), V.protect(false, false), V.protect(nil, true), V.protect(false, true)
p V.ensure(nil), V.ensure(false), V.ensure(:ok)
p V.pairs({ nil => false, false => nil, :s => :t, 1 => 2.5 })
class Echo; def echo(v) = v; end
p [nil, false, true, :q, 3, 2.5, "s"].map { |v| V.call_back(Echo.new, v) }
# rb_str_buf_new + RSTRING_PTR + rb_str_set_len keep what C wrote
p V.buf_build("abc"), V.buf_build("あいう"), V.buf_build("")
# rb_obj_freeze / rb_obj_frozen_p bypass a class's own freeze / frozen?
class Loud; def freeze = raise("not from here"); def frozen? = :lying; end
l = Loud.new
p V.freeze_(l), l.frozen?, V.freeze_("s".dup), V.freeze_(:sym), V.freeze_(nil)
# a VALUE kept in a TypedData, marked by the extension's own mark function
hold = V::Holder.new
[nil, false, :s, "held", [1, 2]].each do |v|
  hold.set(v)
  10_000.times { "garbage #{_1}" }
  GC.start
  p hold.get
end
p V::FALSE_CONST, V::NIL_CONST, V::SYM_CONST
# rb_scan_args_kw: 0 asks the call site, 1 always takes the Hash,
# 2 never, 3 takes a Hash in last position
h = { k: 1 }
def sk = (yield rescue [$!.class, $!.message])
[0, 1, 2, 3].each do |f|
  p [f, sk { V.scan_kw(f, h) }, sk { V.scan_kw(f, k: 1) }, sk { V.scan_kw(f, 2, h) }, sk { V.scan_kw(f, 2, **h) }, sk { V.scan_kw(f) }]
end
# rb_check_funcall: Qundef when the receiver does not respond (private counts)
class Quiet; private def secret = :s; def respond_to_missing?(n, p = false) = n == :ghost; def method_missing(n, *) = n == :ghost ? :boo : super; end
p V.check_call(1, :to_s), V.check_call(Quiet.new, :secret), V.check_call(Quiet.new, :ghost), V.check_call(Object.new, :nope)
# rb_str_tmp_new: a String of n writable bytes
p V.tmp_fill(5, "x".ord), V.tmp_fill(0, 0)
# rb_catch_obj / rb_throw_obj with Kernel#catch / #throw on either side
t = Object.new
p V.catch_(t, nil), V.catch_(t, :thrown), V.catch_(:sym, [1, 2])
p catch(:outer) { V.throw_(:outer, :from_c); :not_here }
p catch { |tag| V.catch_(tag, nil); V.throw_(tag, 42) }
p((V.throw_(:nobody, 1) rescue [$!.class, $!.message]))
p((V.catch_(:a, nil) { }).inspect, catch(:b) { V.catch_(:c, :inner) ; throw :b, :ruby })
