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
# rb_id2str / rb_id2name of the null ID
p V.null_id
# rb_respond_to / rb_obj_respond_to: the builtin respond_to? is answered
# from the method table (public only unless `priv`), then by a
# respond_to_missing? of the program's own; a respond_to? of the program's
# own is called, with the second argument only when `priv` is set and the
# method takes it
class Vis
  def pub = 1
  protected def prot = 2
  private def priv = 3
  def gone = 4
  undef_method :gone
end
class VisSub < Vis; public :priv; end
vis = Vis.new
p %i[pub prot priv gone nope to_s puts initialize].map { V.responds(vis, _1) }
p V.responds(VisSub.new, :priv), V.responds(VisSub.new, "pub"), V.responds(vis, "no such name ever")
p V.responds(nil, :to_a), V.responds(1, :+), V.responds(:s, :to_proc), V.responds(Vis, :new), V.responds(Vis, :pub)
def vis.single = 5
p V.responds(vis, :single), V.responds(Vis.new, :single)
class Ghostly
  def respond_to_missing?(name, all = false) = ($asked << [name, all]; name == :ghost || (all && name == :hidden))
end
$asked = []
p V.responds(Ghostly.new, :ghost), V.responds(Ghostly.new, :hidden), V.responds(Ghostly.new, :other), V.responds(Ghostly.new, :to_s)
p $asked
class TruthyMissing; def respond_to_missing?(*) = :yes; end
class NilMissing; def respond_to_missing?(*) = nil; end
p V.responds(TruthyMissing.new, :x), V.responds(NilMissing.new, :x)
class Own2
  def respond_to?(name, all = false) = ($asked << [name, all]; name == :mine || (all && name == :deep))
end
class Own1
  def respond_to?(name) = ($asked << [name]; name == :mine)
end
class OwnRest
  def respond_to?(*a) = ($asked << a; a.size == 2)
end
$asked = []
p V.responds(Own2.new, :mine), V.responds(Own2.new, :deep), V.responds(Own2.new, :to_s)
p V.responds(Own1.new, :mine), V.responds(Own1.new, :to_s), V.responds(OwnRest.new, :to_s)
p $asked
class Own3; def respond_to?(a, b, c) = true; end
p((V.responds1(Own3.new, :x, true) rescue [$!.class, $!.message]), (V.responds1(Own3.new, :x, false) rescue $!.class))
class Own0; def respond_to?(a = 1, b = 2, c = 3) = [a, b, c] == [:x, true, 3]; end
p V.responds1(Own0.new, :x, true), V.responds1(Own0.new, :x, false)
class Angry
  def respond_to_missing?(name, all) = raise(IOError, "missing #{name} #{all}")
end
class Furious
  def respond_to?(name, all = false) = raise(IOError, "asked #{name} #{all}")
end
p((V.responds(Angry.new, :x) rescue [$!.class, $!.message]), V.responds(Angry.new, :to_s))
p((V.responds1(Angry.new, :x, true) rescue [$!.class, $!.message]), (V.responds1(Furious.new, :x, true) rescue [$!.class, $!.message]))
class Bare < BasicObject
  def here = 1
  private def there = 2
end
p V.responds(Bare.new, :here), V.responds(Bare.new, :there), V.responds(Bare.new, :inspect), V.responds(BasicObject.new, :__id__)
class BareMissing < BasicObject; def respond_to_missing?(name, all) = name == :ghost; end
p V.responds(BareMissing.new, :ghost), V.responds(BareMissing.new, :nope)
# a refinement active where the extension was called from is seen
module VisRefine; refine(Vis) { def refined = 1; private def refined_priv = 2 }; end
using VisRefine
p vis.respond_to?(:refined), V.responds(vis, :refined), V.responds(vis, :refined_priv)
# redefining respond_to? after the first question is seen
late = Class.new { def m = 1 }
p V.responds(late.new, :m), V.responds(late.new, :zz)
late.class_eval { def respond_to?(name, all = false) = name == :zz }
p V.responds(late.new, :m), V.responds(late.new, :zz)
# rb_check_funcall asks with private methods included
p V.check_call(vis, :priv), V.check_call(vis, :prot), V.check_call(vis, :gone), V.check_call(Own2.new, :to_s).class, V.check_call(Own2.new, :pub)
