require "capi_reflect"
require "stringio"
R = CapiReflect

module Mixin
  def mixed = 1
end

class Base
  include Mixin
  A = 1
  B = 2
  @@cv = 3
  def pub(a, b = 1) = a
  def prot_m = 1
  protected :prot_m
  def priv_m(*) = 1
  private :priv_m
end

class Derived < Base; end

p R.ancestors(Derived)
obj = Base.new
def obj.single = 1
p R.methods(Base, obj)
p R.modules(Derived, Mixin)

victim = Class.new { const_set(:A, 1); const_set(:B, 2); class_variable_set(:@@cv, 3) }
o = Object.new
o.instance_variable_set(:@a, 1)
o.instance_variable_set(:@b, 2)
p R.remove(victim, o)

# rb_const_get_from / rb_const_defined_from: ancestors, but not Object.
p R.const_from(Derived, :A)
class Derived
  def self.const_missing(name) = [:missing, name]
end
p R.const_from(Derived, :Integer)
p R.const_from(Object, :Integer)

s = +"str"
s.instance_variable_set(:@x, 1)
s.instance_variable_set(:@y, 2)
p R.ivars(s)

klass = Class.new do
  class_variable_set(:@@cvar, :foo)
  instance_variable_set(:@ivar, :bar)
end
p R.ivar_foreach(klass)

p R.boundp(Base, :pub), R.boundp(Base, :prot_m), R.boundp(Base, :priv_m), R.boundp(Base, :nope)
p R.method_info(obj, Base, :pub), R.method_info(obj, Base, :priv_m)
p R.cvar(Class.new)

outer = Module.new
p R.define_ids(outer, Base).map { |v| v.is_a?(Module) && v.name&.start_with?("#") ? :anon : v }
p outer.constants.sort

p R.class_info(Derived, obj).map { |v| v.is_a?(Class) && v.name.nil? ? :anon : v }
p((R.inherited_p(Derived, 1) rescue [$!.class, $!.message]))

a = [1, 2]
a << a
p R.inspect_rec(a), R.inspect_rec([1, [2, [3]]])
p R.rec_pointer
x = [1]
x << x
y = [1]
y << y
p R.eq_rec([1, [2]], [1, [2]]), R.eq_rec([1, [2]], [1, [3]]), R.eq_rec(x, y)
z = [1, 2]
z << [3, z]
p R.hash_rec([1, [2, 3]]), R.hash_rec(z)

def capture_warnings
  old = $stderr
  $stderr = StringIO.new
  yield
  $stderr.string.lines.map { |l| l.sub(/\A.*?warning: /, "warning: ") }
ensure
  $stderr = old
end
deprecated = Warning[:deprecated]
experimental = Warning[:experimental]
Warning[:deprecated] = true
Warning[:experimental] = true
p capture_warnings { R.warn(1) }
$VERBOSE = true
p capture_warnings { R.warn(2) }
Warning[:deprecated] = false
p capture_warnings { R.warn(3) }
$VERBOSE = nil
p capture_warnings { R.warn(4) }
$VERBOSE = false
Warning[:deprecated] = deprecated
Warning[:experimental] = experimental

p R.backtrace.to_s.include?(File.basename(__FILE__))

p R.aseq(1.step(10, 3), 20, 0)
p R.aseq((1..10).step(2), 20, 0)
p((R.aseq((1...10) % 3, 5, 0) rescue [$!.class, $!.message]))
p R.aseq((-5..-1).step(1), 1, 1)
p R.aseq(10.step(1, -2), 20, 1)
p R.aseq(1..4, 10, 0)
p R.aseq(:nope, 10, 0)
p R.range_beg_len(1..3, 10, 0), R.range_beg_len(-20..3, 10, 0), R.range_beg_len(2..., 10, 0)
p((R.range_beg_len(-20..3, 10, 1) rescue [$!.class, $!.message]))

p R.path2class("Base"), R.path2class_cstr("Comparable")
module Outer; module Inner; end; NotAClass = 1; end
p R.path2class("Outer::Inner")
["Outer::Nope", "Nope", "Outer::NotAClass", "Outer::String", "", "#<Class:0x1>"].each do |path|
  p((R.path2class(path) rescue [$!.class, $!.message]))
end

class Vis
  def pub = :pub
  def blk = yield
  private def priv = :priv
  protected def prot = :prot
end
v = Vis.new
p R.public_call(v, :pub)
p((R.public_call(v, :priv) rescue [$!.class, $!.message[/private|protected/]]))
p((R.public_call(v, :prot) rescue [$!.class, $!.message[/private|protected/]]))
p R.with_block(v, :blk) { :block_ran }
p((R.with_block(v, :priv) { } rescue [$!.class]))

class Kw
  def take(**kw) = kw
end
hashlike = Object.new
def hashlike.to_hash = { a: 1 }
p R.kw_call(Kw.new, :take, { b: 2 }), R.kw_call(Kw.new, :take, hashlike)
p((R.kw_call(Kw.new, :take, 1) rescue [$!.class, $!.message]))
p((R.rescue_bad_class rescue [$!.class, $!.message]))

p R.streams
io = StringIO.new
$stdout = io
same = R.streams.first
$stdout = STDOUT
p same

p R.types(Time.now)
str_sub = Class.new(String)
p R.alloc_funcs(Class.new, str_sub)
p R.instance_exec([3])

R.end_proc($stdout)
puts "main done"
