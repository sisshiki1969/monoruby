require "capi_ids"
I = CapiIds

# One-character operator IDs.
p I.ops(7, 2)

# RETURN_ENUMERATOR / rb_frame_this_func.
t = I::Thing.new
e = t.each_twice(:x)
p e.class, e.to_a, t.each_twice(:y) { |v| print v }.class
p t.this_func

# One C function, two names: one method.
p t.method(:same) == t.method(:same_too), I.method(:same) == I.method(:same_too)
p t.same, t.same_too, I.same_too
p I::Thing.private_instance_methods(false).sort, t.send(:same_private)
p t.method(:same) == t.method(:same_private)

# The coerce protocol.
class Coercible
  def coerce(other) = [other, 10]
end
p I.coerce_cmp(3, Coercible.new), I.coerce_cmp(3, Object.new)
p I.coerce_bin(3, Coercible.new)
p (I.coerce_bin(3, Object.new) rescue [$!.class, $!.message])
p I.coerce_relop(3, Coercible.new)
p (I.coerce_relop(3, Object.new) rescue [$!.class, $!.message])

p I.numbers
p I.marshal([1, "two", :three, 4.0, [:a, nil]])

# $~ around an extension that matches internally.
"abc 123" =~ /(abc)/
p I.match_restoring("x 42 y"), $~ && $~[1]
p I.match_restoring("no digits"), $~ && $~[1]
m = I.match_clobbering("z 7")
p m && m[1], $~ && $~[1]

# Generic ivars (a String's) are copied; a plain Object's are not generic.
a = +"s"
a.instance_variable_set(:@x, 1)
a.instance_variable_set(:@y, [2])
b = I.copy_ivars(a, +"t")
p b.instance_variables.sort, b.instance_variable_get(:@y)
o = Object.new
o.instance_variable_set(:@z, 3)
p I.copy_ivars(o, Object.new).instance_variables
