# Driven on CRuby and on monoruby by tests/capi.rs; the outputs must match.
module CapiData
  class Base
    def greet(x) = "base #{x}"
    def each_twice
      yield 1
      yield 2
    end
    def initialize(a) = @a = a
    attr_reader :a, :b
  end
end

require "capi_data"
D = CapiData

def t = (yield; :ok) rescue [$!.class, $!.message]

# TypedData: alloc function, initialize, readers, GC through dmark
pt = D::Point.new(1, 2.5, "origin")
p [pt.x, pt.y, pt.label, pt.class]
p D.type_name(pt), D.same_ptr?(pt), D.point?(pt), D.point?("no"), D.point?(nil)
p t { D::Point.new(1) }
p t { D.x_of("str") }
p t { D.x_of(nil) }
p t { D.x_of(D::Counter.new) }
p D.has_alloc?(D::Point), D.has_alloc?(D::Point3), D.has_alloc?(D::Nope)

# allocate without initialize, then the payload is read
raw = D::Point.allocate
p [raw.x, raw.y, raw.label]

# dup / clone go through initialize_copy
d = pt.dup
d.label = "copy"
p [d.x, d.y, d.label, pt.label, d.class]
c = pt.clone
p [c.x, c.label, c.frozen?]
p pt.frozen?
# rb_define_method makes initialize / initialize_copy private, as def does
p D::Point.private_instance_methods(false).sort, D::Point.public_instance_methods(false).include?(:initialize)

# a subclass defined in Ruby inherits the allocator
class D::Point < Object; end
class Sub < D::Point
  def initialize(x, y)
    super(x, y, "sub")
    @z = x + y
  end
  attr_reader :z
end
s = Sub.new(3, 4)
p [s.x, s.y, s.label, s.z, s.class, D.type_name(s), D.point?(s)]
p D.has_alloc?(Sub)

# a derived data type is accepted where the parent is expected
p3 = D::Point3.new(5, 6, :three)
p [p3.x, p3.label, D.type_name(p3), D.point?(p3)]
p t { D.x_of(p3) }

# no allocator
p t { D::Nope.new }
p t { D::Nope.allocate }

# untyped Data: Data_Make_Struct, Data_Get_Struct, DATA_PTR as an lvalue, a factory
ctr = D::Counter.new
p [ctr.incr, ctr.incr, ctr.reset, ctr.incr]
w = D::Counter.wrap(10)
p [w.incr, D.type_name(w), D.type_name(pt), D.type_name("s")]
p t { D.x_of(w) }

# values held by the payload survive a collection
pts = (0...200).map { |i| D::Point.new(i, i, "label#{i}") }
GC.start
p pts.map(&:label).uniq.size, pts[199].label, pts[7].x
p D.points_freed >= 0

# blocks from C
p D.map2([1, 2, 3]) { |x| x + 1 }
p D.with_index([:a, :b])
p D.first_over([1, 5, 10, 20], 7)
p D.first_over([1, 2], 7)
class Each
  def initialize(*xs) = @xs = xs
  def each
    @xs.each { |x| yield x }
  ensure
    puts "ensure ran"
  end
end
p D.break_first(Each.new(1, 2, 3))
p D.first_over(Each.new(1, 9), 5)
p t { D.raise_in_block(Each.new(4)) }
add = D.adder(10)
p [add.call(5), add.arity, [1, 2].map(&add)]
p D.iterate(Each.new(:x, :y))
p D.pass_block([1, 2, 3]) { |x| x * 10 }

# rb_call_super
dv = D::Derived.new(1, 2)
p [dv.a, dv.b]
p dv.greet("hi")
dv.each_twice { |i| p [:twice, i] }
p t { D.super_outside }

# Struct, allocation, anonymous classes
p D.structs
built = D.build(D::Base, 7)
p [built.class, built.a]
p D.anonymous
