class C
  attr_accessor :a
  def initialize
    @a=10
  end
  def f
    @a
  end
end

class C1 < C
  attr_accessor :a
  def initialize
    @a=20
  end
end

res = []

a = [C1.new, C1.new, C1.new, C1.new, C.new, C.new]
for i in 0..a.length - 1
  res << a[i].f
end

a = [C.new, C.new, C.new, C.new, C1.new, C1.new]
for i in 0..a.length - 1
  res << a[i].f
end

res