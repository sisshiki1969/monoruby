class C
  attr_accessor :a
  def initialize
    @a=10
  end
  def f
    puts a
  end
end

class C1 < C
  attr_accessor :a
  def initialize
    @a=20
  end
end

a = [C1.new, C1.new, C1.new, C1.new, C1.new, C1.new, C1.new, C.new, C.new, C.new]
for i in 0..a.length - 1
  a[i].f
end