class Class
  def new(...)
    o = allocate
    o.initialize(...)
    o
  end
end

class C
  def initialize(x)
    @x = x
  end

  def show
    @x
  end
end

100.times do
  c = C.new(10)
  puts c.show  # Output: 10
end