class A
  attr_reader "v"
  def f(x)
    @v=x
  end
end
a = A.new
a.f(42)
#puts A.instance_methods
puts a.v