class A
  attr_accessor :w,:x,:y,:z
  def initialize(x,y,z)
    @w=42
    @x=x
    @y=y
    @z=z
  end
end
a = A.new(3,7,11)
#puts A.instance_methods
puts a.x, a.y, a.z