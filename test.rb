class S
  def g
    x = 0
    for i in 0..10
      x += @x
    end
    x
  end
end

class A < S
  def initialize
    @x = 1
    @y = 2
  end
end

class B < S
  def initialize
    @y = 10
    @x = 20
  end
end

x = [A.new, B.new, A.new, B.new, A.new, B.new, B.new, A.new, B.new, A.new, B.new]

50.times {
  $res = []
  for i in 0...x.size
    $res << x[i].g
  end
  puts $res
}