$res = []

class C
  def initialize(*a)
    $res << a.inspect
  end
end

100.times {
  C.new(1,2,3, a:1, b:2)
}

puts $res