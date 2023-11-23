class S
  def f(x,y,z)
    x + y + z
  end
end

class C < S
  def f(*x)
    super *x
  end
end

puts C.new.f(3,4,5)