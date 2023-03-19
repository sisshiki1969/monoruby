class C
  def f(x,y,a)
    x+y+a
  end
end

class D < C
  def f(x,y,a:0)
    super x,y,a
  end
end

puts D.new.f(42, 100, a:50)