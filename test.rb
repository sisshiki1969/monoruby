class C
  def f(*x,**y, &z)
    
  end
end

c = C.new
100.times do
  c.f(1,y:2)
end