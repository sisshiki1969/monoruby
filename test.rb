class C
  def f(x,y)
    x + y
  end
end

c = C.new
100.times do
  c.f(1,2)
end