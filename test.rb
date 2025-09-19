class C
  def m
  end
  def f
    m
  end
end

c = C.new
100.times do
  c.f
end