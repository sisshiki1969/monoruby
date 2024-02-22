class C
  def f(x)
    x*100
  end
end

c = C.new

m = [*(0..15)]

20000000.times do |x|
  if x == 12
    m = c.method(:f)
  end
  m[x]
end