class S
  def f
  end
end

class C < S
  def f(x,y)
    super()
  end
end

100.times do
  C.new.f(1,2)
end