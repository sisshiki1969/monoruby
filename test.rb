def f(x,y)
  g(x, y) + x + y
end

def g(x,y)
  h(x, y) * y
end

def h(x,y)
  x + y
end

100.times do
  1.times do
    f(1,2)
  end
end