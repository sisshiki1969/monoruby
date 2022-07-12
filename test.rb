def f(x)
  if x < 2
      dump
      1
  else
      x*f(x-1)
  end
end
f(5)