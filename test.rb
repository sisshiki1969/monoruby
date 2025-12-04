@a = 2

def f(a,b)
  if @a == 2
    a+b
  else
    a-b
  end
end

100.times do f(2,3) end