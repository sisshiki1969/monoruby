def g
  yield 1,2
end

def f(&p)
  k = 1.1
  puts g(&k)
  puts g(&p)
end
h = 39
f {|a,b| a+b+h}
