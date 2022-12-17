def g
  yield 1,2
end

def f(&p)
  g(&p)
end

puts f {|a,b| a+b}
