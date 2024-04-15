def f(x)
  x
end
  
j = 42
30.times {|i|
    f = ->(x) { x + j }
    puts f.call(i)
}