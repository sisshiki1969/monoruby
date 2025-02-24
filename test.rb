def d(...)
  f(...)
end

def f(...)
  g(2,...)
end

def g(*x,a:nil)
  puts "#{x} #{a}"
end

25.times { d(10,11,a:5) }