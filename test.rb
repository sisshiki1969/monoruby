def d(...)
  f(...)
end

def f(...)
  g(1,2,...)
end

def g(*a,**b,&c)
  puts "#{a} #{b} #{c}"
end

30.times { d(10,a:5) }