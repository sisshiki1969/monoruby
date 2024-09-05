def f(*x)
  puts "#{x}"
end
20.times do
  f(a:1, **{b:2,c:3})
end