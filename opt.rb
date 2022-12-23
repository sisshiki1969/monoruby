def f(a,*x)
  puts "#{x}"
end

for i in 0..10
  f *[1,2,3,4,5,6,7,8,9]
end
