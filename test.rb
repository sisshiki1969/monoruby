def f(x,y)
  yield x,y
end

f(3,5) {|x,y| puts "#{x} & #{y}"}
f(4,1) {|x,y| puts "#{x+y}"}