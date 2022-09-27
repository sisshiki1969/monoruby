class A
  attr_accessor :w
end
a = A.new
a.w = 42
for i in 0..10
  puts (a.w=35)
end