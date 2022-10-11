class A
  attr_accessor :w
end
class B
  attr_accessor :w
end
a = A.new
res = []
for i in 0..20
  #puts i
  if i == 8
    puts "class-change"
    a = B.new
  end
  a.w = 42
  res << a.w
end
puts res.inspect