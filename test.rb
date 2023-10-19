fib = Enumerator.new do |y|
    a = b = 1
    loop do
        y.<< a
        a, b = a + b, a
        if a > 100 then break end
    end
end
puts(fib.with_index do |x,i|
  puts "#{i} #{x}"
  x
end.inspect)
puts([5,4,3,2,1].map.with_index do |x,i|
  puts "#{i} #{x}"
  x
end.inspect)