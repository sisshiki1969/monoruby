f = Fiber.new do
  outer = 42
  puts "invoked #{outer}"
  3.times {|i| puts "yield = #{Fiber.yield i}"}
  puts "terminated #{outer}"
end

4.times do |i|
  puts "resume = #{f.resume i}"
end