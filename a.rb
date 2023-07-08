f = Fiber.new do
  outer = 42
  puts "invoked #{outer}"
  3.times {|i| puts "yield = #{Fiber.yield i}"}
  "terminated #{outer}"
end

6.times do |i|
  puts "resume = #{f.resume i}"
end