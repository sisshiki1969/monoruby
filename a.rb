for i in 0..30 do
  answer = []
  f = Fiber.new do
      outer = 42
      answer << "invoked #{outer}"
      3.times {|i|
          answer << "yield = #{Fiber.yield i}"
      }
      "terminated #{outer}"
  end
  4.times do |i|
    answer << "resume = #{f.resume i}"
  end
  puts "#{answer}"
end