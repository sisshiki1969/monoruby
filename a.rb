  fib = Fiber.new do
    loop { Fiber.yield }
  end

  100000.times do
    puts "#{fib.resume}"
  end