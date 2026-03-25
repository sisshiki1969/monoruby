fiber = Fiber.new { :done }
fiber.transfer
fiber.send(:transfer)
