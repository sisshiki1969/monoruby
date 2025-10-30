def f
  yield 100
end

200.times do
  f do |x, y|
    puts "x: #{x}, y: #{y}"
  end
end